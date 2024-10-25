source("R/header.R")
source("R/aux.R")

pop <- readRDS(file = "DATA/fertility_bias_data.RDS")
geo_info <- pop$geo_info
pop <- pop$fert
pop %>% filter(is.infinite(fertility_rate))
# Population per gender (L x Y x A)
pop_fem <- pop %>% filter(gender == "Female") %>% dplyr::select(mun, year, age, population) %>% acast(mun ~ year ~ age, value.var = "population")
pop_mal <- pop %>% filter(gender ==   "Male") %>% dplyr::select(mun, year, age, population) %>% acast(mun ~ year ~ age, value.var = "population")
lls <- pop$mun %>% unique() %>% as.character() %>% as.numeric()
aas <- pop$age %>% unique() 
yys <- pop$year %>% unique()
# Births per gender (L x Y x A)
brt_fem <- pop %>% filter(gender == "Female") %>% dplyr::select(mun, year, age, births) %>% acast(mun ~ year ~ age, value.var = "births")
brt_mal <- pop %>% filter(gender ==   "Male") %>% dplyr::select(mun, year, age, births) %>% acast(mun ~ year ~ age, value.var = "births")

p <- "fertility_v1_2.stan"
d <- readRDS(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))

data  <- d$data
draws <- d$draws

Y = data$Y
A = data$A
G = data$G
L = data$L
C = data$C
mpi_municip <- data$mpi_municip
sample_size <- nrow(draws[, 1])

# Compute standardized death rate based on the raw data, so we can use it as a denominator for the multiplier 

# Aggregated population based on the census year (i.e., 2018)
pop_2018     <- pop %>% filter(year == 2018) %>% dplyr::select(mun, gender, age, population)
pop_2018     <- pop_2018 %>% mutate(population = ifelse(population == 0, 1, population)) 
pop_2018_mat <- acast(pop_2018, mun ~ gender ~ age, value.var = "population") # (L X G x A)
p_nat        <- pop_2018 %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
p_nat_mat    <- acast(p_nat, gender ~ age, value.var = "p_nat") # (G x A)
p_nat_total  <- sum(pop_2018$population)

std_fertility_rate <- pop %>% dplyr::select(mun, year, age, gender, births, population, fertility_rate)
std_fertility_rate <- std_fertility_rate %>% left_join(y = p_nat, by = c("age", "gender"))
std_fertility_rate <- std_fertility_rate %>% mutate(p_nat_total = p_nat_total)
std_fertility_rate <- std_fertility_rate %>% mutate(std_fertility_rate = ((p_nat / p_nat_total) * fertility_rate))
std_fertility_rate <- std_fertility_rate %>% group_by(mun, year) %>% summarise(std_fertility_rate = sum(std_fertility_rate)) %>% ungroup()
std_fertility_rate_mat <- acast(std_fertility_rate, mun ~ year, value.var = "std_fertility_rate") # (L x Y)

# For each municipality, I must compute: 
# (1) Multiplier for municipalities and years (always shifting upwards).
# (2) Log-birth rate for all municipalities, years and genders (female and male).
# (3) Number of births for all municipalities, years and genders (female and male).

##################################################
# COMPUTE MULTIPLIER 
##################################################

# Added variability: draw from Normal with std. deviation `c(draws[, "std_death_rate_capital_sigma"])`
multiplier_file <- paste("FITTED/DATA/multiplier_",  strsplit(p, "\\.")[[1]][1], ".RDS", sep = "")
if (!file.exists(multiplier_file)) {
  multiplier <- array(data = 0, dim = c(L, Y, sample_size))
  
  error_count <- 0
  pb <- txtProgressBar(min = 1, max = L, initial = 0) 
  for (l in 1:L) {
    for (y in 1:Y) {
      tmp_alpha_0 <- c(draws[, paste("alpha_0[", y, "]", sep = "")])
      tmp_alpha_1 <- c(draws[, paste("alpha_1[", y, "]", sep = "")])
      tmp_mpi_mun <- mpi_municip[l]
      tmp_linear_mean <- tmp_alpha_0 + tmp_alpha_1 * tmp_mpi_mun
      if (any(tmp_linear_mean < 0)) { error_count <- error_count + 1 }
      tmp_log_std_mun <- log(pmax(tmp_linear_mean, 1e-12))
      tmp_log_std_nat <- log(c(draws[, paste("std_fertility_rate_nat[", y, "]", sep = "")]))
      
      tmp_mul <- exp(tmp_log_std_mun - tmp_log_std_nat)
      
      ##############################
      # Empirical 
      ##############################
      
      tmp_log_std_mun_emp <- log(max(std_fertility_rate_mat[l, y], 1e-12))
      tmp_mul_emp         <- exp(tmp_log_std_mun_emp - tmp_log_std_nat)
      
      ##############################
      
      multiplier[l, y, ] <- pmax(tmp_mul, tmp_mul_emp, 1)
    }
    setTxtProgressBar(pb, l)
  }
  close(pb)
  saveRDS(object = multiplier, file = multiplier_file)
} else {
  multiplier <- readRDS(file = multiplier_file)
}

##################################################
# COMPUTE LOG-FERTILITY-RATE (COUNT) - ALL GENDERS 
##################################################

pop_mun_sum <- unname(apply(X = pop_2018_mat, MARGIN = 1, FUN = sum))

tmp_births <- list()
tmp_bth_rt <- list()

for (l in 1:L) {
  print(paste(sprintf("%04d", l), " (of ", L, ")", sep = ""))
  
  log_fertility_rate_fem <- array(data = 0, dim = c(Y, A, sample_size)) 
  log_fertility_rate_mal <- array(data = 0, dim = c(Y, A, sample_size))
  births_fem <-  array(data = 0, dim = c(Y, A, sample_size))
  births_mal <-  array(data = 0, dim = c(Y, A, sample_size))
  bth_rt_fem <-  array(data = 0, dim = c(Y, A, sample_size))
  bth_rt_mal <-  array(data = 0, dim = c(Y, A, sample_size))
  
  for (y in 1:Y) {
    for (a in 1:A) {
      # Only if Negative Binomial
      tmp_phi_dispe <- c(draws[, "phi_dispe"])
      
      # Fitted national rates
      tmp_fertility_rate_nat_fem <- c(draws[, paste("inv_log_fertility_rate_nat[1,", y, ",", a, "]", sep = "")]) # Assuming Poisson or Negative Binomial for National count (otherwise, `logit_`)
      tmp_fertility_rate_nat_mal <- c(draws[, paste("inv_log_fertility_rate_nat[2,", y, ",", a, "]", sep = "")])
      
      log_fertility_rate_fem[y, a, ] <- log(multiplier[l, y, ]) + tmp_fertility_rate_nat_fem # Apply `log(inv_logit(x))` for the second term, if Binomial
      log_fertility_rate_mal[y, a, ] <- log(multiplier[l, y, ]) + tmp_fertility_rate_nat_mal
      
      tmp_births_fem <- rnbinom(n = sample_size, size = tmp_phi_dispe, mu = exp(log_fertility_rate_fem[y, a, ] + log(pop_fem[l, y, a])))
      tmp_births_mal <- rnbinom(n = sample_size, size = tmp_phi_dispe, mu = exp(log_fertility_rate_mal[y, a, ] + log(pop_mal[l, y, a])))
      
      ##############################
      # Deal with impossible cases #
      ##############################
      
      # Zero population
      if (pop_fem[l, y, a] == 0) { tmp_births_fem <- pmin(0, tmp_births_fem) }
      if (pop_mal[l, y, a] == 0) { tmp_births_mal <- pmin(0, tmp_births_mal) }
      
      ##############################
      
      births_fem[y, a, ] <- tmp_births_fem
      births_mal[y, a, ] <- tmp_births_mal
      bth_rt_fem[y, a, ] <- exp(log_fertility_rate_fem[y, a, ])
      bth_rt_mal[y, a, ] <- exp(log_fertility_rate_mal[y, a, ])
    }
  }
  
  saveRDS(object = list(fertility_rate_fem = exp(log_fertility_rate_fem), 
                        fertility_rate_mal = exp(log_fertility_rate_mal), 
                        births_fem = births_fem,
                        births_mal = births_mal), file = paste("FITTED/DATA/FERTILITY/list_", lls[l], ".RDS", sep = ""))
  
  # Compute summary statistics
  
  ##############################
  # FEMALE
  ##############################
  
  # RATE
  
  tmp_bth_rt_fem_mean <- apply(X = bth_rt_fem, MARGIN = c(1, 2), FUN = mean)
  tmp_bth_rt_fem_sd   <- apply(X = bth_rt_fem, MARGIN = c(1, 2), FUN = sd  )
  tmp_bth_rt_fem_medn <- apply(X = bth_rt_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.500)
  tmp_bth_rt_fem_Q025 <- apply(X = bth_rt_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.025)
  tmp_bth_rt_fem_Q975 <- apply(X = bth_rt_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.975)
  
  rownames(tmp_bth_rt_fem_mean) <- yys
  colnames(tmp_bth_rt_fem_mean) <- aas
  tmp_bth_rt_fem_mean <- melt(tmp_bth_rt_fem_mean)
  tmp_bth_rt_fem_sd   <- melt(tmp_bth_rt_fem_sd  )[, 3]
  tmp_bth_rt_fem_medn <- melt(tmp_bth_rt_fem_medn)[, 3]
  tmp_bth_rt_fem_Q025 <- melt(tmp_bth_rt_fem_Q025)[, 3]
  tmp_bth_rt_fem_Q975 <- melt(tmp_bth_rt_fem_Q975)[, 3]
  
  tmp_bth_rt_fem <- cbind(tmp_bth_rt_fem_mean, tmp_bth_rt_fem_sd, tmp_bth_rt_fem_medn, tmp_bth_rt_fem_Q025, tmp_bth_rt_fem_Q975)
  colnames(tmp_bth_rt_fem) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
  tmp_bth_rt_fem <- cbind(Location = lls[l], Gender = "Female", tmp_bth_rt_fem)
  
  # COUNT 
  
  tmp_births_fem_mean <- apply(X = births_fem, MARGIN = c(1, 2), FUN = mean)
  tmp_births_fem_sd   <- apply(X = births_fem, MARGIN = c(1, 2), FUN = sd  )
  tmp_births_fem_medn <- apply(X = births_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.500)
  tmp_births_fem_Q025 <- apply(X = births_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.025)
  tmp_births_fem_Q975 <- apply(X = births_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.975)
  
  rownames(tmp_births_fem_mean) <- yys
  colnames(tmp_births_fem_mean) <- aas
  tmp_births_fem_mean <- melt(tmp_births_fem_mean)
  tmp_births_fem_sd   <- melt(tmp_births_fem_sd  )[, 3]
  tmp_births_fem_medn <- melt(tmp_births_fem_medn)[, 3]
  tmp_births_fem_Q025 <- melt(tmp_births_fem_Q025)[, 3]
  tmp_births_fem_Q975 <- melt(tmp_births_fem_Q975)[, 3]
  
  tmp_births_fem <- cbind(tmp_births_fem_mean, tmp_births_fem_sd, tmp_births_fem_medn, tmp_births_fem_Q025, tmp_births_fem_Q975)
  colnames(tmp_births_fem) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
  tmp_births_fem <- cbind(Location = lls[l], Gender = "Female", tmp_births_fem)
  
  ##############################
  # MALE
  ##############################
  
  # RATE
  
  tmp_bth_rt_mal_mean <- apply(X = bth_rt_mal, MARGIN = c(1, 2), FUN = mean)
  tmp_bth_rt_mal_sd   <- apply(X = bth_rt_mal, MARGIN = c(1, 2), FUN = sd  )
  tmp_bth_rt_mal_medn <- apply(X = bth_rt_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.500)
  tmp_bth_rt_mal_Q025 <- apply(X = bth_rt_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.025)
  tmp_bth_rt_mal_Q975 <- apply(X = bth_rt_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.975)
  
  rownames(tmp_bth_rt_mal_mean) <- yys
  colnames(tmp_bth_rt_mal_mean) <- aas
  tmp_bth_rt_mal_mean <- melt(tmp_bth_rt_mal_mean)
  tmp_bth_rt_mal_sd   <- melt(tmp_bth_rt_mal_sd  )[, 3]
  tmp_bth_rt_mal_medn <- melt(tmp_bth_rt_mal_medn)[, 3]
  tmp_bth_rt_mal_Q025 <- melt(tmp_bth_rt_mal_Q025)[, 3]
  tmp_bth_rt_mal_Q975 <- melt(tmp_bth_rt_mal_Q975)[, 3]
  
  tmp_bth_rt_mal <- cbind(tmp_bth_rt_mal_mean, tmp_bth_rt_mal_sd, tmp_bth_rt_mal_medn, tmp_bth_rt_mal_Q025, tmp_bth_rt_mal_Q975)
  colnames(tmp_bth_rt_mal) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
  tmp_bth_rt_mal <- cbind(Location = lls[l], Gender = "Male", tmp_bth_rt_mal)
  
  # COUNT 
  
  tmp_births_mal_mean <- apply(X = births_mal, MARGIN = c(1, 2), FUN = mean)
  tmp_births_mal_sd   <- apply(X = births_mal, MARGIN = c(1, 2), FUN = sd  )
  tmp_births_mal_medn <- apply(X = births_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.500)
  tmp_births_mal_Q025 <- apply(X = births_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.025)
  tmp_births_mal_Q975 <- apply(X = births_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.975)
  
  rownames(tmp_births_mal_mean) <- yys
  colnames(tmp_births_mal_mean) <- aas
  tmp_births_mal_mean <- melt(tmp_births_mal_mean)
  tmp_births_mal_sd   <- melt(tmp_births_mal_sd  )[, 3]
  tmp_births_mal_medn <- melt(tmp_births_mal_medn)[, 3]
  tmp_births_mal_Q025 <- melt(tmp_births_mal_Q025)[, 3]
  tmp_births_mal_Q975 <- melt(tmp_births_mal_Q975)[, 3]
  
  tmp_births_mal <- cbind(tmp_births_mal_mean, tmp_births_mal_sd, tmp_births_mal_medn, tmp_births_mal_Q025, tmp_births_mal_Q975)
  colnames(tmp_births_mal) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
  tmp_births_mal <- cbind(Location = lls[l], Gender = "Male", tmp_births_mal)
  
  ##########
  
  tmp_births[[l]] <- rbind(tmp_births_fem, tmp_births_mal)
  tmp_bth_rt[[l]] <- rbind(tmp_bth_rt_fem, tmp_bth_rt_mal)
}

updated_births <- do.call(rbind, tmp_births)
updated_bth_rt <- do.call(rbind, tmp_bth_rt)

saveRDS(object = updated_births, file = paste("FITTED/DATA/count_",  strsplit(p, "\\.")[[1]][1], ".RDS", sep = ""))
saveRDS(object = updated_bth_rt, file = paste("FITTED/DATA/rates_",  strsplit(p, "\\.")[[1]][1], ".RDS", sep = ""))
