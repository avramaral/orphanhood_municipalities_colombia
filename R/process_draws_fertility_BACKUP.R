source("R/header.R")
source("R/aux.R")

pop <- readRDS(file = "DATA/fertility_bias_data.RDS")
pop <- pop$fert
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
pop_2018    <- pop %>% filter(year == 2018) %>% dplyr::select(mun, gender, age, population)
p_nat       <- pop_2018 %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
p_nat_total <- sum(pop_2018$population)

std_fertility_rate <- pop %>% dplyr::select(mun, year, age, gender, births, population, fertility_rate)
std_fertility_rate <- std_fertility_rate %>% left_join(y = p_nat, by = c("age", "gender"))
std_fertility_rate <- std_fertility_rate %>% mutate(p_nat_total = p_nat_total)
std_fertility_rate <- std_fertility_rate %>% mutate(std_fertility_rate = ((p_nat / p_nat_total) * fertility_rate))
std_fertility_rate <- std_fertility_rate %>% group_by(mun, year) %>% summarise(std_fertility_rate = sum(std_fertility_rate)) %>% ungroup()
std_fertility_rate_mat <- acast(std_fertility_rate, mun ~ year, value.var = "std_fertility_rate") # (L x Y)

# For each municipality, I must compute: 
# (1) Multiplier for municipalities and years.
# (2) Log-birth rate for all municipalities, years and genders (female and male).
# (3) Number of births for all municipalities, years and genders (female and male).
# In (3), notice that the number of births should not be smaller than the raw data. 
# To do so, it suffices to set the multiplier in (1) to max(multiplier, 1).

##################################################
# (1) COMPUTE MULTIPLIER 
##################################################

multiplier_file <- paste("FITTED/DATA/multiplier_",  strsplit(p, "\\.")[[1]][1], ".RDS", sep = "")
if (!file.exists(multiplier_file)) {
  multiplier <- array(data = 0, dim = c(L, Y, sample_size))
  
  pb <- txtProgressBar(min = 1, max = L, initial = 0) 
  for (l in 1:L) {
    for (y in 1:Y) {
      tmp_alpha_0 <- c(draws[, paste("alpha_0[", y, "]", sep = "")])
      tmp_alpha_1 <- c(draws[, paste("alpha_0[", y, "]", sep = "")])
      tmp_mpi_mun <- mpi_municip[l]
      tmp_log_std_mun <- log(tmp_alpha_0 + tmp_alpha_1 * tmp_mpi_mun)
      
      # OLD approach: denominator is the standardized fertility rate at the national level
      # tmp_log_std_nat <- log(c(draws[, paste("std_fertility_rate_nat[", y, "]", sep = "")]))
      # tmp_mul <- exp(tmp_log_std_mun - tmp_log_std_nat)
      
      # NEW approach: denominator is the standardized fertility rate at each location, based on the raw data
      
      tmp_std_birth_r <- std_fertility_rate_mat[l, y]
      tmp_log_std_loc <- ifelse(tmp_std_birth_r == 0, 0, log(tmp_std_birth_r)) # CHECK THIS
      # tmp_log_std_loc <- log(tmp_std_death_r + 1e-12)
      tmp_mul <- exp(tmp_log_std_mun - tmp_log_std_loc)

      multiplier[l, y, ] <- tmp_mul
    }
    setTxtProgressBar(pb, l)
  }
  close(pb)
  saveRDS(object = multiplier, file = multiplier_file)
} else {
  multiplier <- readRDS(file = multiplier_file)
}

##################################################
# (1) COMPUTE LOG-FERTILITY-RATE FOR BOTH GENDERS
##################################################

tmp_births <- list()
pb <- txtProgressBar(min = 1, max = L, initial = 0) 
for (l in 1:L) {
  print(l)
  
  log_fertility_rate_fem <- array(data = 0, dim = c(Y, A, sample_size)) # Temporarily defined for each location
  log_fertility_rate_mal <- array(data = 0, dim = c(Y, A, sample_size))
  births_fem <-  array(data = 0, dim = c(Y, A, sample_size))
  births_mal <-  array(data = 0, dim = c(Y, A, sample_size))
  
  for (y in 1:Y) {
    for (a in 1:A) {
      tmp_fertility_rate_nat_fem <- c(draws[, paste("inv_log_fertility_rate_nat[1,", y, ",", a, "]", sep = "")]) # Assuming Poisson or Negative Binomial for National count (otherwise, `logit_`)
      tmp_fertility_rate_nat_mal <- c(draws[, paste("inv_log_fertility_rate_nat[2,", y, ",", a, "]", sep = "")])
      
      log_fertility_rate_fem[y, a, ] <- log(multiplier[l, y, ]) + tmp_fertility_rate_nat_fem # Apply `log(inv_logit(x))` for the second term, if Binomial
      log_fertility_rate_mal[y, a, ] <- log(multiplier[l, y, ]) + tmp_fertility_rate_nat_mal
      
      # Only if Negative Binomial
      tmp_phi_dispe <- c(draws[, "phi_dispe"])
      
      # Assuming Negative Binomial, i.e., change the distribution if required
      
          # Unnecessary reparameterization
          # size_fem <- (exp(log_fertility_rate_fem[y, a, ]) ** 2) / tmp_phi_dispe
          # prob_fem <- size_fem / (size_fem + (exp(log_fertility_rate_fem[y, a, ]) ** 2))
          # 
          # size_mal <- (exp(log_fertility_rate_mal[y, a, ]) ** 2) / tmp_phi_dispe
          # prob_mal <- size_mal / (size_mal + (exp(log_fertility_rate_mal[y, a, ]) ** 2))
          # 
          # births_fem[y, a, ] <- pmax(brt_fem[l, y, a], rnbinom(n = sample_size, size = size_fem, prob = prob_fem))
          # births_mal[y, a, ] <- pmax(brt_mal[l, y, a], rnbinom(n = sample_size, size = size_mal, prob = prob_mal))
      
      
      tmp_births_fem <- pmax(brt_fem[l, y, a], rnbinom(n = sample_size, size = tmp_phi_dispe, mu = exp(log_fertility_rate_fem[y, a, ] + log(pop_fem[l, y, a]))))
      tmp_births_mal <- pmax(brt_mal[l, y, a], rnbinom(n = sample_size, size = tmp_phi_dispe, mu = exp(log_fertility_rate_mal[y, a, ] + log(pop_mal[l, y, a]))))
      
      ##############################
      # Deal with impossible cases #
      ##############################
      
      # Zero population
      if (pop_fem[l, y, a] == 0) { tmp_births_fem <- pmin(0, tmp_births_fem) }
      if (pop_mal[l, y, a] == 0) { tmp_births_mal <- pmin(0, tmp_births_mal) }
      
      ##############################
      
      births_fem[y, a, ] <- tmp_births_fem
      births_mal[y, a, ] <- tmp_births_mal
      
    }
  }
  
  saveRDS(object = list(log_fertility_rate_fem = log_fertility_rate_fem, 
                        log_fertility_rate_mal = log_fertility_rate_mal, 
                        births_fem = births_fem,
                        births_mal = births_mal), file = paste("FITTED/DATA/FERTILITY/list_", lls[l], ".RDS", sep = ""))
  
  # Compute summary statistics
  
  ##############################
  # FEMALE
  ##############################
  
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
  
  tmp_births[[l]] <- rbind(tmp_births_fem, tmp_births_mal)
  
  setTxtProgressBar(pb, l)
}
close(pb)

updated_births <- do.call(rbind, tmp_births)
saveRDS(object = updated_births, file = paste("FITTED/DATA/count_",  strsplit(p, "\\.")[[1]][1], ".RDS", sep = ""))
