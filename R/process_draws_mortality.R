source("R/header.R")
source("R/aux.R")

use_raw_data <- FALSE

pop <- readRDS(file = "DATA/mortality_bias_data.RDS")
geo_info <- pop$geo_info
pop <- pop$mort
# Population per gender (L x Y x A)
pop_fem <- pop %>% filter(gender == "Female") %>% dplyr::select(mun, year, age, population) %>% acast(mun ~ year ~ age, value.var = "population")
pop_mal <- pop %>% filter(gender ==   "Male") %>% dplyr::select(mun, year, age, population) %>% acast(mun ~ year ~ age, value.var = "population")
lls <- pop$mun %>% unique() %>% as.character() %>% as.numeric()
aas <- pop$age %>% unique() 
yys <- pop$year %>% unique()
# Deaths per gender (L x Y x A)
dth_fem <- pop %>% filter(gender == "Female") %>% dplyr::select(mun, year, age, deaths) %>% acast(mun ~ year ~ age, value.var = "deaths")
dth_mal <- pop %>% filter(gender ==   "Male") %>% dplyr::select(mun, year, age, deaths) %>% acast(mun ~ year ~ age, value.var = "deaths")

p <- "mortality_v1_1.stan"
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

std_death_rate <- pop %>% dplyr::select(mun, year, age, gender, deaths, population, death_rate)
std_death_rate <- std_death_rate %>% left_join(y = p_nat, by = c("age", "gender"))
std_death_rate <- std_death_rate %>% mutate(p_nat_total = p_nat_total)
std_death_rate <- std_death_rate %>% mutate(std_death_rate = ((p_nat / p_nat_total) * death_rate))
std_death_rate <- std_death_rate %>% group_by(mun, year) %>% summarise(std_death_rate = sum(std_death_rate)) %>% ungroup()
std_death_rate_mat <- acast(std_death_rate, mun ~ year, value.var = "std_death_rate") # (L x Y)

# For each municipality, I must compute: 
# (1) Multiplier for municipalities and years.
# (2) Log-death rate for all municipalities, years and genders (female and male).
# (3) Number of deaths for all municipalities, years and genders (female and male).
# In (3), notice that the number of deaths should not be smaller than the raw data. 
# To do so, it suffices to set the multiplier in (1) to pmax(multiplier, 1).

##################################################
# (1) COMPUTE MULTIPLIER 
##################################################

tmp_cum <- c()
multiplier_file <- paste("FITTED/DATA/multiplier_",  strsplit(p, "\\.")[[1]][1], ".RDS", sep = "")
if (!file.exists(multiplier_file)) {
  multiplier <- array(data = 0, dim = c(L, Y, sample_size))
  
  error_count <- 0 # ~0.171%
  pb <- txtProgressBar(min = 1, max = L, initial = 0) 
  for (l in 1:L) {
    for (y in 1:Y) {
      tmp_alpha_0 <- c(draws[, paste("alpha_0[", y, "]", sep = "")])
      tmp_alpha_1 <- c(draws[, paste("alpha_1[", y, "]", sep = "")])
      tmp_mpi_mun <- mpi_municip[l]
      tmp_linear_mean <- tmp_alpha_0 + tmp_alpha_1 * tmp_mpi_mun # + rnorm(n = sample_size, mean = 0, sd = c(draws[, "std_death_rate_capital_sigma"]))
      if (any(tmp_linear_mean < 0)) { error_count <- error_count + 1 }
      tmp_log_std_mun <- log(pmax(tmp_linear_mean, 1e-12))
      tmp_log_std_nat <- log(c(draws[, paste("std_death_rate_nat[", y, "]", sep = "")]))
      
      tmp_mul <- exp(tmp_log_std_mun - tmp_log_std_nat)
      
      ##############################
      # Empirical (for comparison) #
      ##############################
      
      tmp_log_std_mun_emp <- log(max(std_death_rate_mat[l, y], 1e-12))
      tmp_mul_emp         <- exp(tmp_log_std_mun_emp - tmp_log_std_nat)
      tmp_cum <- c(tmp_cum, range(tmp_mul_emp))
      
      ##############################
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
# (1) COMPUTE LOG-DEATH-RATE FOR BOTH GENDERS
##################################################

pop_mun_sum <- unname(apply(X = pop_2018_mat, MARGIN = 1, FUN = sum))

tmp_deaths <- list()
tmp_dth_rt <- list()
# pb <- txtProgressBar(min = 1, max = L, initial = 0) 
for (l in 1:L) {
  print(paste(sprintf("%04d", l), " (of ", L, ")", sep = ""))
  
  log_death_rate_fem <- array(data = 0, dim = c(Y, A, sample_size)) # Temporarily defined for each location
  log_death_rate_mal <- array(data = 0, dim = c(Y, A, sample_size))
  deaths_fem <-  array(data = 0, dim = c(Y, A, sample_size))
  deaths_mal <-  array(data = 0, dim = c(Y, A, sample_size))
  dth_rt_fem <-  array(data = 0, dim = c(Y, A, sample_size))
  dth_rt_mal <-  array(data = 0, dim = c(Y, A, sample_size))
  
  for (y in 1:Y) {
    for (a in 1:A) {
      # Fitted national rates
      tmp_death_rate_nat_fem <- c(draws[, paste("inv_logit_death_rate_nat[1,", y, ",", a, "]", sep = "")])
      tmp_death_rate_nat_mal <- c(draws[, paste("inv_logit_death_rate_nat[2,", y, ",", a, "]", sep = "")])
      
      # Population factors
      # tmp_pop_mun_fem <- exp(log(pop_mun_sum[l]) - log(pop_2018_mat[l, 1, a]))
      # tmp_pop_mun_mal <- exp(log(pop_mun_sum[l]) - log(pop_2018_mat[l, 2, a])) # CHECK IT, WHAT IS THE NUMERATOR
      # 
      # tmp_pop_nat_fem <- exp(log(p_nat_mat[1, a]) - log(p_nat_total))
      # tmp_pop_nat_mal <- exp(log(p_nat_mat[2, a]) - log(p_nat_total)) # CHECK IT, WHAT IS THE DENOMINATOR
      
      # New rates (municipality level)
      # log_death_rate_fem[y, a, ] <- log(multiplier[l, y, ]) + log(tmp_pop_mun_fem) + log(tmp_pop_nat_fem) + log(inv_logit(tmp_death_rate_nat_fem))
      # log_death_rate_mal[y, a, ] <- log(multiplier[l, y, ]) + log(tmp_pop_mun_mal) + log(tmp_pop_nat_mal) + log(inv_logit(tmp_death_rate_nat_mal))
      
      log_death_rate_fem[y, a, ] <- log(multiplier[l, y, ]) + log(inv_logit(tmp_death_rate_nat_fem))
      log_death_rate_mal[y, a, ] <- log(multiplier[l, y, ]) + log(inv_logit(tmp_death_rate_nat_mal))
      
      prob_fem <- pmin(exp(log_death_rate_fem[y, a, ]), 1)
      prob_mal <- pmin(exp(log_death_rate_mal[y, a, ]), 1)
      
      if (use_raw_data) {
        tmp_deaths_fem <- pmax(dth_fem[l, y, a], rbinom(n = sample_size, size = pop_fem[l, y, a], prob = prob_fem))
        tmp_deaths_mal <- pmax(dth_mal[l, y, a], rbinom(n = sample_size, size = pop_mal[l, y, a], prob = prob_mal))
        # tmp_deaths_fem <- pmax(dth_fem[l, y, a], (pop_fem[l, y, a] * prob_fem))
        # tmp_deaths_mal <- pmax(dth_mal[l, y, a], (pop_mal[l, y, a] * prob_mal))
      } else {
        tmp_deaths_fem <- rbinom(n = sample_size, size = pop_fem[l, y, a], prob = prob_fem)
        tmp_deaths_mal <- rbinom(n = sample_size, size = pop_mal[l, y, a], prob = prob_mal)
      }
      
      ##############################
      # Deal with impossible cases #
      ##############################
      
      # Zero population
      if (pop_fem[l, y, a] == 0) { tmp_deaths_fem <- pmin(0, tmp_deaths_fem) }
      if (pop_mal[l, y, a] == 0) { tmp_deaths_mal <- pmin(0, tmp_deaths_mal) }
      
      # Number of deaths should be smaller or equal than the population 
      tmp_deaths_fem <- pmin(pop_fem[l, y, a], tmp_deaths_fem)
      tmp_deaths_mal <- pmin(pop_mal[l, y, a], tmp_deaths_mal)
    
      ##############################
      
      deaths_fem[y, a, ] <- tmp_deaths_fem
      deaths_mal[y, a, ] <- tmp_deaths_mal
      dth_rt_fem[y, a, ] <- prob_fem
      dth_rt_mal[y, a, ] <- prob_mal
    }
  }
  
  if (use_raw_data) {
    saveRDS(object = list(death_rate_fem = dth_rt_fem, 
                          death_rate_mal = dth_rt_mal, 
                          deaths_fem = deaths_fem,
                          deaths_mal = deaths_mal), file = paste("FITTED/DATA/MORTALITY/list_", lls[l], ".RDS", sep = ""))
  }
  
  # Compute summary statistics
  
  ##############################
  # FEMALE
  ##############################
  
  # RATE
  
  tmp_dth_rt_fem_mean <- apply(X = dth_rt_fem, MARGIN = c(1, 2), FUN = mean)
  tmp_dth_rt_fem_sd   <- apply(X = dth_rt_fem, MARGIN = c(1, 2), FUN = sd  )
  tmp_dth_rt_fem_medn <- apply(X = dth_rt_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.500)
  tmp_dth_rt_fem_Q025 <- apply(X = dth_rt_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.025)
  tmp_dth_rt_fem_Q975 <- apply(X = dth_rt_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.975)
  
  rownames(tmp_dth_rt_fem_mean) <- yys
  colnames(tmp_dth_rt_fem_mean) <- aas
  tmp_dth_rt_fem_mean <- melt(tmp_dth_rt_fem_mean)
  tmp_dth_rt_fem_sd   <- melt(tmp_dth_rt_fem_sd  )[, 3]
  tmp_dth_rt_fem_medn <- melt(tmp_dth_rt_fem_medn)[, 3]
  tmp_dth_rt_fem_Q025 <- melt(tmp_dth_rt_fem_Q025)[, 3]
  tmp_dth_rt_fem_Q975 <- melt(tmp_dth_rt_fem_Q975)[, 3]
  
  tmp_dth_rt_fem <- cbind(tmp_dth_rt_fem_mean, tmp_dth_rt_fem_sd, tmp_dth_rt_fem_medn, tmp_dth_rt_fem_Q025, tmp_dth_rt_fem_Q975)
  colnames(tmp_dth_rt_fem) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
  tmp_dth_rt_fem <- cbind(Location = lls[l], Gender = "Female", tmp_dth_rt_fem)
  
  # COUNT
  
  tmp_deaths_fem_mean <- apply(X = deaths_fem, MARGIN = c(1, 2), FUN = mean)
  tmp_deaths_fem_sd   <- apply(X = deaths_fem, MARGIN = c(1, 2), FUN = sd  )
  tmp_deaths_fem_medn <- apply(X = deaths_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.500)
  tmp_deaths_fem_Q025 <- apply(X = deaths_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.025)
  tmp_deaths_fem_Q975 <- apply(X = deaths_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.975)
  
  rownames(tmp_deaths_fem_mean) <- yys
  colnames(tmp_deaths_fem_mean) <- aas
  tmp_deaths_fem_mean <- melt(tmp_deaths_fem_mean)
  tmp_deaths_fem_sd   <- melt(tmp_deaths_fem_sd  )[, 3]
  tmp_deaths_fem_medn <- melt(tmp_deaths_fem_medn)[, 3]
  tmp_deaths_fem_Q025 <- melt(tmp_deaths_fem_Q025)[, 3]
  tmp_deaths_fem_Q975 <- melt(tmp_deaths_fem_Q975)[, 3]
  
  tmp_deaths_fem <- cbind(tmp_deaths_fem_mean, tmp_deaths_fem_sd, tmp_deaths_fem_medn, tmp_deaths_fem_Q025, tmp_deaths_fem_Q975)
  colnames(tmp_deaths_fem) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
  tmp_deaths_fem <- cbind(Location = lls[l], Gender = "Female", tmp_deaths_fem)
  
  ##############################
  # MALE
  ##############################

  # RATE
  
  tmp_dth_rt_mal_mean <- apply(X = dth_rt_mal, MARGIN = c(1, 2), FUN = mean)
  tmp_dth_rt_mal_sd   <- apply(X = dth_rt_mal, MARGIN = c(1, 2), FUN = sd  )
  tmp_dth_rt_mal_medn <- apply(X = dth_rt_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.500)
  tmp_dth_rt_mal_Q025 <- apply(X = dth_rt_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.025)
  tmp_dth_rt_mal_Q975 <- apply(X = dth_rt_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.975)
  
  rownames(tmp_dth_rt_mal_mean) <- yys
  colnames(tmp_dth_rt_mal_mean) <- aas
  tmp_dth_rt_mal_mean <- melt(tmp_dth_rt_mal_mean)
  tmp_dth_rt_mal_sd   <- melt(tmp_dth_rt_mal_sd  )[, 3]
  tmp_dth_rt_mal_medn <- melt(tmp_dth_rt_mal_medn)[, 3]
  tmp_dth_rt_mal_Q025 <- melt(tmp_dth_rt_mal_Q025)[, 3]
  tmp_dth_rt_mal_Q975 <- melt(tmp_dth_rt_mal_Q975)[, 3]
  
  tmp_dth_rt_mal <- cbind(tmp_dth_rt_mal_mean, tmp_dth_rt_mal_sd, tmp_dth_rt_mal_medn, tmp_dth_rt_mal_Q025, tmp_dth_rt_mal_Q975)
  colnames(tmp_dth_rt_mal) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
  tmp_dth_rt_mal <- cbind(Location = lls[l], Gender = "Male", tmp_dth_rt_mal)
  
  # COUNT
  
  tmp_deaths_mal_mean <- apply(X = deaths_mal, MARGIN = c(1, 2), FUN = mean)
  tmp_deaths_mal_sd   <- apply(X = deaths_mal, MARGIN = c(1, 2), FUN = sd  )
  tmp_deaths_mal_medn <- apply(X = deaths_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.500)
  tmp_deaths_mal_Q025 <- apply(X = deaths_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.025)
  tmp_deaths_mal_Q975 <- apply(X = deaths_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.975)
  
  rownames(tmp_deaths_mal_mean) <- yys
  colnames(tmp_deaths_mal_mean) <- aas
  tmp_deaths_mal_mean <- melt(tmp_deaths_mal_mean)
  tmp_deaths_mal_sd   <- melt(tmp_deaths_mal_sd  )[, 3]
  tmp_deaths_mal_medn <- melt(tmp_deaths_mal_medn)[, 3]
  tmp_deaths_mal_Q025 <- melt(tmp_deaths_mal_Q025)[, 3]
  tmp_deaths_mal_Q975 <- melt(tmp_deaths_mal_Q975)[, 3]
  
  tmp_deaths_mal <- cbind(tmp_deaths_mal_mean, tmp_deaths_mal_sd, tmp_deaths_mal_medn, tmp_deaths_mal_Q025, tmp_deaths_mal_Q975)
  colnames(tmp_deaths_mal) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
  tmp_deaths_mal <- cbind(Location = lls[l], Gender = "Male", tmp_deaths_mal)
  
  ##########
  
  tmp_deaths[[l]] <- rbind(tmp_deaths_fem, tmp_deaths_mal)
  tmp_dth_rt[[l]] <- rbind(tmp_dth_rt_fem, tmp_dth_rt_mal)
  
  # setTxtProgressBar(pb, l)
}
# close(pb)

updated_deaths <- do.call(rbind, tmp_deaths)
updated_dth_rt <- do.call(rbind, tmp_dth_rt)

saveRDS(object = updated_deaths, file = paste("FITTED/DATA/new_new_new_count_",  strsplit(p, "\\.")[[1]][1], "_empirical_", as.character(use_raw_data), ".RDS", sep = ""))
saveRDS(object = updated_dth_rt, file = paste("FITTED/DATA/new_new_new_rates_",  strsplit(p, "\\.")[[1]][1], "_empirical_", as.character(use_raw_data), ".RDS", sep = ""))

