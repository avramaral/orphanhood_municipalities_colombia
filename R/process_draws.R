source("R/header.R")

inv_logit <- function (x, ...) { exp(x) / (1 + exp(x)) }

pop <- readRDS(file = "DATA/mortality_bias_data.RDS")
pop <- pop$mort
# Population per gender (L x Y x A)
pop_fem <- pop %>% filter(gender == "Female") %>% dplyr::select(mun, year, age, population) %>% acast(mun ~ year ~ age, value.var = "population")
pop_mal <- pop %>% filter(gender ==   "Male") %>% dplyr::select(mun, year, age, population) %>% acast(mun ~ year ~ age, value.var = "population")
lls <- pop$mun %>% unique() %>% as.character() %>% as.numeric()
aas <- pop$age %>% unique() 
yys <- pop$year %>% unique()

p <- "mortality_v1.stan"
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

# For each municipality, I must compute: 
# (1) Multiplier for municipalities and years.
# (2) Log-death rate for all municipalities, years and genders (female and male).
# (3) Number of deaths for all municipalities, years and genders (female and male).
# In (3), notice that the number of deaths should not be smaller than the raw data. 
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
      tmp_log_std_nat <- log(c(draws[, paste("std_death_rate_nat[", y, "]", sep = "")]))
      tmp_mul <- tmp_log_std_mun - tmp_log_std_nat
      tmp_mul <- pmax(tmp_mul, 1) # Alternatively, I could check the final number of deaths
      
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
# (1) COMPUTE LOG-DEATH-RATE FOR BOTH GENDERS
##################################################

tmp_deaths <- list()
pb <- txtProgressBar(min = 1, max = L, initial = 0) 
for (l in 1:L) {
  
  log_death_rate_fem <- array(data = 0, dim = c(Y, A, sample_size)) # Temporarily defined for each location
  log_death_rate_mal <- array(data = 0, dim = c(Y, A, sample_size))
  deaths_fem <-  array(data = 0, dim = c(Y, A, sample_size))
  deaths_mal <-  array(data = 0, dim = c(Y, A, sample_size))
  
  for (y in 1:Y) {
    for (a in 1:A) {
      tmp_death_rate_nat_fem <- c(draws[, paste("inv_logit_death_rate_nat[1,", y, ",", a, "]", sep = "")])
      tmp_death_rate_nat_mal <- c(draws[, paste("inv_logit_death_rate_nat[2,", y, ",", a, "]", sep = "")])
      
      log_death_rate_fem[y, a, ] <- multiplier[l, y, ] + log(inv_logit(tmp_death_rate_nat_fem))
      log_death_rate_mal[y, a, ] <- multiplier[l, y, ] + log(inv_logit(tmp_death_rate_nat_mal))
      deaths_fem[y, a, ] <- exp(log(pop_fem[l, y, a]) + log_death_rate_fem[y, a, ])
      deaths_mal[y, a, ] <- exp(log(pop_mal[l, y, a]) + log_death_rate_mal[y, a, ])
    }
  }
  
  saveRDS(object = list(log_death_rate_fem = log_death_rate_fem, 
                        log_death_rate_mal = log_death_rate_mal, 
                        deaths_fem = deaths_fem,
                        deaths_mal = deaths_mal), file = paste("FITTED/DATA/MORTALITY/list_", lls[l], ".RDS", sep = ""))
  
  # Compute summary statistics
  
  ##############################
  # FEMALE
  ##############################
  
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
  # FEMALE
  ##############################

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
  
  tmp_deaths[[l]] <- rbind(tmp_deaths_fem, tmp_deaths_mal)
  
  setTxtProgressBar(pb, l)
}
close(pb)

updated_deaths <- do.call(rbind, tmp_deaths)
saveRDS(object = updated_deaths, file = paste("FITTED/DATA/count_",  strsplit(p, "\\.")[[1]][1], ".RDS", sep = ""))























