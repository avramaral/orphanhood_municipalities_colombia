source("R/header.R")
source("R/header_plotting.R")
source("R/aux.R")

pop <- readRDS(file = "DATA/fertility_bias_data.RDS")
pop <- pop$fert
# Population per gender 
pop_fem <- pop %>% filter(gender == "Female") %>% dplyr::select(mun, year, age, population, births) %>% group_by(year, age) %>% summarise(population = sum(population), births = sum(births)) %>% ungroup()
pop_fem_mat <- pop_fem %>% acast(year ~ age, value.var = "population") # (Y, A)
bth_fem_mat <- pop_fem %>% acast(year ~ age, value.var = "births")     # (Y, A)
pop_mal <- pop %>% filter(gender ==   "Male") %>% dplyr::select(mun, year, age, population, births) %>% group_by(year, age) %>% summarise(population = sum(population), births = sum(births)) %>% ungroup()
pop_mal_mat <- pop_mal %>% acast(year ~ age, value.var = "population") # (Y, A)
bth_mal_mat <- pop_mal %>% acast(year ~ age, value.var = "births")     # (Y, A)

lls <- pop$mun  %>% unique() %>% as.character() %>% as.numeric()
aas <- pop$age  %>% unique() 
yys <- pop$year %>% unique()

p <- "fertility_v2_1.stan"
d <- readRDS(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))

data  <- d$data
draws <- d$draws

Y = data$Y
# A = data$A
A_fem = data$A_fem
A_mal = data$A_mal
G = data$G
L = data$L
C = data$C
mpi_municip <- data$mpi_municip
sample_size <- nrow(draws[, 1])

# `gender`, `year`, and `age group`

# fit_birth_rate_nat_fem <- array(data = 0, dim = c(Y, A, sample_size))
# fit_birth_rate_nat_mal <- array(data = 0, dim = c(Y, A, sample_size)) 
# 
# fit_bth_counts_nat_fem <- array(data = 0, dim = c(Y, A, sample_size))
# fit_bth_counts_nat_mal <- array(data = 0, dim = c(Y, A, sample_size)) 

fit_birth_rate_nat_fem <- array(data = 0, dim = c(Y, A_fem, sample_size))
fit_birth_rate_nat_mal <- array(data = 0, dim = c(Y, A_mal, sample_size)) 

fit_bth_counts_nat_fem <- array(data = 0, dim = c(Y, A_fem, sample_size))
fit_bth_counts_nat_mal <- array(data = 0, dim = c(Y, A_mal, sample_size)) 


for (y in 1:Y) {
  print(paste(y, " (of ", Y, ")", sep = ""))
  for (a in 1:A_fem) {
    tmp_phi_fem <- c(draws[, paste("phi_dispe[1]", sep = "")])
    fit_birth_rate_nat_fem[y, a, ] <- exp(c(draws[, paste("inv_log_fertility_rate_nat_fem[", y, ",", a, "]", sep = "")]))
    fit_bth_counts_nat_fem[y, a, ] <- rnbinom(n = sample_size, size = tmp_phi_fem, mu = exp(log(fit_birth_rate_nat_fem[y, a, ]) + log(pop_fem_mat[y, a])))
  }
  for (a in 1:A_mal) {
    tmp_phi_mal <- c(draws[, paste("phi_dispe[2]", sep = "")])
    fit_birth_rate_nat_mal[y, a, ] <- exp(c(draws[, paste("inv_log_fertility_rate_nat_mal[", y, ",", a, "]", sep = "")]))
    fit_bth_counts_nat_mal[y, a, ] <- rnbinom(n = sample_size, size = tmp_phi_mal, mu = exp(log(fit_birth_rate_nat_mal[y, a, ]) + log(pop_mal_mat[y, a])))
  }
}

# for (y in 1:Y) {
#   print(paste(y, " (of ", Y, ")", sep = ""))
#   for (a in 1:A) {
#     # tmp_phi_dispe_fem <- c(draws[, paste("phi_dispe_year_fem[", y, "]", sep = "")])
#     # tmp_phi_dispe_mal <- c(draws[, paste("phi_dispe_year_mal[", y, "]", sep = "")])
#     
#     tmp_phi_fem <- c(draws[, paste("phi_dispe_fem[1]", sep = "")])
#     tmp_phi_mal <- c(draws[, paste("phi_dispe_mal[2]", sep = "")])
#     
#     fit_birth_rate_nat_fem[y, a, ] <- exp(c(draws[, paste("inv_log_fertility_rate_nat[1,", y, ",", a, "]", sep = "")]))
#     fit_birth_rate_nat_mal[y, a, ] <- exp(c(draws[, paste("inv_log_fertility_rate_nat[2,", y, ",", a, "]", sep = "")]))
#     
#     fit_bth_counts_nat_fem[y, a, ] <- rnbinom(n = sample_size, size = tmp_phi_fem, mu = exp(log(fit_birth_rate_nat_fem[y, a, ]) + log(pop_fem_mat[y, a])))
#     fit_bth_counts_nat_mal[y, a, ] <- rnbinom(n = sample_size, size = tmp_phi_mal, mu = exp(log(fit_birth_rate_nat_mal[y, a, ]) + log(pop_mal_mat[y, a])))
#   
#     #fit_bth_counts_nat_fem[y, a, ] <- exp(log(fit_birth_rate_nat_fem[y, a, ]) + log(pop_fem_mat[y, a]))
#     #fit_bth_counts_nat_mal[y, a, ] <- exp(log(fit_birth_rate_nat_mal[y, a, ]) + log(pop_mal_mat[y, a]))
#   }
# }


fit_bth_counts_nat_fem <- apply(X = fit_bth_counts_nat_fem, MARGIN = c(1, 3), FUN = sum) # Sum over the age groups
fit_bth_counts_nat_mal <- apply(X = fit_bth_counts_nat_mal, MARGIN = c(1, 3), FUN = sum) # Sum over the age groups

tmp_bth_fem_mean <- apply(X = fit_bth_counts_nat_fem, MARGIN = c(1), FUN = mean) # Summarize by `year` (1) only, i.e., aggregate `age` (2)
tmp_bth_fem_sd   <- apply(X = fit_bth_counts_nat_fem, MARGIN = c(1), FUN = sd  )
tmp_bth_fem_medn <- apply(X = fit_bth_counts_nat_fem, MARGIN = c(1), FUN = quantile, probs = 0.500)
tmp_bth_fem_Q025 <- apply(X = fit_bth_counts_nat_fem, MARGIN = c(1), FUN = quantile, probs = 0.025)
tmp_bth_fem_Q975 <- apply(X = fit_bth_counts_nat_fem, MARGIN = c(1), FUN = quantile, probs = 0.975)

tmp_bth_mal_mean <- apply(X = fit_bth_counts_nat_mal, MARGIN = c(1), FUN = mean)
tmp_bth_mal_sd   <- apply(X = fit_bth_counts_nat_mal, MARGIN = c(1), FUN = sd  )
tmp_bth_mal_medn <- apply(X = fit_bth_counts_nat_mal, MARGIN = c(1), FUN = quantile, probs = 0.500)
tmp_bth_mal_Q025 <- apply(X = fit_bth_counts_nat_mal, MARGIN = c(1), FUN = quantile, probs = 0.025)
tmp_bth_mal_Q975 <- apply(X = fit_bth_counts_nat_mal, MARGIN = c(1), FUN = quantile, probs = 0.975)

tmp_bth_fem_mean <- as.matrix(tmp_bth_fem_mean); tmp_bth_fem_sd <- as.matrix(tmp_bth_fem_sd); tmp_bth_fem_medn <- as.matrix(tmp_bth_fem_medn); tmp_bth_fem_Q025 <- as.matrix(tmp_bth_fem_Q025); tmp_bth_fem_Q975 <- as.matrix(tmp_bth_fem_Q975)
tmp_bth_mal_mean <- as.matrix(tmp_bth_mal_mean); tmp_bth_mal_sd <- as.matrix(tmp_bth_mal_sd); tmp_bth_mal_medn <- as.matrix(tmp_bth_mal_medn); tmp_bth_mal_Q025 <- as.matrix(tmp_bth_mal_Q025); tmp_bth_mal_Q975 <- as.matrix(tmp_bth_mal_Q975)
rownames(tmp_bth_fem_mean) <- yys; rownames(tmp_bth_mal_mean) <- yys
# colnames(tmp_bth_fem_mean) <- aas; colnames(tmp_bth_mal_mean) <- aas

# Female 
tmp_bth_fem_mean <- melt(tmp_bth_fem_mean)[,]
tmp_bth_fem_sd   <- melt(tmp_bth_fem_sd  )[, 3]
tmp_bth_fem_medn <- melt(tmp_bth_fem_medn)[, 3]
tmp_bth_fem_Q025 <- melt(tmp_bth_fem_Q025)[, 3]
tmp_bth_fem_Q975 <- melt(tmp_bth_fem_Q975)[, 3]
tmp_bth_fem <- cbind(tmp_bth_fem_mean, tmp_bth_fem_sd, tmp_bth_fem_medn, tmp_bth_fem_Q025, tmp_bth_fem_Q975)
colnames(tmp_bth_fem) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
tmp_bth_fem <- cbind(Gender = "Female", tmp_bth_fem)

# Male
tmp_bth_mal_mean <- melt(tmp_bth_mal_mean)
tmp_bth_mal_sd   <- melt(tmp_bth_mal_sd  )[, 3]
tmp_bth_mal_medn <- melt(tmp_bth_mal_medn)[, 3]
tmp_bth_mal_Q025 <- melt(tmp_bth_mal_Q025)[, 3]
tmp_bth_mal_Q975 <- melt(tmp_bth_mal_Q975)[, 3]
tmp_bth_mal <- cbind(tmp_bth_mal_mean, tmp_bth_mal_sd, tmp_bth_mal_medn, tmp_bth_mal_Q025, tmp_bth_mal_Q975)
colnames(tmp_bth_mal) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
tmp_bth_mal <- cbind(Gender = "Male", tmp_bth_mal)

fit_nat_births <- as_tibble(rbind(tmp_bth_fem, tmp_bth_mal)) %>% dplyr::select(-Age)
raw_nat_births <- pop %>% dplyr::select(year, mun, gender, age, births) %>% group_by(year, gender) %>% summarise(births = sum(births)) %>% ungroup() %>% dplyr::select(gender, year, births) %>% arrange(gender, year) %>% rename(Gender = gender, Year = year, Births = births)

# PLOTTING
merged_data <- cbind(fit_nat_births, raw_nat_births[, 3]) %>% as_tibble()
female_data <- merged_data %>% filter(Gender == "Female") %>% dplyr::select(-Gender)
  male_data <- merged_data %>% filter(Gender ==   "Male") %>% dplyr::select(-Gender)


p_fem <- ggplot(female_data, aes(x = Births, y = Median, color = as.factor(Year))) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = Q025, ymax = Q975), width = 1e3) +
  geom_abline(slope = 1, intercept = 0, colour = "#00000033", linetype = "solid") +
  labs(title = "Female", x = "Empirical births", y = "Fitted deaths", color = "Year") +
  theme_bw() +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma) + 
  # coord_fixed() +
  theme(legend.position = "none", text = element_text(size = 12, family = "LM Roman 10"))

p_mal <- ggplot(  male_data, aes(x = Births, y = Median, color = as.factor(Year))) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = Q025, ymax = Q975), width = 1e3) +
  geom_abline(slope = 1, intercept = 0, colour = "#00000033", linetype = "solid") +
  labs(title = "Male",   x = "Empirical births", y = "Fitted births", color = "Year") +
  theme_bw() +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma) + 
  # coord_fixed() +
  theme(legend.position = "none", text = element_text(size = 12, family = "LM Roman 10"))

p_tot <- (p_fem + p_mal) + plot_layout(guides = "collect") & theme(legend.position = "right")

ggsave(filename = paste("IMAGES/national_births_comparison.jpeg", sep = ""), plot = p_tot , width = 3000, height = 1200, units = c("px"), dpi = 300, bg = "white")



