source("R/header.R")
source("R/header_plotting.R")
source("R/aux.R")

pop <- readRDS(file = "DATA/mortality_bias_data.RDS")
pop <- pop$mort
# Population per gender 
pop_fem <- pop %>% filter(gender == "Female") %>% dplyr::select(mun, year, age, population, deaths) %>% group_by(year, age) %>% summarise(population = sum(population), deaths = sum(deaths)) %>% ungroup()
pop_fem_mat <- pop_fem %>% acast(year ~ age, value.var = "population") # (Y, A)
dth_fem_mat <- pop_fem %>% acast(year ~ age, value.var = "deaths")     # (Y, A)
pop_mal <- pop %>% filter(gender ==   "Male") %>% dplyr::select(mun, year, age, population, deaths) %>% group_by(year, age) %>% summarise(population = sum(population), deaths = sum(deaths)) %>% ungroup()
pop_mal_mat <- pop_mal %>% acast(year ~ age, value.var = "population") # (Y, A)
dth_mal_mat <- pop_mal %>% acast(year ~ age, value.var = "deaths")     # (Y, A)

lls <- pop$mun  %>% unique() %>% as.character() %>% as.numeric()
aas <- pop$age  %>% unique() 
yys <- pop$year %>% unique()

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


# `gender`, `year`, and `age group`

fit_death_rate_nat_fem <- array(data = 0, dim = c(Y, A, sample_size))
fit_death_rate_nat_mal <- array(data = 0, dim = c(Y, A, sample_size)) 

fit_dth_counts_nat_fem <- array(data = 0, dim = c(Y, A, sample_size))
fit_dth_counts_nat_mal <- array(data = 0, dim = c(Y, A, sample_size)) 

for (y in 1:Y) {
  print(paste(y, " (of ", Y, ")", sep = ""))
  for (a in 1:A) {
    fit_death_rate_nat_fem[y, a, ] <- inv_logit(c(draws[, paste("inv_logit_death_rate_nat[1,", y, ",", a, "]", sep = "")]))
    fit_death_rate_nat_mal[y, a, ] <- inv_logit(c(draws[, paste("inv_logit_death_rate_nat[2,", y, ",", a, "]", sep = "")]))
    
    fit_dth_counts_nat_fem[y, a, ] <- rbinom(n = sample_size, size = pop_fem_mat[y, a], prob = fit_death_rate_nat_fem[y, a, ])
    fit_dth_counts_nat_mal[y, a, ] <- rbinom(n = sample_size, size = pop_mal_mat[y, a], prob = fit_death_rate_nat_mal[y, a, ])
  }
}


fit_dth_counts_nat_fem <- apply(X = fit_dth_counts_nat_fem, MARGIN = c(1, 3), FUN = sum) # Sum over the age groups
fit_dth_counts_nat_mal <- apply(X = fit_dth_counts_nat_mal, MARGIN = c(1, 3), FUN = sum) # Sum over the age groups

tmp_dth_fem_mean <- apply(X = fit_dth_counts_nat_fem, MARGIN = c(1), FUN = mean) # Summarize by `year` (1) only, i.e., aggregate `age` (2)
tmp_dth_fem_sd   <- apply(X = fit_dth_counts_nat_fem, MARGIN = c(1), FUN = sd  )
tmp_dth_fem_medn <- apply(X = fit_dth_counts_nat_fem, MARGIN = c(1), FUN = quantile, probs = 0.500)
tmp_dth_fem_Q025 <- apply(X = fit_dth_counts_nat_fem, MARGIN = c(1), FUN = quantile, probs = 0.025)
tmp_dth_fem_Q975 <- apply(X = fit_dth_counts_nat_fem, MARGIN = c(1), FUN = quantile, probs = 0.975)

tmp_dth_mal_mean <- apply(X = fit_dth_counts_nat_mal, MARGIN = c(1), FUN = mean)
tmp_dth_mal_sd   <- apply(X = fit_dth_counts_nat_mal, MARGIN = c(1), FUN = sd  )
tmp_dth_mal_medn <- apply(X = fit_dth_counts_nat_mal, MARGIN = c(1), FUN = quantile, probs = 0.500)
tmp_dth_mal_Q025 <- apply(X = fit_dth_counts_nat_mal, MARGIN = c(1), FUN = quantile, probs = 0.025)
tmp_dth_mal_Q975 <- apply(X = fit_dth_counts_nat_mal, MARGIN = c(1), FUN = quantile, probs = 0.975)

tmp_dth_fem_mean <- as.matrix(tmp_dth_fem_mean); tmp_dth_fem_sd <- as.matrix(tmp_dth_fem_sd); tmp_dth_fem_medn <- as.matrix(tmp_dth_fem_medn); tmp_dth_fem_Q025 <- as.matrix(tmp_dth_fem_Q025); tmp_dth_fem_Q975 <- as.matrix(tmp_dth_fem_Q975)
tmp_dth_mal_mean <- as.matrix(tmp_dth_mal_mean); tmp_dth_mal_sd <- as.matrix(tmp_dth_mal_sd); tmp_dth_mal_medn <- as.matrix(tmp_dth_mal_medn); tmp_dth_mal_Q025 <- as.matrix(tmp_dth_mal_Q025); tmp_dth_mal_Q975 <- as.matrix(tmp_dth_mal_Q975)
rownames(tmp_dth_fem_mean) <- yys; rownames(tmp_dth_mal_mean) <- yys
# colnames(tmp_dth_fem_mean) <- aas; colnames(tmp_dth_mal_mean) <- aas

# Female 
tmp_dth_fem_mean <- melt(tmp_dth_fem_mean)[,]
tmp_dth_fem_sd   <- melt(tmp_dth_fem_sd  )[, 3]
tmp_dth_fem_medn <- melt(tmp_dth_fem_medn)[, 3]
tmp_dth_fem_Q025 <- melt(tmp_dth_fem_Q025)[, 3]
tmp_dth_fem_Q975 <- melt(tmp_dth_fem_Q975)[, 3]
tmp_dth_fem <- cbind(tmp_dth_fem_mean, tmp_dth_fem_sd, tmp_dth_fem_medn, tmp_dth_fem_Q025, tmp_dth_fem_Q975)
colnames(tmp_dth_fem) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
tmp_dth_fem <- cbind(Gender = "Female", tmp_dth_fem)

# Male
tmp_dth_mal_mean <- melt(tmp_dth_mal_mean)
tmp_dth_mal_sd   <- melt(tmp_dth_mal_sd  )[, 3]
tmp_dth_mal_medn <- melt(tmp_dth_mal_medn)[, 3]
tmp_dth_mal_Q025 <- melt(tmp_dth_mal_Q025)[, 3]
tmp_dth_mal_Q975 <- melt(tmp_dth_mal_Q975)[, 3]
tmp_dth_mal <- cbind(tmp_dth_mal_mean, tmp_dth_mal_sd, tmp_dth_mal_medn, tmp_dth_mal_Q025, tmp_dth_mal_Q975)
colnames(tmp_dth_mal) <- c("Year", "Age", "Mean", "Sd", "Median", "Q025", "Q975")
tmp_dth_mal <- cbind(Gender = "Male", tmp_dth_mal)

fit_nat_deaths <- as_tibble(rbind(tmp_dth_fem, tmp_dth_mal)) %>% dplyr::select(-Age)
raw_nat_deaths <- pop %>% dplyr::select(year, mun, gender, age, deaths) %>% group_by(year, gender) %>% summarise(deaths = sum(deaths)) %>% ungroup() %>% dplyr::select(gender, year, deaths) %>% arrange(gender, year) %>% rename(Gender = gender, Year = year, Deaths = deaths)

# PLOTTING
merged_data <- cbind(fit_nat_deaths, raw_nat_deaths[, 3]) %>% as_tibble()
female_data <- merged_data %>% filter(Gender == "Female") %>% dplyr::select(-Gender)
  male_data <- merged_data %>% filter(Gender ==   "Male") %>% dplyr::select(-Gender)
  
  
p_fem <- ggplot(female_data, aes(x = Deaths, y = Mean, color = as.factor(Year))) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = Q025, ymax = Q975), width = 1e3) +
  geom_abline(slope = 1, intercept = 0, colour = "#00000033", linetype = "solid") +
  labs(title = "Female", x = "Empirical deaths", y = "Fitted deaths", color = "Year") +
  theme_bw() +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma) + 
  coord_fixed() +
  theme(legend.position = "none", text = element_text(size = 12, family = "LM Roman 10"))

p_mal <- ggplot(  male_data, aes(x = Deaths, y = Mean, color = as.factor(Year))) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = Q025, ymax = Q975), width = 1e3) +
  geom_abline(slope = 1, intercept = 0, colour = "#00000033", linetype = "solid") +
  labs(title = "Male",   x = "Empirical deaths", y = "Fitted deaths", color = "Year") +
  theme_bw() +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma) + 
  coord_fixed() +
  theme(legend.position = "none", text = element_text(size = 12, family = "LM Roman 10"))

p_tot <- (p_fem + p_mal) + plot_layout(guides = "collect") & theme(legend.position = "right")
 
ggsave(filename = paste("IMAGES/national_deaths_comparison.jpeg", sep = ""), plot = p_tot , width = 3000, height = 1200, units = c("px"), dpi = 300, bg = "white")



