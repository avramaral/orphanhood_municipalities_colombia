source("R/header.R")
source("R/aux.R")

#############
# Mortality #
#############

p_mort <- "mortality_v1_1.stan"
d_mort <- readRDS(file = paste("FITTED/", strsplit(p_mort, "\\.")[[1]][1], "_dat.RDS", sep = ""))

data_mort  <- d_mort$data
draws_mort <- d_mort$draws

Y = data_mort$Y
A = data_mort$A
G = data_mort$G
L = data_mort$L
C = data_mort$C
mpi_municip <- data_mort$mpi_municip
sample_size <- nrow(draws_mort[, 1])

gp_sigma_posterior        <- c(draws_mort[, "gp_sigma"])
gp_length_scale_posterior <- c(draws_mort[, "gp_length_scale"])
alpha_0_posterior <- matrix(data = 0, nrow = sample_size, ncol = Y)
alpha_1_posterior <- matrix(data = 0, nrow = sample_size, ncol = Y)
for (y in 1:Y) {
  alpha_0_posterior[, y] <- c(draws_mort[, paste("alpha_0[", y, "]", sep = "")])
  alpha_1_posterior[, y] <- c(draws_mort[, paste("alpha_1[", y, "]", sep = "")])
}

gp_sigma_prior <- rnorm(n = 1e6, mean = 0, sd = 1); gp_sigma_prior <- gp_sigma_prior[gp_sigma_prior >= 0][1:sample_size]
gp_length_scale_prior <- 1 / rgamma(n = 1e6, shape = 5, scale = 1); gp_length_scale_prior <- gp_length_scale_prior[gp_length_scale_prior >= 0][1:sample_size]
alpha_0_prior <- rnorm(n = sample_size, mean = 0, sd = 0.0025)
alpha_1_prior <- rnorm(n = sample_size, mean = 0, sd = 1)

samples_mort <- as_tibble(data.frame(gp_sigma_prior            = gp_sigma_prior, 
                                     gp_length_scale_prior     = gp_length_scale_prior,
                                     alpha_0_prior             = alpha_0_prior,
                                     alpha_1_prior             = alpha_1_prior,
                                     gp_sigma_posterior        = gp_sigma_posterior, 
                                     gp_length_scale_posterior = gp_length_scale_posterior,
                                     alpha_0_posterior         = alpha_0_posterior[, 21],
                                     alpha_1_posterior         = alpha_1_posterior[, 21])) # Y = 20 implies 2018

samples_mort <- samples_mort %>% pivot_longer(cols = everything(), 
                                              names_to = c("parameter", "distribution"), 
                                              names_pattern = "(gp_sigma|gp_length_scale|alpha_0|alpha_1)_(prior|posterior)", 
                                              values_to = "value") %>%
                                 mutate(parameter = recode(parameter, "gp_sigma" = "Sigma", 
                                                                      "gp_length_scale" = "Length Scale", 
                                                                      "alpha_0" = "Alpha 0", 
                                                                      "alpha_1" = "Alpha 1"), 
                                        distribution = recode(distribution, "prior" = "Prior", 
                                                              "posterior" = "Posterior")) %>% 
                                 mutate(distribution = factor(distribution, levels = c("Prior", "Posterior")))


p_par_mort <- ggplot(samples_mort) +
  geom_boxplot(mapping = aes(x = distribution, y = value, fill = distribution)) +
  scale_fill_manual(name = "Distribution", values =  c("#FF000099", "#0000FF99")) + 
  facet_wrap(~ parameter, scales = "free_y") +
  labs(title = "Comparison of Priors and Posteriors (Mortality) in 2018", x = "Distribution", y = "Value") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10")) 

ggsave(filename = paste("IMAGES/pars_mortality.jpeg" , sep = ""), plot = p_par_mort , width = 2000, height = 2000, units = c("px"), dpi = 300, bg = "white")

#############
# Fertility #
#############

p_fert <- "fertility_v1_2.stan"
d_fert <- readRDS(file = paste("FITTED/", strsplit(p_mort, "\\.")[[1]][1], "_dat.RDS", sep = ""))

draws_fert <- d_fert$draws

gp_sigma_posterior        <- c(draws_mort[, "gp_sigma"])
gp_length_scale_posterior <- c(draws_mort[, "gp_length_scale"])
alpha_0_posterior <- matrix(data = 0, nrow = sample_size, ncol = Y)
alpha_1_posterior <- matrix(data = 0, nrow = sample_size, ncol = Y)
for (y in 1:Y) {
  alpha_0_posterior[, y] <- c(draws_mort[, paste("alpha_0[", y, "]", sep = "")])
  alpha_1_posterior[, y] <- c(draws_mort[, paste("alpha_1[", y, "]", sep = "")])
}

gp_sigma_prior <- rnorm(n = 1e6, mean = 0, sd = 0.5); gp_sigma_prior <- gp_sigma_prior[gp_sigma_prior >= 0][1:sample_size]
gp_length_scale_prior <- 1 / rgamma(n = 1e6, shape = 3, scale = 1); gp_length_scale_prior <- gp_length_scale_prior[gp_length_scale_prior >= 0][1:sample_size]
alpha_0_prior <- rnorm(n = sample_size, mean = 0, sd = 0.025)
alpha_1_prior <- rnorm(n = sample_size, mean = 0, sd = 1)

samples_fert <- as_tibble(data.frame(gp_sigma_prior            = gp_sigma_prior, 
                                     gp_length_scale_prior     = gp_length_scale_prior,
                                     alpha_0_prior             = alpha_0_prior,
                                     alpha_1_prior             = alpha_1_prior,
                                     gp_sigma_posterior        = gp_sigma_posterior, 
                                     gp_length_scale_posterior = gp_length_scale_posterior,
                                     alpha_0_posterior         = alpha_0_posterior[, 21],
                                     alpha_1_posterior         = alpha_1_posterior[, 21])) # Y = 21 implies 2018

samples_fert <- samples_fert %>% pivot_longer(cols = everything(), 
                                              names_to = c("parameter", "distribution"), 
                                              names_pattern = "(gp_sigma|gp_length_scale|alpha_0|alpha_1)_(prior|posterior)", 
                                              values_to = "value") %>%
                                 mutate(parameter = recode(parameter, "gp_sigma" = "Sigma", 
                                                           "gp_length_scale" = "Length Scale", 
                                                           "alpha_0" = "Alpha 0", 
                                                           "alpha_1" = "Alpha 1"), 
                                        distribution = recode(distribution, "prior" = "Prior", 
                                                              "posterior" = "Posterior")) %>% 
                                 mutate(distribution = factor(distribution, levels = c("Prior", "Posterior")))


p_par_fert <- ggplot(samples_fert) +
  geom_boxplot(mapping = aes(x = distribution, y = value, fill = distribution)) +
  scale_fill_manual(name = "Distribution", values =  c("#FF000099", "#0000FF99")) + 
  facet_wrap(~ parameter, scales = "free_y") +
  labs(title = "Comparison of Priors and Posteriors (Fertility) in 2018", x = "Distribution", y = "Value") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10")) 

ggsave(filename = paste("IMAGES/pars_fertility.jpeg" , sep = ""), plot = p_par_fert , width = 2000, height = 2000, units = c("px"), dpi = 300, bg = "white")

