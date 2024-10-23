source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

use_raw_data <- FALSE

data <- readRDS(file = "DATA/fertility_bias_data.RDS")
mpi_info <- data$fert %>% dplyr::select(mun, mpi) %>% distinct()
geo_info <- data$geo_info 
colombia <- data$colombia

Y = data$fert$year   %>% unique() %>% length()
A = data$fert$age    %>% unique() %>% length()
G = data$fert$gender %>% unique() %>% length()
L = data$fert$mun    %>% unique() %>% length()

pop <- readRDS(file = "DATA/fertility_bias_data.RDS")
raw_births <- pop$fert
raw_births <- raw_births %>% filter(gender == "Male" | (gender == "Female" & !(age %in% c("50-54", "55-59")))) # Ignore female `50-54` and `55-59`.

p <- "fertility_v1_2.stan"
d <- readRDS(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))
draws <- d$draws
fit_d <- d$data

fit_births <- readRDS(paste("FITTED/DATA/count_fertility_v1_2_empirical_", as.character(use_raw_data), ".RDS", sep = ""))
idx_male   <- which(fit_births$Gender == "Male")
fit_births <- fit_births %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, fit_births = Mean) %>% mutate(mun = factor(mun))
fit_births <- fit_births %>% filter(gender == "Male" | (gender == "Female" & !(age %in% c("50-54", "55-59")))) # Ignore female `50-54` and `55-59`.

fit_bth_rt <- readRDS(paste("FITTED/DATA/rates_fertility_v1_2_empirical_", as.character(use_raw_data), ".RDS", sep = ""))
fit_bth_rt[idx_male, "Gender"] <- "Male"
fit_bth_rt <- fit_bth_rt %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, fertility_rate = Mean) %>% mutate(mun = factor(mun))
fit_bth_rt <- fit_bth_rt %>% dplyr::select(mun, gender, year, age, fertility_rate) %>% left_join(y = raw_births[, c("mun", "gender", "year", "age", "population")], by = c("mun", "gender", "year", "age"))
fit_bth_rt <- fit_bth_rt %>% mutate(births = population * fertility_rate) %>% dplyr::select(year, mun, gender, age, deaths, population, fertility_rate) %>% arrange(year, mun, gender, age)
fit_bth_rt <- fit_bth_rt %>% left_join(y = mpi_info, by = "mun")

n_raw_births <- sum(raw_births$births)
n_fit_births <- sum(fit_births$fit_births)

print(paste("The fitted number of births is ", round(n_fit_births / n_raw_births, 2), " larger than the raw number of births.", sep = ""))

raw_births <- raw_births %>% left_join(y = fit_births[, c("mun", "gender", "year", "age", "fit_births")], by = c("mun", "gender", "year", "age"))
raw_births <- raw_births %>% dplyr::select(year, mun, gender, age, population, births, fit_births) %>% mutate(mult_fact = fit_births / births)

mult_fact <- raw_births$mult_fact; mult_fact <- mult_fact[!is.infinite(mult_fact) & !is.na(mult_fact)]

# COMPUTE STANDARDISED RATES AND PLOT

data_raw <- raw_births %>% 
            dplyr::select(year, mun, gender, age, births, population) %>% 
            mutate(births = ifelse(population == 0, 0, births)) %>% 
            mutate(fertility_rate = compute_rate(count = births, pop = population))
data_fit <- fit_births %>% 
            dplyr::select(year, mun, gender, age, fit_births) %>% rename(births = fit_births) %>% 
            arrange(year, mun, gender, age) %>% 
            left_join(y = raw_births[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) %>%
            mutate(births = ifelse(population == 0, 0, births)) %>% 
            mutate(fertility_rate = compute_rate(count = births, pop = population))

############
# NOT USED #
############

std_raw_gender <- compute_std_rate_gender(data_raw); std_raw_gender <- std_raw_gender %>% filter(year == 2018) %>% dplyr::select(-year)
std_fit_gender <- compute_std_rate_gender(data_fit); std_fit_gender <- std_fit_gender %>% filter(year == 2018) %>% dplyr::select(-year)

std_raw_fem <- std_raw_gender %>% filter(gender == "Female") %>% dplyr::select(-gender)
std_raw_mal <- std_raw_gender %>% filter(gender ==   "Male") %>% dplyr::select(-gender)
std_fit_fem <- std_fit_gender %>% filter(gender == "Female") %>% dplyr::select(-gender)
std_fit_mal <- std_fit_gender %>% filter(gender ==   "Male") %>% dplyr::select(-gender)

std_raw_fem <- std_raw_fem %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
std_raw_mal <- std_raw_mal %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
std_fit_fem <- std_fit_fem %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
std_fit_mal <- std_fit_mal %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))

y_lim <- range(rbind(std_raw_fem, std_fit_fem)[, "std_rate"])
p_raw_pts_fem <- plot_std_rate(data = std_raw_fem, tt = "Raw data (fertility) Female", y_lim = c(0, 0.01))
p_fit_pts_fem <- plot_std_rate(data = std_fit_fem, tt = "Fit data (fertility) Female", y_lim = c(0, 0.01))
p_tot_pts_fem <- p_raw_pts_fem + p_fit_pts_fem

y_lim <- range(rbind(std_raw_mal, std_fit_mal)[, "std_rate"])
p_raw_pts_mal <- plot_std_rate(data = std_raw_mal, tt = "Raw data (fertility) Male", y_lim = c(0, 0.01))
p_fit_pts_mal <- plot_std_rate(data = std_fit_mal, tt = "Fit data (fertility) Male", y_lim = c(0, 0.01))
p_tot_pts_mal <- p_raw_pts_mal + p_fit_pts_mal

# ggsave(filename = paste("IMAGES/std_fertility_points_comparison_fem.jpeg" , sep = ""), plot = p_tot_pts_fem , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")
# ggsave(filename = paste("IMAGES/std_fertility_points_comparison_mal.jpeg" , sep = ""), plot = p_tot_pts_mal , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")

############
############

# PLOT STANDARDIZED RATES
y  <- 2018
yy <- y - 1998 + 1
y_lim <- c(0, 0.15)

std_raw <- compute_std_rate(data_raw); std_raw <- std_raw %>% filter(year == y) %>% dplyr::select(-year)
std_raw <- std_raw %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))

p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (fertility)", y_lim = y_lim)

alpha_0 <- c(draws[, paste("alpha_0[", yy, "]", sep = "")])
alpha_1 <- c(draws[, paste("alpha_1[", yy, "]", sep = "")])
mpi_mun <- seq(0, 1, by = 0.01)
std_mun <- alpha_0 + outer(alpha_1, mpi_mun)
mpi_obs <- fit_d$mpi_municip
std_obs <- alpha_0 + outer(alpha_1, mpi_obs) 

# Calculate the quantiles (2.5%, 50%, 97.5%) for each x value (column-wise)
quantiles_lin <- apply(std_mun, 2, quantile, probs = c(0.025, 0.5, 0.975))
quantiles_lin <- data.frame(x = mpi_mun, ll = quantiles_lin[1, ], mm = quantiles_lin[2, ], uu = quantiles_lin[3, ])

quantiles_obs <- apply(std_obs, 2, quantile, probs = c(0.025, 0.5, 0.975))
quantiles_obs <- data.frame(x = mpi_obs, ll = quantiles_obs[1, ], mm = quantiles_obs[2, ], uu = quantiles_obs[3, ])

std_fit <- compute_std_rate(data_fit); std_fit <- std_fit %>% filter(year == y) %>% dplyr::select(-year)
std_fit <- std_fit %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital), mpi = mpi / 100)

# IGNORE THIS ####
std_bth_rate_fit <- compute_std_rate(fit_bth_rt); std_bth_rate_fit <- std_bth_rate_fit %>% filter(year == y) %>% dplyr::select(-year)
std_bth_rate_fit <- std_bth_rate_fit %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital), mpi = mpi / 100)
##################

p_fit_pts <- plot_fit_std_rate(data_lin = quantiles_lin, data_fit = std_fit, tt = "Fitted data (mortality)", y_lim = y_lim)

(p_tot_pts <- p_raw_pts + p_fit_pts)

# BOXPLOT

# Compute standardized rage
std_raw_age_gender <- compute_std_rate_age_gender(data_raw); std_raw_age_gender <- std_raw_age_gender %>% filter(year == 2018) %>% dplyr::select(-year)
std_fit_age_gender <- compute_std_rate_age_gender(data_fit); std_fit_age_gender <- std_fit_age_gender %>% filter(year == 2018) %>% dplyr::select(-year)
# Add capital and MPI information
std_raw_age_gender <- std_raw_age_gender %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
std_fit_age_gender <- std_fit_age_gender %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
# Add MPI class
std_raw_age_gender <- std_raw_age_gender %>% mutate(mpi_class = ifelse(mpi <= 25, 1, ifelse(mpi <= 50, 2, ifelse(mpi <= 75, 3, 4)))) %>% mutate(mpi_class = factor(mpi_class, labels = c("0-25", "26-50", "51-75", "76-100")))
std_fit_age_gender <- std_fit_age_gender %>% mutate(mpi_class = ifelse(mpi <= 25, 1, ifelse(mpi <= 50, 2, ifelse(mpi <= 75, 3, 4)))) %>% mutate(mpi_class = factor(mpi_class, labels = c("0-25", "26-50", "51-75", "76-100")))
# Filter out unnecessary age classes
# TBD: Not for now

p_raw_box <- plot_std_rate_boxplot(data = std_raw_age_gender, tt = "Raw data (fertility)")
p_fit_box <- plot_std_rate_boxplot(data = std_fit_age_gender, tt = "Fit data (fertility)")
p_tot_box <- p_raw_box + p_fit_box
ggsave(filename = paste("IMAGES/std_fertility_boxplot_comparison.jpeg" , sep = ""), plot = p_tot_box , width = 3000, height = 2000, units = c("px"), dpi = 300, bg = "white")

# MAP

std_raw <- compute_std_rate(data_raw); std_raw <- std_raw %>% filter(year == 2018) %>% dplyr::select(-year)
std_fit <- compute_std_rate(data_fit); std_fit <- std_fit %>% filter(year == 2018) %>% dplyr::select(-year)

u_limit <- max(std_raw$std_rate, std_fit$std_rate)
l_limit <- min(std_raw$std_rate, std_fit$std_rate)

std_raw <- std_raw %>% left_join(y = colombia, by = "mun")
std_fit <- std_fit %>% left_join(y = colombia, by = "mun")

# Filter out islands
isl1_rw <- std_raw %>% filter((mun %in% c(88001))) 
isl2_rw <- std_raw %>% filter((mun %in% c(88564))) 
std_raw <- std_raw %>% filter(!(mun %in% c(88001, 88564)))

isl1_ft <- std_fit %>% filter((mun %in% c(88001))) 
isl2_ft <- std_fit %>% filter((mun %in% c(88564))) 
std_fit <- std_fit %>% filter(!(mun %in% c(88001, 88564)))

p_raw <- plot_maps(data = std_raw, my_var = "std_rate", tt = "Original data", nm_var = "Standardised\nfertility rate\n(in 2018)", ll = c(l_limit, u_limit))
p_fit <- plot_maps(data = std_fit, my_var = "std_rate", tt = "Fitted data",   nm_var = "Standardised\nfertility rate\n(in 2018)", ll = c(l_limit, u_limit))
p_tot <- p_raw + p_fit
ggsave(filename = paste("IMAGES/std_fertility_comparison.jpeg" , sep = ""), plot = p_tot , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")













