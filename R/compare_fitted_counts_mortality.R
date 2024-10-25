source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")
mpi_info <- data$mort %>% dplyr::select(mun, mpi) %>% distinct()
geo_info <- data$geo_info 
colombia <- data$colombia

prop_65_66_fem <- readRDS(file = "DATA/prop_65_66_fem.RDS")
prop_75_76_mal <- readRDS(file = "DATA/prop_75_76_mal.RDS")

Y = data$mort$year   %>% unique() %>% length()
A = data$mort$age    %>% unique() %>% length()
G = data$mort$gender %>% unique() %>% length()
L = data$mort$mun    %>% unique() %>% length()

raw_deaths <- data$mort
raw_deaths_filtered <- adj_mort_data(mort = raw_deaths, prop_fem = prop_65_66_fem, prop_75_76_mal)

p <- "mortality_v1_1.stan"
d <- readRDS(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))
draws <- d$draws
fit_d <- d$data

fit_deaths <- readRDS(paste("FITTED/DATA/count_", strsplit(p, "\\.")[[1]][1], ".RDS", sep = ""))
fit_deaths <- fit_deaths %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, deaths = Median) %>% mutate(mun = factor(mun))
fit_deaths <- fit_deaths %>% left_join(y = data$mort[, c("mun", "gender", "year", "age", "population")], by = c("mun", "gender", "year", "age"))
fit_deaths <- fit_deaths %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
fit_deaths_filtered <- adj_mort_data(mort = fit_deaths, prop_fem = prop_65_66_fem, prop_75_76_mal)
fit_deaths <- fit_deaths %>% dplyr::select(year, mun, gender, age, deaths, population, death_rate) %>% arrange(year, mun, gender, age)
fit_deaths <- fit_deaths %>% rename(fit_deaths = deaths)

if (FALSE) {
  n_raw_deaths <- sum(raw_deaths_filtered$deaths)
  n_fit_deaths <- sum(fit_deaths_filtered$deaths)
} else {
  n_raw_deaths <- sum(raw_deaths$deaths)
  n_fit_deaths <- sum(fit_deaths$fit_deaths)
}

print(paste("The fitted number of deaths is ", round(n_fit_deaths / n_raw_deaths, 2), " larger than the raw number of deaths.", sep = ""))

raw_deaths <- raw_deaths %>% left_join(y = fit_deaths[, c("mun", "gender", "year", "age", "fit_deaths")], by = c("mun", "gender", "year", "age"))
raw_deaths <- raw_deaths %>% dplyr::select(year, mun, gender, age, population, deaths, fit_deaths) %>% mutate(mult_fact = fit_deaths / deaths)

mult_fact <- raw_deaths$mult_fact; mult_fact <- mult_fact[!is.infinite(mult_fact) & !is.na(mult_fact)]

# COMPUTE STANDARDISED RATES AND PLOTS

data_raw <- raw_deaths %>% 
            dplyr::select(year, mun, gender, age, deaths, population) %>% 
            mutate(deaths = ifelse(population == 0, 0, deaths)) %>% 
            mutate(mortality_rate = compute_rate(count = deaths, pop = population))
data_fit <- fit_deaths %>% 
            dplyr::select(year, mun, gender, age, fit_deaths) %>% rename(deaths = fit_deaths) %>% 
            arrange(year, mun, gender, age) %>% 
            left_join(y = raw_deaths[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) %>%
            mutate(deaths = ifelse(population == 0, 0, deaths)) %>% 
            mutate(mortality_rate = compute_rate(count = deaths, pop = population))

# PLOT STANDARDIZED RATES
for (y in 1998:2021) {
  # y <- 2018
  yy <- y - 1998 + 1
  y_lim <- c(0, 0.02)
  
  std_raw <- compute_std_rate(data_raw); std_raw <- std_raw %>% filter(year == y) %>% dplyr::select(-year)
  std_raw <- std_raw %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
  
  p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (mortality)", y_lim = y_lim)
  
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
  
  p_fit_pts <- plot_fit_std_rate(data_lin = quantiles_lin, data_fit = std_fit, tt = "Fitted data (mortality)", y_lim = y_lim)
  
  (p_tot_pts <- p_raw_pts + p_fit_pts)
  ggsave(filename = paste("IMAGES/STD_RATES_COMPARISON/MORTALITY/std_mortality_comparison_", y ,".jpeg" , sep = ""), plot = p_tot_pts , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")
}


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

p_raw_box <- plot_std_rate_boxplot(data = std_raw_age_gender, tt = "Raw data (mortality)")
p_fit_box <- plot_std_rate_boxplot(data = std_fit_age_gender, tt = "Fit data (mortality)")
p_tot_box <- p_raw_box + p_fit_box
ggsave(filename = paste("IMAGES/std_mortality_boxplot_comparison.jpeg" , sep = ""), plot = p_tot_box , width = 3000, height = 2000, units = c("px"), dpi = 300, bg = "white")

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

p_raw <- plot_maps(data = std_raw, my_var = "std_rate", tt = "Original data", nm_var = "Standardised\nmortality rate\n(in 2018)", ll = c(l_limit, u_limit))
p_fit <- plot_maps(data = std_fit, my_var = "std_rate", tt = "Fitted data",   nm_var = "Standardised\nmortality rate\n(in 2018)", ll = c(l_limit, u_limit))
p_tot <- p_raw + p_fit
ggsave(filename = paste("IMAGES/std_mortality_comparison.jpeg" , sep = ""), plot = p_tot , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")


