source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")
colombia  <- data$colombia

pop <- readRDS(file = "DATA/mortality_bias_data.RDS")
raw_deaths <- pop$mort

fit_deaths <- readRDS("FITTED/DATA/count_mortality_v1_1.RDS")
fit_deaths <- fit_deaths %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, fit_deaths = Mean) %>% mutate(mun = factor(mun))

n_raw_deaths <- sum(raw_deaths$deaths)
n_fit_deaths <- sum(fit_deaths$fit_deaths)

print(paste("The fitted number of deaths is ", round(n_fit_deaths / n_raw_deaths, 2), " larger than the raw number of deaths.", sep = ""))

raw_deaths <- raw_deaths %>% left_join(y = fit_deaths[, c("mun", "gender", "year", "age", "fit_deaths")], by = c("mun", "gender", "year", "age"))
raw_deaths <- raw_deaths %>% dplyr::select(year, mun, gender, age, population, deaths, fit_deaths) %>% mutate(mult_fact = fit_deaths / deaths)

mult_fact <- raw_deaths$mult_fact; mult_fact <- mult_fact[!is.infinite(mult_fact) & !is.na(mult_fact)]

# COMPUTE STANDARDISED RATES AND PLOT MAPS

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


