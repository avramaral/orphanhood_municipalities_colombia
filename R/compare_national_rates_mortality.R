source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")
mpi_info <- data$mort %>% dplyr::select(mun, mpi) %>% distinct()
geo_info <- data$geo_info 
colombia <- data$colombia

pop <- readRDS(file = "DATA/mortality_bias_data.RDS")
raw_deaths <- pop$mort
raw_deaths <- raw_deaths %>% filter(!(age %in% c("80+"))) # Ignore `80+` as it makes visualization difficult

fit_deaths <- readRDS("FITTED/DATA/count_mortality_v1_1.RDS")
fit_deaths <- fit_deaths %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, fit_deaths = Mean) %>% mutate(mun = factor(mun))
fit_deaths <- fit_deaths %>% filter(!(age %in% c("80+"))) # Ignore `80+` as it makes visualization difficult

# COMPUTE STANDARDISED RATES

data_raw <- raw_deaths %>% 
            dplyr::select(year, mun, gender, age, deaths, population) %>% 
            mutate(deaths = ifelse(population == 0, 0, deaths)) %>% 
            group_by(year, gender, age) %>% summarise(deaths = sum(deaths), population = sum(population)) %>% ungroup() %>% 
            mutate(mortality_rate = compute_rate(count = deaths, pop = population))
data_fit <- fit_deaths %>% 
            dplyr::select(year, mun, gender, age, fit_deaths) %>% rename(deaths = fit_deaths) %>% 
            arrange(year, mun, gender, age) %>% 
            left_join(y = raw_deaths[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) %>%
            mutate(deaths = ifelse(population == 0, 0, deaths)) %>% 
            group_by(year, gender, age) %>% summarise(deaths = sum(deaths), population = sum(population)) %>% ungroup() %>% 
            mutate(mortality_rate = compute_rate(count = deaths, pop = population))

for (yy in 1998:2021) {
  std_raw <- compute_national_std_rate_age_gender(data_raw); std_raw <- std_raw %>% filter(year == yy) %>% dplyr::select(-year)
  std_fit <- compute_national_std_rate_age_gender(data_fit); std_fit <- std_fit %>% filter(year == yy) %>% dplyr::select(-year)
  
  std_rate <- std_raw %>% rename(std_rate_raw = std_rate) %>% left_join(y = std_fit, by = c("age", "gender")) %>% rename(std_rate_fit = std_rate)
  std_data <- std_rate %>% dplyr::select(std_rate_raw, std_rate_fit, age, gender)
  
  p_raw_fit <- plot_national_std_rate(data = std_data, tt = paste("National std. mortality rates (by age and gender)\nin ", yy, sep = ""))
  ggsave(filename = paste("IMAGES/NATIONAL_RATES/std_national_mortality_comparison_", yy,".jpeg" , sep = ""), plot = p_raw_fit , width = 1500, height = 1500, units = c("px"), dpi = 300, bg = "white")
}


