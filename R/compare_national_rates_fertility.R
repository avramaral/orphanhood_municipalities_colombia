source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

data <- readRDS(file = "DATA/fertility_bias_data.RDS")
mpi_info <- data$fert %>% dplyr::select(mun, mpi) %>% distinct()
geo_info <- data$geo_info 
colombia <- data$colombia

pop <- readRDS(file = "DATA/fertility_bias_data.RDS")
raw_births <- pop$fert
raw_births <- raw_births %>% filter(gender == "Male" | (gender == "Female" & !(age %in% c("50-54", "55-59")))) # Ignore female `50-54` and `55-59`.

fit_births <- readRDS("FITTED/DATA/count_fertility_v1_2.RDS")
fit_births <- fit_births %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, fit_births = Mean) %>% mutate(mun = factor(mun))
fit_births <- fit_births %>% filter(gender == "Male" | (gender == "Female" & !(age %in% c("50-54", "55-59")))) # Ignore female `50-54` and `55-59`.

# COMPUTE STANDARDISED RATES

data_raw <- raw_births %>% 
            dplyr::select(year, mun, gender, age, births, population) %>% 
            mutate(births = ifelse(population == 0, 0, births)) %>%
            group_by(year, gender, age) %>% summarise(births = sum(births), population = sum(population)) %>% ungroup() %>% 
            mutate(fertility_rate = compute_rate(count = births, pop = population))
data_fit <- fit_births %>% 
            dplyr::select(year, mun, gender, age, fit_births) %>% rename(births = fit_births) %>% 
            arrange(year, mun, gender, age) %>% 
            left_join(y = raw_births[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) %>%
            mutate(births = ifelse(population == 0, 0, births)) %>% 
            group_by(year, gender, age) %>% summarise(births = sum(births), population = sum(population)) %>% ungroup() %>% 
            mutate(fertility_rate = compute_rate(count = births, pop = population))

for (yy in 1998:2021) {
  std_raw <- compute_national_std_rate_age_gender(data_raw); std_raw <- std_raw %>% filter(year == yy) %>% dplyr::select(-year)
  std_fit <- compute_national_std_rate_age_gender(data_fit); std_fit <- std_fit %>% filter(year == yy) %>% dplyr::select(-year)
  
  std_rate <- std_raw %>% rename(std_rate_raw = std_rate) %>% left_join(y = std_fit, by = c("age", "gender")) %>% rename(std_rate_fit = std_rate)
  std_data <- std_rate %>% dplyr::select(std_rate_raw, std_rate_fit, age, gender)
  
  p_raw_fit <- plot_national_std_rate(data = std_data, tt = paste("National std. fertility rates (by age and gender)\nin ", yy, sep = ""))
  ggsave(filename = paste("IMAGES/NATIONAL_RATES/std_national_fertility_comparison_", yy,".jpeg" , sep = ""), plot = p_raw_fit , width = 1500, height = 1200, units = c("px"), dpi = 300, bg = "white")
}


