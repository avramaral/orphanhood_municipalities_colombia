source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

data <- readRDS(file = "DATA/fertility_bias_data.RDS")
colombia  <- data$colombia

pop <- readRDS(file = "DATA/fertility_bias_data.RDS")
raw_births <- pop$fert
raw_births <- raw_births %>% filter(gender == "Male" | (gender == "Female" & !(age %in% c("50-54", "55-59")))) # Ignore female `50-54` and `55-59`.

fit_births <- readRDS("FITTED/DATA/count_fertility_v1_2.RDS")
fit_births <- fit_births %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, fit_births = Mean) %>% mutate(mun = factor(mun))
fit_births <- fit_births %>% filter(gender == "Male" | (gender == "Female" & !(age %in% c("50-54", "55-59")))) # Ignore female `50-54` and `55-59`.

n_raw_births <- sum(raw_births$births)
n_fit_births <- sum(fit_births$fit_births)

print(paste("The fitted number of births is ", round(n_fit_births / n_raw_births, 2), " larger than the raw number of births.", sep = ""))

raw_births <- raw_births %>% left_join(y = fit_births[, c("mun", "gender", "year", "age", "fit_births")], by = c("mun", "gender", "year", "age"))
raw_births <- raw_births %>% dplyr::select(year, mun, gender, age, population, births, fit_births) %>% mutate(mult_fact = fit_births / births)

mult_fact <- raw_births$mult_fact; mult_fact <- mult_fact[!is.infinite(mult_fact) & !is.na(mult_fact)]

# COMPUTE STANDARDISED RATES AND PLOT MAPS

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













