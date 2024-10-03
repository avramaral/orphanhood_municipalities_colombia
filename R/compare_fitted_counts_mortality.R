source("R/header.R")
source("R/aux.R")

pop <- readRDS(file = "DATA/mortality_bias_data.RDS")
raw_deaths <- pop$mort

fit_deaths <- readRDS("FITTED/DATA/count_mortality_v1_1.RDS")
fit_deaths <- fit_deaths %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, fit_deaths = Mean) %>% mutate(mun = factor(mun))

########## TEMPORARY ########## 

fitted_mun <- fit_deaths$mun %>% unique() %>% as.character() %>% as.numeric()
raw_deaths <- raw_deaths %>% filter(mun %in% fitted_mun) %>% mutate(mun = factor(as.numeric(as.character(mun))))

############################### 
###############################

n_raw_deaths <- sum(raw_deaths$deaths)
n_fit_deaths <- sum(fit_deaths$fit_deaths)

print(paste("The fitted number of deaths is ", round(n_fit_deaths / n_raw_deaths, 2), " larger than the raw number of deaths.", sep = ""))

raw_deaths <- raw_deaths %>% left_join(y = fit_deaths[, c("mun", "gender", "year", "age", "fit_deaths")], by = c("mun", "gender", "year", "age"))
raw_deaths <- raw_deaths %>% dplyr::select(year, mun, gender, age, population, deaths, fit_deaths) %>% mutate(mult_fact = fit_deaths / deaths)

mult_fact <- raw_deaths$mult_fact; mult_fact <- mult_fact[!is.infinite(mult_fact) & !is.na(mult_fact)]

