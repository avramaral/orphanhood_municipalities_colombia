source("R/header.R")
source("R/aux.R")

pop <- readRDS(file = "DATA/fertility_bias_data.RDS")
raw_births <- pop$fert
raw_births <- raw_births %>% filter(gender == "Male" | (gender == "Female" & !(age %in% c("50-54", "55-59")))) # Ignore female `50-54` and `55-59`.

fit_births <- readRDS("FITTED/DATA/count_fertility_v1_2.RDS")
fit_births <- fit_births %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, fit_births = Mean) %>% mutate(mun = factor(mun))
fit_births <- fit_births %>% filter(gender == "Male" | (gender == "Female" & !(age %in% c("50-54", "55-59")))) # Ignore female `50-54` and `55-59`.

########## TEMPORARY ########## 

fitted_mun <- fit_births$mun %>% unique() %>% as.character() %>% as.numeric()
raw_births <- raw_births %>% filter(mun %in% fitted_mun) %>% mutate(mun = factor(as.numeric(as.character(mun))))

############################### 
###############################

n_raw_births <- sum(raw_births$births)
n_fit_births <- sum(fit_births$fit_births)

print(paste("The fitted number of births is ", round(n_fit_births / n_raw_births, 2), " larger than the raw number of births.", sep = ""))

raw_births <- raw_births %>% left_join(y = fit_births[, c("mun", "gender", "year", "age", "fit_births")], by = c("mun", "gender", "year", "age"))
raw_births <- raw_births %>% dplyr::select(year, mun, gender, age, population, births, fit_births) %>% mutate(mult_fact = fit_births / births)

mult_fact <- raw_births$mult_fact; mult_fact <- mult_fact[!is.infinite(mult_fact) & !is.na(mult_fact)]

