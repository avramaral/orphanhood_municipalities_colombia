source("R/header.R")
source("R/aux.R")

# POSSIBLE AGE GROUPS
# - MORTALITY
# -- FEMALE: 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-66
# -- MALE:   10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-76
# - FERTILITY
# -- FEMALE: 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49,
# -- MALE:   10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59

group_death <- function (data, ...) {
  # Group death in the following way
  # Female: 50-54, 55-59, 60-64, 65-66 is grouped into 50-66
  # Male:   60-64, 65-69, 70-74, 75-76 is grouped into 60-76
  
  data_fem <- data %>% filter(gender == "Female")
  data_mal <- data %>% filter(gender == "Male")
  
  data_fem <- data_fem %>% mutate(age = ifelse(age %in% c("50-54", "55-59", "60-64", "65-66"), "50-66", age))
  data_fem <- data_fem %>% group_by(year, mun, gender, age) %>% summarise(deaths = sum(deaths), population = sum(population), mpi = mean(mpi)) %>% ungroup()
  data_fem <- data_fem %>% mutate(death_rate = compute_rate(count = deaths, pop = population)) %>% dplyr::select(year, mun, gender, age, deaths, population, death_rate, mpi)
  
  data_mal <- data_mal %>% mutate(age = ifelse(age %in% c("60-64", "65-69", "70-74", "75-76"), "60-76", age))
  data_mal <- data_mal %>% group_by(year, mun, gender, age) %>% summarise(deaths = sum(deaths), population = sum(population), mpi = mean(mpi)) %>% ungroup()
  data_mal <- data_mal %>% mutate(death_rate = compute_rate(count = deaths, pop = population)) %>% dplyr::select(year, mun, gender, age, deaths, population, death_rate, mpi)
  
  data <- bind_rows(data_fem, data_mal) %>% arrange(year, mun, gender, age)
  data
}

##################
# Empirical data #
##################

# MORTALITY

mort_emp <- readRDS(file = "DATA/mortality_bias_data.RDS")
mort_emp <- mort_emp$mort %>% arrange(year, mun, gender, age)
# Temporary quantities #
pop <- mort_emp %>% dplyr::select(year, mun, gender, age, population) %>% distinct()
mpi <- mort_emp %>% dplyr::select(mun, mpi) %>% distinct()
########################
mort_emp <- group_death(data = mort_emp)
saveRDS(object = mort_emp, file = "DATA/COUNTS/mort_emp.RDS")

# FERTILITY

fert_emp <- readRDS(file = "DATA/fertility_bias_data.RDS")
fert_emp <- fert_emp$fert %>% arrange(year, mun, gender, age)
saveRDS(object = fert_emp, file = "DATA/COUNTS/fert_emp.RDS")

##########################
# Summarized fitted data #
##########################

# MORTALITY

mort_summ <- readRDS(file = "FITTED/DATA/count_mortality_v1.RDS")

mort_mean <- mort_summ %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, deaths = Mean) %>% dplyr::select(year, mun, gender, age, deaths) %>% mutate(mun = factor(mun))
mort_mean <- mort_mean %>% left_join(y = pop, by = c("year", "mun", "gender", "age"))
mort_mean <- mort_mean %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
mort_mean <- mort_mean %>% left_join(y = mpi, by = c("mun"))
mort_mean <- group_death(data = mort_mean)
saveRDS(object = mort_mean, file = "DATA/COUNTS/mort_mean.RDS")

mort_median <- mort_summ   %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, deaths = Median) %>% dplyr::select(year, mun, gender, age, deaths) %>% mutate(mun = factor(mun))
mort_median <- mort_median %>% left_join(y = pop, by = c("year", "mun", "gender", "age"))
mort_median <- mort_median %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
mort_median <- mort_median %>% left_join(y = mpi, by = c("mun"))
mort_median <- group_death(data = mort_median)
saveRDS(object = mort_median, file = "DATA/COUNTS/mort_median.RDS")

# FERTILITY

fert_summ <- readRDS(file = "FITTED/DATA/count_fertility_v1.RDS")

fert_mean <- fert_summ %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, births = Mean) %>% dplyr::select(year, mun, gender, age, births) %>% mutate(mun = factor(mun))
fert_mean <- fert_mean %>% left_join(y = pop, by = c("year", "mun", "gender", "age"))
fert_mean <- fert_mean %>% mutate(fertility_rate = compute_rate(count = births, pop = population))
fert_mean <- fert_mean %>% left_join(y = mpi, by = c("mun"))
fert_mean <- fert_mean %>% arrange(year, mun, gender, age)
saveRDS(object = fert_mean, file = "DATA/COUNTS/fert_mean.RDS")

fert_median <- fert_summ   %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, births = Median) %>% dplyr::select(year, mun, gender, age, births) %>% mutate(mun = factor(mun))
fert_median <- fert_median %>% left_join(y = pop, by = c("year", "mun", "gender", "age"))
fert_median <- fert_median %>% mutate(fertility_rate = compute_rate(count = births, pop = population))
fert_median <- fert_median %>% left_join(y = mpi, by = c("mun"))
fert_median <- fert_median %>% arrange(year, mun, gender, age)
saveRDS(object = fert_median, file = "DATA/COUNTS/fert_median.RDS")

#######################
# Monte Carlo samples #
#######################

# TBD

