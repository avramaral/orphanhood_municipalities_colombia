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

# This one takes lots of time to be completed

fert_emp <- readRDS(file = "DATA/fertility_bias_data.RDS"); fert_emp <- fert_emp$fert
mort_emp <- readRDS(file = "DATA/mortality_bias_data.RDS"); mort_emp <- mort_emp$mort

fert_summ <- readRDS(file = "FITTED/DATA/count_fertility_v1.RDS") %>% as_tibble()
mort_summ <- readRDS(file = "FITTED/DATA/count_mortality_v1.RDS") %>% as_tibble()

muns <- as.numeric(as.character(unique(fert_summ$Location)))
nmcs <- 2000
yys  <- as.character(1998:2021)
fert_age_fem <- fert_summ %>% filter(Gender == "Female") %>% dplyr::select(Age) %>% unique() %>% c() %>% unlist() %>% unname() %>% as.character()
fert_age_mal <- fert_summ %>% filter(Gender ==   "Male") %>% dplyr::select(Age) %>% unique() %>% c() %>% unlist() %>% unname() %>% as.character()
mort_age_fem <- mort_summ %>% filter(Gender == "Female") %>% dplyr::select(Age) %>% unique() %>% c() %>% unlist() %>% unname() %>% as.character()
mort_age_mal <- mort_summ %>% filter(Gender ==   "Male") %>% dplyr::select(Age) %>% unique() %>% c() %>% unlist() %>% unname() %>% as.character()

# For each Monte Carlo sample, create one data set for fertility and one data set for mortality
for (nn in 322:nmcs) {
  
  print(paste("Sample: ", sprintf("%04d", nn), " (out of ", nmcs, ")", sep = ""))
  
  pb <- txtProgressBar(min = 1, max = length(muns), initial = 1)
  count <- 0
  for (mm in muns) {
    count <- count + 1
    
    # Fertility
    
    tmp_fert <- readRDS(paste("FITTED/DATA/FERTILITY/list_", mm, ".RDS", sep = ""))
    tmp_fert_fem <- tmp_fert$births_fem[, , nn]
    tmp_fert_mal <- tmp_fert$births_mal[, , nn]
    
    rownames(tmp_fert_fem) <- yys; colnames(tmp_fert_fem) <- fert_age_fem
    rownames(tmp_fert_mal) <- yys; colnames(tmp_fert_mal) <- fert_age_mal
    
    tmp_fert_fem <- melt(tmp_fert_fem); colnames(tmp_fert_fem) <- c("year", "age", "births"); tmp_fert_fem$gender <- "Female"; tmp_fert_fem$mun <- mm
    tmp_fert_mal <- melt(tmp_fert_mal); colnames(tmp_fert_mal) <- c("year", "age", "births"); tmp_fert_mal$gender <-   "Male"; tmp_fert_mal$mun <- mm
     
    tmp_fert <- rbind(tmp_fert_fem, tmp_fert_mal)
    
    if (count == 1) {
      res_fert <- tmp_fert
    } else {
      res_fert <- rbind(res_fert, tmp_fert)
    }
  
    # Mortality
    
    tmp_mort <- readRDS(paste("FITTED/DATA/MORTALITY/list_", mm, ".RDS", sep = ""))
    tmp_mort_fem <- tmp_mort$deaths_fem[, , nn]
    tmp_mort_mal <- tmp_mort$deaths_mal[, , nn]
    
    rownames(tmp_mort_fem) <- yys; colnames(tmp_mort_fem) <- mort_age_fem
    rownames(tmp_mort_mal) <- yys; colnames(tmp_mort_mal) <- mort_age_mal
    
    tmp_mort_fem <- melt(tmp_mort_fem); colnames(tmp_mort_fem) <- c("year", "age", "deaths"); tmp_mort_fem$gender <- "Female"; tmp_mort_fem$mun <- mm
    tmp_mort_mal <- melt(tmp_mort_mal); colnames(tmp_mort_mal) <- c("year", "age", "deaths"); tmp_mort_mal$gender <-   "Male"; tmp_mort_mal$mun <- mm
    
    tmp_mort <- rbind(tmp_mort_fem, tmp_mort_mal)
    
    if (count == 1) {
      res_mort <- tmp_mort
    } else {
      res_mort <- rbind(res_mort, tmp_mort)
    }
    
    setTxtProgressBar(pb, count)
  }
  close(pb)
  
  res_fert_cp <- res_fert %>% as_tibble()
  res_fert_cp <- res_fert_cp %>% mutate(mun = factor(mun))
  res_fert_cp <- res_fert_cp %>% left_join(y = fert_emp[, c("year", "mun", "gender", "age", "population", "mpi")], by = c("year", "mun", "gender", "age"))
  res_fert_cp <- res_fert_cp %>% mutate(fertility_rate = compute_rate(count = births, pop = population))
  res_fert_cp <- res_fert_cp %>% dplyr::select(year, mun, gender, age, births, population, fertility_rate, mpi) %>% arrange(year, mun, gender, age)
  
  res_mort_cp <- res_mort %>% as_tibble()
  res_mort_cp <- res_mort_cp %>% mutate(mun = factor(mun))
  res_mort_cp <- res_mort_cp %>% left_join(y = mort_emp[, c("year", "mun", "gender", "age", "population", "mpi")], by = c("year", "mun", "gender", "age"))
  res_mort_cp <- res_mort_cp %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
  res_mort_cp <- res_mort_cp %>% dplyr::select(year, mun, gender, age, deaths, population, death_rate, mpi) %>% arrange(year, mun, gender, age)
  res_mort_cp <- group_death(data = res_mort_cp)
  
  
  saveRDS(object = res_fert_cp, file = paste("DATA/COUNTS/MC/fert_", nn, ".RDS", sep = ""))
  saveRDS(object = res_mort_cp, file = paste("DATA/COUNTS/MC/mort_", nn, ".RDS", sep = ""))
}


















