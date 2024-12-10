source("R/header.R")
source("R/header_orphanhood.R")
source("R/aux_orphanhood.R")

#############
### FLAGS ###
#############

file_name <- "_MEAN"

geo_info <- readRDS(file = "DATA/geo_info.RDS")

population <- readRDS(file = "DATA/population_all.RDS")
population <- population %>% dplyr::select(year, mun, gender, age, population) %>% mutate(mun = as.numeric(as.character(mun))) %>% mutate(mun = factor(mun))
population <- population %>% filter(year %in% 1998:2021) %>% arrange(year, mun, gender, age)
population <- population %>% filter(age %in% c("0-9", "10-14", "15-19")) %>% rename(loc = mun)
prop_15_17 <- readRDS(file = "DATA/prop_15_17.RDS") %>% rename(loc = mun)
population <- population %>% left_join(y = prop_15_17, by = "loc")
population <- population %>% mutate(population = ifelse(age == "15-19", population * prop_15_17, population)) %>% dplyr::select(-prop_15_17)

#################
### READ DATA ###
#################

# INCIDENCE
inc_orphans <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_inc_orphans_abs", file_name, ".csv", sep = ""))
inc_orphans <- inc_orphans %>% filter(year == 2021)
inc_orphans <- inc_orphans$n_orp %>% sum()
print(paste("Orphanhood incidence in Colombia in 2021: ", inc_orphans, sep = ""))

inc_orphans_per_children <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_inc_orphans_per_100000", file_name, ".csv", sep = ""))
inc_orphans_per_children <- inc_orphans_per_children %>% filter(year == 2021)
inc_orphans_per_children <- inc_orphans_per_children$n_orp %>% sum()
print(paste("Orphanhood incidence (per children) in Colombia in 2021: ", round(inc_orphans_per_children / 100000 * 100, 2), "%", sep = ""))

###################
# Analysis by MPI #
###################

total_pop <- readRDS(file = "DATA/population_all.RDS")
total_pop_21 <- total_pop %>% filter(year == 2021) %>% group_by(mun) %>% summarise(total_pop_21 = sum(population)) %>% ungroup()

# Relative
inc_orphans_per_children <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/municipality_inc_orphans_per_1000", file_name, ".csv", sep = ""))
# Absolute
inc_orphans <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/municipality_inc_orphans_abs", file_name, ".csv", sep = ""))

orphans <- inc_orphans

orphans <- orphans %>% mutate(mun = factor(mun)) %>% filter(year == 2021)
orphans <- orphans %>% left_join(y = geo_info[, c("mun", "mpi")], by = "mun")
orphans <- orphans %>% group_by(mun, mun_name, year, mpi) %>% summarise(n_orp = sum(n_orp)) %>% ungroup()
orphans <- orphans %>% left_join(total_pop_21, by = "mun")
orphans <- orphans %>% arrange(desc(mpi))

###################
###################

# PREVALENCE (at national level)

pre_orphans <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_pre_orphans_abs", file_name, ".csv", sep = ""))
pre_orphans <- pre_orphans$n_orp %>% sum()
print(paste("Orphanhood prevalence in Colombia in 2021: ", pre_orphans, sep = ""))

pre_orphans_per_children <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_pre_orphans_per_100000", file_name, ".csv", sep = ""))
pre_orphans_per_children <- pre_orphans_per_children$n_orp %>% sum()
print(paste("Orphanhood prevalence (per children) in Colombia in 2021: ", round(pre_orphans_per_children / 100000 * 100, 2), "%", sep = ""))
