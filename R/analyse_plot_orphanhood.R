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

# Analysis by MPI
mpi_threshold <- 40.8
population_21 <- population %>% filter(year == 2021) %>% dplyr::select(-year)
population_21 <- population_21 %>% group_by(loc, gender) %>% summarise(n_children = sum(population)) %>% ungroup() %>% rename(mun = loc)
inc_orphans_mun <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/municipality_inc_orphans_abs", file_name, ".csv", sep = ""))
inc_orphans_mun <- inc_orphans_mun %>% filter(year == 2021)
inc_orphans_mun <- inc_orphans_mun %>% mutate(mun = factor(mun)) 
inc_orphans_mun <- inc_orphans_mun %>% left_join(y = geo_info[, c("mun", "mpi")], by = "mun")
inc_orphans_mun <- inc_orphans_mun %>% left_join(y = population_21, by = c("mun", "gender"))
inc_orphans_mun <- inc_orphans_mun %>% mutate(orphan_per_children = n_orp / n_children)
inc_poor <- inc_orphans_mun %>% filter(mpi >  mpi_threshold) %>% dplyr::select(orphan_per_children) %>% c() %>% unlist() %>% mean()
inc_rich <- inc_orphans_mun %>% filter(mpi <= mpi_threshold) %>% dplyr::select(orphan_per_children) %>% c() %>% unlist() %>% mean()
print(paste("x% in Colombia in 2021 (median): ", round((1 - (inc_rich / inc_poor)) * 100, 2), "%", sep = ""))
# Poorest versus richest
ordered_mpi <- inc_orphans_mun %>% arrange(mpi) %>% dplyr::select(mun) %>% c() %>% unlist() %>% unname(); 
inc_poor <- inc_orphans_mun %>% filter(mun == ordered_mpi[length(ordered_mpi)]) %>% dplyr::select(orphan_per_children) %>% c() %>% unlist() %>% mean()
inc_rich <- inc_orphans_mun %>% filter(mun == ordered_mpi[1])                   %>% dplyr::select(orphan_per_children) %>% c() %>% unlist() %>% mean()
print(paste("x% in Colombia in 2021 (median): ", round((1 - (inc_rich / inc_poor)) * 100, 2), "%", sep = ""))
# 10 most deprived versus 10 least deprived
inc_poor <- inc_orphans_mun %>% filter(mun %in% tail(ordered_mpi, 100)) %>% dplyr::select(orphan_per_children) %>% c() %>% unlist() %>% mean()
inc_rich <- inc_orphans_mun %>% filter(mun %in% head(ordered_mpi, 100)) %>% dplyr::select(orphan_per_children) %>% c() %>% unlist() %>% mean()
print(paste("x% in Colombia in 2021 (median): ", round((1 - (inc_rich / inc_poor)) * 100, 2), "%", sep = ""))
# 10%
pps <- inc_orphans_mun$mpi %>% quantile(probs = c(0.1, 0.9))
inc_poor <- inc_orphans_mun %>% filter(mpi >= pps[2]) %>% dplyr::select(orphan_per_children) %>% c() %>% unlist() %>% mean()
inc_rich <- inc_orphans_mun %>% filter(mpi <= pps[1]) %>% dplyr::select(orphan_per_children) %>% c() %>% unlist() %>% mean()
print(paste("x% in Colombia in 2021 (median): ", round((1 - (inc_rich / inc_poor)) * 100, 2), "%", sep = ""))


# PREVALENCE (at national level)

pre_orphans <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_pre_orphans_abs", file_name, ".csv", sep = ""))
pre_orphans <- pre_orphans$n_orp %>% sum()
print(paste("Orphanhood prevalence in Colombia in 2021: ", pre_orphans, sep = ""))

pre_orphans_per_children <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/national_pre_orphans_per_100000", file_name, ".csv", sep = ""))
pre_orphans_per_children <- pre_orphans_per_children$n_orp %>% sum()
print(paste("Orphanhood prevalence (per children) in Colombia in 2021: ", round(pre_orphans_per_children / 100000 * 100, 2), "%", sep = ""))
