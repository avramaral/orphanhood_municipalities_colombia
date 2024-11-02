source("R/header.R")
source("R/header_orphanhood.R")
source("R/aux_orphanhood.R")

#############
### FLAGS ###
#############

file_name <- "_EMP"

per1K <- TRUE
type.input <- "National"
per_n_children <- ifelse(type.input == "Municipality", 1000, 100000)
should_round <- FALSE

#################
### READ DATA ###
#################

geo_info   <- readRDS(file = "DATA/geo_info.RDS")
valid_muns <- as.numeric(as.character(unique(geo_info$mun)))
population <- readRDS(file = "DATA/population_all.RDS") # WHY? (Until now, only for computing the number of children)
population <- population %>% dplyr::select(year, mun, gender, age, population) %>% mutate(mun = as.numeric(as.character(mun)))
population <- population %>% filter(mun %in% valid_muns) %>% mutate(mun = factor(mun))
population <- population %>% filter(year <= 2021) %>% arrange(year, mun, gender, age) %>% rename(loc = mun)

# Aggregate population and children proportion
tmp <- convert_resolution_pop(type.input = "Municipality", population = population, geo_info = geo_info)
population <- tmp$population
prop_15_17 <- tmp$prop_15_17

######################################
### COMPUTE INCIDENCE & PREVALENCE ###
######################################

#################
### INCIDENCE ###
#################

# Pre-computed in `orphanhood.R`
inc_tab <- readRDS(file = paste("ORPHANHOOD/SUMMARY/municipality_inc_orphans", file_name, ".RDS", sep = ""))

if (type.input != "Municipality") {
  tmp_orphans <- inc_tab$orphans
  tmp_orp_per <- inc_tab$orphans
  tmp_pop_chl <- inc_tab$tmp_population; tmp_pop_chl <- tmp_pop_chl %>% rename(mun = loc)
  
  if (type.input == "Department") {
    tmp_orphans <- tmp_orphans %>% left_join(geo_info[, c("mun", "dep", "dep_name")], by = "mun") %>% dplyr::select(dep, dep_name, gender, year, n_orp)
    tmp_orphans <- tmp_orphans %>% group_by(dep, dep_name, gender, year) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(dep, gender, year)
    tmp_pop_chl <- tmp_pop_chl %>% left_join(geo_info[, c("mun", "dep")], by = "mun") %>% dplyr::select(dep, year, children)
    tmp_pop_chl <- tmp_pop_chl %>% group_by(dep, year) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(dep, year)
    tmp_orp_per <- tmp_orphans
    tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("dep", "year")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% dplyr::select(-children)
    if (should_round) { tmp_orp_per$n_orp <- ceiling(tmp_orp_per$n_orp) }
  } else if (type.input == "Region") {
    tmp_orphans <- tmp_orphans %>% left_join(geo_info[, c("mun", "reg", "reg_name")], by = "mun") %>% dplyr::select(reg, reg_name, gender, year, n_orp)
    tmp_orphans <- tmp_orphans %>% group_by(reg, reg_name, gender, year) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(reg, gender, year)
    tmp_pop_chl <- tmp_pop_chl %>% left_join(geo_info[, c("mun", "reg")], by = "mun") %>% dplyr::select(reg, year, children)
    tmp_pop_chl <- tmp_pop_chl %>% group_by(reg, year) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(reg, year)
    tmp_orp_per <- tmp_orphans
    tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("reg", "year")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% dplyr::select(-children)
  } else if (type.input == "National") {
    tmp_orphans <- tmp_orphans %>% left_join(geo_info[, c("mun", "nat", "nat_name")], by = "mun") %>% dplyr::select(nat, nat_name, gender, year, n_orp)
    tmp_orphans <- tmp_orphans %>% group_by(nat, nat_name, gender, year) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(nat, gender, year)
    tmp_pop_chl <- tmp_pop_chl %>% left_join(geo_info[, c("mun", "nat")], by = "mun") %>% dplyr::select(nat, year, children)
    tmp_pop_chl <- tmp_pop_chl %>% group_by(nat, year) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(nat, year)
    tmp_orp_per <- tmp_orphans
    tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("nat", "year")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% dplyr::select(-children)
  } else { stop("Invalid `type.input`.") }
  
  inc_tab$orphans <- tmp_orphans
  inc_tab$orphans_per_child <- tmp_orp_per
}

if (should_round) { inc_tab$orphans_per_child$n_orp <- ceiling(inc_tab$orphans_per_child$n_orp) }

write_csv(x = inc_tab$orphans          , file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/", tolower(type.input), "_inc_orphans_abs", file_name, ".csv", sep = ""))
write_csv(x = inc_tab$orphans_per_child, file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/", tolower(type.input), "_inc_orphans_per_", as.integer(per_n_children), file_name, ".csv", sep = ""))

###################
### PREVALENCE ####
###################

if (type.input != "Municipality") { # As prevalence is computed for departments (or larger) only
  
  # Pre-computed in `orphanhood.R`
  pre_tab <- readRDS(file = paste("ORPHANHOOD/SUMMARY/department_pre_orphans", file_name, ".RDS", sep = ""))
  
  if (type.input != "Department") {
    tmp_orphans <- pre_tab$orphans
    tmp_orp_per <- pre_tab$orphans
    tmp_pop_chl <- pre_tab$nb_children
    
    if (type.input == "Region") {
      tmp_orphans <- tmp_orphans %>% left_join(distinct(geo_info[, c("dep", "reg", "reg_name")]), by = "dep") %>% dplyr::select(reg, reg_name, gender, n_orp)
      tmp_orphans <- tmp_orphans %>% group_by(reg, reg_name, gender) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(reg, gender)
      tmp_pop_chl <- tmp_pop_chl %>% left_join(distinct(geo_info[, c("dep", "reg")]), by = "dep") %>% dplyr::select(reg, gender, children)
      tmp_pop_chl <- tmp_pop_chl %>% group_by(reg, gender) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(reg, gender)
      tmp_orp_per <- tmp_orphans
      tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("reg", "gender")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% dplyr::select(-children)
    } else if (type.input == "National") {
      tmp_orphans <- tmp_orphans %>% left_join(distinct(geo_info[, c("dep", "nat", "nat_name")]), by = "dep") %>% dplyr::select(nat, nat_name, gender, n_orp)
      tmp_orphans <- tmp_orphans %>% group_by(nat, nat_name, gender) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(nat, gender)
      tmp_pop_chl <- tmp_pop_chl %>% left_join(distinct(geo_info[, c("dep", "nat")]), by = "dep") %>% dplyr::select(nat, gender, children)
      tmp_pop_chl <- tmp_pop_chl %>% group_by(nat, gender) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(nat, gender)
      tmp_orp_per <- tmp_orphans
      tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("nat", "gender")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% dplyr::select(-children)
    } else { stop("Invalid `type.input`") }
    
    pre_tab$orphans <- tmp_orphans
    pre_tab$orphans_per_child <- tmp_orp_per
  }
  
  if (should_round) { pre_tab$orphans_per_child$n_orp <- ceiling(pre_tab$orphans_per_child$n_orp) }
  
  write_csv(x = pre_tab$orphans          , file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/", tolower(type.input), "_pre_orphans_abs", file_name, ".csv", sep = ""))
  write_csv(x = pre_tab$orphans_per_child, file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/", tolower(type.input), "_pre_orphans_per_", as.integer(per_n_children), file_name, ".csv", sep = ""))
  
} else{ print(":)") }

