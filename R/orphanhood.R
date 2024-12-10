source("R/header.R")
source("R/header_orphanhood.R")
source("R/aux.R")
source("R/aux_orphanhood.R")

#############
### FLAGS ###
#############

per1K <- TRUE
type.input <- "Municipality"
compute_prevalence <- ifelse(type.input != "Municipality", TRUE, FALSE)
per_n_children <- ifelse(type.input == "Municipality", 1000, 100000)

yys <- seq(ifelse(compute_prevalence, 2004, 2015), 2021)

# TODO
# can you please provide a README similar to 
# https://github.com/MLGlobalHealth/bayes-rate-consistency
# and move the github to https://github.com/MLGlobalHealth

# TODO
# can you please provide an installation script for all dependencies
# in a vanilla environment, see eg
# https://github.com/MLGlobalHealth/bayes-rate-consistency

# TODO  
# typically we have the following workflow:
# 1/ we have the code in a personal code directory on the HPC/cloud say git/orphanhood_code
# 2/ we spawn a local compute instance with directory say /rds/jobs/tmp/tmp12345
# 3/ we copy all input files needed to /rds/jobs/tmp/tmp12345 in one go at the start from a personal directory say or105/projects/orphanhood_with_fertility_correction_241117
#   this is because doing many read calls to a location outside the local compute instance is super inefficient
#   
# 3b/ we may also copy code to the local compute instance, but in general this may not be so
#   
# 4/ we generate all output files in subdirs of /rds/jobs/tmp/tmp12345 
#   this is because doing many write calls to a location outside the local compute instance is super inefficient
# 5/ we copy & zip the outputs from the local compute instance to the personal directory say or105/projects/orphanhood_with_fertility_correction_241117
# 
# can you please update the code so that a workflow using HPC or cloud is supported
# I think best to use 'here' and assume this is set to a vanilla local compute instance eg /rds/jobs/tmp/tmp12345
# https://cran.r-project.org/web/packages/here/vignettes/here.html
# then change this script slightly so that code is sourced from a different directory, which can be a git directory somewhere completely different

path_mortality <- "DATA/COUNTS/mort_mean.RDS"
path_fertility <- "DATA/COUNTS/fert_mean.RDS"

#################
### READ DATA ###
#################

mortality_rates <- readRDS(file = path_mortality) %>% rename(loc = mun)
fertility_rates <- readRDS(file = path_fertility) %>% rename(loc = mun)
valid_muns <- as.numeric(as.character(c(unique(mortality_rates$loc))))

if (per1K) {
  mortality_rates <- mortality_rates %>% mutate(death_rate     = death_rate     * 1000)
  fertility_rates <- fertility_rates %>% mutate(fertility_rate = fertility_rate * 1000)
}

# Oliver: please, change it so the adjusted deaths remain real-valued, they should not be rounded
# André: these counts are realizations of a Binomial, therefore, integers (so are the birth counts)
death_count <- mortality_rates %>% dplyr::select(year, loc, gender, age, deaths)

# Used to compute the total number of children, i.e., 0-17
population <- readRDS(file = "DATA/population_all.RDS")
population <- population %>% dplyr::select(year, mun, gender, age, population) %>% mutate(mun = as.numeric(as.character(mun)))
population <- population %>% filter(mun %in% valid_muns) %>% mutate(mun = factor(mun))
population <- population %>% filter(year %in% 1998:2021) %>% arrange(year, mun, gender, age)
population <- population %>% filter(age %in% c("0-9", "10-14", "15-19")) %>% rename(loc = mun)
prop_15_17 <- readRDS(file = "DATA/prop_15_17.RDS") %>% rename(loc = mun)

# Geo information
geo_info <- readRDS(file = "DATA/geo_info.RDS")

# Convert it to the desired resolution
if (type.input != "Municipality") {
  tmp_data <- convert_resolution(geo_info = geo_info, mortality_rates = mortality_rates, fertility_rates = fertility_rates, population = population, death_count = death_count, prop_15_17 = prop_15_17, type.input = type.input, per1K = per1K)

  mortality_rates <- tmp_data$mortality_rates
  fertility_rates <- tmp_data$fertility_rates
  population      <- tmp_data$population
  death_count     <- tmp_data$death_count
  prop_15_17      <- tmp_data$prop_15_17
}

# Add years if computing PREVALENCE
if (compute_prevalence) {
  
  prev_yys <- rev(2004:2014 - 17)
  
  tmp_mortality_rates <- mortality_rates %>% filter(year == (2015 - 17))
  tmp_fertility_rates <- fertility_rates %>% filter(year == (2015 - 17))
  tmp_population      <- population      %>% filter(year == (2015 - 17))
  
  for (prev_yy in prev_yys) {
    
    tmp_mortality_rates$year <- prev_yy
    tmp_fertility_rates$year <- prev_yy
    tmp_population$year      <- prev_yy
    
    mortality_rates <- bind_rows(tmp_mortality_rates, mortality_rates)
    fertility_rates <- bind_rows(tmp_fertility_rates, fertility_rates)
    population      <- bind_rows(tmp_population     , population     )
  }
  
  mortality_rates <- mortality_rates %>% arrange(year, loc, gender, age)
  fertility_rates <- fertility_rates %>% arrange(year, loc, gender, age)
  population      <- population      %>% arrange(year, loc, gender, age)
  
  death_count     <- mortality_rates %>% dplyr::select(year, loc, gender, age, deaths)
  
} else { print("At municipality level, we are not computing prevalence. This is *not* an error.") }

######################
# COMPUTE ORPHANHOOD #
######################

for (yy in yys) {
  print(paste("Year: ", yy, " (until ", yys[length(yys)], ")", sep = ""))

  # TODO 
  # is there a particular reason why this is written to csv?
  # 
  # will be faster if we just return the male fertility rates that we
  # need for this particular year yy
  # and similar for females, rather than do IO to a csv
  
  # TODO
  # it would also be useful if we could explicitly specify the age cutoff in the script, rather 
  # than have it hard coded please
  
  # TODO
  # I would not be surprised if this can be coded >10x more efficiently by avoiding so many separate R commands and separate R indexing 
  print("Processing number of children.")
  process_number_children_year(yy = yy, type.input = type.input, fertility_rates = fertility_rates, per1K = per1K)

  # Process number of orphans.
  print("Processing number of orphans.")
  # TODO 
  # this code failed for me without creating the assumed directory structure, please fix
  # this code should not read in from csv and all inputs should be specified
  # if you use data.table, then these won t be duplicated inside the downstream function
  process_nb_orphans_table_dep_national_year(yy = yy, type.input = type.input, death_count = death_count)

  print(paste("Done for year ", yy, ".", sep = ""))
}

# FINAL RESULTS ARE IN `ORPHANHOOD/SUMMARY/`
file_name <- gsub("^.*/|\\..*$", "", path_mortality)
file_name <- gsub("^.*_", "", file_name) %>% toupper()
file_name <- paste("_", file_name, sep = "")
orphan_table(type.input = type.input, population = population, prop_15_17 = prop_15_17, geo_info = geo_info, per_n_children = per_n_children, file_name = file_name)

##########################
# INCIDENCE & PREVALENCE #
##########################

# It is better to compute these quantities here, as some intermediate results will be overwritten when fitting the model for different fertility and mortality datasets.

if (type.input == "Municipality") {
  inc_tab <- generate_incidence_table(type.input = "Municipality", per_n_children = per_n_children, geo_info = geo_info, should_round = FALSE, file_name = file_name)
  saveRDS(object = inc_tab, file = paste("ORPHANHOOD/SUMMARY/", tolower(type.input), "_inc_orphans", file_name, ".RDS", sep = ""))
} else if (type.input == "Department") {
  pre_tab <- generate_prevalence_table(type.input = "Department", per_n_children = per_n_children, geo_info = geo_info, should_round = FALSE, file_name = file_name)
  saveRDS(object = pre_tab, file = paste("ORPHANHOOD/SUMMARY/", tolower(type.input), "_pre_orphans", file_name, ".RDS", sep = ""))
} else { print(":)") }
