# NATIONAL MODEL (ONLY) FOR BIRTHS

source("R/header.R")
source("R/aux.R")

data <- readRDS(file = "DATA/fertility_bias_data.RDS")

fert      <- data$fert # To make age groups for female and male comparable, we included ages `50-54` and `55-59` for female always with `0` births.
fert      <- fert %>% filter(gender == "Male" | (gender == "Female" & !(age %in% c("50-54", "55-59")))) # Model men and women separately, so that we can ignore female `50-54` and `55-59`.
geo_info  <- data$geo_info
colombia  <- data$colombia

# National mortality
nat_fert <- fert %>% dplyr::select(year, mun, gender, age, births, population) %>% group_by(year, gender, age) %>% summarise(births = sum(births), population = sum(population))
nat_fert_fem <- nat_fert %>% filter(gender == "Female") %>% dplyr::select(-gender)
nat_fert_mal <- nat_fert %>% filter(gender ==   "Male") %>% dplyr::select(-gender)

###########################################################
# INCLUDE !HERE! THE CODE FOR MUNICIAPLITY-LEVEL ANALYSES #
###########################################################

##############################
# Stan model
##############################

stan_directory <- "STAN/"
p <- "fertility_v2_2.stan"
m <- cmdstan_model(paste(stan_directory, p, sep = ""))

# Construct `data_list`
Y <- length(unique(fert$year))   # Total number of years
A_fem <- length(unique(filter(fert, gender == "Female")$age)) # Total number of female age groups
A_mal <- length(unique(filter(fert, gender ==   "Male")$age)) # Total number of   male age groups
L <- length(unique(fert$mun)) # Total number of municipalities
C <- sum(geo_info$capital)    # Total number of capitals (or departments)

births_array_nat_fem <- acast(nat_fert_fem, year ~ age, value.var = "births")     # (Y x A_fem)
popula_array_nat_fem <- acast(nat_fert_fem, year ~ age, value.var = "population") # (Y x A_fem)
births_array_nat_mal <- acast(nat_fert_mal, year ~ age, value.var = "births")     # (Y x A_mal)
popula_array_nat_mal <- acast(nat_fert_mal, year ~ age, value.var = "population") # (Y x A_mal)

# Create data file for STAN
data_list <- list(
  # COUNTS
  Y = Y,
  A_fem = A_fem,
  A_mal = A_mal,
  L = L,
  C = C,
  
  # NATIONAL LEVEL
  births_nat_fem     = births_array_nat_fem, # Birth female counts (Y x A_fem)
  population_nat_fem = popula_array_nat_fem, # Population female   (Y x A_fem)
  births_nat_mal     = births_array_nat_mal, # Birth   male counts (Y x A_mal)
  population_nat_mal = popula_array_nat_mal, # Population   male   (Y x A_mal)
  
  # MUNICIPALITY LEVEL
  # TBD
  # TBD
  
  # OTHERS
  age_value_fem = range_0_1(1:A_fem), # Age female group values (A_fem)
  age_value_mal = range_0_1(1:A_mal)  # Age   male group values (A_mal)
  # TBD
  # TBD
)

# Fit the model
fitted_model <- m$sample(data = data_list,
                         seed = 1,             # Set seed for reproducibility
                         chains = 4,           # Number of Markov chains
                         parallel_chains = 4,  # Number of parallel chains
                         iter_warmup = 2000,   # Number of warm up iterations
                         iter_sampling = 4000, # Number of sampling iterations
                         thin = 4)             # Thinning (period between saved samples) to save memory

fitted_model$save_object(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_fit.RDS", sep = ""))

d <- fitted_model$draws(variables = NULL, inc_warmup = FALSE, format = "draws_matrix")
saveRDS(object = list(data = data_list, draws = d), file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))

if (TRUE) { mcmc_trace(d, pars = c("fertility_rate_fem[1]", "fertility_rate_mal[1]", "inv_log_fertility_rate_nat_fem[1,1]", "inv_log_fertility_rate_nat_mal[1,1]")) }
