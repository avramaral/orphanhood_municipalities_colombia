# JOINT MODEL FOR NATIONAL COUNT AND MUNICIPALITY DEATHS

source("R/header.R")
source("R/aux.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")

mort      <- data$mort 
geo_info  <- data$geo_info
colombia  <- data$colombia

# National mortality
nat_mort <- mort %>% dplyr::select(year, mun, gender, age, deaths, population) %>% group_by(year, gender, age) %>% summarise(deaths = sum(deaths), population = sum(population))

# Aggregated population based on the census year (i.e., 2018)
pop_2018    <- mort %>% filter(year == 2018) %>% dplyr::select(mun, gender, age, population)
p_nat       <- pop_2018 %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
p_nat_mat   <- acast(p_nat, age ~ gender, value.var = "p_nat") # (A x G)
p_nat_total <- sum(pop_2018$population)
p_nat_mat_prop <- p_nat_mat / p_nat_total
p_nat_mat_prop_fem <- p_nat_mat_prop[, "Female"]; p_nat_mat_prop_fem <- as.matrix(p_nat_mat_prop_fem[!is.na(p_nat_mat_prop_fem)]); colnames(p_nat_mat_prop_fem) <- "Female"; 
p_nat_mat_prop_mal <- p_nat_mat_prop[,   "Male"]; p_nat_mat_prop_mal <- as.matrix(p_nat_mat_prop_mal[!is.na(p_nat_mat_prop_mal)]); colnames(p_nat_mat_prop_mal) <-   "Male"; 

mpi <- mort %>% filter(year == 2018, gender == "Female", age == "10-14") %>% mutate(mpi = mpi / 100) # Between 0 and 1
mpi <- mpi  %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% dplyr::select(mpi, capital)
mpi_capital <- mpi %>% filter(capital == 1) %>% dplyr::select(mpi) %>% c() %>% unlist() %>% unname()
mpi <- mpi %>% dplyr::select(mpi) %>% c() %>% unlist() %>% unname()

std_death_rate <- mort %>% dplyr::select(mun, year, age, gender, deaths, population, death_rate)
std_death_rate <- std_death_rate %>% left_join(y = p_nat, by = c("age", "gender"))
std_death_rate <- std_death_rate %>% mutate(p_nat_total = p_nat_total)
std_death_rate <- std_death_rate %>% mutate(std_death_rate = ((p_nat / p_nat_total) * death_rate))
std_death_rate <- std_death_rate %>% group_by(mun, year) %>% summarise(std_death_rate = sum(std_death_rate)) %>% ungroup()
std_death_rate <- std_death_rate %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")
std_death_rate_mat <- acast(std_death_rate, mun ~ year, value.var = "std_death_rate") # (L x Y)

std_death_rate_capital <- std_death_rate %>% filter(capital == 1) %>% dplyr::select(-capital)
std_death_rate_capital_mat <- acast(std_death_rate_capital, mun ~ year, value.var = "std_death_rate") # (C x Y)

##############################
# Stan model
##############################

stan_directory <- "STAN/"
p <- "mortality_v1.stan"
m <- cmdstan_model(paste(stan_directory, p, sep = ""))

# Construct `data_list`
Y <- length(unique(mort$year))   # Total number of years
A_fem <- length(unique(mort[mort$gender == "Female", ]$age)) # Total number of age groups
A_mal <- length(unique(mort[mort$gender ==   "Male", ]$age)) # Total number of age groups
G <- length(unique(mort$gender)) # Total number of genders 
L <- length(unique(mort$mun))    # Total number of municipalities
C <- sum(geo_info$capital)       # Total number of capitals (or departments)

nat_mort_fem <- nat_mort %>% filter(gender == "Female") %>% dplyr::select(-gender)
nat_mort_mal <- nat_mort %>% filter(gender ==   "Male") %>% dplyr::select(-gender)
deaths_array_nat_fem <- acast(nat_mort_fem, year ~ age, value.var = "deaths") # (Y x A_fem)
deaths_array_nat_mal <- acast(nat_mort_mal, year ~ age, value.var = "deaths") # (Y x A_mal)
popula_array_nat_fem <- acast(nat_mort_fem, year ~ age, value.var = "population") # (Y x A_fem)
popula_array_nat_mal <- acast(nat_mort_mal, year ~ age, value.var = "population") # (Y x A_mal)

# Create data file for STAN
data_list <- list(
  # COUNTS
  Y = Y,
  A_fem = A_fem,
  A_mal = A_mal,
  G = G,
  L = L,
  C = C,
  
  # NATIONAL LEVEL
  deaths_nat_fem     = deaths_array_nat_fem, # Death counts (Y x A_fem)
  deaths_nat_mal     = deaths_array_nat_mal, # Death counts (Y x A_mal)
  population_nat_fem = popula_array_nat_fem, # Population   (Y x A_fem)
  population_nat_mal = popula_array_nat_mal, # Population   (Y x A_mal)
  
  # MUNICIPALITY LEVEL
  std_death_rate_capital = std_death_rate_capital_mat, # Pre-computed standardized death rates in the capitals (C x Y)
  proportion_pop_nat_fem = c(p_nat_mat_prop_fem),
  proportion_pop_nat_mal = c(p_nat_mat_prop_mal),
  
  # OTHERS
  age_value_fem = range_0_1(1:A_fem), # Age group values (A_fem)
  age_value_mal = range_0_1(1:A_mal), # Age group values (A_mal)
  mpi_municip = mpi,          # MPI in all municipalities (L)
  mpi_capital = mpi_capital   # MPI in the capitals (C)
)

# Fit the model
fitted_model <- m$sample(data = data_list,
                         seed = 1,             # Set seed for reproducibility
                         chains = 4,           # Number of Markov chains
                         parallel_chains = 4,  # Number of parallel chains
                         iter_warmup = 18000,  # Number of warm up iterations
                         iter_sampling = 2000, # Number of sampling iterations
                         thin = 4)             # Thinning (period between saved samples) to save memory

summ <- fitted_model$summary()

fitted_model$save_object(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_fit.RDS", sep = ""))

d <- fitted_model$draws(variables = NULL, inc_warmup = FALSE, format = "draws_matrix")
saveRDS(object = list(data = data_list, draws = d), file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))

if (TRUE) { mcmc_trace(d, pars = c("alpha_1[1]", "death_rate_mal[1]", "inv_logit_death_rate_nat_mal[1,1]", "std_death_rate_nat[1]", 
                                   "gp_sigma_fem", "gp_sigma_mal", "gp_length_scale_fem", "gp_length_scale_mal")) }
