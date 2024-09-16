source("header.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")

mort      <- data$mort
geo_info  <- data$geo_info
colombia  <- data$colombia

# National mortality
nat_mort <- mort %>% dplyr::select(year, mun, gender, age, deaths, population) %>% group_by(year, gender, age) %>% summarise(deaths = sum(deaths), population = sum(population))

##############################
# Stan model
##############################

p <- "new_mortality_bias.stan"
m <- cmdstan_model(p)

# Construct `data_list`
Y <- length(unique(mort$year))   # Total number of years
A <- length(unique(mort$age))    # Total number of age groups
G <- length(unique(mort$gender)) # Total number of genders 

deaths_array_nat <- acast(nat_mort, year ~ age ~ gender, value.var = "deaths")     # (Y x A x G)
popula_array_nat <- acast(nat_mort, year ~ age ~ gender, value.var = "population") # (Y x A x G)

# Create data file for STAN
data_list <- list(
  # COUNTS
  Y = Y,
  A = A,
  G = G,
  
  # NATIONAL LEVEL
  deaths_nat     = deaths_array_nat, # Death counts (Y x A x G)
  population_nat = popula_array_nat, # Population   (Y x A x G)
  
  # OTHERS
  age_value = range_0_1(1:A) # Age group values (A)
)

# Fit the model
fitted_model <- m$sample(data = data_list,
                         seed = 999,           # Set seed for reproducibility
                         chains = 4,           # Number of Markov chains
                         parallel_chains = 4,  # Number of parallel chains
                         iter_warmup = 1000,   # Number of warm up iterations
                         iter_sampling = 3000) # Number of sampling iterations

fitted_model$save_object(file = "FITTED/fitted_national_model.RDS")
