# JOINT MODEL FOR NATIONAL COUNT AND MUNICIPALITY BIRTHS

source("R/header.R")
source("R/aux.R")

data <- readRDS(file = "DATA/fertility_bias_data.RDS")

fert      <- data$fert # To make age groups for female and male comparable, we included ages `50-54` and `55-59` for female always with `0` births.
geo_info  <- data$geo_info
colombia  <- data$colombia

# National fertility
nat_fert <- fert %>% dplyr::select(year, mun, gender, age, births, population) %>% group_by(year, gender, age) %>% summarise(births = sum(births), population = sum(population))

# Aggregated population based on the census year (i.e., 2018)
pop_2018    <- fert %>% filter(year == 2018) %>% dplyr::select(mun, gender, age, population)
p_nat       <- pop_2018 %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
p_nat_mat   <- acast(p_nat, age ~ gender, value.var = "p_nat") # (A x G)
p_nat_total <- sum(pop_2018$population)
p_nat_mat_prop <- p_nat_mat / p_nat_total

mpi <- fert %>% filter(year == 1998, gender == "Female", age == "10-14") %>% mutate(mpi = mpi / 100) # Between 0 and 1
mpi <- mpi  %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% dplyr::select(mpi, capital)
mpi_capital <- mpi %>% filter(capital == 1) %>% dplyr::select(mpi) %>% c() %>% unlist() %>% unname()
mpi <- mpi %>% dplyr::select(mpi) %>% c() %>% unlist() %>% unname()

std_fertility_rate <- fert %>% dplyr::select(mun, year, age, gender, births, population, fertility_rate)
std_fertility_rate <- std_fertility_rate %>% left_join(y = p_nat, by = c("age", "gender"))
std_fertility_rate <- std_fertility_rate %>% mutate(p_nat_total = p_nat_total)
std_fertility_rate <- std_fertility_rate %>% mutate(std_fertility_rate = ((p_nat / p_nat_total) * fertility_rate))
std_fertility_rate <- std_fertility_rate %>% group_by(mun, year) %>% summarise(std_fertility_rate = sum(std_fertility_rate)) %>% ungroup()
std_fertility_rate <- std_fertility_rate %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")
std_fertility_rate_mat <- acast(std_fertility_rate, mun ~ year, value.var = "std_fertility_rate") # (L x Y)

std_fertility_rate_capital <- std_fertility_rate %>% filter(capital == 1) %>% dplyr::select(-capital)
std_fertility_rate_capital_mat <- acast(std_fertility_rate_capital, mun ~ year, value.var = "std_fertility_rate") # (C x Y)

# E.g., in 2018
# ggplot(data = data.frame(mpi = mpi_capital * 100, std_fertility_rate = std_fertility_rate_capital_mat[, "2018"]), mapping = aes(x = mpi, y = std_fertility_rate)) + geom_point() + geom_smooth(method = "lm") + labs(x = "MPI", y = "Standardised fertility rate") + scale_x_continuous(limits = c(0, 100)) + theme_bw()

##############################
# Stan model
##############################

stan_directory <- "STAN/"
p <- "fertility_v1_2.stan"
m <- cmdstan_model(paste(stan_directory, p, sep = ""))

# Construct `data_list`
Y <- length(unique(fert$year))   # Total number of years
A <- length(unique(fert$age))    # Total number of age groups
G <- length(unique(fert$gender)) # Total number of genders 
L <- length(unique(fert$mun))    # Total number of municipalities
C <- sum(geo_info$capital)       # Total number of capitals (or departments)

births_array_nat <- acast(nat_fert, year ~ age ~ gender, value.var = "births")     # (Y x A x G)
popula_array_nat <- acast(nat_fert, year ~ age ~ gender, value.var = "population") # (Y x A x G)

# Create data file for STAN
data_list <- list(
  # COUNTS
  Y = Y,
  A = A,
  G = G,
  L = L,
  C = C,
  
  # NATIONAL LEVEL
  births_nat     = births_array_nat, # Birth counts (Y x A x G)
  population_nat = popula_array_nat, # Population   (Y x A x G)
  
  # MUNICIPALITY LEVEL
  std_fertility_rate_capital = std_fertility_rate_capital_mat, # Pre-computed standardized fertility rates in the capitals (C x Y)
  proportion_pop_nat = p_nat_mat_prop,
  
  # OTHERS
  age_value = range_0_1(1:A), # Age group values (A)
  mpi_municip = mpi,          # MPI in all municipalities (L)
  mpi_capital = mpi_capital   # MPI in the capitals (C)
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

if (TRUE) { mcmc_trace(d, pars = c("alpha_0[1]", "fertility_rate_baseline[1]", "inv_log_fertility_rate_nat[1,1,1]", "std_fertility_rate_nat[1]")) }
