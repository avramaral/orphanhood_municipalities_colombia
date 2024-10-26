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

mpi <- mort %>% filter(year == 1998, gender == "Female", age == "10-14") %>% mutate(mpi = mpi / 100) # Between 0 and 1
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

# E.g., in 2018
# ggplot(data = data.frame(mpi = mpi_capital * 100, std_death_rate = std_death_rate_capital_mat[, "2018"]), mapping = aes(x = mpi, y = std_death_rate)) + geom_point() + geom_smooth(method = "lm") + labs(x = "MPI", y = "Standardised death rate") + scale_x_continuous(limits = c(0, 100)) + theme_bw()

fitted_lm <- lm(c(std_fertility_rate_capital_mat[, 21]) ~ 1 + mpi_capital) # year = 2018
summary(fitted_lm)

##############################
# Stan model
##############################

stan_directory <- "STAN/"
p <- "mortality_v1.stan"
m <- cmdstan_model(paste(stan_directory, p, sep = ""))

# Construct `data_list`
Y <- length(unique(mort$year))   # Total number of years
A <- length(unique(mort$age))    # Total number of age groups
G <- length(unique(mort$gender)) # Total number of genders 
L <- length(unique(mort$mun))    # Total number of municipalities
C <- sum(geo_info$capital)       # Total number of capitals (or departments)

deaths_array_nat <- acast(nat_mort, year ~ age ~ gender, value.var = "deaths")     # (Y x A x G)
popula_array_nat <- acast(nat_mort, year ~ age ~ gender, value.var = "population") # (Y x A x G)

# Create data file for STAN
data_list <- list(
  # COUNTS
  Y = Y,
  A = A,
  G = G,
  L = L,
  C = C,
  
  # NATIONAL LEVEL
  deaths_nat     = deaths_array_nat, # Death counts (Y x A x G)
  population_nat = popula_array_nat, # Population   (Y x A x G)
  
  # MUNICIPALITY LEVEL
  std_death_rate_capital = std_death_rate_capital_mat, # Pre-computed standardized death rates in the capitals (C x Y)
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
                         iter_sampling = 2000, # Number of sampling iterations
                         thin = 4)             # Thinning (period between saved samples) to save memory

fitted_model$save_object(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_fit.RDS", sep = ""))

d <- fitted_model$draws(variables = NULL, inc_warmup = FALSE, format = "draws_matrix")
saveRDS(object = list(data = data_list, draws = d), file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))

if (TRUE) { mcmc_trace(d, pars = c("alpha_0[1]", "death_rate_baseline[1]", "inv_logit_death_rate_nat[1,1,1]", "std_death_rate_nat[1]")) }
