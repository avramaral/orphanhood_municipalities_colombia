source("header.R")
source("aux.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")

mort      <- data$mort
geo_info  <- data$geo_info
colombia  <- data$colombia

# National mortality
nat_mort <- mort %>% dplyr::select(year, mun, gender, age, deaths, population) %>% group_by(year, gender, age) %>% summarise(deaths = sum(deaths), population = sum(population))

##############################
# Stan model
##############################

p <- "mortality_bias.stan"
m <- cmdstan_model(p)

# Construct `data_list`
Y <- length(unique(mort$year))   # Total number of years
A <- length(unique(mort$age))    # Total number of age groups
G <- length(unique(mort$gender)) # Total number of genders 
L <- length(unique(mort$mun))    # Total number of genders 
N <- nrow(mort)         # Total number of observations at the municipality level
N_nat <- nrow(nat_mort) # Total number of observations at the national level

Ys_nat <- nat_mort$year - min(nat_mort$year) + 1    # Year indices at the national level (N_nat)
Gs_nat <- ifelse(nat_mort$gender == "Female", 1, 2) # Gender indices at the national level (N_nat)
As_nat <- as.integer(factor(nat_mort$age))          # Age group indices at the national level (N_nat)

Ys <- mort$year - min(mort$year) + 1        # Year indices (N)
Ls <- as.integer(factor(mort$mun))          # Municipality indices (N)
Gs <- ifelse(mort$gender == "Female", 1, 2) # Gender indices (N)
As <- as.integer(factor(mort$age))          # Age group indices (N)

pop_2018 <- mort %>% filter(year == 2018) %>% dplyr::select(mun, gender, age, population)
p_nat <- pop_2018 %>% group_by(gender, age) %>% summarise(populations = sum(population)) %>% ungroup() %>% pivot_wider(names_from = gender, values_from = populations) %>% column_to_rownames(var = "age") %>% as.matrix() %>% `rownames<-`(NULL) %>% `colnames<-`(NULL)
p_nat_total <- sum(pop_2018$population)

death_rate_pop <- death_rate_pop_array(array_file = "DATA/death-rate_pop_array.RDS") # Convert tibble to array, or simple read it (if it has been done before)
death_rate <- death_rate_pop$death_rate
total_popu <- death_rate_pop$total_popu

# Create data file for STAN
data_list <- list(
  # COUNTS
  Y = Y,
  A = A,
  G = G,
  L = L,
  N = N,
  N_nat = N_nat,
  
  # NATIONAL LEVEL
  year_nat       = Ys_nat,
  gender_nat     = Gs_nat,
  age_nat        = As_nat,
  deaths_nat     = nat_mort$deaths,     # Death counts (N_nat)
  population_nat = nat_mort$population, # Population   (N_nat)
  
  # MUNICIPALITY LEVEL
  year       = Ys,
  location   = Ls,
  gender     = Gs,
  age        = As,
  deaths     = mort$deaths,     # Death counts (N)
  population = mort$population, # Population (N)
  death_rate = death_rate,      # Death rate (L x Y x A x G)
  total_popu = total_popu,      # Total population (L x Y x A x G)
  P_nat = p_nat,
  P_nat_total = p_nat_total,
  
  # OTHERS
  age_values = 1:length(unique(As)), # Age group values (A)
  mpi = c(scale(mort %>% filter(year == 1998, gender == "Female", age == "10-14") %>% dplyr::select(mpi) %>% c() %>% unlist() %>% unname())) # Scaled MPI values (L)
)

# Fit the model
fitted_model <- m$sample(data = data_list,
                         seed = 1,             # Set seed for reproducibility
                         chains = 4,           # Number of Markov chains
                         parallel_chains = 4,  # Number of parallel chains
                         iter_warmup = 1000,   # Number of warm up iterations
                         iter_sampling = 2000) # Number of sampling iterations
                        
fitted_model$save_object(file = "fitted_model.RDS")
