data {
  // COUNTS
  int<lower=0> Y; // Number of years
  int<lower=0> A; // Number of age groups
  int<lower=0> G; // Number of genders
  int<lower=0> L; // Number of locations
  int<lower=0> C; // Number of capitals
  
  // NATIONAL LEVEL
  array[Y, A, G] int<lower=0> deaths_nat;     // Array of death counts (National level)
  array[Y, A, G] int<lower=0> population_nat; // Array of population   (National level)
  
  // MUNICIPALITY LEVEL
  matrix<lower=0>[C, Y] std_death_rate_capital; // Array of standardized death rates in the capitals
  matrix<lower=0>[A, G] proportion_national_pop_by_gender; // Total population in the reference year (e.g., 2018) by age group and gender 
  
  // OTHERS
  array[A] int age_value; // Age group values (for GP)
  vector<lower=0, upper=1>[L] mpi_municip;         // MPI in all municipalities
  vector<lower=0, upper=1>[C] mpi_capital; // MPI in the capitals
}

parameters {
  // modelling national death by age curve
  vector[Y] log_mrate_baseline_year;   // Intercept (National level)
  vector[Y] log_mrate_male_effect;   // Coefficient for male indicator for each year (National level) // TODO consider scalar?
  
  // modelling national death by age curve -- GP part
  matrix[A,Y] z_fem; // Random variable to create GP for women
  matrix[A,Y] z_mal; // Random variable to create GP for men
  real<lower=0> gp_sigma;        
  real<lower=0> gp_length_scale; 
  
  vector[Y] alpha_0; // Intercept (linear regression for the capitals)
  vector[Y] alpha_1; // MPI coefficient (linear regression for the capitals)
  real<lower=0> std_mrate_capital_sigma; // Should it vary (or be commonly defined) across the years?
}

transformed parameters {
  array[G] matrix[Y, A] inv_logit_mrate_nat;
  matrix[C,Y] std_mrate_capital_mean;
  
  // Linear predictor with GP (National level)
  {
    matrix[A, A] L_mrate_nat = cholesky_decompose( gp_exp_quad_cov(age_value, gp_sigma, gp_length_scale) + diag_matrix(rep_vector(1e-10, A)) );
    
    for (y in 1:Y) {
      inv_logit_mrate_nat[1][y, ] = (log_mrate_baseline_year[y] + L_mrate_nat * z_fem[,y])';             // Female
      inv_logit_mrate_nat[2][y, ] = (log_mrate_baseline_year[y] + log_mrate_male_effect[y] + L_mrate_nat * z_mal[,y])'; // Male
    }
  }
  
  for (y in 1:Y) {
     std_mrate_capital_mean[,y] = alpha_0[y] + alpha_1[y] * mpi_capital;
  }
}

model {
  // log likelihood for national death-by-age curve
  for (g in 1:G) { // Can be improved for efficiency if I can flatten array; however, the performance should not be affected
      target += binomial_logit_lupmf( 
        to_array_1d(deaths_nat[, , g]) |  // collapse in row-major order
        to_array_1d(population_nat[, , g]), // collapse in row-major order
        to_vector(inv_logit_mrate_nat[g]')
        );
    }
  
  // TODO change everything to target += 
  
  // priors for national death-by-age curve
  log_mrate_baseline_year ~ normal(0, 3);
  log_mrate_male_effect ~ std_normal();
  z_fem ~ std_normal();
  z_mal ~ std_normal();
  gp_sigma ~ normal(0, 1);
  gp_length_scale ~ inv_gamma(5, 1);

  // likelihood for relationship of standardised mortality rates  
  to_vector(std_death_rate_capital) ~ normal(to_vector(std_mrate_capital_mean), std_mrate_capital_sigma); 
  
  // priors for relationship of standardised mortality rates  
  alpha_0 ~ normal(0, 0.0025);
  alpha_1 ~ normal(0, 1);
  std_mrate_capital_sigma ~ normal(0, 0.0025);
  
  // Intermediate quantities that are not required to be saved
  {
    for (y in 1:Y) {
      // Standardised death rates for all municipalities
      std_death_rate[, y] ~ normal(alpha_0[y] + alpha_1[y] * mpi, std_mrate_capital_sigma); // Prediction model
    }
  }
}

generated quantities {
  vector<lower=0>[Y] std_mrate_national;
  matrix<lower=0>[L, Y] log_std_mrate_municip_mult; 
  array[Y] matrix[L, A] log_mu_municipality_fem; // Predicted death rates for all municipalities 
  array[Y] matrix[L, A] log_mu_municipality_mal;
  
  
  // initialise transformed parameters
  std_mrate_national = rep_vector(0, Y);
  
  for (y in 1:Y) {
    for (g in 1:G) {
     std_mrate_national[y] += dot_product( proportion_national_pop_by_gender[, g], inv_logit(inv_logit_mrate_nat[g][y, ] );
    }
    
    log_std_mrate_municip_mult[,y] = log( alpha_0[y] + alpha_1[y] * mpi_municip ) - log(std_mrate_national[y]);
    
    log_mu_municipality_fem[y] = log_std_mrate_municip_mult[,y] + log( inv_logit(inv_logit_mrate_nat[1][y, ]) ); // Female first
    log_mu_municipality_mal[y] = log_std_mrate_municip_mult[,y] + log( inv_logit(inv_logit_mrate_nat[2][y, ]) );
  }
}
