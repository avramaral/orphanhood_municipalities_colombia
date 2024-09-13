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
  matrix<lower=0>[A, G] p_nat; // Total population in the reference year (e.g., 2018) by age group and gender 
  real<lower=0> p_nat_total;   // Total population in the reference year (e.g., 2018)
  
  // OTHERS
  array[A] int age_value; // Age group values (for GP)
  vector<lower=0, upper=100>[L] mpi;         // MPI in all municipalities
  vector<lower=0, upper=100>[C] mpi_capital; // MPI in the capitals
}

parameters {
  vector[Y] beta_0;   // Intercept (National level)
  vector[Y] beta_1;   // Coefficient for male indicator for each year (National level)
  array[Y] vector[A] d_fem; // Random effects for females across years and age groups (National level)
  array[Y] vector[A] d_mal; // Random effects for males   across years and age groups (National level)
  
  vector[Y] alpha_0; // Intercept (linear regression for the capitals)
  vector[Y] alpha_1; // MPI coefficient (linear regression for the capitals)
  real<lower=0> sigma_a; // Should it vary (or be commonly defined) across the years?
  
  matrix<lower=0>[L, Y] std_death_rate; // Array of standardized death rates for prediction, given the coefficients from the capitals 
  
  // Hyperparameters for the GP (National level)
  real<lower=0> sigma_f_nat_fem;        
  real<lower=0> sigma_f_nat_mal;        
  real<lower=0> length_scale_f_nat_fem; 
  real<lower=0> length_scale_f_nat_mal; 
}

transformed parameters {
  array[G] matrix[Y, A] mu_nat;
  vector<lower=0>[Y] std_death_rate_national;
  matrix<lower=0>[L, Y] gamma_mult; // Multiplier factor
  
  // Linear predictor with GP (National level)
  {
    matrix[A, A] K_f_nat_fem = gp_exp_quad_cov(age_value, sigma_f_nat_fem, length_scale_f_nat_fem) + diag_matrix(rep_vector(1e-10, A));
    matrix[A, A] K_f_nat_mal = gp_exp_quad_cov(age_value, sigma_f_nat_mal, length_scale_f_nat_mal) + diag_matrix(rep_vector(1e-10, A));
    matrix[A, A] L_f_nat_fem = cholesky_decompose(K_f_nat_fem);
    matrix[A, A] L_f_nat_mal = cholesky_decompose(K_f_nat_mal);
    
    for (y in 1:Y) {
      mu_nat[1][y, ] = (beta_0[y] + L_f_nat_fem * d_fem[y])';             // Female
      mu_nat[2][y, ] = (beta_0[y] + beta_1[y] + L_f_nat_mal * d_mal[y])'; // Male
    }
  }
  
  // Standardised death rate for the country
  for (y in 1:Y) {
    std_death_rate_national[y] = 0; // Initialize the sum to zero
    // Vectorized sum for age group (it is difficult to vectorize if for gender due to how `mu_nat` was constucted, but the performance should not be affected) 
    for (g in 1:G) {
      std_death_rate_national[y] = std_death_rate_national[y] + dot_product(p_nat[, g], inv_logit(mu_nat[g][y, ])); // Convert (1 x 1) matrix into real 
    }
    std_death_rate_national[y] = std_death_rate_national[y] / p_nat_total;
  }
  
  // Multiplier factor 
  gamma_mult = std_death_rate ./ rep_matrix(std_death_rate_national', L);
  
}

model {
 // Priors
  beta_0 ~ normal(0, 3);
  beta_1 ~ normal(0, 3);
  
  alpha_0 ~ normal(0, 3);
  alpha_1 ~ normal(0, 3);
  sigma_a ~ normal(0, 1);
  
  for (y in 1:Y) { 
    // Random effects (National level)
    d_fem[y] ~ normal(0, 1);
    d_mal[y] ~ normal(0, 1);
    
    // Standardised death rates for the capitals
    std_death_rate_capital[, y] ~ normal(alpha_0[y] + alpha_1[y] * mpi_capital, sigma_a); // Observational model
    std_death_rate[, y] ~ normal(alpha_0[y] + alpha_1[y] * mpi, sigma_a);                 // Prediction model
  }
  
  // Priors for hyperparameters
  sigma_f_nat_fem ~ normal(0, 1);
  sigma_f_nat_mal ~ normal(0, 1);
  length_scale_f_nat_fem ~ inv_gamma(5, 1);
  length_scale_f_nat_mal ~ inv_gamma(5, 1);

  for (g in 1:G) { // Can be improved for efficiency if I can flatten array; however, the performance should not be affected
    for (y in 1:Y) {
      target += binomial_logit_lupmf(deaths_nat[y, , g] | population_nat[y, , g], mu_nat[g][y, ]);
    }
  }
}

generated quantities {
  array[Y] matrix[L, A] mu_municipality_fem; // Estimated death rate for all municipalities 
  array[Y] matrix[L, A] mu_municipality_mal;
  
  for (y in 1:Y) {
    mu_municipality_fem[y] =  gamma_mult[, y] * mu_nat[1][y, ]; // Female first
    mu_municipality_mal[y] =  gamma_mult[, y] * mu_nat[2][y, ];
  }
}
