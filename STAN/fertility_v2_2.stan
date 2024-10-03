// NATIONAL MODEL (ONLY) WITH NEGATIVE BINOMIAL FOR THE NATIONAL COUNT

data {
  // COUNTS
  int<lower=0> Y;     // Number of years
  int<lower=0> A_fem; // Number of age female groups
  int<lower=0> A_mal; // Number of age   male groups
  int<lower=0> L;     // Number of locations
  int<lower=0> C;     // Number of capitals
  
  // NATIONAL LEVEL
  array[Y, A_fem] int<lower=0> births_nat_fem;     // Array of female births counts (National level)
  array[Y, A_fem] int<lower=0> population_nat_fem; // Array of female population    (National level)
  array[Y, A_mal] int<lower=0> births_nat_mal;     // Array of   male births counts (National level)
  array[Y, A_mal] int<lower=0> population_nat_mal; // Array of   male population    (National level)
  
  // MUNICIPALITY LEVEL
  // TBD
  // TBD
  
  // OTHERS
  array[A_fem] real age_value_fem; // Age female group values (for GP), between 0 and 1
  array[A_mal] real age_value_mal; // Age   male group values (for GP), between 0 and 1
  // TBD
  // TBD
}

parameters {
  // National births
  vector[Y] fertility_rate_fem; // Intercept for female group (National level)
  vector[Y] fertility_rate_mal; // Intercept for   male group (National level)
  
  // GP for national births
  matrix[Y, A_fem] z_fem; // Random variable to create GP (female)
  matrix[Y, A_mal] z_mal; // Random variable to create GP (male)
  real<lower=0> gp_sigma_fem;
  real<lower=0> gp_sigma_mal;
  real<lower=0> gp_length_scale_fem;
  real<lower=0> gp_length_scale_mal;
  
  // Dispersion parameters for Negative Binomial
  real<lower=0> phi_fem; // Dispersion for female
  real<lower=0> phi_mal; // Dispersion for   male
}

transformed parameters {
  matrix[Y, A_fem] inv_log_fertility_rate_nat_fem;
  matrix[Y, A_mal] inv_log_fertility_rate_nat_mal;
  
  // Linear predictor (National level)
  {
    matrix[A_fem, A_fem] L_fertility_rate_nat_fem = cholesky_decompose(gp_exp_quad_cov(age_value_fem, gp_sigma_fem, gp_length_scale_fem) + diag_matrix(rep_vector(1e-10, A_fem)));
    matrix[A_mal, A_mal] L_fertility_rate_nat_mal = cholesky_decompose(gp_exp_quad_cov(age_value_mal, gp_sigma_mal, gp_length_scale_mal) + diag_matrix(rep_vector(1e-10, A_mal)));
    
    
    for (y in 1:Y) {
      inv_log_fertility_rate_nat_fem[y, ] = (fertility_rate_fem[y] + L_fertility_rate_nat_fem * z_fem[y, ]')'; // Female
      inv_log_fertility_rate_nat_mal[y, ] = (fertility_rate_mal[y] + L_fertility_rate_nat_mal * z_mal[y, ]')'; // Male
    }
  }
}

model {
  // Log-likelihood (National level)
  target += neg_binomial_2_log_lpmf(to_array_1d(births_nat_fem[, ]) | log(to_vector(to_array_1d(population_nat_fem[, ]))) + to_vector(inv_log_fertility_rate_nat_fem'), phi_fem);
  target += neg_binomial_2_log_lpmf(to_array_1d(births_nat_mal[, ]) | log(to_vector(to_array_1d(population_nat_mal[, ]))) + to_vector(inv_log_fertility_rate_nat_mal'), phi_mal);
  
  // Priors (National level)
  target += normal_lpdf(fertility_rate_fem | 0, 1); 
  target += normal_lpdf(fertility_rate_mal | 0, 1);
  
  target += normal_lpdf(to_vector(z_fem) | 0, 1);
  target += normal_lpdf(to_vector(z_mal) | 0, 1);
  
  target += normal_lpdf(gp_sigma_fem | 0, 0.5);
  target += normal_lpdf(gp_sigma_mal | 0, 0.5); 
  target += inv_gamma_lpdf(gp_length_scale_fem | 5, 1); 
  target += inv_gamma_lpdf(gp_length_scale_mal | 5, 1); 
  
  // Hyperparameters
  target += normal_lpdf(phi_fem | 0, 1);
  target += normal_lpdf(phi_mal | 0, 1);
}
