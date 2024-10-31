// JOINT MODEL WITH NEGATIVE BINOMIAL FOR THE NATIONAL COUNT

data {
  // COUNTS
  int<lower=0> Y; // Number of years
  int<lower=0> A; // Number of age groups
  int<lower=0> G; // Number of genders
  int<lower=0> L; // Number of locations
  int<lower=0> C; // Number of capitals
  
  // NATIONAL LEVEL
  array[Y, A, G] int<lower=0> births_nat;     // Array of births counts (National level)
  array[Y, A, G] int<lower=0> population_nat; // Array of population    (National level)
  
  // MUNICIPALITY LEVEL
  matrix<lower=0>[C, Y] std_fertility_rate_capital; // Array of standardized fertility rates in the capitals
  matrix<lower=0>[A, G] proportion_pop_nat;         // Population in the reference year (e.g., 2018) by age group and gender 
  
  // OTHERS
  array[A] real age_value; // Age group values (for GP), between 0 and 1
  vector<lower=0, upper=1>[L] mpi_municip; // MPI in all municipalities
  vector<lower=0, upper=1>[C] mpi_capital; // MPI in the capitals
}

parameters {
  // National births
  real fertility_rate_baseline_fem_0; // Initial intercept for female (National level)
  real fertility_rate_baseline_mal_0; // Initial intercept for   male (National level)
  real<lower=-1, upper=1> ar_phi_fem; // AR1 coefficient for temporal smoothing for female
  real<lower=-1, upper=1> ar_phi_mal; // AR1 coefficient for temporal smoothing for   male
  
  vector[(Y - 1)] fertility_rate_baseline_fem_residuals; // Female residuals for AR1 process
  vector[(Y - 1)] fertility_rate_baseline_mal_residuals; //   Male residuals for AR1 process
  real<lower=0> sigma_residuals_fem; // Standard deviation of residuals (female)
  real<lower=0> sigma_residuals_mal; // Standard deviation of residuals (  male)
  
  // GP for national births
  matrix[Y, A] z_fem; // Random variable to create GP (female)
  matrix[Y, A] z_mal; // Random variable to create GP (male)
  real<lower=0> gp_sigma_fem;
  real<lower=0> gp_sigma_mal;
  real<lower=0> gp_length_scale;
  real<lower=0> gp_length_scale_fem;
  real<lower=0> gp_length_scale_mal; 
  
  // Dispersion parameter for Negative Binomial
  vector<lower=0>[G] phi_dispe;          // Overall dispersion for NB, one for each gender
  vector<lower=0>[Y] phi_dispe_year_fem; // Year-specific dispersion for female
  vector<lower=0>[Y] phi_dispe_year_mal; // Year-specific dispersion for   male
  
  // Municipalities births
  vector[Y] alpha_0; // Intercept (Capitals)
  vector[Y] alpha_1; // MPI coefficient (Capitals)
  real<lower=0> std_fertility_rate_capital_sigma; // Common across all years
}

transformed parameters {
  array[G] matrix[Y, A] inv_log_fertility_rate_nat;
  matrix[C, Y] std_fertility_rate_capital_mean;
  
  // Temporal smoothing: AR1 process for fertility baselines
  vector[Y] fertility_rate_baseline_fem;
  vector[Y] fertility_rate_baseline_mal;
  
  fertility_rate_baseline_fem[1] = fertility_rate_baseline_fem_0;
  fertility_rate_baseline_mal[1] = fertility_rate_baseline_mal_0;
  for (y in 2:Y) {
    fertility_rate_baseline_fem[y] = ar_phi_fem * fertility_rate_baseline_fem[(y - 1)] + fertility_rate_baseline_fem_residuals[(y - 1)];
    fertility_rate_baseline_mal[y] = ar_phi_mal * fertility_rate_baseline_mal[(y - 1)] + fertility_rate_baseline_mal_residuals[(y - 1)];
  }
  
  // Linear predictor (National level)
  {
    matrix[A, A] L_fertility_rate_nat_fem = cholesky_decompose(gp_exp_quad_cov(age_value, gp_sigma_fem, gp_length_scale_fem) + diag_matrix(rep_vector(1e-10, A)));
    matrix[A, A] L_fertility_rate_nat_mal = cholesky_decompose(gp_exp_quad_cov(age_value, gp_sigma_mal, gp_length_scale_mal) + diag_matrix(rep_vector(1e-10, A)));
    
    for (y in 1:Y) {
      inv_log_fertility_rate_nat[1][y, ] = (fertility_rate_baseline_fem[y] + L_fertility_rate_nat_fem * z_fem[y, ]')'; // Female
      inv_log_fertility_rate_nat[2][y, ] = (fertility_rate_baseline_mal[y] + L_fertility_rate_nat_mal * z_mal[y, ]')'; // Male
    }
  }
  
  for (y in 1:Y) {
     std_fertility_rate_capital_mean[, y] = alpha_0[y] + alpha_1[y] * mpi_capital;
  }
}

model {
  // Log-likelihood (National level)
  for (g in 1:G) {
    // First two items collapse in row-major order, and the third item collaps in column-major order
    // Updated (Negative Binomial)
    if (g == 1) { 
      target += neg_binomial_2_log_lpmf(to_array_1d(births_nat[, , g]) | log(to_vector(to_array_1d(population_nat[, , g]))) + to_vector(inv_log_fertility_rate_nat[g]'), to_array_1d(rep_matrix(phi_dispe_year_fem, A)));
    } else {     
      target += neg_binomial_2_log_lpmf(to_array_1d(births_nat[, , g]) | log(to_vector(to_array_1d(population_nat[, , g]))) + to_vector(inv_log_fertility_rate_nat[g]'), to_array_1d(rep_matrix(phi_dispe_year_mal, A)));
    }
  }
  
  // Priors (National level)
  
  // Temporal smoothing priors
  target += normal_lpdf(fertility_rate_baseline_fem_residuals | 0, sigma_residuals_fem);
  target += normal_lpdf(fertility_rate_baseline_mal_residuals | 0, sigma_residuals_mal);
  
  // Priors for initial values and AR1 coefficient
  target += normal_lpdf(fertility_rate_baseline_fem_0 | 0, 3);
  target += normal_lpdf(fertility_rate_baseline_mal_0 | 0, 3);
  target += normal_lpdf(ar_phi_fem | 0.5, 0.25);   
  target += normal_lpdf(ar_phi_mal | 0.5, 0.25);   
  target += normal_lpdf(sigma_residuals_fem | 0, 1);
  target += normal_lpdf(sigma_residuals_mal | 0, 1);
  
  // Priors for GP
  
  target += normal_lpdf(to_vector(z_fem) | 0, 1);
  target += normal_lpdf(to_vector(z_mal) | 0, 1);
  
  // Priors for GP sigma and length scales (hierarchical structure)
  target += normal_lpdf(gp_sigma_fem | 0, 0.25);    
  target += normal_lpdf(gp_sigma_mal | 0, 0.25); 
  
  target += inv_gamma_lpdf(gp_length_scale | 5, 1); // Global prior for mean length scale
  target += normal_lpdf(gp_length_scale_fem | gp_length_scale, 0.25); // Female length scale around global mean
  target += normal_lpdf(gp_length_scale_mal | gp_length_scale, 0.25); //   Male length scale around global mean

  // Hyperparameters for dispersion (hierarchical structure)
  target += normal_lpdf(phi_dispe | 0, 1); // Prior for global dispersion
  target += normal_lpdf(phi_dispe_year_fem | phi_dispe[1], 0.1); // Year-gender specific dispersion (female)
  target += normal_lpdf(phi_dispe_year_mal | phi_dispe[2], 0.1); // Year-gender specific dispersion (  male)

  // Likelihood (Municipality level)
  target += normal_lpdf(to_vector(std_fertility_rate_capital) | to_vector(std_fertility_rate_capital_mean), std_fertility_rate_capital_sigma);
  
  // Priors (Municipality level)
  target += normal_lpdf(alpha_0 | 0, 0.25); 
  target += normal_lpdf(alpha_1 | 0, 1);
  target += normal_lpdf(std_fertility_rate_capital_sigma | 0, 0.025); 
}

generated quantities {
  vector[Y] std_fertility_rate_nat;

  // Initialise paramters
  std_fertility_rate_nat = rep_vector(0, Y);
  
  for (y in 1:Y) {
    // Vectorized sum over age groups
    for (g in 1:G) {
     std_fertility_rate_nat[y] = std_fertility_rate_nat[y] + dot_product(proportion_pop_nat[, g], exp(inv_log_fertility_rate_nat[g][y, ]));
    }
  }
}
