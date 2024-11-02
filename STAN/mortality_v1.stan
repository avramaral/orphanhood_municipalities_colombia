// JOINT MODEL WITH BINOMIAL FOR THE NATIONAL COUNT

data {
  // COUNTS
  int<lower=0> Y; // Number of years
  int<lower=0> A_fem; // Number of age groups
  int<lower=0> A_mal; // Number of age groups
  int<lower=0> G; // Number of genders
  int<lower=0> L; // Number of locations
  int<lower=0> C; // Number of capitals
  
  // NATIONAL LEVEL
  array[Y, A_fem] int<lower=0> deaths_nat_fem;     // Array of death counts (National level)
  array[Y, A_mal] int<lower=0> deaths_nat_mal;     // Array of death counts (National level)
  array[Y, A_fem] int<lower=0> population_nat_fem; // Array of population   (National level)
  array[Y, A_mal] int<lower=0> population_nat_mal; // Array of population   (National level)
  
  // MUNICIPALITY LEVEL
  matrix<lower=0>[C, Y] std_death_rate_capital;  // Array of standardized death rates in the capitals
  vector<lower=0>[A_fem] proportion_pop_nat_fem; // Population in the reference year (e.g., 2018) by age group (female)
  vector<lower=0>[A_mal] proportion_pop_nat_mal; // Population in the reference year (e.g., 2018) by age group (  male)
  
  // OTHERS
  array[A_fem] real age_value_fem; // Age group values (for GP), between 0 and 1 (female)
  array[A_mal] real age_value_mal; // Age group values (for GP), between 0 and 1 (  male)
  vector<lower=0, upper=1>[L] mpi_municip; // MPI in all municipalities
  vector<lower=0, upper=1>[C] mpi_capital; // MPI in the capitals
}

parameters {
  // National deaths
  vector[Y] death_rate_fem; // Intercept (National level) for female
  vector[Y] death_rate_mal; // Intercept (National level) for   male
  
  // GP for national deaths
  matrix[Y, A_fem] z_fem; // Random variable to create GP (female)
  matrix[Y, A_mal] z_mal; // Random variable to create GP (male)
  real<lower=0> gp_sigma_fem;
  real<lower=0> gp_sigma_mal;
  real<lower=0> gp_length_scale_fem;
  real<lower=0> gp_length_scale_mal;
  
  // Municipalities deaths
  vector[Y] alpha_0; // Intercept (Capitals)
  vector[Y] alpha_1; // MPI coefficient (Capitals)
  real<lower=0> std_death_rate_capital_sigma; // Common across all years
}

transformed parameters {
  matrix[Y, A_fem] inv_logit_death_rate_nat_fem;
  matrix[Y, A_mal] inv_logit_death_rate_nat_mal;
  matrix[C, Y] std_death_rate_capital_mean;
  
  // Linear predictor (National level)
  {
    matrix[A_fem, A_fem] L_death_rate_nat_fem = cholesky_decompose(gp_exp_quad_cov(age_value_fem, gp_sigma_fem, gp_length_scale_fem) + diag_matrix(rep_vector(1e-10, A_fem)));
    matrix[A_mal, A_mal] L_death_rate_nat_mal = cholesky_decompose(gp_exp_quad_cov(age_value_mal, gp_sigma_mal, gp_length_scale_mal) + diag_matrix(rep_vector(1e-10, A_mal)));
    
    for (y in 1:Y) {
      inv_logit_death_rate_nat_fem[y, ] = (death_rate_fem[y] + L_death_rate_nat_fem * z_fem[y, ]')'; // Female
      inv_logit_death_rate_nat_mal[y, ] = (death_rate_mal[y] + L_death_rate_nat_mal * z_mal[y, ]')'; // Male
    }
  }
  
  for (y in 1:Y) {
     std_death_rate_capital_mean[, y] = alpha_0[y] + alpha_1[y] * mpi_capital;
  }
}

model {
  // Log-likelihood (National level)
  // First two items collapse in row-major order, and the third item collaps in column-major order
  target += binomial_logit_lupmf(to_array_1d(deaths_nat_fem[,]) | to_array_1d(population_nat_fem[,]), to_vector(inv_logit_death_rate_nat_fem'));
  target += binomial_logit_lupmf(to_array_1d(deaths_nat_mal[,]) | to_array_1d(population_nat_mal[,]), to_vector(inv_logit_death_rate_nat_mal'));
  
  // Priors (National level)
  target += normal_lpdf(death_rate_fem | 0, 3);
  target += normal_lpdf(death_rate_mal | 0, 3);
  
  target += normal_lpdf(to_vector(z_fem) | 0, 1);
  target += normal_lpdf(to_vector(z_mal) | 0, 1);
  
  target += normal_lpdf(gp_sigma_fem | 0, 3);
  target += normal_lpdf(gp_sigma_mal | 0, 3);
  target += inv_gamma_lpdf(gp_length_scale_fem | 2.5, 1);
  target += inv_gamma_lpdf(gp_length_scale_mal | 2.5, 1);

  // Likelihood (Municipality level)
  target += normal_lpdf(to_vector(std_death_rate_capital) | to_vector(std_death_rate_capital_mean), std_death_rate_capital_sigma);
  
  // Priors (Municipality level)
  target += normal_lpdf(alpha_0 | 0, 0.25); 
  target += normal_lpdf(alpha_1 | 0, 1);
  target += normal_lpdf(std_death_rate_capital_sigma | 0, 0.0025);
}

generated quantities {
  vector[Y] std_death_rate_nat;

  // Initialise paramters
  std_death_rate_nat = rep_vector(0, Y);
  
  for (y in 1:Y) {
    // Vectorized sum over age groups
    std_death_rate_nat[y] = std_death_rate_nat[y] + dot_product(proportion_pop_nat_fem, inv_logit(inv_logit_death_rate_nat_fem[y, ]));
    std_death_rate_nat[y] = std_death_rate_nat[y] + dot_product(proportion_pop_nat_mal, inv_logit(inv_logit_death_rate_nat_mal[y, ]));
  }
}
