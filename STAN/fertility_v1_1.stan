// JOINT MODEL WITH BINOMIAL FOR THE NATIONAL COUNT

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
  vector[Y] fertility_rate_baseline;    // Intercept (National level)
  vector[Y] fertility_rate_male_effect; // Coefficient for male indicator for each year (National level)
  
  // GP for national births
  matrix[Y, A] z_fem; // Random variable to create GP (female)
  matrix[Y, A] z_mal; // Random variable to create GP (male)
  real<lower=0> gp_sigma;        
  real<lower=0> gp_length_scale; 
  
  // Municipalities births
  vector[Y] alpha_0; // Intercept (Capitals)
  vector[Y] alpha_1; // MPI coefficient (Capitals)
  real<lower=0> std_fertility_rate_capital_sigma; // Common across all years
}

transformed parameters {
  array[G] matrix[Y, A] inv_logit_fertility_rate_nat;
  matrix[C, Y] std_fertility_rate_capital_mean;
  
  // Linear predictor (National level)
  {
    matrix[A, A] L_fertility_rate_nat = cholesky_decompose(gp_exp_quad_cov(age_value, gp_sigma, gp_length_scale) + diag_matrix(rep_vector(1e-10, A)));
    
    for (y in 1:Y) {
      inv_logit_fertility_rate_nat[1][y, ] = (fertility_rate_baseline[y] + L_fertility_rate_nat * z_fem[y, ]')';                                 // Female
      inv_logit_fertility_rate_nat[2][y, ] = (fertility_rate_baseline[y] + fertility_rate_male_effect[y] + L_fertility_rate_nat * z_mal[y, ]')'; // Male
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
    target += binomial_logit_lupmf(to_array_1d(births_nat[, , g]) | to_array_1d(population_nat[, , g]), to_vector(inv_logit_fertility_rate_nat[g]'));
  }
  
  // Priors (National level)
  target += normal_lpdf(fertility_rate_baseline    | 0, 3); 
  target += normal_lpdf(fertility_rate_male_effect | 0, 1);
  
  target += normal_lpdf(to_vector(z_fem) | 0, 1);
  target += normal_lpdf(to_vector(z_mal) | 0, 1);
  
  target += normal_lpdf(gp_sigma | 0, 1); 
  target += inv_gamma_lpdf(gp_length_scale | 3, 1); // Updated

  // Likelihood (Municipality level)
  target += normal_lpdf(to_vector(std_fertility_rate_capital) | to_vector(std_fertility_rate_capital_mean), std_fertility_rate_capital_sigma);
  
  // Priors (Municipality level)
  target += normal_lpdf(alpha_0 | 0, 0.025); // Updated
  target += normal_lpdf(alpha_1 | 0, 1);
  target += normal_lpdf(std_fertility_rate_capital_sigma | 0, 0.025); // Updated
}

generated quantities {
  vector[Y] std_fertility_rate_nat;

  // Initialise paramters
  std_fertility_rate_nat = rep_vector(0, Y);
  
  for (y in 1:Y) {
    // Vectorized sum over age groups
    for (g in 1:G) {
     std_fertility_rate_nat[y] = std_fertility_rate_nat[y] + dot_product(proportion_pop_nat[, g], inv_logit(inv_logit_fertility_rate_nat[g][y, ]));
    }
  }
}
