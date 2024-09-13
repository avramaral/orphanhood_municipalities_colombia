data {
  // COUNTS
  int<lower=0> Y; // Number of years
  int<lower=0> A; // Number of age groups
  int<lower=0> G; // Number of genders
  
  // NATIONAL LEVEL
  array[Y, A, G] int<lower=0> deaths_nat;     // Array of death counts (National level)
  array[Y, A, G] int<lower=0> population_nat; // Array of population   (National level)
  
  // OTHERS
  array[A] int age_value; // Age group values (for GP)
}

parameters {
  vector[Y] beta_0;   // Intercept (National level)
  vector[Y] beta_1;   // Coefficient for male indicator for each year (National level)
  array[Y] vector[A] d_fem; // Random effects for females across years and age groups (National level)
  array[Y] vector[A] d_mal; // Random effects for males   across years and age groups (National level)
  
  // Hyperparameters for the GP (National level)
  real<lower=0> sigma_f_nat_fem;        
  real<lower=0> sigma_f_nat_mal;        
  real<lower=0> length_scale_f_nat_fem; 
  real<lower=0> length_scale_f_nat_mal; 
}

transformed parameters {
  array[G] matrix[Y, A] mu_nat;
  
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
}

model {
 // Priors
  beta_0 ~ normal(0, 3);
  beta_1 ~ normal(0, 3);
  
  for (y in 1:Y) {
    d_fem[y] ~ normal(0, 1);
    d_mal[y] ~ normal(0, 1);
  }
  
  // Priors for hyperparameters
  sigma_f_nat_fem ~ normal(0, 1);
  sigma_f_nat_mal ~ normal(0, 1);
  length_scale_f_nat_fem ~ inv_gamma(5, 1);
  length_scale_f_nat_mal ~ inv_gamma(5, 1);

  for (g in 1:G) { // Can be improved for efficiency if I can flatten array
    for (y in 1:Y) {
      target += binomial_logit_lupmf(deaths_nat[y, , g] | population_nat[y, , g], mu_nat[g][y, ]);
    }
  }
}
