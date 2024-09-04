data {
  // COUNTS
  int<lower=0> Y; // Number of years
  int<lower=0> A; // Number of age groups
  int<lower=0> G; // Number of genders
  int<lower=0> L; // Number of locations (Municipality level)
  int<lower=0> N;     // Total number of observations
  int<lower=0> N_nat; // Total number of observations (National level)
  
  // NATIONAL LEVEL
  array[N_nat] int<lower=1, upper=Y> year_nat;   // Array of year indices      (National level)
  array[N_nat] int<lower=1, upper=G> gender_nat; // Array of gender indices    (National level)
  array[N_nat] int<lower=1, upper=A> age_nat;    // Array of age group indices (National level)
  array[N_nat] int<lower=0> deaths_nat;          // Array of death counts      (National level)
  array[N_nat] int<lower=0> population_nat;      // Array of population        (National level)
  
  // MUNICIPALITY LEVEL
  array[N] int<lower=1, upper=Y> year;     // Array of year indices
  array[N] int<lower=1, upper=L> location; // Array of location indices
  array[N] int<lower=1, upper=G> gender;   // Array of gender indices
  array[N] int<lower=1, upper=A> age;      // Array of age group indices
  array[N] int<lower=0> deaths;            // Array of death counts 
  array[N] int<lower=0> population;        // Array of population
  
  array[L, Y, A, G] real<lower=0> death_rate; // Array of death rate 
  array[L, Y, A, G] real<lower=0> total_popu; // Array of total population
  array[A, G] int<lower=0> P_nat; // National reference population (e.g., census 2018) per age group and gender
  int<lower=0> P_nat_total;       // National reference population (e.g., census 2018) total

  // OTHERS
  array[A] real age_values; // Age group values (for GP)
  array[L] real mpi;        // MPI values for each municipality (for GP)
}


transformed data {
  matrix[L, Y] nu; // Standardised death rate for all years and locations
    
  // Standardised death rate
  for (l in 1:L) {
    for (y in 1:Y) {
      nu[l, y] = 0.0;  // Initialize the sum to zero
      for (a in 1:A) {
        for (g in 1:G) {
          nu[l, y] = nu[l, y] + ((P_nat[a, g] * 1.0 / P_nat_total) * death_rate[l, y, a, g]) + 1e-10; // Avoid `0` "standardised death rate" 
        }
      }
    }
  }
}

parameters {
  vector[Y] beta_0;   // Intercept (National level)
  vector[Y] beta_1;   // Coefficient for male indicator for each year (National level)
  matrix[Y, A] d_fem; // Random effects for females across age groups (National level)
  matrix[Y, A] d_mal; // Random effects for males   across age groups (National level)
  // Parameters for the standardised death rate and multipliers
  real alpha_0;       // Intercept for standardised death rate
  vector[L] f_mpi;    // Random effects   for municipalities (based on MPI)
  vector[L] gamma;    // Shifting factors for municipalities 
  
  // Hyperparameters for the GP (National level)
  real<lower=0> sigma_f_nat_fem;        
  real<lower=0> sigma_f_nat_mal;        
  real<lower=0> length_scale_f_nat_fem; 
  real<lower=0> length_scale_f_nat_mal; 
  
  // Hyperparameters at the GP (Municipality level)
  real<lower=0> sigma_nu;
  real<lower=0> sigma_f_mpi;
  real<lower=0> length_scale_f_mpi;
}

transformed parameters {
  vector[N_nat] mu_nat; // Logit-transformed: linear predictor for death (National level)
  matrix[L, Y] mu_nu;   // Log-transformed:   linear predictor for standardised death rates (Municipality level)
  vector[Y] nu_nat;     // Standardised death rate (National level)
  matrix[L, Y] gamma_mult; // Multiplier factor
  array[L, Y, A, G] real log_death_rate; // Array of estimated death rates (Municipality level)
  
  
  // Linear predictor for death (National level)
  for (n in 1:N_nat) {
    if (gender_nat[n] == 1) { // Female
      mu_nat[n] = inv_logit(beta_0[year_nat[n]] + d_fem[year_nat[n], age_nat[n]]);
    } else {                  // Male
      mu_nat[n] = inv_logit(beta_0[year_nat[n]] + beta_1[year_nat[n]] + d_mal[year_nat[n], age_nat[n]]);
    }
  }
  
  // Linear predictor for standardised death rates (Municipality level)
  for (l in 1:L) {
    for (y in 1:Y) {
      mu_nu[l, y] = exp(alpha_0 + f_mpi[l]);
    }
  }
  
  // Standardised death rate (National level)
  for (y in 1:Y) {
    nu_nat[y] = 0.0;  // Initialize the sum to zero
    for (g in 1:G) {  // Order should be `c("gender", "age")`, as this is how `mu_nat` is defined
      for (a in 1:A) {
        int flatten_index = (y - 1) * G * A + (g - 1) * A + a;
        nu_nat[y] = nu_nat[y] + ((P_nat[a, g] * 1.0 / P_nat_total) * mu_nat[flatten_index]); 
      }
    }
  }
  
  // Multiplier factor
  for (l in 1:L) {
    for (y in 1:Y) {
      gamma_mult[l, y] = nu[l, y] / nu_nat[y];
    }
  }
  
  // Estimated death rates (Municipality level) 
  for (l in 1:L) {
    for (y in 1:Y) {
      for (g in 1:G) {  // Order should be `c("gender", "age")`, as this is how `mu_nat` is defined
        for (a in 1:A) {
          int flatten_index = (y - 1) * G * A + (g - 1) * A + a;
          log_death_rate[l, y, a, g] = log(gamma_mult[l, y]) + log(mu_nat[flatten_index]);
        }
      }
    }
  }
}

model {
  // Priors
  beta_0 ~ normal(0, 3);
  beta_1 ~ normal(0, 3);

  // Gaussian Process for age effects (National level)
  {
    matrix[A, A] K_f_nat_fem = gp_exp_quad_cov(age_values, sigma_f_nat_fem, length_scale_f_nat_fem) + diag_matrix(rep_vector(1e-10, A));
    matrix[A, A] K_f_nat_mal = gp_exp_quad_cov(age_values, sigma_f_nat_mal, length_scale_f_nat_mal) + diag_matrix(rep_vector(1e-10, A));
    matrix[A, A] L_f_nat_fem = cholesky_decompose(K_f_nat_fem);
    matrix[A, A] L_f_nat_mal = cholesky_decompose(K_f_nat_mal);

    for (y in 1:Y) {
      d_fem[y] ~ multi_normal_cholesky(rep_vector(0.0, A), L_f_nat_fem);
      d_mal[y] ~ multi_normal_cholesky(rep_vector(0.0, A), L_f_nat_mal);
    }
  }
  
  // Gaussian Process for MPI effects (Municipality level)
  {
    matrix[L, L] K_f_mpi = gp_exp_quad_cov(mpi, sigma_f_mpi, length_scale_f_mpi) + diag_matrix(rep_vector(1e-10, L));
    matrix[L, L] L_f_mpi = cholesky_decompose(K_f_mpi);
    
    f_mpi ~ multi_normal_cholesky(rep_vector(0.0, L), L_f_mpi);
  }

  // Priors for hyperparameters (Revise them!)
  sigma_f_nat_fem ~ normal(0, 1); 
  sigma_f_nat_mal ~ normal(0, 1); 
  length_scale_f_nat_fem ~ inv_gamma(5, 5); 
  length_scale_f_nat_mal ~ inv_gamma(5, 5); 
  
  sigma_f_mpi ~ normal(0, 1); 
  length_scale_f_mpi ~ inv_gamma(2, 1);
  
  sigma_nu ~ normal(0, 1);

  // ----------
  // Likelihood
  // ----------
  
  // Binomial model (National level)
  deaths_nat ~ binomial_logit(population_nat, logit(mu_nat));
  // Lognormal model (Municipality level)
  for (l in 1:L) {
    for (y in 1:Y) {
      nu[l, y] ~ lognormal(log(mu_nu[l, y]), sigma_nu);
    }
  }
}

generated quantities {
  array[L, Y, A, G] real new_deaths; // Array of new deaths

  for (l in 1:L) {
    for (y in 1:Y) {
        for (a in 1:A) {
          for (g in 1:G) {
            // Calculate the new death count
            new_deaths[l, y, a, g] = exp(log_death_rate[l, y, a, g]) * total_popu[l, y, a, g];
        }
      }
    }
  }
}
