library("cmdstanr")

stan_code <- "
data {
  int<lower=0> N;
  array[N] real y;
}
parameters {
  real mu;
}
model {
  y ~ normal(mu, 1);
}
"

file  <- write_stan_file(stan_code, dir = "TEST/", basename = "test_model")
model <- cmdstan_model(file)

N <- 10
y <- rnorm(N, mean = 0, sd = 1)
data <- list(N = N, y = y)

# Fit the model
fit <- model$sample(data = data, seed = 1)
fit$save_object(file = "TEST/fitted_model.RDS")
