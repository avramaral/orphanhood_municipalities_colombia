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
