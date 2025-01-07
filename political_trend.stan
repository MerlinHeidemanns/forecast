
data {
  int<lower=1> N;        // number of observations
  int<lower=1> K;        // number of basis functions
  vector[N] x;           // time
  vector[N] y;           // vote share
  matrix[N, K] B;        // B-spline basis matrix
}

parameters {
  vector[K] beta;        // spline coefficients
  real<lower=0> sigma;   // observation noise
}

transformed parameters {
  vector[N] mu = B * beta;
}

model {
  // Fairly strong prior for smooth trend
  beta ~ normal(0, 5);
  sigma ~ normal(0, 1);
  
  // Likelihood
  y ~ normal(mu, sigma);
}

generated quantities {
  vector[N] y_rep;
  for (n in 1:N) {
    y_rep[n] = normal_rng(mu[n], sigma);
  }
}

