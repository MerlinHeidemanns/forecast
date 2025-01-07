
data {
  int<lower=1> N;        // number of observations
  int<lower=1> K;        // number of basis functions
  vector[N] x;           // time
  vector[N] y;           // vote share
  matrix[N, K] B;        // B-spline basis matrix
  real<lower=0> tau;     // smoothing parameter
}

parameters {
  vector[K] beta;        // spline coefficients
  real<lower=0> sigma;   // observation noise
}

transformed parameters {
  vector[N] mu = B * beta;
}

model {
  // Likelihood
  y ~ normal(mu, sigma);
  
  // Prior on noise
  sigma ~ normal(0, 1);
  
  // Second differences smoothing prior
  for (k in 3:K) {
    (beta[k] - 2 * beta[k-1] + beta[k-2]) ~ normal(0, tau);
  }
}

generated quantities {
  vector[N] y_rep;
  for (n in 1:N) {
    y_rep[n] = normal_rng(mu[n], sigma);
  }
}

