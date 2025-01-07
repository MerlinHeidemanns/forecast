
data {
  int<lower=1> N;        // number of observations
  int<lower=1> K;        // number of basis functions
  vector[N] x;           // predictor
  vector[N] y;           // response
  matrix[N, K] B;        // B-spline basis matrix
}

parameters {
  vector<lower=0>[K-1] delta;  // positive differences between coefficients
  real beta1;                  // first coefficient
  real<lower=0> sigma;         // observation noise
}

transformed parameters {
  vector[K] beta;
  vector[N] mu;
  
  // Construct monotonic coefficients
  beta[1] = beta1;
  for (k in 2:K) {
    beta[k] = beta[k-1] + delta[k-1];
  }
  
  // Compute fitted values
  mu = B * beta;
}

model {
  // Priors
  beta1 ~ normal(0, 5);
  delta ~ exponential(2);
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

