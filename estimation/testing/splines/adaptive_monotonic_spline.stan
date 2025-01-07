
data {
  int<lower=1> N;        // number of observations
  int<lower=1> K;        // number of basis functions
  vector[N] x;           // predictor
  vector[N] y;           // response
  matrix[N, K] B;        // B-spline basis matrix
}

parameters {
  vector<lower=0>[K-1] delta;    // magnitude of differences
  real beta1;                    // first coefficient
  real<lower=0> sigma;           // observation noise
  real<lower=0,upper=1> p_inc;   // probability of increasing
}

transformed parameters {
  vector[K] beta;
  vector[N] mu;
  
  // Construct monotonic coefficients with learned direction
  beta[1] = beta1;
  for (k in 2:K) {
    beta[k] = beta[k-1] + (2 * p_inc - 1) * delta[k-1];
  }
  
  // Compute fitted values
  mu = B * beta;
}

model {
  // Priors
  beta1 ~ normal(0, 5);
  delta ~ exponential(2);
  sigma ~ normal(0, 1);
  p_inc ~ beta(1, 1);  // uniform prior on direction
  
  // Likelihood
  y ~ normal(mu, sigma);
}

generated quantities {
  vector[N] y_rep;
  real direction_prob = p_inc;  // store probability of increasing
  int<lower=0,upper=1> is_increasing = p_inc > 0.5;
  
  for (n in 1:N) {
    y_rep[n] = normal_rng(mu[n], sigma);
  }
}

