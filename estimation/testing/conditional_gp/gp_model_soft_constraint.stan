data {
  int<lower=1> N;  // total number of points
  int<lower=1> N_obs;  // number of noisy observations
  int<lower=0> N_fixed;  // number of fixed points
  array[N] real x;  // regularly spaced locations
  array[N_obs] int<lower=1,upper=N> obs_idx;  // indices of observed points
  array[N_fixed] int<lower=1,upper=N> fixed_idx;  // indices of fixed points
  vector[N_obs] y_obs;  // noisy observations
  vector[N_fixed] y_fixed;  // fixed values
  real<lower=0> sigma;  // observation noise
}

parameters {
  real<lower=0> alpha;  // GP standard deviation
  real<lower=0> rho;    // GP length scale
  vector[N] eta;        // standard normal variables for GP
}

transformed parameters {
  vector[N] f;  // latent function values
  matrix[N, N] K = gp_exp_quad_cov(x, alpha, rho);
  
  // Add nugget to diagonal for numerical stability
  for (n in 1:N) {
    K[n,n] = K[n,n] + 0.00001;
  }
  
  f = cholesky_decompose(K) * eta;
}

model {
  // Priors for spatial scale of 1-50
  alpha ~ std_normal();
  rho ~ normal(15, 10);
  
  // Non-centered parameterization
  eta ~ std_normal();
  
  // Likelihood for noisy observations
  y_obs ~ normal(f[obs_idx], sigma);
  
  // Soft constraint for fixed values
  target += -1e3 * dot_self(f[fixed_idx] - y_fixed);
}

generated quantities {
  vector[N_obs] log_lik;
  
  for (n in 1:N_obs) {
    log_lik[n] = normal_lpdf(y_obs[n] | f[obs_idx[n]], sigma);
  }
}