data {
  int<lower=0> N;  // number of polls
  int<lower=0> T;  // number of time points
  int<lower=0> P;  // number of pollsters
  
  array[N] int<lower=0> n_resp;    // number of respondents per poll
  array[N] int<lower=0> n_pos;     // number of positive responses per poll
  array[N] int<lower=1> time;      // time index for each poll
  array[N] int<lower=1> pollster;  // pollster index for each poll
  
  real<lower=0> sigma_prior;  // prior scale for innovation variance
  real<lower=0> tau_prior;    // prior scale for pollster effects
  real<lower=0> eta_prior;    // prior scale for methodology change scale
}

parameters {
  vector[T] theta;      // true latent state (logit scale)
  vector[P] alpha;      // baseline pollster-specific biases (logit scale)
  vector[T] delta;      // potential methodology changes for pollster 1
  real<lower=0> sigma;  // innovation standard deviation
  real<lower=0> tau;    // pollster effect standard deviation
  real<lower=0> eta;    // scale for methodology changes
}

transformed parameters {
  vector[N] p;  // probability of positive response for each poll
  
  for (n in 1:N) {
    real pollster_effect = alpha[pollster[n]];
    // Add methodology change effect for pollster 1
    if (pollster[n] == 1) {
      pollster_effect = pollster_effect + delta[time[n]];
    }
    p[n] = inv_logit(theta[time[n]] + pollster_effect);
  }
}

model {
  // Priors
  theta[1] ~ normal(0, 1);
  alpha ~ normal(0, tau);
  sigma ~ normal(0, sigma_prior);
  tau ~ normal(0, tau_prior);
  eta ~ normal(0, 1);
  
  // Sparse Cauchy prior for methodology changes
  delta ~ cauchy(0, eta);
  
  // Random walk for latent state
  for (t in 2:T) {
    theta[t] ~ normal(theta[t-1], sigma);
  }
  
  // Binomial likelihood
  n_pos ~ binomial(n_resp, p);
}

generated quantities {
  array[N] int y_rep;
  vector[T] theta_prob;     // state on probability scale
  vector[T] change_prob;    // probability of methodology change
  
  for (n in 1:N) {
    y_rep[n] = binomial_rng(n_resp[n], p[n]);
  }
  
  for (t in 1:T) {
    theta_prob[t] = inv_logit(theta[t]);
    // Compute probability of substantial change
    // (using 0.1 as threshold on logit scale)
    change_prob[t] = abs(delta[t]) > 0.1 ? 1 : 0;
  }
}
