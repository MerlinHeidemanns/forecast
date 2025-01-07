data {
  int<lower=0> N;          // number of observations
  int<lower=0> q;          // MA order
  vector[N] y;             // observed estimates
  vector<lower=0>[N] se;   // standard errors of estimates
}

parameters {
  real mu;                 // mean parameter
  real<lower=0> sigma;     // innovation standard deviation
  vector<lower=-1,upper=1>[q] theta;  // MA coefficients
  vector[N] eta;           // standardized innovations
}

transformed parameters {
  vector[N] epsilon;       // scaled innovations
  vector[N] y_true;       // true latent values
  
  // Scale innovations
  epsilon = sigma * eta;
  
  // Initialize latent states with mean
  y_true = rep_vector(mu, N);
  
  // Add MA terms
  for (t in (q+1):N) {
    for (j in 1:q) {
      y_true[t] = y_true[t] + theta[j] * epsilon[t-j];
    }
  }
}

model {
  // Priors
  mu ~ normal(0, 10);
  sigma ~ cauchy(0, 2.5);
  theta ~ normal(0, 0.5);  // slightly informative prior toward stationarity
  eta ~ std_normal();      // non-centered parameterization
  
  // Measurement model
  y ~ normal(y_true, se);
}

generated quantities {
  vector[N] y_rep;         // posterior predictive replications
  
  // Generate posterior predictions
  for (t in 1:N) {
    y_rep[t] = normal_rng(y_true[t], se[t]);
  }
}