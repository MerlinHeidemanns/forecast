
  data {
    int<lower=0> N;          // number of observations
    vector[N] x;             // input locations
    vector[N] y;             // observations
    real<lower=0> alpha;     // step size parameter
  }

  parameters {
    vector[N] z;             // unbounded random walk
    real<lower=0> sigma;     // observation noise
    real<lower=0> tau;       // random walk scale
    real mu_min;             // minimum bound
    real<lower=0> mu_range;  // range (mu_max - mu_min)
  }

  transformed parameters {
    vector[N] mu;            // bounded random walk
    
    // First point
    mu[1] = mu_min + mu_range * inv_logit(z[1]);
    
    // Subsequent points - random walk with soft bounds via logit
    for (n in 2:N) {
      mu[n] = mu_min + mu_range * inv_logit(z[n]);
    }
  }

  model {
    // Priors
    mu_min ~ normal(0, 10);
    mu_range ~ normal(5, 5);
    tau ~ normal(0, 1);
    sigma ~ normal(0, 1);
    
    // Random walk prior on z
    z[1] ~ normal(0, 1);
    for (n in 2:N) {
      z[n] ~ normal(z[n-1], tau);
    }
    
    // Likelihood
    y ~ normal(mu, sigma);
  }

  generated quantities {
    real mu_max = mu_min + mu_range;
    vector[N] y_pred;
    for (n in 1:N) {
      y_pred[n] = normal_rng(mu[n], sigma);
    }
  }
  
