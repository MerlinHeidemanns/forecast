
  data {
    int<lower=0> N;        // number of observations
    vector[N] x;           // input locations
    vector[N] y;           // observations
  }

  parameters {
    real mu_min;           // minimum asymptote
    real<lower=0> mu_range;// range (mu_max - mu_min)
    real<lower=0> r;       // rate of transition
    real c;                // center point
    real<lower=0> sigma;   // observation noise
  }

  model {
    // Priors
    mu_min ~ normal(0, 10);
    mu_range ~ normal(5, 5);
    r ~ normal(1, 1);
    c ~ normal(5, 5);
    sigma ~ normal(0, 1);
    
    // Likelihood
    for (n in 1:N) {
      real mu = mu_min + mu_range * inv_logit(r * (x[n] - c));
      y[n] ~ normal(mu, sigma);
    }
  }

  generated quantities {
    real mu_max = mu_min + mu_range;
    vector[N] y_pred;
    for (n in 1:N) {
      real mu = mu_min + mu_range * inv_logit(r * (x[n] - c));
      y_pred[n] = normal_rng(mu, sigma);
    }
  }
  
