
  data {
    int<lower=0> N;          // number of observations
    vector[N] x;             // input locations
    vector[N] y;             // observations
  }

  parameters {
    real z_init;  // initial state (constrained)
    vector[N-1] z_delta;  // increments (constrained)
    real<lower=0> sigma;     // observation noise
    real<lower=0,upper=2> tau;       // random walk scale (constrained)
    real<lower=0,upper=0.4> a;     // lower bound in [0,1]
    real<lower=0,upper=0.5> range;  // range (ensures b <= 1)
  }

  transformed parameters {
    vector[N] z;            // latent random walk
    vector[N] mu;           // bounded random walk
    real b = a + range;     // upper bound computed to ensure b <= 1
    
    // Construct random walk
    z[1] = z_init;
    for (n in 2:N) {
      z[n] = z[n-1] + tau * z_delta[n-1];
    }
    
    // Transform to bounded walk in [a,b]
    for (n in 1:N) {
      mu[n] = a + range * inv_logit(z[n]);
    }
  }

  model {
    // Priors
    z_init ~ std_normal();
    z_delta ~ std_normal();  // standardized increments
    a ~ beta(2, 5);         // prior favoring lower values
    range ~ beta(2, 2);     // symmetric prior on range
    tau ~ normal(0, 0.5);
    sigma ~ normal(0, 0.1);
    
    // Likelihood
    y ~ normal(mu, sigma);
  }

  generated quantities {
    real upper_bound = a + range;  // actual upper bound achieved
    vector[N] y_pred;
    for (n in 1:N) {
      y_pred[n] = normal_rng(mu[n], sigma);
    }
  }
  
