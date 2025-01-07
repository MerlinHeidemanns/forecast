functions {
  vector party_weight(vector t, real t_emerge, real t_disappear, real rate) {
    vector[num_elements(t)] emergence = inv_logit(rate * (t - t_emerge));
    vector[num_elements(t)] survival = inv_logit(-rate * (t - t_disappear));
    return emergence .* survival;
  }
}

data {
  int<lower=1> N;                     // number of observations
  int<lower=1> K_fixed;               // number of fixed parties (4)
  int<lower=1> K_trans;               // number of transitioning parties
  int<lower=1> T;                     // number of unique time points
  array[T] real time_points;          // time grid
  array[N] int<lower=1> time_idx;     // time index for each observation
  vector[N] n_resp;       // number of respondents
  array[N, K_fixed + K_trans] int y;  // vote counts for each party
  
  // Survey reporting structure
  int<lower=1> R;                     // number of different reporting patterns
  array[N] int<lower=1,upper=R> reporting_pattern;  // which pattern each survey follows
  array[R, K_trans] int<lower=0,upper=1> pattern_matrix; // which trans parties are reported in each pattern
  
  // Transition times for each party
  array[K_trans] real<lower=0> t_emerge;
  array[K_trans] real<lower=0> t_disappear;
  
  // Indices for parties that can be combined into residual
  int<lower=1> residual_idx;         // index of residual category
}
transformed data {
  matrix[K_fixed + K_trans, K_fixed + K_trans] ones = rep_vector(1.0, K_fixed + K_trans, K_fixed + K_trans);
  array[R] matrix[K_fixed + K_trans, K_fixed + K_trans] agg_matrix;
  for (r in 1:R) {
    // Initialize with identity matrix
    matrix[K_fixed + K_trans, K_fixed + K_trans] pattern_agg = diag_matrix(rep_vector(1.0, K_fixed + K_trans));
    
    // For non-reported parties, redirect probability to residual
    for (k in 1:K_trans) {
      if (pattern_matrix[r,k] == 0) {
        pattern_agg[residual_idx, K_fixed + k] = 1.0;
        pattern_agg[K_fixed + k, K_fixed + k] = 0.0;
      }
    }
    agg_matrix[r] = pattern_agg;
  }
}


parameters {
  real<lower=0> alpha;               // marginal standard deviations
  real<lower=0> rho;                 // length scales
  matrix[T_obs, K_fixed + K_trans - 1] eta;  // GP random effects
  vector<lower=0>[K_trans] transition_rate;  // rates of emergence/disappearance
  cholesky_factor_corr[K_fixed + K_trans - 1] L_Omega;  // Cholesky of party correlation matrix
}

transformed parameters {
  matrix[T, K_fixed + K_trans] f;     // logits for all parties
  matrix[T, K_fixed + K_trans] probs; // base probability matrix
  matrix[N, K_fixed + K_trans] pattern_probs;  // probabilities under each reporting pattern
  
  // Multi-output GP structure
  matrix[T, T] cov = gp_exp_quad_cov(time_points, alpha, rho);
  matrix[T, T] L_k = cholesky_decompose(add_diag(cov, 1e-9));
  
  // Transform GP effects
  f[,2:(K_fixed + K_trans)] = L_k * eta * L_Omega';
  f[,1] = rep_vector(0.0, T);  // reference category
  
  // Apply transition weights for each transitioning party
  for (k in 1:K_trans) {
    vector[T] w = party_weight(to_vector(time_points), t_emerge[k], t_disappear[k], transition_rate[k]);
    f[,K_fixed + k] = w .* f[,K_fixed + k] + (1 - w) * (-10);
  }
  
  // Calculate base probabilities
  {
    matrix[N, K_fixed + K_trans] tmp = f[time_idx,];
    probs = exp(tmp - log(exp(tmp) * ones));
  }
  
  // Calculate probabilities under each reporting pattern using matrix multiplication
  for (r in 1:R) {
    int start = pattern_starts[r];
    int end = pattern_starts[r + 1] - 1;
    pattern_probs[start:end,] = probs[start:end,] * agg_matrix[r];
  }
}

model {
  // Priors
  alpha ~ normal(0, 2);
  rho ~ inv_gamma(5, 5);
  transition_rate ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(eta) ~ std_normal();
  
  // Likelihood - using sorted observations
  for (k in 1:K) {
    y_obs[, k] ~ poisson(n_resp .* probs[, k]);
    }
  }

generated quantities {
  matrix[K_fixed + K_trans - 1, K_fixed + K_trans - 1] Omega;
  array[K_trans] vector[T] transition_weights;
  
  // Store transition weights for each party
  for (k in 1:K_trans) {
    transition_weights[k] = party_weight(to_vector(time_points), 
                                       t_emerge[k], t_disappear[k], 
                                       transition_rate[k]);
  }
  
  // Recover correlation matrix
  Omega = multiply_lower_tri_self_transpose(L_Omega);
}
