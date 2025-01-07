functions {
  vector party_weight(vector t, real t_emerge, real t_disappear, real rate) {
    // Vectorized computation of weights for each time point
    vector[num_elements(t)] emergence = inv_logit(rate * (t - t_emerge));
    vector[num_elements(t)] survival = inv_logit(-rate * (t - t_disappear));
    return emergence .* survival;  // Element-wise multiplication
  }
}

data {
  int<lower=1> N;                     // number of observations
  int<lower=1> K;                     // number of parties (including new party)
  int<lower=1> T;                     // number of unique time points
  array[T] real time_points;              // time grid
  array[N] int<lower=1> time_idx;     // time index for each observation
  array[N] int<lower=1> n_resp;       // number of respondents
  array[N, K] int y;                  // vote counts for each party
  real<lower=0> t_emerge;             // emergence time of party
  real<lower=t_emerge> t_disappear;   // disappearance time of party
  int<lower=1> trans_party_idx;       // index of transitioning party
}

parameters {
  real<lower=0> alpha;         // marginal standard deviations
  real<lower=0> rho;           // length scales
  matrix[T, K-1] eta;                 // GP random effects
  real<lower=0> transition_rate;      // rate of emergence/disappearance
  cholesky_factor_corr[K-1] L_Omega;  // Cholesky of party correlation matrix
}

transformed parameters {
  matrix[T, K] f;                     // logits for all parties
  matrix[T, K] probs;                 // probability matrix
  matrix[T, T] cov = gp_exp_quad_cov(time_points, alpha, rho);
  matrix[T, T] L_k = cholesky_decompose(add_diag(cov, 1e-9));
  vector[K] ones = rep_vector(1, K);  // vector of ones for matrix mult
  real large_neg = -100;              // effectively zero probability after softmax
  
  // Transform GP effects using multi-output structure
  f[,1:K - 1] = L_k * eta * L_Omega';
  f[, K] = rep_vector(0.0, T);
  {
    vector[T] w = party_weight(to_vector(time_points),
                                t_emerge, t_disappear, transition_rate);
    f[, trans_party_idx] = w .* f[, trans_party_idx] + (1 - w) * large_neg;
  }
  
  // Efficient softmax using matrix multiplication
  {
    matrix[T, K] exp_f = exp(f);
    vector[T] normalizers = log(exp_f * ones);
    for (t in 1:T)
      probs[t] = exp(f[t] - normalizers[t]);
  }
}

model {
  // Priors
  alpha ~ normal(0, 2);
  rho ~ inv_gamma(5, 5);
  transition_rate ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(eta) ~ std_normal();
  
  // Likelihood using Poisson approximation to multinomial
  for (k in 1:K){
    y[, k] ~ poisson(to_vector(n_resp) .* probs[time_idx, k]);
  }
}

generated quantities {
  matrix[T, K] vote_shares;
  matrix[K-1, K-1] Omega;
  vector[T] transition_weights;
  
  // Vote shares already calculated in transformed parameters
  vote_shares = probs;
  
  // Store transition weights for plotting
  transition_weights = party_weight(to_vector(time_points), t_emerge, t_disappear, transition_rate);
  
  // Recover correlation matrix
  Omega = multiply_lower_tri_self_transpose(L_Omega);
}


