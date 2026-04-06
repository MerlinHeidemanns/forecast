// B-Spline Polling Model — Long-Term Trend
//
// Polling aggregation model:
//   - B-spline basis on log-odds, knots at election dates
//   - Dirichlet-multinomial likelihood (overdispersed)
//   - One observation per occupied time period (polls pre-aggregated)
//   - Tight RW1 smoothing prior → captures long-term trend only
//   - Softmax only evaluated at observed time indices during sampling

data {
  int<lower=1> T;                       // number of time periods
  int<lower=2> P;                       // number of parties
  int<lower=1> K;                       // number of basis functions
  matrix[T, K] B;                       // B-spline basis matrix (T x K)

  int<lower=1> N;                       // number of occupied time periods
  array[N] int<lower=1,upper=T> t_idx;  // time index per observation
  array[N, P] int<lower=0> y;           // aggregated response counts per period

  // Unique observed time indices (union of poll + election times)
  int<lower=1> N_obs;                   // number of unique observed time points
  array[N_obs] int<lower=1,upper=T> obs_idx; // unique observed time indices
  // Lookup: poll i → position in obs_idx
  array[N] int<lower=1,upper=N_obs> poll_obs_lookup;

  // Elections (optional anchoring)
  int<lower=0> E;                       // number of elections
  array[E] int<lower=1,upper=N_obs> e_obs_lookup; // election → position in obs_idx
  matrix[E, P] e_vote;                  // election vote shares (simplex rows)
}

parameters {
  matrix[K, P - 1] beta_raw;           // standardized spline coefficients
  vector<lower=0>[P - 1] tau;          // smoothing SD per party (smaller = smoother)
  real<lower=0> phi;                    // DirMult concentration
}

transformed parameters {
  matrix[K, P - 1] beta;               // actual spline coefficients

  // Non-centered RW1: build beta from cumulative sum of innovations
  profile("spline_coefs") {
    for (p in 1:(P - 1)) {
      beta[1, p] = beta_raw[1, p] * 2.0;
      for (k in 2:K)
        beta[k, p] = beta[k - 1, p] + beta_raw[k, p] * tau[p];
    }
  }
}

model {
  // Compute log-odds and softmax only at observed time points
  matrix[N_obs, P] alpha_obs;
  matrix[N_obs, P] pi_obs;

  profile("spline_eval") {
    for (i in 1:N_obs) {
      // B[obs_idx[i], ] is a row vector of length K
      for (p in 1:(P - 1))
        alpha_obs[i, p] = dot_product(to_vector(B[obs_idx[i]]), beta[, p]);
      alpha_obs[i, P] = 0;
    }
  }

  profile("softmax_transform") {
    for (i in 1:N_obs)
      pi_obs[i] = to_row_vector(softmax(to_vector(alpha_obs[i])));
  }

  // Priors
  profile("priors") {
    to_vector(beta_raw) ~ std_normal();
    tau ~ normal(0, 0.3);
    phi ~ normal(50, 25);
  }

  // Likelihood: Dirichlet-multinomial
  profile("likelihood_polls") {
    for (i in 1:N) {
      target += dirichlet_multinomial_lupmf(y[i] | to_vector(pi_obs[poll_obs_lookup[i]]) * phi);
    }
  }

  // Soft election anchoring
  profile("election_anchoring") {
    if (E > 0) {
      for (j in 1:E) {
        vector[P] trend_at_election = to_vector(pi_obs[e_obs_lookup[j]]);
        to_vector(e_vote[j]) ~ dirichlet(trend_at_election * 100);
      }
    }
  }
}

generated quantities {
  // Full trajectory (only computed once per draw, not during gradient)
  matrix[T, P] alpha;
  matrix[T, P] pi;

  alpha[, 1:(P - 1)] = B * beta;
  for (t in 1:T) {
    alpha[t, P] = 0;
    pi[t] = to_row_vector(softmax(to_vector(alpha[t])));
  }

  // Log-likelihood for LOO
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = dirichlet_multinomial_lpmf(y[i] | to_vector(pi[t_idx[i]]) * phi);
  }
}
