// Simple Random Walk on Log-Odds
//
// Minimal polling aggregation model:
//   - Multivariate random walk on log-odds (Cholesky-parameterized)
//   - Dirichlet-multinomial likelihood (overdispersed)
//   - One observation per occupied time period (polls pre-aggregated)
//   - No party emergence, no reporting patterns

data {
  int<lower=1> T;                       // number of time periods
  int<lower=2> P;                       // number of parties
  int<lower=1> N;                       // number of occupied time periods
  array[N] int<lower=1,upper=T> t_idx;  // time index per observation
  array[N, P] int<lower=0> y;           // aggregated response counts per period

  // Elections (optional anchoring)
  int<lower=0> E;                       // number of elections
  array[E] int<lower=1,upper=T> e_idx;  // time index per election
  matrix[E, P] e_vote;                  // election vote shares (simplex rows)
}

parameters {
  // Random walk innovations (non-centered)
  matrix[T, P - 1] z;                   // standard normal innovations
  vector<lower=0>[P - 1] sigma;         // volatility per party
  cholesky_factor_corr[P - 1] L_Omega;  // Cholesky factor of correlation matrix
  simplex[P] pi_init;                   // initial vote share simplex

  // Dirichlet-multinomial overdispersion
  real<lower=0> phi;                    // concentration (higher = less overdispersion)
}

transformed parameters {
  matrix[T, P] alpha;                   // log-odds trajectory
  matrix[T, P] pi;                      // vote share trajectory

  // Cholesky factor of covariance: L = diag(sigma) * L_Omega
  matrix[P - 1, P - 1] L_Sigma = diag_pre_multiply(sigma, L_Omega);

  // Initialize log-odds from prior simplex (party P is reference = 0)
  alpha[1, P] = 0;
  for (p in 1:(P - 1))
    alpha[1, p] = log(pi_init[p]) - log(pi_init[P]);

  // Multivariate random walk (non-centered)
  profile("random_walk") {
    for (t in 2:T) {
      alpha[t, P] = 0;
      alpha[t, 1:(P - 1)] = alpha[t - 1, 1:(P - 1)]
                             + (L_Sigma * to_vector(z[t]))';
    }
  }

  // Softmax -> vote shares (precomputed once for all T)
  profile("softmax_transform") {
    for (t in 1:T)
      pi[t] = to_row_vector(softmax(to_vector(alpha[t])));
  }
}

model {
  // Priors
  profile("priors") {
    to_vector(z) ~ std_normal();
    sigma ~ normal(0, 0.15);
    L_Omega ~ lkj_corr_cholesky(2);
    pi_init ~ dirichlet(rep_vector(5, P));
    phi ~ normal(50, 25);
  }

  // Likelihood: one Dirichlet-multinomial per occupied time period
  profile("likelihood_polls") {
    for (i in 1:N) {
      target += dirichlet_multinomial_lupmf(y[i] | to_vector(pi[t_idx[i]]) * phi);
    }
  }

  // Soft election anchoring (optional)
  profile("election_anchoring") {
    if (E > 0) {
      for (j in 1:E) {
        vector[P] trend_at_election = to_vector(pi[e_idx[j]]);
        to_vector(e_vote[j]) ~ dirichlet(trend_at_election * 100);
      }
    }
  }
}

generated quantities {
  // Correlation matrix (for diagnostics)
  corr_matrix[P - 1] Omega = multiply_lower_tri_self_transpose(L_Omega);

  // Log-likelihood for LOO (per occupied period)
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = dirichlet_multinomial_lpmf(y[i] | to_vector(pi[t_idx[i]]) * phi);
  }
}
