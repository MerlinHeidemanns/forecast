functions {
  // Sigmoid emergence: w(t) = logit⁻¹(rate × (t - t_emerge))
  vector party_weight(vector t, real t_emerge, real rate) {
    return inv_logit(rate * (t - t_emerge));
  }
}

data {
  // Dimensions
  int n_surveys;
  int n_parties_fixed;
  int n_parties_trans;
  int n_layer1;
  
  // Spline basis
  int n_knots;
  matrix[n_layer1, n_knots] spline_basis;  // B-spline basis matrix B[t,k]
  
  // Survey metadata
  int<lower=1> R;
  array[n_surveys] int<lower=1, upper=n_layer1> survey_layer1_idx;
  array[n_surveys] int<lower=1, upper=R> idx_r;
  
  // Party presence tracking
  array[n_parties_fixed + n_parties_trans] int<lower=1, upper=n_surveys> n_parties_presence;
  array[n_parties_fixed + n_parties_trans, n_surveys] int parties_survey_idx;
  array[n_surveys] int<lower=1, upper=R> reporting_pattern;
  array[R, n_parties_trans] int<lower=0, upper=1> pattern_matrix;
  array[R, 2] int pattern_starts_ends;
  
  // Party indexing per reporting pattern
  array[R, n_parties_fixed + n_parties_trans] int<upper=n_parties_fixed + n_parties_trans> mat_party_indices;
  array[R] int<upper=n_parties_fixed + n_parties_trans> party_count;
  
  // Response data
  array[n_surveys, n_parties_fixed + n_parties_trans] int survey_y;
  int<lower=1> residual_idx;
  
  // Party emergence timing
  array[n_parties_trans] real party_emergence_layer1_idx;
  
  // LFO cross-validation
  int<lower=1, upper=n_layer1> lfo_cutoff_index;
  
  // Prior hyperparameters
  real<lower=0> prior_spline_2nd_diff_scale;
  real<lower=0> prior_floating_alpha;  // beta prior param for floating
  real<lower=0> prior_floating_beta;
}

transformed data {
  int n_parties = n_parties_fixed + n_parties_trans;
  vector[n_layer1] time_idx = linspaced_vector(n_layer1, 1, n_layer1);
  
  // Surveys in fitting window
  int n_surveys_fit = 0;
  for (i in 1:n_surveys) {
    if (survey_layer1_idx[i] <= lfo_cutoff_index) n_surveys_fit += 1;
  }
  
  array[n_surveys_fit] int fit_survey_idx;
  {
    int idx = 0;
    for (i in 1:n_surveys) {
      if (survey_layer1_idx[i] <= lfo_cutoff_index) {
        idx += 1;
        fit_survey_idx[idx] = i;
      }
    }
  }
  
  // Aggregation: A[r] maps non-reported parties to residual category
  array[R] matrix[n_parties, n_parties] agg_matrix;
  for (r in 1:R) {
    agg_matrix[r] = diag_matrix(rep_vector(1.0, n_parties));
    for (k in 1:n_parties_trans) {
      if (pattern_matrix[r, k] == 0) {
        agg_matrix[r, n_parties_fixed + k, residual_idx] = 1.0;
        agg_matrix[r, n_parties_fixed + k, n_parties_fixed + k] = 0.0;
      }
    }
  }
  
  array[n_surveys] int n_survey_responses;
  for (i in 1:n_surveys) {
    n_survey_responses[i] = sum(survey_y[i,]);
  }
  
  matrix[n_parties, n_parties] ones = rep_matrix(1, n_parties, n_parties);
}

parameters {
  // Spline coefficients: β[k, p] for knot k, party p (excluding reference)
  matrix[n_knots, n_parties - 1] spline_coefs_raw;
  
  // Smoothing
  vector<lower=0>[n_parties - 1] spline_smoothness;
  
  // Party correlation
  cholesky_factor_corr[n_parties - 1] party_corr_chol;
  
  // Party emergence
  array[n_parties_trans] real<lower=0> transition_rate;
  
  // Base + floating voter model
  simplex[n_parties_fixed] base_share_fixed;  // loyal base for fixed parties
  real<lower=0, upper=1> floating;            // proportion of floating voters
}

transformed parameters {
  matrix[n_layer1, n_parties] trend_log_odds;
  matrix[n_layer1, n_parties] trend_voteshares;
  matrix[n_surveys, n_parties] survey_probs;
  matrix[n_knots, n_parties - 1] spline_coefs;
  
  // Correlated spline coefficients: β[k] = L × β_raw[k]
  profile("spline_coefficients") {
    for (k in 1:n_knots) {
      spline_coefs[k,] = (party_corr_chol * spline_coefs_raw[k,]')';
    }
  }
  
  // B-spline trend: α[t, p] = Σ_k B[t,k] × β[k, p]
  profile("spline_trend") {
    trend_log_odds[, 1] = rep_vector(0, n_layer1);  // reference party
    trend_log_odds[, 2:n_parties] = spline_basis * spline_coefs;
  }
  
  // Party emergence: α[t,k] → w[t] × α[t,k] + (1 - w[t]) × (-10)
  profile("party_emergence") {
    for (k in 1:n_parties_trans) {
      int p = n_parties_fixed + k;
      vector[n_layer1] w = party_weight(time_idx, party_emergence_layer1_idx[k], transition_rate[k]);
      trend_log_odds[, p] = w .* trend_log_odds[, p] + (1 - w) * (-10);
    }
  }
  
  // Base + floating voter model:
  //   π[t] = (1 - f) × base + f × softmax(α[t])
  // 
  // Interpretation:
  //   - base_p: loyal voters who always support party p
  //   - floating: proportion of electorate that responds to dynamics
  //   - floor_p = (1 - f) × base_p
  //   - ceiling_p = (1 - f) × base_p + f
  profile("base_floating") {
    // Extend base_share to all parties (transitory parties have no loyal base)
    vector[n_parties] base_share = append_row(base_share_fixed, rep_vector(0, n_parties_trans));
    
    for (t in 1:n_layer1) {
      // Dynamic component: softmax of log-odds
      vector[n_parties] dynamic = softmax(trend_log_odds[t,]');
      
      // Combine: loyal base + floating voters
      trend_voteshares[t,] = ((1 - floating) * base_share + floating * dynamic)';
    }
  }
  
  // Aggregate for survey-level predictions
  // π̃ = π × A[r]  (redirects non-reported → residual)
  profile("residual_correction") {
    survey_probs = trend_voteshares[survey_layer1_idx,];
    
    for (r in 1:R) {
      int start = pattern_starts_ends[r, 1];
      int end = pattern_starts_ends[r, 2];
      survey_probs[start:end,] = survey_probs[start:end,] * agg_matrix[r];
    }
  }
}

model {
  // Priors:
  //   β_raw[k] ~ N(0, 1)
  //   L ~ LKJ(2)
  //   Δ²β[k] ~ N(0, τ)  (2nd difference penalty)
  //   τ ~ N⁺(0, prior_scale)
  //   rate ~ InvGamma(3, 2)
  //   base ~ Dirichlet(2)
  //   floating ~ Beta(α, β)
  profile("priors") {
    // Spline coefficients
    to_vector(spline_coefs_raw) ~ std_normal();
    party_corr_chol ~ lkj_corr_cholesky(2);
    
    // Smoothness prior on second differences
    spline_smoothness ~ normal(0, prior_spline_2nd_diff_scale);
    for (p in 1:(n_parties - 1)) {
      for (k in 3:n_knots) {
        real second_diff = spline_coefs[k, p] - 2 * spline_coefs[k - 1, p] + spline_coefs[k - 2, p];
        second_diff ~ normal(0, spline_smoothness[p]);
      }
    }
    
    // Party emergence
    transition_rate ~ inv_gamma(3, 2);
    
    // Base + floating model
    base_share_fixed ~ dirichlet(rep_vector(2, n_parties_fixed));
    floating ~ beta(prior_floating_alpha, prior_floating_beta);
  }

  // Likelihood: y[i] ~ Multinomial(n[i], π̃[i])
  profile("likelihood") {
    for (j in 1:n_surveys_fit) {
      int i = fit_survey_idx[j];
      int r = idx_r[i];
      int n_p = party_count[r];
      
      vector[n_p] pred = survey_probs[i, mat_party_indices[r, 1:n_p]]';
      array[n_p] int obs = survey_y[i, mat_party_indices[r, 1:n_p]];
      
      obs ~ multinomial(pred);
    }
  }
}

generated quantities {
  vector[n_surveys] log_lik;
  array[n_surveys, n_parties] int y_rep = rep_array(0, n_surveys, n_parties);
  array[n_surveys, n_parties] int y_rep_geq_obs = rep_array(0, n_surveys, n_parties);
  
  vector[n_parties] mse_fit;
  vector[n_parties] mse_holdout;
  real elpd_fit = 0;
  real elpd_holdout = 0;
  
  // Derived quantities: floors and ceilings per party
  vector[n_parties] floor_share;
  vector[n_parties] ceiling_share;
  
  {
    vector[n_parties] base_share = append_row(base_share_fixed, rep_vector(0, n_parties_trans));
    floor_share = (1 - floating) * base_share;
    ceiling_share = (1 - floating) * base_share + floating * rep_vector(1, n_parties);
  }
  
  {
    vector[n_parties] sse_fit = rep_vector(0, n_parties);
    vector[n_parties] sse_holdout = rep_vector(0, n_parties);
    vector[n_parties] n_fit = rep_vector(0, n_parties);
    vector[n_parties] n_holdout = rep_vector(0, n_parties);
    
    for (i in 1:n_surveys) {
      int r = idx_r[i];
      int n_p = party_count[r];
      int n_resp = n_survey_responses[i];
      int is_fit = survey_layer1_idx[i] <= lfo_cutoff_index;
      
      vector[n_p] pred = survey_probs[i, mat_party_indices[r, 1:n_p]]';
      array[n_p] int obs = survey_y[i, mat_party_indices[r, 1:n_p]];
      array[n_p] int rep = multinomial_rng(pred, n_resp);
      
      // log p(y[i] | π̃[i])
      log_lik[i] = multinomial_lpmf(obs | pred);
      
      if (is_fit) {
        elpd_fit += log_lik[i];
      } else {
        elpd_holdout += log_lik[i];
      }
      
      for (j in 1:n_p) {
        int p = mat_party_indices[r, j];
        real sq_err = square(obs[j] * 1.0 / n_resp - pred[j]);
        
        y_rep[i, p] = rep[j];
        y_rep_geq_obs[i, p] = rep[j] >= obs[j];
        
        if (is_fit) {
          sse_fit[p] += sq_err;
          n_fit[p] += 1;
        } else {
          sse_holdout[p] += sq_err;
          n_holdout[p] += 1;
        }
      }
    }
    
    // MSE = Σ(obs - pred)² / n
    for (p in 1:n_parties) {
      mse_fit[p] = n_fit[p] > 0 ? sse_fit[p] / n_fit[p] : 0;
      mse_holdout[p] = n_holdout[p] > 0 ? sse_holdout[p] / n_holdout[p] : 0;
    }
  }
  
  real elpd_holdout_per_obs = elpd_holdout / (n_surveys - n_surveys_fit);
}