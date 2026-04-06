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
}

transformed parameters {
  matrix[n_layer1, n_parties] trend_log_odds;
  matrix[n_surveys, n_parties] survey_probs;
  matrix[n_knots, n_parties - 1] spline_coefs;
  
  // Correlated spline coefficients: β[k] = L × β_raw[k]
  //   where L = chol(Σ) for party correlations
  profile("spline_coefficients") {
    for (k in 1:n_knots) {
      spline_coefs[k,] = (party_corr_chol * spline_coefs_raw[k,]')';
    }
  }
  
  // B-spline trend: α[t, p] = Σ_k B[t,k] × β[k, p]
  //   where B is the spline basis matrix
  profile("spline_trend") {
    trend_log_odds[, 1] = rep_vector(0, n_layer1);  // reference party
    trend_log_odds[, 2:n_parties] = spline_basis * spline_coefs;
  }
  
  // Party emergence: α[t,k] → w[t] × α[t,k] + (1 - w[t]) × (-10)
  //   where w[t] = logit⁻¹(rate × (t - t_emerge))
  profile("party_emergence") {
    for (k in 1:n_parties_trans) {
      int p = n_parties_fixed + k;
      vector[n_layer1] w = party_weight(time_idx, party_emergence_layer1_idx[k], transition_rate[k]);
      trend_log_odds[, p] = w .* trend_log_odds[, p] + (1 - w) * (-10);
    }
  }
  
  // Softmax: π[t] = softmax(α[t]) = exp(α[t]) / Σ exp(α[t])
  // Aggregation: π̃ = π × A[r]  (redirects non-reported → residual)
  profile("residual_correction") {
    matrix[n_surveys, n_parties] log_odds_at_surveys = trend_log_odds[survey_layer1_idx,];
    survey_probs = exp(log_odds_at_surveys - log(exp(log_odds_at_surveys) * ones));
    
    for (r in 1:R) {
      int start = pattern_starts_ends[r, 1];
      int end = pattern_starts_ends[r, 2];
      survey_probs[start:end,] = survey_probs[start:end,] * agg_matrix[r];
    }
  }
}

model {
  // Priors:
  //   β_raw[k] ~ N(0, 1)  (pre-transform)
  //   L ~ LKJ(2)
  //   Δ²β[k] ~ N(0, τ)  (2nd difference penalty for smoothness)
  //   τ ~ N⁺(0, prior_scale)
  //   rate ~ InvGamma(3, 2)
  profile("priors") {
    // Raw coefficients
    to_vector(spline_coefs_raw) ~ std_normal();
    
    // Party correlations
    party_corr_chol ~ lkj_corr_cholesky(2);
    
    // Smoothness prior on second differences: Δ²β[k] = β[k] - 2β[k-1] + β[k-2]
    spline_smoothness ~ normal(0, prior_spline_2nd_diff_scale);
    for (p in 1:(n_parties - 1)) {
      for (k in 3:n_knots) {
        real second_diff = spline_coefs[k, p] - 2 * spline_coefs[k - 1, p] + spline_coefs[k - 2, p];
        second_diff ~ normal(0, spline_smoothness[p]);
      }
    }
    
    // Party emergence
    transition_rate ~ inv_gamma(3, 2);
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
  matrix[n_layer1, n_parties] trend_voteshares;
  vector[n_surveys] log_lik;
  array[n_surveys, n_parties] int y_rep = rep_array(0, n_surveys, n_parties);
  array[n_surveys, n_parties] int y_rep_geq_obs = rep_array(0, n_surveys, n_parties);
  
  vector[n_parties] mse_fit;
  vector[n_parties] mse_holdout;
  real elpd_fit = 0;
  real elpd_holdout = 0;
  
  // π[t] = softmax(α[t])
  trend_voteshares = exp(trend_log_odds - log(exp(trend_log_odds) * ones));
  
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