functions {
  #include fft.stan
  #include utils.stan
  // Party emergence function
  vector party_weight(vector t, 
                      real t_emerge, 
                      real t_disappear, 
                      real always_entered,
                      real never_exited, 
                      real rate) {
    vector[num_elements(t)] emergence = always_entered + (1 - always_entered) * inv_logit(rate * (t - t_emerge));
    vector[num_elements(t)] survival = never_exited + (1 - never_exited) * inv_logit(-rate * (t - t_disappear));
    return emergence .* survival;
  }
}

data {
  // Dimensions
  int n_layer1;
  int n_layer2;
  int n_parties_fixed;
  int n_parties_trans;
  int n_surveys;
  //int n_elections;

  // Indices
  array[n_surveys] int<lower = 1, upper = n_layer1> survey_layer1_idx;
  //array[n_surveys] int<lower = 1, upper = n_elections> survey_election_idx;
  array[n_layer1]  int<lower = 1, upper = n_layer2> trend_layer2_idx;
  //array[n_elections] int<lower = 1, upper = n_layer1> election_layer1_idx;
  
  // Survey data
  int<lower=1> R;                     // number of different reporting patterns
  array[n_parties_fixed + n_parties_trans] int<lower=1, upper = n_surveys> n_parties_presence;         // in how many surveys was the party asked about
  array[n_parties_fixed + n_parties_trans, n_surveys] int parties_survey_idx;       // which surveys ask for party i
  array[n_surveys] int<lower=1,upper=R> reporting_pattern;  // which pattern each survey follows
  array[R, n_parties_trans] int<lower=0,upper=1> pattern_matrix; // which trans parties are reported in each pattern
  array[R, 2] int pattern_starts_ends;
  
  // Survey data prediction
  array[n_surveys] int n_parties_presence_prediction;
  array[n_surveys, 
        max(n_parties_presence_prediction)] int parties_survey_prediction_idx;       // which surveys ask for party i

  
  // Transition times for each party
  array[n_parties_trans] real party_emergence_layer1_idx;
  array[n_parties_trans] real party_disappearance_layer1_idx;
  array[n_parties_trans] real<lower=0, upper = 1> party_always_entered;
  array[n_parties_trans] real<lower=0, upper = 1> party_never_exited;
  
  
  // Indices for parties that can be combined into residual
  int<lower=1> residual_idx;         // index of residual category
  

  array[n_surveys, n_parties_fixed + n_parties_trans] int survey_y;
  vector[n_surveys] rounding_error_scale;
  
  // Election data
  // matrix[n_elections, n_parties + n_parties_trans] election_y;
  
  // Prior parameters
  real prior_volatility_short_term_mean_mu;
  real prior_volatility_short_term_sigma;

  real prior_trend_short_term_length_scale_mean;
  real<lower = 0> prior_spline_2nd_diff_scale;
  
  // Splines
  int n_knots;
  matrix[n_layer1, n_knots] spline_basis;
  
  // Data input
  real volatility_short_term_length_scale_data;

  // Control flags
  int flag_inference;
  
  real negative_logit_zero;
  
  int n_remove;
}

transformed data {
  // Dimensions for padding
  int n_layer1_padding = n_layer1 + 52;
  int n_layer2_padding = n_layer2 + 26;

  vector[n_layer1] vec_n_layer1_lin;
  for (i in 1:n_layer1){
    vec_n_layer1_lin[i] = i;
  }
  
  array[R] matrix[n_parties_fixed + n_parties_trans, 
                  n_parties_fixed + n_parties_trans] agg_matrix;
  for (r in 1:R) {
    // Initialize with identity matrix
    matrix[n_parties_fixed + n_parties_trans, n_parties_fixed + n_parties_trans] pattern_agg = diag_matrix(rep_vector(1.0, n_parties_fixed + n_parties_trans));
    
    // For non-reported parties, redirect probability to residual
    for (k in 1:n_parties_trans) {
      if (pattern_matrix[r,k] == 0) {
        pattern_agg[n_parties_fixed + k, residual_idx] = 1.0;
        pattern_agg[n_parties_fixed + k, n_parties_fixed + k] = 0.0;
      }
    }
    agg_matrix[r] = pattern_agg;
  }
  
  // Survey totals
  array[n_surveys] int n_survey_responses;
  for (i in 1:n_surveys) {
    n_survey_responses[i] = sum(survey_y[i, ]);
  }
  
  // Pre-computed matrices
  matrix[n_parties_fixed + n_parties_trans, n_parties_fixed + n_parties_trans] ones = rep_matrix(1, n_parties_fixed + n_parties_trans, n_parties_fixed + n_parties_trans);
  matrix[n_layer1_padding, n_parties_fixed + n_parties_trans - 1] trend_short_term_zero_matrix = rep_matrix(0.0, n_layer1_padding, n_parties_fixed + n_parties_trans - 1);

}

parameters {
  // GP and correlation parameters
  matrix[n_layer1_padding, n_parties_fixed + n_parties_trans - 1] trend_short_term_std_normal;
  cholesky_factor_corr[n_parties_fixed + n_parties_trans - 1] trend_party_correlation_chol;
  real<lower = 0> trend_length_scale;

  // Alpha parameters
  vector[n_parties_fixed + n_parties_trans - 2] z_alpha;
  
  // Sigma parameters - SIMPLIFIED: constant volatility
  real<lower = 0> volatility_short_term;

  // Long term trend
  matrix[n_knots, n_parties_fixed + n_parties_trans - 1] spline_coefficients;    // spline coefficients for P-1 parties (relative to reference)

  // Transition rates
  array[n_parties_trans] real<lower = 0> transition_rate;
  
  // Rounding error
  matrix<lower = -1, upper = 1>[n_surveys, n_parties_fixed + n_parties_trans] rounding_error_std_normal;
  
  // Polling (model) error
  //matrix[n_elections, n_parties + n_parties_trans - 1] polling_error_std_normal;
  //vector[n_parties - 1] polling_error_scale;
  
  real neg_logit_end_state;
}

transformed parameters {
  // Declare transformed parameters
  // Trend
  vector[n_layer1_padding %/% 2 + 1] trend_cov_rfft;
  vector<lower=0>[n_parties_fixed + n_parties_trans - 1] alpha;
  matrix[n_layer1, n_parties_fixed + n_parties_trans] trend_short_term;
  matrix[n_layer1, n_parties_fixed + n_parties_trans] trend_long_term;
  matrix[n_layer1, n_parties_fixed + n_parties_trans] trend;
  // predictions
  matrix[n_surveys, n_parties_fixed + n_parties_trans] survey_trend_probabilities;
  //matrix[n_elections - 1, n_parties_fixed + n_parties_trans] election_trend_probabilities;
  matrix[n_surveys, n_parties_fixed + n_parties_trans] rounding_error;
  
  matrix[n_layer1, n_parties_fixed + n_parties_trans - 1] tmp_trend;
  //
  //
  // REMOVED: Time-varying volatility computation
  // Now volatility_short_term is a constant parameter
  
  // Compute alpha and covariance
  alpha[2:n_parties_fixed + n_parties_trans - 1] = exp(z_alpha);
  alpha[1] = 1;
  
  // Compute short term trend
      // -- Covariance matrix
  profile("rfft_kernel") {
    trend_cov_rfft = gp_periodic_exp_quad_cov_rfft(n_layer1_padding, 
                                                1, 
                                                trend_length_scale,
                                                n_layer1_padding) + 1e-9;
  }

  {
    profile("rfft_matrix_inversion_short_term"){
      tmp_trend = gp_inv_rfft_multi_outcome(
                                        trend_short_term_std_normal, 
                                        trend_short_term_zero_matrix, 
                                        trend_cov_rfft
                                      )[1:n_layer1];
    }

    profile("Inducing correlation"){
      // SIMPLIFIED: Use constant volatility instead of time-varying
      trend_short_term[, 2:n_parties_fixed + n_parties_trans] = 
                                      (volatility_short_term * tmp_trend) * // Constant volatility
                                      diag_pre_multiply(alpha, trend_party_correlation_chol)'; // Scaling the correlation matrix
      trend_short_term[, 1] = rep_vector(0.0, n_layer1);

    }
  }  
  
  profile("trend_spline_long_term"){
    // Long term splines
    trend_long_term[, 1] = rep_vector(0.0, n_layer1);
    trend_long_term[ ,2:n_parties_fixed + n_parties_trans] = spline_basis * spline_coefficients;
  }
  
  profile("transition_weights"){
    // Adjust transitioning parties
    for (i in 1:n_parties_trans) {
      vector[n_layer1] w = party_weight(vec_n_layer1_lin, 
                                        party_emergence_layer1_idx[i],
                                        party_disappearance_layer1_idx[i],
                                        party_always_entered[i],
                                        party_never_exited[i], 
                                        transition_rate[i]);
      //vector[n_layer1] w = party_weight(vec_n_layer1_lin, party_disappearance_layer1_idx[i], 0.5);
      trend_long_term[,  n_parties_fixed + i]  = w .* trend_long_term[,n_parties_fixed + i] + (1 - w) * (negative_logit_zero);
      trend_short_term[, n_parties_fixed + i]  = w .* trend_short_term[,n_parties_fixed + i] + (1 - w) * (negative_logit_zero);
      
    }
  }
  
    
  profile("combine"){
    // This section combines the short term trend "trend_short_term" with the long term trend "trend_long_term".
    trend[, 1] = rep_vector(0.0, n_layer1);
    trend[, 2:n_parties_fixed + n_parties_trans] = trend_short_term[, 2:n_parties_fixed + n_parties_trans] + 
                           trend_long_term[, 2:n_parties_fixed + n_parties_trans];
                           
  }
  
  // Compute rounding error
  for (i in 1:n_parties_fixed + n_parties_trans){
    rounding_error[, i] = rounding_error_std_normal[, i] .* rounding_error_scale;
  } 
  
  // Compute polling (model) error
  // polling_error[, 2:n_parties] = polling_error_std_normal * diag_pre_multiply(polling_error_scale, trend_party_correlation_chol);
  // for (i in 1:n_elections){
  //   polling_error[i, 1] = -sum(polling_error[i, 2:n_parties]);
  // }
  
  profile("residual correction"){
    {
      matrix[n_surveys, n_parties_fixed + n_parties_trans] surveys_tmp = trend[survey_layer1_idx,];
      survey_trend_probabilities = exp(surveys_tmp - log(exp(surveys_tmp) * ones));

      for (r in 1:R) {
        int start = pattern_starts_ends[r, 1];
        int end = pattern_starts_ends[r, 2];
        survey_trend_probabilities[start:end,] = survey_trend_probabilities[start:end,] * agg_matrix[r];
      }
    }
  }
}

model {
  // Priors for sigma parameters - SIMPLIFIED
  volatility_short_term ~ normal(prior_volatility_short_term_mean_mu, prior_volatility_short_term_sigma);

  // Priors for other parameters
  trend_length_scale ~ normal(10, 4);
  to_vector(trend_short_term_std_normal) ~ std_normal();
  transition_rate ~ inv_gamma(3, 2);


  // Long term trend
  for(p in 1:(n_parties_fixed + n_parties_trans-1)) {
    for (k in 3:n_knots) {
      (spline_coefficients[k, p] - 2 * spline_coefficients[k-1, p] + spline_coefficients[k-2, p]) ~ normal(0, prior_spline_2nd_diff_scale);
    }
  }

  z_alpha ~ std_normal();
  trend_party_correlation_chol ~ lkj_corr_cholesky(1);
  
  // Rounding error
  to_vector(rounding_error_std_normal) ~ std_normal();
  
  neg_logit_end_state ~ normal(negative_logit_zero, 3);
  // Polling error
  // polling_error_scale ~ normal(0, 0.2);
  // to_vector(polling_error_std_normal) ~ std_normal();
  
  // Likelihood
  if (flag_inference == 0) {
    profile("llh_contributions") {
      for (i in 1:n_parties_fixed + n_parties_trans) {
        survey_y[parties_survey_idx[i, 1:n_parties_presence[i] - n_remove], i] ~ 
          poisson((survey_trend_probabilities[parties_survey_idx[i, 1:n_parties_presence[i] - n_remove], i] + 
                               rounding_error[parties_survey_idx[i, 1:n_parties_presence[i] - n_remove], i]) .* to_vector(n_survey_responses)[parties_survey_idx[i, 1:n_parties_presence[i] - n_remove]]);
      }
    }
  }
  // target += -1e3 * sum(columns_dot_product(election_y[1:n_elections - 1, ], election_trend_probabilities));
}

generated quantities {
  // Posterior predictive checks
  array[n_surveys, n_parties_fixed + n_parties_trans] int survey_y_rep = rep_array(-1, n_surveys, n_parties_fixed + n_parties_trans);

  // Derived quantities
  matrix[n_parties_fixed + n_parties_trans - 1, n_parties_fixed + n_parties_trans - 1] trend_party_correlation;
  matrix[n_layer1, n_parties_fixed + n_parties_trans] trend_shares;
  matrix[n_layer1, n_parties_fixed + n_parties_trans] trend_long_term_shares;
  matrix[n_layer1, n_parties_fixed + n_parties_trans] trend_short_term_shares;
  matrix[n_layer1, n_parties_trans] transition_weights;
  for (i in 1:n_parties_trans){
    transition_weights[, i] = party_weight(vec_n_layer1_lin, 
                                        party_emergence_layer1_idx[i],
                                        party_disappearance_layer1_idx[i],
                                        party_always_entered[i],
                                        party_never_exited[i], 
                                        transition_rate[i]);
  }
  // matrix[n_elections - 1, n_parties] polling_error_distribution_at_observed;

  // Compute trend
  trend_shares = exp(trend - log(exp(trend) * ones));
  trend_long_term_shares = exp(trend_long_term - log(exp(trend_long_term) * ones));
  trend_short_term_shares = trend_shares - trend_long_term_shares;
  
  // Generate posterior predictions
  for (i in 1:n_surveys){
    survey_y_rep[i,parties_survey_prediction_idx[i, 1:n_parties_presence_prediction[i]]] = 
      multinomial_rng(survey_trend_probabilities[i, parties_survey_prediction_idx[i, 
                                                        1:n_parties_presence_prediction[i]]]', 
                            n_survey_responses[i]);
  }
  
  array[n_surveys, n_parties_fixed + n_parties_trans] real log_lik;
  for (i in 1:n_parties_fixed + n_parties_trans) {
    for (j in 1:n_parties_presence[i]){
      // Note: Parties_survey_idx is parties x 1:n_surveys
      //       For a given party for the 1:n_parties_presence
      //       It locates which survey it belongs to
      //       log_lik is ordered by survey_id and party
        log_lik[parties_survey_idx[i, j], i] = 
          poisson_lpmf(survey_y[parties_survey_idx[i, j], i] | (survey_trend_probabilities[parties_survey_idx[i, j], i] + 
                               rounding_error[parties_survey_idx[i, j], i]) .* to_vector(n_survey_responses)[parties_survey_idx[i, j]]);
      }
  }
  
  
  // Compute correlation matrix
  trend_party_correlation = tcrossprod(trend_party_correlation_chol);

}
