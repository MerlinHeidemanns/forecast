
functions {
  #include fft.stan
  #include utils.stan
}

data {
  // Dimensions
  int n_layer1;
  int n_layer2;
  int n_layer3;
  int n_parties;
  int n_surveys;
  int n_elections;

  // Indices
  array[n_surveys] int<lower = 1, upper = n_layer1> survey_layer1_idx;
  array[n_surveys] int<lower = 1, upper = n_elections> survey_election_idx;
  array[n_layer1]  int<lower = 1, upper = n_layer2> trend_layer2_idx;
  array[n_layer1]  int<lower = 1, upper = n_layer3> trend_layer3_idx;
  array[n_elections] int<lower = 1, upper = n_layer1> election_layer1_idx;
  
  // Survey data
  array[n_surveys, n_parties] int survey_y;
  vector[n_surveys] rounding_error_scale;
  
  // Election data
  matrix[n_elections, n_parties] election_y;
  
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
}

transformed data {
  // Dimensions for padding
  int n_layer1_padding = n_layer1 + 150;
  int n_layer2_padding = n_layer2 + 50;
  int n_layer3_padding = n_layer3 + 10;
  
  // Survey totals
  array[n_surveys] int n_survey_responses;
  for (i in 1:n_surveys) {
    n_survey_responses[i] = sum(survey_y[i, ]);
  }
  
  // Pre-computed matrices
  matrix[n_parties, n_parties] ones = rep_matrix(1, n_parties, n_parties);
  matrix[n_layer1_padding, n_parties - 1] trend_short_term_zero_matrix = rep_matrix(0.0, n_layer1_padding, n_parties - 1);

  // Covariance volatility
  vector[n_layer2_padding %/% 2 + 1] volatility_short_term_cov_rfft = 
    gp_periodic_exp_quad_cov_rfft(
      n_layer2_padding, // Scale
      1, // sigma
      volatility_short_term_length_scale_data,  // Length scale
      n_layer2_padding) + 1e-9; // Periodicity

}

parameters {
  // GP and correlation parameters
  matrix[n_layer1_padding, n_parties - 1] trend_short_term_std_normal;
  cholesky_factor_corr[n_parties - 1] trend_party_correlation_chol;
  positive_ordered[2] trend_length_scale;

  // Alpha parameters
  vector[n_parties - 2] z_alpha;
  
  // Sigma parameters
  real volatility_short_term_mean;
  vector[n_layer2_padding] volatility_short_term_std_normal;
  real<lower = 0> volatility_short_term_scale;

  // Long term trend
  matrix[n_knots, n_parties - 1] spline_coefficients;    // spline coefficients for P-1 parties (relative to reference)

  
  // Rounding error
  matrix<lower = -1, upper = 1>[n_surveys, n_parties] rounding_error_std_normal;
  
  // Polling (model) error
  matrix[n_elections, n_parties - 1] polling_error_std_normal;
  vector[n_parties - 1] polling_error_scale;
}

transformed parameters {
  // Declare transformed parameters
  vector[n_layer1_padding %/% 2 + 1] trend_cov_rfft;
  vector<lower=0>[n_parties - 1] alpha;
  vector[n_layer2] volatility_short_term;
  matrix[n_layer1, n_parties] trend_short_term;
  matrix[n_layer1, n_parties] trend_long_term;
  matrix[n_layer1, n_parties] trend;
  matrix[n_surveys, n_parties] survey_trend_probabilities;
  matrix[n_elections - 1, n_parties] election_trend_probabilities;
  matrix[n_surveys, n_parties] rounding_error;
  
  matrix[n_elections, n_parties] polling_error;
  
  // Compute short term volatility
  volatility_short_term = exp(volatility_short_term_mean + 
                                volatility_short_term_scale * 
                                gp_inv_rfft(
                                  volatility_short_term_std_normal, 
                                  rep_vector(0.0, n_layer2_padding), 
                                  volatility_short_term_cov_rfft
                                )[1:n_layer2]
                              );
  
  profile("rfft_matrix_inversion") {
    // Compute alpha and covariance
    alpha[2:n_parties - 1] = exp(z_alpha);
    alpha[1] = 1;
    
    // Compute short term trend
    // -- Covariance matrix
    trend_cov_rfft = gp_sum_exp_quad_cov_rfft(n_layer1_padding, 
                                                1, 
                                                1, 
                                                trend_length_scale[1], 
                                                trend_length_scale[2], 
                                                //365.0/7.0,
                                                n_layer1_padding) + 1e-9;
    // -- 
    trend_short_term[, 2:n_parties] = (volatility_short_term[trend_layer2_idx] * rep_row_vector(1.0, n_parties - 1)) .* 
                                      gp_inv_rfft_multi_outcome(
                                        trend_short_term_std_normal, 
                                        trend_short_term_zero_matrix, 
                                        trend_cov_rfft
                                      )[1:n_layer1] * 
                                      diag_pre_multiply(alpha, trend_party_correlation_chol)';
    // -- Set first element to 0
    trend_short_term[, 1] = rep_vector(0.0, n_layer1);
    
    // Long term splines
    trend_long_term[, 1] = rep_vector(0.0, n_layer1);
    trend_long_term[ ,2:n_parties] = spline_basis * spline_coefficients;
    
    // Add mean
    trend[, 1] = rep_vector(0.0, n_layer1);
    trend[, 2:n_parties] = trend_short_term[, 2:n_parties] + 
                           trend_long_term[, 2:n_parties];
  }
  
  // Compute rounding error
  for (i in 1:n_parties){
    rounding_error[, i] = rounding_error_std_normal[, i] .* rounding_error_scale;
  } 
  
  // Compute polling (model) error
  polling_error[, 2:n_parties] = polling_error_std_normal * diag_pre_multiply(polling_error_scale, trend_party_correlation_chol);
  for (i in 1:n_elections){
    polling_error[i, 1] = -sum(polling_error[i, 2:n_parties]);
  }
  
  profile("softmax") {
    matrix[n_surveys, n_parties] surveys_tmp = trend[survey_layer1_idx,];
    matrix[n_elections - 1, n_parties] elections_tmp = trend[election_layer1_idx[1:n_elections - 1],] + polling_error[1:n_elections - 1,];
    survey_trend_probabilities = exp(surveys_tmp - log(exp(surveys_tmp) * ones));
    election_trend_probabilities = exp(elections_tmp - log(exp(elections_tmp) * ones));
  }
}

model {
  // Priors for sigma parameters
  volatility_short_term_mean ~ normal(prior_volatility_short_term_mean_mu, 2);
  volatility_short_term_scale ~ normal(0, prior_volatility_short_term_sigma);
  volatility_short_term_std_normal ~ std_normal();

  // Priors for other parameters
  trend_length_scale[1] ~ normal(5, 20);
  trend_length_scale[2] ~ normal(365.0 / 7.0, 10);
  to_vector(trend_short_term_std_normal) ~ std_normal();

  // Long term trend
  for(p in 1:(n_parties-1)) {
    for (k in 3:n_knots) {
      (spline_coefficients[k, p] - 2 * spline_coefficients[k-1, p] + spline_coefficients[k-2, p]) ~ normal(0, prior_spline_2nd_diff_scale);
    }
  }

  z_alpha ~ std_normal();
  trend_party_correlation_chol ~ lkj_corr_cholesky(1);
  
  // Rounding error
  to_vector(rounding_error_std_normal) ~ std_normal();
  
  // Polling error
  polling_error_scale ~ normal(0, 0.2);
  to_vector(polling_error_std_normal) ~ std_normal();
  
  // Likelihood
  if (flag_inference == 1) {
    profile("llh_contributions") {
      for (i in 1:n_parties) {
        survey_y[, i] ~ poisson((survey_trend_probabilities[, i] + rounding_error[, i]) .* to_vector(n_survey_responses));
      }
    }
  }
  target += -1e3 * sum(columns_dot_product(election_y[1:n_elections - 1, ], election_trend_probabilities));
}

generated quantities {
  // Posterior predictive checks
  array[n_surveys, n_parties] int survey_y_rep;

  // Derived quantities
  matrix[n_parties - 1, n_parties - 1] trend_party_correlation;
  matrix[n_layer1, n_parties] trend_shares;
  matrix[n_layer1, n_parties] trend_long_term_shares;
  matrix[n_layer1, n_parties] trend_short_term_shares;
  matrix[n_elections - 1, n_parties] polling_error_distribution_at_observed;

  // Compute trend
  trend_shares = exp(trend - log(exp(trend) * ones));
  trend_short_term_shares = exp(trend_short_term - log(exp(trend_short_term) * ones)) - 1.0 / (1.0 * n_parties);
  trend_long_term_shares = exp(trend_long_term - log(exp(trend_long_term) * ones));
  
  // Generate posterior predictions
  for (ii in 1:n_surveys) {
    survey_y_rep[ii,] = multinomial_rng(survey_trend_probabilities[ii, ]', n_survey_responses[ii]);
  }
  
  // Compute correlation matrix
  trend_party_correlation = multiply_lower_tri_self_transpose(trend_party_correlation_chol);
  
  // Polling (model) error distribution
  {
    matrix[n_elections - 1, n_parties] elections_tmp = trend[election_layer1_idx[1:n_elections - 1],];
    matrix[n_elections - 1, n_parties] elections_polling_error_tmp = trend[election_layer1_idx[1:n_elections - 1],] + polling_error[1:n_elections - 1,];
    polling_error_distribution_at_observed = exp(elections_polling_error_tmp - log(exp(elections_polling_error_tmp) * ones)) - 
                                             exp(elections_tmp - log(exp(elections_tmp) * ones));
  }
}







