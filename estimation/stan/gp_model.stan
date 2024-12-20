
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
  
  // Indices
  array[n_surveys] int<lower = 1, upper = n_layer1> survey_layer1_idx;
  array[n_layer1]  int<lower = 1, upper = n_layer2> trend_layer2_idx;
  array[n_layer1]  int<lower = 1, upper = n_layer3> trend_layer3_idx;
  
  // Survey data
  array[n_surveys, n_parties] int y;
  
  vector[n_surveys] rounding_error_scale;
  
  // Prior parameters
  real prior_volatility_short_term_mean_mu;
  real prior_volatility_short_term_sigma;
  real prior_trend_mean_sigma;
  real prior_trend_short_term_length_scale_mean;
  
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
  array[n_surveys] int SurveySampleSize;
  for (i in 1:n_surveys) {
    SurveySampleSize[i] = sum(y[i, ]);
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
  cholesky_factor_corr[n_parties - 1] L_Omega;
  real<lower = 1, upper = n_layer1> trend_short_term_length_scale;
  
  // Alpha parameters
  vector[n_parties - 2] z_alpha;
  
  // Sigma parameters
  real volatility_short_term_mean;
  vector[n_layer2_padding] volatility_short_term_std_normal;
  real<lower = 0> volatility_short_term_scale;

  // Trend parameters
  row_vector[n_parties - 1] trend_mean;
  
  // Rounding error
  matrix<lower = -1, upper = 1>[n_surveys, n_parties] rounding_error_std_normal;
}

transformed parameters {
  // Declare transformed parameters
  vector[n_layer1_padding %/% 2 + 1] trend_short_term_cov_rfft;
  vector<lower=0>[n_parties - 1] alpha;
  vector[n_layer2] volatility_short_term;
  matrix[n_layer1, n_parties] trend_short_term;
  matrix[n_layer1, n_parties] trend;
  matrix[n_surveys, n_parties] delta;
  
  matrix[n_surveys, n_parties] rounding_error;
  
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
    trend_short_term_cov_rfft = gp_periodic_exp_quad_cov_rfft(n_layer1_padding, 1, trend_short_term_length_scale, n_layer1_padding) + 1e-9;
    // -- 
    trend_short_term[, 2:n_parties] = (volatility_short_term[trend_layer2_idx] * rep_row_vector(1.0, n_parties - 1)) .* 
                                      gp_inv_rfft_multi_outcome(
                                        trend_short_term_std_normal, 
                                        trend_short_term_zero_matrix, 
                                        trend_short_term_cov_rfft
                                      )[1:n_layer1] * 
                                      diag_pre_multiply(alpha, L_Omega)';
    // -- Set first element to 0
    trend_short_term[, 1] = rep_vector(0.0, n_layer1);
    
    // Add mean
    trend[, 1] = rep_vector(0.0, n_layer1);
    trend[, 2:n_parties] = trend_short_term[, 2:n_parties] + 
                           rep_vector(1.0, n_layer1) * trend_mean;
  }
  
  // Compute rounding error
  for (i in 1:n_parties){
    rounding_error[, i] = rounding_error_std_normal[, i] .* rounding_error_scale;
  }
  
  profile("softmax") {
    matrix[n_surveys, n_parties] tmp = trend[survey_layer1_idx,];
    delta = exp(tmp - log(exp(tmp) * ones));
  }
}

model {
  // Priors for sigma parameters
  volatility_short_term_mean ~ normal(prior_volatility_short_term_mean_mu, 0.2);
  volatility_short_term_scale ~ normal(0, prior_volatility_short_term_sigma);
  volatility_short_term_std_normal ~ std_normal();

  // Priors for other parameters
  trend_mean ~ normal(0, prior_trend_mean_sigma);
  trend_short_term_length_scale ~ normal(prior_trend_short_term_length_scale_mean, 10);
  to_vector(trend_short_term_std_normal) ~ std_normal();

  z_alpha ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(1);
  
  // Rounding error
  to_vector(rounding_error_std_normal) ~ std_normal();
  
  // Likelihood
  if (flag_inference == 1) {
    profile("llh_contributions") {
      for (ix_k in 1:n_parties) {
        y[, ix_k] ~ poisson((delta[, ix_k] + rounding_error[, ix_k]) .* to_vector(SurveySampleSize));
      }
    }
  }
}

generated quantities {
  // Posterior predictive checks
  array[n_surveys, n_parties] int y_rep;

  // Derived quantities
  matrix[n_parties - 1, n_parties - 1] Omega;
  matrix[n_layer1, n_parties] trend_shares;

  // Compute trend
  trend_shares = exp(trend - log(exp(trend) * ones));
  
  // Generate posterior predictions
  for (ii in 1:n_surveys) {
    y_rep[ii,] = multinomial_rng(delta[ii, ]', SurveySampleSize[ii]);
  }
  
  // Compute correlation matrix
  Omega = multiply_lower_tri_self_transpose(L_Omega);
}







