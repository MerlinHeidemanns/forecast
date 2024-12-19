
functions {
  #include fft.stan
  #include utils.stan
}

data {
  // Dimensions
  int NTimePoints;
  int NWeeks;
  int NGroups;
  int NSurveys;
  
  // Indices
  array[NTimePoints] int ix_week;
  array[NSurveys] int ix_date;
  
  // Survey data
  array[NSurveys, NGroups] int y;
  
  vector[NSurveys] rounding_error_scale;
  
  // Prior parameters
  real prior_mu_sigma;
  real prior_length_scale;
  real period;
  real prior_sd_tau_sigma;
  real prior_mu_f_sigma;
  real chosen_length_scale_sigma;
  
  // Control flags
  int flag_inference;
}

transformed data {
  // Dimensions for padding
  int NTimePointsPadding = NTimePoints + 150;
  int NWeeksPadding = NTimePoints + 50;
  
  // Survey totals
  array[NSurveys] int SurveySampleSize;
  for (i in 1:NSurveys) {
    SurveySampleSize[i] = sum(y[i, ]);
  }
  
  // Pre-computed matrices
  matrix[NGroups, NGroups] ones = rep_matrix(1, NGroups, NGroups);
  matrix[NTimePointsPadding, NGroups - 1] zero_matrix = rep_matrix(0.0, NTimePointsPadding, NGroups - 1);
  matrix[NGroups - 1, NGroups - 1] ones_diag_k = rep_matrix(0.0, NGroups - 1, NGroups - 1);
  for (ii in 1:NGroups - 1) ones_diag_k[NGroups - 1, NGroups - 1] = 1.0;
  
  // Covariance FFT
  vector[NWeeks %/% 2 + 1] cov_rfft_sigma = 
    gp_periodic_exp_quad_cov_rfft(NWeeks, 1, chosen_length_scale_sigma, NWeeks) + 1e-9;
}

parameters {
  // GP and correlation parameters
  matrix[NTimePointsPadding, NGroups - 1] z;
  cholesky_factor_corr[NGroups - 1] L_Omega;
  real<lower = 1, upper = NTimePoints> trend_length_scale;
  
  // Alpha parameters
  vector[NGroups - 2] z_alpha;
  
  // Sigma parameters
  real mu_sigma;
  vector[NWeeks] z_sigma;
  real<lower = 0> tau_sigma;
  
  // Trend parameters
  row_vector[NGroups - 1] mu_f;
  
  // Rounding error
  matrix<lower = -1, upper = 1>[NSurveys, NGroups] z_rounding_error;
}

transformed parameters {
  // Declare transformed parameters
  vector[NTimePointsPadding %/% 2 + 1] cov_rfft;
  vector<lower=0>[NGroups - 1] alpha;
  matrix[NWeeks, NGroups - 1] sigma;
  matrix[NTimePoints, NGroups] f;
  matrix[NSurveys, NGroups] delta;
  
  matrix[NSurveys, NGroups] rounding_error;
  for (i in 1:NGroups){
    rounding_error[, i] = z_rounding_error[, i] .* rounding_error_scale;
  }
  
  // Compute sigma
  sigma = exp(mu_sigma + tau_sigma * gp_inv_rfft(z_sigma, rep_vector(0.0, NWeeks), cov_rfft_sigma)) 
          * rep_row_vector(1.0, NGroups - 1);
  
  profile("rfft_matrix_inversion") {
    // Compute alpha and covariance
    alpha[2:NGroups - 1] = exp(z_alpha);
    alpha[1] = 1;
    cov_rfft = gp_periodic_exp_quad_cov_rfft(NTimePointsPadding, 1, trend_length_scale, period) + 1e-9;
    
    // Compute f
    f[, 2:NGroups] = sigma[ix_week, ] .* 
                     gp_inv_rfft_multi_outcome(z, zero_matrix, cov_rfft)[1:NTimePoints] * 
                     diag_pre_multiply(alpha, L_Omega)';
    f[, 1] = rep_vector(0.0, NTimePoints);
    f[, 2:NGroups] = f[, 2:NGroups] + rep_vector(1.0, NTimePoints) * mu_f;
  }
  
  // Compute rounding error
  
  profile("softmax") {
    matrix[NSurveys, NGroups] tmp = f[ix_date,];
    delta = exp(tmp - log(exp(tmp) * ones));
  }
}

model {
  // Priors for sigma parameters
  mu_sigma ~ normal(prior_mu_sigma, 0.2);
  tau_sigma ~ normal(0, prior_sd_tau_sigma);
  z_sigma ~ std_normal();
  
  // Priors for other parameters
  mu_f ~ normal(0, prior_mu_f_sigma);
  trend_length_scale ~ normal(prior_length_scale, 10);
  to_vector(z) ~ std_normal();
  z_alpha ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(1);
  
  // Rounding error
  to_vector(z_rounding_error) ~ std_normal();
  
  // Likelihood
  if (flag_inference == 1) {
    profile("llh_contributions") {
      for (ix_k in 1:NGroups) {
        y[, ix_k] ~ poisson((delta[, ix_k] + rounding_error[, ix_k]) .* to_vector(SurveySampleSize));
      }
    }
  }
}

generated quantities {
  // Posterior predictive checks
  array[NSurveys, NGroups] int y_rep;

  // Derived quantities
  matrix[NGroups - 1, NGroups - 1] Omega;
  matrix[NTimePoints, NGroups] trend;
  
  // Compute trend
  trend = exp(f - log(exp(f[1:NTimePoints, ]) * ones));
  
  // Generate posterior predictions
  for (ii in 1:NSurveys) {
    y_rep[ii,] = multinomial_rng(delta[ii, ]', SurveySampleSize[ii]);
  }
  
  // Compute correlation matrix
  Omega = multiply_lower_tri_self_transpose(L_Omega);
}







