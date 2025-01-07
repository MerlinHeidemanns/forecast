data {
  int n_surveys;
  int n_factors;
  int n_states;
  vector[n_surveys] survey_responses;
  vector[n_surveys] survey_sd;
  array[n_surveys] int survey_factor_idx;
  array[n_surveys] int survey_state_idx;
  vector[n_factors] factor_shares;
  vector[n_factors] factor_values;
  
  int flag_inference;
}

parameters {
  vector[n_states] mean_response;  // Direct state effects (no hierarchy)
  sum_to_zero_vector[n_factors] delta;
}

model {
  // Priors
  mean_response ~ normal(0, 10);  // Direct prior on state effects
  delta ~ normal(0, 10);
  
  if (flag_inference == 1){
    survey_responses ~ normal(mean_response[survey_state_idx] + delta[survey_factor_idx], survey_sd);
  }
}

generated quantities {
  vector[n_surveys] survey_responses_rep;
  vector[n_factors - 1] delta_difference = delta[1:n_factors - 1] - delta[2:n_factors];
  
  for (i in 1:n_surveys){
    survey_responses_rep[i] = normal_rng(
      (mean_response[survey_state_idx[i]] + factor_values[survey_factor_idx[i]]), 
      survey_sd[i]
    );
  }
}