################################################################################
# Setup and Configuration
################################################################################

## Working Directory
setwd("/Users/merlinheidemanns/projects/forecast")

## Load Required Libraries
library(tidyverse)
library(cmdstanr)
library(lubridate)

################################################################################


mod = cmdstan_model("estimation/components/stan/adjustment_terms.stan",
                    stanc_options = list("O1"), force_recompile=TRUE)
simulate_and_infer_factors_n1 = function(
    factor_shares = c(0.25, 0.25, 0.25, 0.25),
    factor_values = c(0.8, 0.3, -0.1, -0.2),
    n_draws = 10,
    survey_sd = 0.005,
    seed = NULL
) {
  n_surveys = 100
  # Input validation
  if (!is.null(seed)) set.seed(seed)
  
  stopifnot(
    "Factor shares must sum to 1" = abs(sum(factor_shares) - 1) < 1e-10,
    "Number of factor shares must match values" = length(factor_shares) == length(factor_values),
    "Survey SD must be positive" = survey_sd > 0,
    "Number of draws must be positive" = n_draws > 0
  )
  
  n_factors = length(factor_shares)
  
  # Create factor assignments based on shares
  survey_factor_idx = c()
  for (i in 1:length(factor_shares)){
    survey_factor_idx = c(survey_factor_idx, rep(i, n_surveys * factor_shares[i]))
  }
  
  # Create initial data list
  data_list = list(
    n_surveys = n_surveys,
    n_factors = n_factors,
    survey_responses = rep(0, n_surveys),
    survey_sd = rep(survey_sd, n_surveys),
    survey_factor_idx = survey_factor_idx,
    factor_shares = factor_shares,
    factor_values = factor_values,
    flag_inference = 0
  )
  
  # Initial fit to generate synthetic data
  fit = mod$sample(
    data = data_list,
    parallel_chains = 4
  )
  
  # Function to process a single draw
  process_draw_mean_response = function(fit, draw_id, factor_values) {
    # Extract replicated responses for this draw
    survey_responses_rep = fit$draws("survey_responses_rep") %>%
      posterior::as_draws_df() %>%
      select(!contains(".")) %>%
      slice(draw_id) %>%
      as.matrix() %>%
      c()
    
    # Extract replicated responses for this draw
    mean_response = fit$draws("mean_response") %>%
      posterior::as_draws_df() %>%
      select(!contains(".")) %>%
      slice(draw_id) %>%
      as.matrix() %>%
      c()
    
    # Update data list with replicated responses
    data_list$survey_responses = survey_responses_rep
    data_list$flag_inference = 1
    
    # Fit inference model
    fit_inference = mod$sample(
      data = data_list,
      parallel_chains = 4,
      refresh = 0
    )
    return(data.frame(
      true_value = mean_response,
      estimate   = fit_inference$summary("mean_response")$mean,
      mean_delta = mean(factor_values)
    ))
  }
  process_draw_delta = function(fit, draw_id, factor_values) {
    # Extract replicated responses for this draw
    survey_responses_rep = fit$draws("survey_responses_rep") %>%
      posterior::as_draws_df() %>%
      select(!contains(".")) %>%
      slice(draw_id) %>%
      as.matrix() %>%
      c()
    
    # Extract replicated responses for this draw
    survey_responses_rep = fit$draws("survey_responses_rep") %>%
      posterior::as_draws_df() %>%
      select(!contains(".")) %>%
      slice(draw_id) %>%
      as.matrix() %>%
      c()
    
    # Update data list with replicated responses
    data_list$survey_responses = survey_responses_rep
    data_list$flag_inference = 1
    
    # Fit inference model
    fit_inference = mod$sample(
      data = data_list,
      parallel_chains = 4,
      refresh = 0
    )
    
    # Return summary statistics
    fit_inference$summary(
      "delta", 
      ~quantile(., c(0.025, 0.25, 0.5, 0.75, 0.975))
    ) %>%
      mutate(draw_id = draw_id,
             true_value = factor_values,
             mean_delta = mean(factor_values))
  }
  
  
  # Process multiple draws
  draws = sample(1:1000, size = n_draws)
  results_delta = lapply(draws, function(i){
    process_draw_delta(fit, i, factor_values)
  }) %>%
    bind_rows()
  results_delta %>%
    mutate(
      error = true_value - `50%`
    )
  
  
  # Process multiple draws
  draws = sample(1:1000, size = n_draws)
  results_mean = lapply(draws, function(i){
    process_draw_mean_response(fit, i, factor_values)
  }) %>%
    bind_rows() %>%
    mutate(
      error = true_value - estimate
    )

  # Return results
  list(
    results_delta = results_delta,
    results_mean = results_mean
  )
}

df = simulate_and_infer_factors()
  

simulate_and_infer_factors_n1 = function(
    n_states = 2,
    state_shares = c(0.6, 0.4),
    factor_shares = c(0.25, 0.25, 0.25, 0.25),
    factor_values = c(0.8, 0.3, -0.1, -0.2),
    n_draws = 10,
    survey_sd = 0.005,
    seed = NULL
) {
  n_surveys = 100
  # Input validation
  if (!is.null(seed)) set.seed(seed)
  
  stopifnot(
    "Factor shares must sum to 1" = abs(sum(factor_shares) - 1) < 1e-10,
    "Number of factor shares must match values" = length(factor_shares) == length(factor_values),
    "Survey SD must be positive" = survey_sd > 0,
    "Number of draws must be positive" = n_draws > 0
  )
  
  n_factors = length(factor_shares)
  
  # Create factor assignments based on shares
  survey_factor_idx = c()
  for (i in 1:length(factor_shares)){
    survey_factor_idx = c(survey_factor_idx, rep(i, n_surveys * factor_shares[i]))
  }
  # Create state assignments based on shares
  survey_state_idx = c()
  for (i in 1:length(state_shares)){
    survey_state_idx = c(survey_state_idx, rep(i, n_surveys * state_shares[i]))
  }
  
  # Create initial data list
  data_list = list(
    n_states = n_states,
    n_surveys = n_surveys,
    n_factors = n_factors,
    survey_responses = rep(0, n_surveys),
    survey_sd = rep(survey_sd, n_surveys),
    survey_state_idx = survey_state_idx,
    survey_factor_idx = survey_factor_idx,
    factor_shares = factor_shares,
    factor_values = factor_values,
    flag_inference = 0
  )
  
  # Initial fit to generate synthetic data
  fit = mod$sample(
    data = data_list,
    parallel_chains = 4
  )
  
  # Function to process a single draw
  process_draw_mean_response = function(fit, draw_id, factor_values) {
    # Extract replicated responses for this draw
    survey_responses_rep = fit$draws("survey_responses_rep") %>%
      posterior::as_draws_df() %>%
      select(!contains(".")) %>%
      slice(draw_id) %>%
      as.matrix() %>%
      c()
    
    # Extract replicated responses for this draw
    mean_response = fit$draws("mean_response") %>%
      posterior::as_draws_df() %>%
      select(!contains(".")) %>%
      slice(draw_id) %>%
      as.matrix() %>%
      c()
    
    # Update data list with replicated responses
    data_list$survey_responses = survey_responses_rep
    data_list$flag_inference = 1
    
    # Fit inference model
    fit_inference = mod$sample(
      data = data_list,
      parallel_chains = 4,
      refresh = 0
    )
    
    bayesplot::mcmc_pairs(fit_inference$draws(c('mean_response', "delta")))
    
    return(data.frame(
      true_value = mean_response,
      estimate   = fit_inference$summary("mean_response")$mean,
      mean_delta = mean(factor_values)
    ))
  }
  process_draw_delta = function(fit, draw_id, factor_values) {
    # Extract replicated responses for this draw
    survey_responses_rep = fit$draws("survey_responses_rep") %>%
      posterior::as_draws_df() %>%
      select(!contains(".")) %>%
      slice(draw_id) %>%
      as.matrix() %>%
      c()
    
    # Extract replicated responses for this draw
    survey_responses_rep = fit$draws("survey_responses_rep") %>%
      posterior::as_draws_df() %>%
      select(!contains(".")) %>%
      slice(draw_id) %>%
      as.matrix() %>%
      c()
    
    # Update data list with replicated responses
    data_list$survey_responses = survey_responses_rep
    data_list$flag_inference = 1
    
    # Fit inference model
    fit_inference = mod$sample(
      data = data_list,
      parallel_chains = 4,
      refresh = 0
    )
    
    # Return summary statistics
    fit_inference$summary(
      "delta", 
      ~quantile(., c(0.025, 0.25, 0.5, 0.75, 0.975))
    ) %>%
      mutate(draw_id = draw_id,
             true_value = factor_values,
             mean_delta = mean(factor_values))
  }
  
  
  # Process multiple draws
  draws = sample(1:1000, size = n_draws)
  results_delta = lapply(draws, function(i){
    process_draw_delta(fit, i, factor_values)
  }) %>%
    bind_rows()
  results_delta %>%
    mutate(
      error = true_value - `50%`
    )
  
  
  # Process multiple draws
  draws = sample(1:1000, size = n_draws)
  results_mean = lapply(draws, function(i){
    process_draw_mean_response(fit, i, factor_values)
  }) %>%
    bind_rows() %>%
    mutate(
      error = true_value - estimate
    )
  
  # Return results
  list(
    results_delta = results_delta,
    results_mean = results_mean
  )
}