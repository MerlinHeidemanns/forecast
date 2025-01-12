################################################################################
# Setup and Configuration
################################################################################

## Working Directory
setwd("/Users/merlinheidemanns/projects/forecast")

## Load Required Libraries
library(tidyverse)
library(cmdstanr)
library(lubridate)
library(ggthemes)

## Source Custom Functions
source("estimation/utils.R")
source("estimation/plotting.R")

################################################################################
# Data Import
################################################################################

## Load Reference Data
election_results = read_csv("estimation/dta/processed/germany_federal_elections_wide.csv")
election_dates = read_csv("estimation/dta/reference/election_dates.csv")

################################################################################
# Global Constants
################################################################################

## Directory Paths
POLL_DATA_DIR <- "web/public/polling_data/"

## Time Period Constants
LAYER1_PERIOD <- "7 days"     # Weekly
LAYER2_PERIOD <- "14 days"    # Bi-weekly
LAYER3_PERIOD <- "quarter"    # Quarterly

START_DATE = as.Date("1998-01-01")
END_DATE = as.Date("2005-09-27")

PARTIES_FIXED = c("CDU/CSU", "SPD", "FDP","GRÜNE", "LINKE")
RESIDUAL_CATEGORY = c("Sonstige")
PARTIES_TRANS = c("REP")

################################################################################
# Date Transformation
################################################################################

## Election data
df_elections = election_results %>%
  filter(
    election_date > START_DATE,
    election_date < END_DATE
  ) %>%
  arrange(election_date) %>%
  mutate(election_idx = 1:n())


## Polling data

df = load_institute_data("Allensbach", POLL_DATA_DIR)

df %>%
  filter(vote_share > 0) %>% 
  group_by(party) %>%
  summarize(min_date = min(publishing_date)) %>%
  arrange(min_date)

# Initial data transformation
df <- df %>%
  rename(
    date = publishing_date
  ) %>%
  # Handle survey count
  mutate(
    survey_count = case_when(
      survey_count == 0 ~ NA_real_,
      TRUE ~ survey_count
    ),
    survey_count = if_else(is.na(survey_count), 
                           mean(survey_count, na.rm = TRUE),
                           survey_count),
    # Group dates into periods
    layer1_aggregate = floor_date(date, unit = LAYER1_PERIOD)
  ) %>%
  # Filter and select relevant columns
  filter(
    date <   END_DATE,
    date > START_DATE,
    !is.na(vote_share)
  ) %>%
  select(uuid, date, layer1_aggregate,
         survey_count, party, pollster, vote_share) %>%
  mutate(
    party = factor(party, levels = c(PARTIES_FIXED, RESIDUAL_CATEGORY, PARTIES_TRANS))
  )

df_party_combination = df %>%
  filter(vote_share > 0) %>%
  select(uuid, party) %>%
  group_by(uuid) %>%
  arrange(uuid, party) %>%
  mutate(
    party_combination = paste0(party, collapse = " | ")
  ) %>%
  filter(grepl("Sonstige", party_combination)) 

df_reporting_pattern = df_party_combination %>%
  ungroup() %>%
  distinct(party_combination, party) %>%
  mutate(flag = 1) %>%
  pivot_wider(
    id_cols = party_combination,
    names_from = party,
    values_from = flag,
    values_fill = 0
  ) %>%
  select(party_combination, all_of(PARTIES_TRANS))

vec_party_combination = df_party_combination %>%
  ungroup() %>%
  distinct(party_combination) %>%
  pull(party_combination) %>%
  unique() %>%
  sort()

df = df %>%
  left_join(df_party_combination %>%
              distinct(uuid, party_combination),
       by = "uuid") %>%
  filter(party_combination %in% vec_party_combination) %>%
  mutate(
    party_combination_idx = as.integer(factor(party_combination, levels = vec_party_combination))
  )


# Create party index
index_party <- data.frame(
  party = sort(unique(df$party))
) %>%
  mutate(party_idx = as.integer(factor(party, levels = c(PARTIES_FIXED, RESIDUAL_CATEGORY, PARTIES_TRANS))))

# Create pollster index
index_pollster <- data.frame(
  pollster = sort(unique(df$pollster))
) %>%
  mutate(pollster_idx = row_number())

# Create date index with adjusted periods
index_date <- data.frame(
  date = seq(START_DATE, END_DATE, by = 1)
) %>%
  mutate(
    layer1_aggregate = floor_date(date, unit = LAYER1_PERIOD),
    layer2_aggregate = floor_date(date, unit = LAYER2_PERIOD),
    layer3_aggregate = floor_date(date, unit = LAYER3_PERIOD),
    # First create a simple sequence of week numbers
    layer2_aggregate_idx = dense_rank(layer2_aggregate),
    layer3_aggregate_idx = dense_rank(layer3_aggregate)
  )

index_date_aggregate = index_date %>%
  distinct(layer1_aggregate, layer2_aggregate, layer3_aggregate,
           layer2_aggregate_idx, layer3_aggregate_idx) %>%
  arrange(layer1_aggregate) %>%
  mutate(layer1_aggregate_idx = row_number())

index_date = index_date %>%
  left_join(index_date_aggregate)

df_pollster_rounding = lapply(index_pollster$pollster, function(i){
  x = df %>%
    filter(pollster == i) %>%
    mutate(vote_share = vote_share / 100) %>%
    pull(vote_share)
  data.frame(
    pollster = i, 
    rounding = detect_rounding(x)
  )
}) %>%
  do.call("bind_rows", .) %>%
  left_join(index_pollster)


# Transform input data
max_date = max(df$date)
input_data <- df %>%
  filter(date < (max_date - 120)) %>%
  mutate(y = floor(vote_share / 100 * survey_count)) %>%
  select(-vote_share) %>% 
  arrange(party_combination_idx, uuid) %>%
  group_by(party_combination_idx, uuid) %>%
  mutate(survey_idx = cur_group_id()) %>%
  ungroup() %>%
  arrange(survey_idx) %>%
  pivot_wider(
    id_cols = c(uuid, survey_idx, date, survey_count, pollster, party_combination_idx, party_combination),
    names_from = party,
    values_from = y,
    values_fill = -99
  ) %>%
  left_join(index_date, by = "date") %>%
  left_join(df_pollster_rounding) %>%
  ungroup() %>%
  arrange(party_combination_idx)



# Transform election data
df_elections = df_elections %>%
  left_join(index_date,
            by = c("election_date" = "date"))

R = length(vec_party_combination)
reporting_pattern = input_data %>% 
  pull(party_combination_idx)
pattern_matrix = df_reporting_pattern %>%
  arrange(factor(party_combination, levels = vec_party_combination)) %>%
  select(-party_combination)


obs_idx = matrix(-99, nrow = nrow(input_data),
                      ncol = length(c(PARTIES_FIXED, RESIDUAL_CATEGORY, PARTIES_TRANS)))
PARTIES = c(PARTIES_FIXED, RESIDUAL_CATEGORY, PARTIES_TRANS)
for (i in PARTIES){
  vec_tmp = input_data %>%
    select(survey_idx, all_of(i)) %>%
    filter(get(i) > 0) %>%
    pull(survey_idx)
  obs_idx[1:length(vec_tmp), match(i, PARTIES)] = vec_tmp
}

n_parties_presence = apply(obs_idx, 2, function(x)sum(x != -99))
pattern_starts_ends = input_data %>%
  arrange(survey_idx) %>%
  group_by(party_combination_idx) %>%
  summarize(
    min = min(survey_idx),
    max = max(survey_idx)
  ) %>%
  select(-party_combination_idx)


# Calculate model dimensions
n_layer1 <- max(index_date$layer1_aggregate_idx)
n_layer2 <- max(index_date$layer2_aggregate_idx)
n_layer3 <- max(index_date$layer3_aggregate_idx)
n_parties <- 6  # Number of parties
n_surveys <- nrow(input_data)

# Extract indices
survey_layer1_idx <- input_data$layer1_aggregate_idx
trend_layer2_idx <- index_date_aggregate$layer2_aggregate_idx  # From index_date, not input_data
trend_layer3_idx <- index_date_aggregate$layer3_aggregate_idx  # From index_date, not input_data
survey_election_idx <- input_data$election_idx

# Rounding vector
rounding_error_scale = input_data$rounding

# Create response matrix
survey_y <- input_data %>%
  select(all_of(c(PARTIES_FIXED, RESIDUAL_CATEGORY, PARTIES_TRANS))) %>%
  as.matrix()

# elections
n_elections = df_elections %>% nrow
election_layer1_idx = df_elections$layer1_aggregate_idx
election_y = df_elections %>%
  select(all_of(c("CDU/CSU", "FDP", "GRÜNE", "LINKE", "Sonstige", "SPD")))

# spline
n_knots <- 5
spline_basis <- splines::bs(1:n_layer1, df = n_knots, degree = 3, intercept = TRUE)

# Create data list for Stan model
data_list <- list(
  # Dimensions
  n_layer1 = n_layer1,
  n_layer2 = n_layer2,
  n_layer3 = n_layer3,
  n_parties_fixed = length(PARTIES_FIXED) + 1,
  n_parties_trans = length(PARTIES_TRANS),
  n_surveys = n_surveys,
  
  # Indices
  survey_layer1_idx = survey_layer1_idx,
  #survey_election_idx = survey_election_idx,
  trend_layer2_idx  = trend_layer2_idx,
  trend_layer3_idx  = trend_layer3_idx,
  parties_survey_idx = t(obs_idx),
  residual_idx = length(PARTIES_FIXED) + 1,
  negative_logit_zero = -10,
  party_emergence_layer1_idx = 13 - 12,
  party_disappearance_layer1_idx = 267 + 20,

  
  # Rounding
  rounding_error_scale = rounding_error_scale,
  
  # Varying response options
  R = R,
  reporting_pattern = reporting_pattern,
  pattern_matrix = as.matrix(pattern_matrix),
  n_parties_presence = n_parties_presence,
  obs_idx = obs_idx,
  pattern_starts_ends = as.matrix(pattern_starts_ends),
  
  # Response data
  survey_y = survey_y,
  
  # Dimensions elections
  #n_elections = n_elections,
  
  # Indices elections
  #election_layer1_idx = election_layer1_idx,
  
  # Election data
  #election_y = election_y,
  
  # Prior parameters
  prior_volatility_short_term_mean_mu = 0.2,
  prior_trend_short_term_length_scale_mean = 0,
  prior_volatility_short_term_sigma = 0.2,
  prior_trend_short_term_mean_sigma = 2,
  prior_trend_mean_sigma = 20,
  prior_spline_2nd_diff_scale = 1,
  
  # Splints
  n_knots = n_knots,
  spline_basis = as.matrix(spline_basis),
  
  # Model parameters
  volatility_short_term_length_scale_data = 14,
  trend_long_term_length_scale_data = 16,
  
  flag_inference = 1
)


################################################################################
# Model Fitting
################################################################################


mod <- cmdstan_model(
  stan_file = "estimation/stan/gp_model_entering.stan",
  stanc_options = list("O1")
)

# Fit the model
fit <- mod$sample(
  data = data_list,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 250
)


################################################################################
# Model Evaluation and Visualization
################################################################################

## Posterior Predictive Checks
create_vote_share_ppc(fit, data_list, 100)

## Parameter Analysis
# Compare alpha parameters across parties
compare_alpha_parameters(fit, 
                         party_names = PARTIES)

# Analyze party correlations
plot_party_correlations(fit, 
                        party_names = PARTIES,
                        n_draws = 10,
                        save_path = "estimation/plt/prior_posterior/")

# Compare length scales
plot_length_scale_comparison(fit, 
                             data_list$prior_trend_short_term_length_scale_mean)

################################################################################
# Trend Analysis and Results Export
################################################################################

## Visualize Trends
# Plot main vote share trends
plot_vote_share_trends(fit, 
                       input_data, 
                       index_date, 
                       df, 
                       party_names = PARTIES,
                       election_dates, 
                       cutoff_date = max(df$date) - 120)

# Plot trend volatility
plot_trend_volatility(fit, index_date, election_dates)

## Export Results
# Save daily vote share estimates
save_daily_vote_shares(fit,
                       index_date = index_date,
                       save_path = "web/public/estimated_trends/",
                       filename = "daily_vote_shares.csv")

fit$draws("transition_weights") %>% 
  posterior::as_draws_df() %>% 
  mutate(iter = 1:n()) %>% 
  select(!contains(".")) %>%
  pivot_longer(
    cols = c(-iter),
    names_pattern = "transition_weights\\[(\\d+),(\\d+)\\]",
    names_to = c("time", "party"),
    values_to = "weight"
  ) %>%
  filter(time == 1) %>%
  arrange(iter) %>% 
  summarize(
    min = min(weight)
  )


library(tidyverse)
library(posterior)
library(bayesplot)

# Assuming fit is your cmdstanr fit object
draws <- fit$draws()

# Convert to tibble for easier plotting
posterior_df <- as_draws_df(draws)

# 1. Transition Weight Functions Plot
plot_transition_weights <- function(posterior_df) {
  weights_long <- posterior_df %>%
    select(starts_with("transition_weights")) %>%
    pivot_longer(everything(), 
                 names_to = c("time", "party"), 
                 names_pattern = "transition_weights\\[(\\d+),(\\d+)\\]",
                 values_to = "weight") %>%
    group_by(time, party) %>%
    summarise(
      mean_weight = mean(weight),
      lower = quantile(weight, 0.025),
      upper = quantile(weight, 0.975)
    )
  
  ggplot(weights_long, aes(x = as.numeric(time), y = mean_weight, color = party)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = party), alpha = 0.2) +
    labs(title = "Transition Weight Functions",
         subtitle = "Posterior mean with 95% credible intervals",
         x = "Time", y = "Weight") +
    theme_minimal()
}
plot_transition_weights(posterior_df)









library(cmdstanr)
library(bayesplot)
library(posterior)

# Assuming 'fit' is your CmdStanMCMC object
draws <- fit$draws("transition_rate[1]")  # Gets all parameter draws

# Basic trace plot for trend_length_scale[1]
mcmc_trace(draws, pars = "transition_rate[1]")

create_trace_plot(fit, parameter_name = "transition_rate[1]")



# Function to analyze chain performance from a cmdstanr fit object
analyze_chain_performance <- function(fit) {
  # Extract sampling diagnostics
  sampler_params <- fit$sampler_diagnostics()
  
  # Get dimensions
  n_iterations <- dim(sampler_params)[1]
  n_chains <- dim(sampler_params)[2]
  
  # Calculate metrics for each chain
  chain_metrics <- lapply(1:n_chains, function(chain) {
    # Create data frame of metrics for this chain
    data.frame(
      chain = chain,
      mean_stepsize = mean(sampler_params[, chain, "stepsize__", drop = TRUE]),
      mean_treedepth = mean(sampler_params[, chain, "treedepth__", drop = TRUE]),
      mean_divergent = mean(sampler_params[, chain, "divergent__", drop = TRUE]),
      mean_accept_stat = mean(sampler_params[, chain, "accept_stat__", drop = TRUE]),
      max_treedepth = max(sampler_params[, chain, "treedepth__", drop = TRUE]),
      n_divergent = sum(sampler_params[, chain, "divergent__", drop = TRUE])
    )
  })
  
  # Combine chain metrics into a single data frame
  chain_metrics_df <- do.call(rbind, chain_metrics)
  
  # Add timing information if available
  timing <- fit$time()
  if (!is.null(timing) && !is.null(timing$chains)) {
    total_times <- sapply(timing$chains, function(x) sum(unlist(x)))
    chain_metrics_df$total_time <- total_times
    chain_metrics_df$relative_slowdown <- total_times / min(total_times)
  }
  
  return(chain_metrics_df)
}

# Function to analyze treedepth distribution
analyze_treedepth_distribution <- function(fit) {
  sampler_params <- fit$sampler_diagnostics()
  n_chains <- dim(sampler_params)[2]
  
  # Extract treedepth for each chain
  treedepth_dist <- lapply(1:n_chains, function(chain) {
    data.frame(
      chain = chain,
      iteration = 1:dim(sampler_params)[1],
      treedepth = sampler_params[, chain, "treedepth__", drop = TRUE]
    )
  })
  
  do.call(rbind, treedepth_dist)
}

# Function to plot treedepth distribution
plot_treedepth_distribution <- function(treedepth_dist) {
  library(ggplot2)
  
  ggplot(treedepth_dist, aes(x = treedepth, fill = factor(chain))) +
    geom_histogram(position = "dodge", bins = 30) +
    facet_wrap(~chain) +
    theme_minimal() +
    labs(title = "Distribution of Treedepth by Chain",
         x = "Treedepth",
         y = "Count",
         fill = "Chain")
}

# Function to analyze divergences
analyze_divergences <- function(fit) {
  sampler_params <- fit$sampler_diagnostics()
  n_chains <- dim(sampler_params)[2]
  
  # Extract divergence information for each chain
  divergence_info <- lapply(1:n_chains, function(chain) {
    divergences <- sampler_params[, chain, "divergent__", drop = TRUE]
    if (length(which(divergences == 1)) > 0){
      data.frame(
        chain = chain,
        iteration = which(divergences == 1),
        accept_stat = sampler_params[which(divergences == 1), chain, "accept_stat__", drop = TRUE],
        treedepth = sampler_params[which(divergences == 1), chain, "treedepth__", drop = TRUE]
      )
    }
  })
  
  do.call(rbind, divergence_info)
}


param_names = c("trend_length_scale", "alpha", "volatility_short_term", "volatility_short_term_scale", "transition_rate")

analyze_parameters <- function(fit, param_names) {
  # Get Stan's diagnostics
  stan_diagnostics <- fit$summary(param_names)
  
  # Extract draws for parameter-specific analysis
  draws_df <- as_draws_df(fit$draws(param_names))
  param_names <- grep("^(?!.*__$)", names(draws_df), value = TRUE, perl = TRUE)
  param_names = param_names[!grepl("^\\.", param_names)]
  # Function to calculate parameter stability
  calculate_stability <- function(values) {
    n <- length(values)
    first_half <- values[1:(n/2)]
    second_half <- values[(n/2 + 1):n]
    mean_diff <- abs(mean(first_half) - mean(second_half))
    var_ratio <- var(first_half) / var(second_half)
    c(mean_diff = mean_diff, var_ratio = var_ratio)
  }
  
  # Analyze each parameter
  param_diagnostics <- lapply(param_names, function(param) {
    print(param)
    # Extract parameter values by chain
    chain_values <- split(draws_df[[param]], draws_df$.chain)
    
    # Get Stan's diagnostics for this parameter
    stan_diag <- stan_diagnostics[stan_diagnostics$variable == param, ]
    
    # Calculate diagnostics for each chain
    chain_diagnostics <- lapply(seq_along(chain_values), function(chain) {
      values <- chain_values[[chain]]
      stability <- calculate_stability(values)
      try(
        data.frame(
          parameter = param,
          chain = chain,
          mean = mean(values),
          sd = sd(values),
          rhat = stan_diag$rhat,               # Use Stan's Rhat
          ess_bulk = stan_diag$ess_bulk,       # Use Stan's ESS bulk
          ess_tail = stan_diag$ess_tail,       # Use Stan's ESS tail
          mean_stability = stability["mean_diff"],
          var_stability = stability["var_ratio"],
          min = min(values),
          max = max(values)
        )
      )
    })
    
    do.call(rbind, chain_diagnostics)
  })
  
  param_summary <- do.call(rbind, param_diagnostics)
  
  # Add parameter type classification
  param_summary$param_type <- case_when(
    grepl("^trend_party_correlation", param_summary$parameter) ~ "correlation",
    grepl("^alpha", param_summary$parameter) ~ "scale",
    grepl("^trend_length_scale", param_summary$parameter) ~ "hyperparameter",
    grepl("^volatility", param_summary$parameter) ~ "volatility",
    grepl("^spline", param_summary$parameter) ~ "spline",
    TRUE ~ "other"
  )
  
  return(param_summary)
}
View(analyze_parameters(fit, param_names))
find_problematic_parameters <- function(param_diagnostics) {
  # Define thresholds
  thresholds <- list(
    ess_bulk = 100,    # Minimum effective sample size for bulk
    ess_tail = 50,     # Minimum effective sample size for tails
    rhat = 1.05,       # Standard Rhat threshold
    var_stability = c(0.5, 2)  # Variance ratio bounds
  )
  
  # Flag parameters that exceed thresholds
  problems <- param_diagnostics %>%
    group_by(parameter) %>%
    summarize(
      min_ess_bulk = min(ess_bulk),
      min_ess_tail = min(ess_tail),
      max_rhat = max(rhat),
      worst_var_stability = ifelse(
        mean(abs(1 - var_stability)) > mean(abs(1 - 1/var_stability)),
        min(var_stability),
        max(var_stability)
      ),
      param_type = first(param_type),
      n_chains_problematic = sum(
        ess_bulk < thresholds$ess_bulk |
          ess_tail < thresholds$ess_tail |
          rhat > thresholds$rhat |
          var_stability < thresholds$var_stability[1] |
          var_stability > thresholds$var_stability[2]
      )
    ) %>%
    filter(
      min_ess_bulk < thresholds$ess_bulk |
        min_ess_tail < thresholds$ess_tail |
        max_rhat > thresholds$rhat |
        worst_var_stability < thresholds$var_stability[1] |
        worst_var_stability > thresholds$var_stability[2]
    ) %>%
    arrange(desc(n_chains_problematic), desc(max_rhat))
  
  return(problems)
}

plot_parameter_diagnostics <- function(fit, param_name) {
  draws_df <- as_draws_df(fit$draws())
  
  # Create trace plot
  p1 <- ggplot(draws_df, aes(x = .iteration, y = .data[[param_name]], color = factor(.chain))) +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Trace Plot:", param_name),
         x = "Iteration",
         y = "Value",
         color = "Chain")
  
  # Create density plot
  p2 <- ggplot(draws_df, aes(x = .data[[param_name]], fill = factor(.chain))) +
    geom_density(alpha = 0.3) +
    theme_minimal() +
    labs(title = paste("Density Plot:", param_name),
         x = "Value",
         y = "Density",
         fill = "Chain")
  
  # Arrange plots
  gridExtra::grid.arrange(p1, p2, ncol = 1)
}