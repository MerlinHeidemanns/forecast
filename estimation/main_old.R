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
source("estimation/libs_input.R")

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
LAYER1_PERIOD <- "14 days"     # Weekly
LAYER2_PERIOD <- "28 days"    # Bi-weekly

START_DATE = as.Date("1998-01-01")
END_DATE = as.Date("2024-09-27")

PARTIES_FIXED = c("CDU/CSU", "SPD", "FDP","GRÜNE", "LINKE")
RESIDUAL_CATEGORY = c("Sonstige")
PARTIES_TRANS = c("AfD", "BSW")

PARTIES = c(PARTIES_FIXED, RESIDUAL_CATEGORY, PARTIES_TRANS)

PARTY_END_DATES = data.frame(
  party = c(
    "AfD",
    "BSW"
  ),
  start_date = c(
    as.Date("2013-04-01"),
    as.Date("2024-01-08")
  ),
  end_date = c(
    as.Date("2099-01-01"),
    as.Date("2099-01-01")
  )
)



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
df = bind_rows(
  load_institute_data("Allensbach", POLL_DATA_DIR),
  #load_institute_data("Forsa", POLL_DATA_DIR)#,
  #load_institute_data("INSA", POLL_DATA_DIR)
) %>%
  mutate(
    uuid = paste0(publishing_date, "_", uuid)
  )

df = df %>%
  mutate(
    party = ifelse(party %in% c("REP", "Rechte", "PIRATEN", "FW"), "Sonstige", party)
  ) %>%
  group_by(uuid, publishing_date, Befragte, Zeitraum, mode, survey_count, 
           sampling_start_day, sampling_end_day, pollster, party) %>%
  summarize(
    vote_share = sum(vote_share, na.rm = TRUE)
  ) %>%
  ungroup()

df %>%
  filter(vote_share > 0) %>% 
  group_by(party) %>%
  summarize(min_date = min(publishing_date),
            max_date = max(publishing_date)) %>%
  arrange(min_date)


df = transform_polling_data(
  df,
  START_DATE,
  END_DATE,
  layer1_period = LAYER1_PERIOD,
  parties = PARTIES
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
  select(party_combination, all_of(PARTIES_TRANS)) %>%
  filter(party_combination != "CDU/CSU | SPD | FDP | GRÜNE | Sonstige | AfD | BSW")

vec_party_combination = df_party_combination %>%
  ungroup() %>%
  distinct(party_combination) %>%
  pull(party_combination) %>%
  unique() %>%
  sort()
vec_party_combination = vec_party_combination[vec_party_combination != "CDU/CSU | SPD | FDP | GRÜNE | Sonstige | AfD | BSW"]

df = df %>%
  left_join(df_party_combination %>%
              distinct(uuid, party_combination),
       by = "uuid") %>%
  filter(party_combination %in% vec_party_combination) %>%
  mutate(
    party_combination_idx = as.integer(factor(party_combination, levels = vec_party_combination))
  )


# Create party index
index_party <- create_party_index(PARTIES)
# Create index pollster
index_pollster = create_pollster_index(df)

# Create date index with adjusted periods
date_index_list <- create_date_indexes(
  START_DATE, END_DATE,
  LAYER1_PERIOD, LAYER2_PERIOD) # index_date, index_date_aggregate

df_pollster_rounding = detect_pollster_rounding(df = df, 
                                                index_pollster = index_pollster)


# Transform input data
input_data = transform_input_data(df = df, 
                                  date_index_list = date_index_list, 
                                  df_pollster_rounding = df_pollster_rounding, 
                                  days_cutoff = 0)


# Transform election data
df_elections = df_elections %>%
  left_join(date_index_list$index_date,
            by = c("election_date" = "date"))

N_reporting_patterns = length(vec_party_combination)
reporting_pattern = input_data %>% 
  pull(party_combination_idx)

pattern_matrix = df_reporting_pattern %>%
  arrange(factor(party_combination, levels = vec_party_combination)) %>%
  select(-party_combination)

party_list <- setNames(index_party$party_idx, index_party$party)

party_indices = matrix(-99,
           nrow = length(vec_party_combination),
           ncol = length(party_list))
party_count = rep(NA, length(vec_party_combination))
for (i in 1:length(vec_party_combination)){
  b = c()
  for (j in names(party_list)){
    if (grepl(j, vec_party_combination[i])){
      b = c(b, party_list[[j]])
    }
  }
  party_indices[i, 1:length(b)] = b
  party_count[i] = length(b)
}





list_party_presence = calculate_party_presence(input_data, PARTIES)

pattern_starts_ends = calculate_pattern_starts_ends(input_data)



# Calculate model dimensions
n_layer1 <- max(date_index_list$index_date$layer1_aggregate_idx)
n_layer2 <- max(date_index_list$index_date$layer2_aggregate_idx)
n_parties <- length(PARTIES) - 1  # Number of parties
n_surveys <- nrow(input_data)

# Extract indices
survey_layer1_idx <- input_data$layer1_aggregate_idx
trend_layer2_idx <- date_index_list$index_date_aggregate$layer2_aggregate_idx  # From index_date, not input_data

# Rounding vector
rounding_error_scale = input_data$rounding

# Create response matrix
survey_y <- input_data %>%
  select(all_of(PARTIES)) %>%
  as.matrix()

# elections
n_elections = df_elections %>% nrow
election_layer1_idx = df_elections$layer1_aggregate_idx
#election_y = df_elections %>%
#  select(all_of(PARTIES))

# spline
n_knots <- 10
spline_basis <- splines::bs(1:n_layer1, df = n_knots, degree = 3, intercept = TRUE)


mat = matrix(-99, nrow = nrow(list_party_presence$obs_idx), ncol = ncol(list_party_presence$obs_idx))
for (i in 1:nrow(mat)){
  for (j in 1:ncol(mat)){
    if (list_party_presence$obs_idx[i, j] != -99){
      mat[list_party_presence$obs_idx[i, j], j] = j
    }
  }
  k = sum(mat[i, ] > 0)
  v = mat[i, ]
  mat[i, 1:k] = v[v > 0]
  mat[i, (k + 1):ncol(mat)] = -99
}
parties_included = apply(mat, 1, function(x) sum( x> 0))
mat = mat[, 1:max(parties_included)]

PARTY_END_DATES = PARTY_END_DATES %>%
  left_join(
    date_index_list$index_date %>%
      select(date, layer1_aggregate_idx) %>%
      rename(start_date = date,
             layer1_aggregate_idx_start = layer1_aggregate_idx)
  ) %>%
  left_join(
    date_index_list$index_date %>%
      select(date, layer1_aggregate_idx) %>%
      rename(end_date = date,
             layer1_aggregate_idx_end = layer1_aggregate_idx)
  ) %>%
  mutate(
    party_always_entered = as.integer(is.na(layer1_aggregate_idx_start)),
    party_never_exited = as.integer(is.na(layer1_aggregate_idx_end)),
    layer1_aggregate_idx_start = ifelse(is.na(layer1_aggregate_idx_start),
                                        -99, layer1_aggregate_idx_start),
    layer1_aggregate_idx_end = ifelse(is.na(layer1_aggregate_idx_end),
                                        -99, layer1_aggregate_idx_end),
  )

# Create data list for Stan model
data_list <- list(
  # Dimensions
  n_layer1 = n_layer1,
  n_layer2 = n_layer2,
  n_parties_fixed = length(PARTIES_FIXED) + 1,
  n_parties_trans = length(PARTIES_TRANS),
  n_surveys = n_surveys,
  time_aggregation = 7,
  polling_error_correlation = 0.25,
  
  # Indices
  survey_layer1_idx = survey_layer1_idx,
  #survey_election_idx = survey_election_idx,
  trend_layer2_idx  = trend_layer2_idx,
  parties_survey_idx = t(list_party_presence$obs_idx),
  residual_idx = length(PARTIES_FIXED) + 1,
  negative_logit_zero = -5,
  party_emergence_layer1_idx = PARTY_END_DATES$layer1_aggregate_idx_start,
  party_disappearance_layer1_idx = PARTY_END_DATES$layer1_aggregate_idx_end,
  party_always_entered = PARTY_END_DATES$party_always_entered,
  party_never_exited = PARTY_END_DATES$party_never_exited,
  
  # Rounding
  rounding_error_scale = rounding_error_scale,
  
  # Varying response options
  R = nrow(pattern_starts_ends),
  reporting_pattern = reporting_pattern,
  pattern_matrix = as.matrix(pattern_matrix),
  n_parties_presence = list_party_presence$n_parties_presence,
  pattern_starts_ends = as.matrix(pattern_starts_ends),
  
  parties_survey_prediction_idx = mat,
  n_parties_presence_prediction = parties_included,
  
  # Response data
  survey_y = survey_y,
  
  # Dimensions elections
  n_elections = n_elections,
  
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
  
  flag_inference = 1,
  
  n_remove = 0,
  
  ## Baseline
  mat_party_indices = party_indices,
  party_count = party_count,
  idx_r = input_data$party_combination_idx,
  
  lfo_cutoff_index = 800
)

################################################################################
# Model Fitting - Benchmark
################################################################################

mod = cmdstan_model(
  stan_file = 'estimation/stan/benchmark/benchmark_forecast.stan',
  stanc_options = list("O1")
)


################################################################################
# Model Fitting
################################################################################

# Fit the model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  parallel_chains = 6,
  iter_warmup = 500,
  iter_sampling = 500,
  refresh = 500
)


diagnostics <- extract_model_diagnostics(fit, party_names, data_list)

diagnostics$elpd
diagnostics$mse
diagnostics$coverage


plot_voting_trends(fit, date_index_list$index_date, df, election_dates, party_names = PARTIES,
                   cutoff_date = max(df$date))



extract_profile_summary(fit)



extract_model_diagnostics <- function(fit, 
                                      party_names, 
                                      data_list,
                                      horizon_weeks = c(4, 12, 26)) {
  n_parties <- length(party_names)
  n_surveys <- data_list$n_surveys
  
  # Horizon for each survey (negative = fit, positive = holdout)
  horizon <- data_list$survey_layer1_idx - data_list$lfo_cutoff_index
  is_fit <- horizon <= 0
  
  # Bin holdout observations by horizon
  horizon_bins <- cut(
    horizon,
    breaks = c(-Inf, 0, horizon_weeks, Inf),
    labels = c("fit", paste0("1-", horizon_weeks[1], "w"), 
               paste0(horizon_weeks[-length(horizon_weeks)] + 1, "-", horizon_weeks[-1], "w"),
               paste0(">", horizon_weeks[length(horizon_weeks)], "w")),
    right = TRUE
  )
  
  # Log-likelihood
  log_lik <- fit$draws("log_lik", format = "draws_matrix")
  log_lik_mean <- colMeans(log_lik)
  
  # ELPD by horizon
  elpd_by_horizon <- tibble(
    survey_idx = seq_len(n_surveys),
    horizon = horizon,
    horizon_bin = horizon_bins,
    log_lik_mean = log_lik_mean
  ) %>%
    group_by(horizon_bin) %>%
    summarize(
      elpd_mean = sum(log_lik_mean),
      n_obs = n(),
      elpd_per_obs = elpd_mean / n_obs,
      .groups = "drop"
    )
  
  # MSE by party
  mse_fit <- fit$draws("mse_fit", format = "draws_matrix")
  mse_holdout <- fit$draws("mse_holdout", format = "draws_matrix")
  
  mse_summary <- tibble(
    party = party_names,
    mse_fit_mean = colMeans(mse_fit),
    mse_fit_se = apply(mse_fit, 2, sd),
    mse_holdout_mean = colMeans(mse_holdout),
    mse_holdout_se = apply(mse_holdout, 2, sd)
  )
  
  # Coverage / PIT values
  party_in_survey <- matrix(FALSE, n_surveys, n_parties)
  for (i in seq_len(n_surveys)) {
    r <- data_list$idx_r[i]
    party_in_survey[i, data_list$mat_party_indices[r, 1:data_list$party_count[r]]] <- TRUE
  }
  
  y_rep_geq_obs <- fit$draws("y_rep_geq_obs", format = "draws_matrix")
  n_draws <- nrow(y_rep_geq_obs)
  y_rep_geq_obs <- array(y_rep_geq_obs, dim = c(n_draws, n_surveys, n_parties))
  
  pit_values <- apply(y_rep_geq_obs, c(2, 3), mean)
  colnames(pit_values) <- party_names
  pit_values[!party_in_survey] <- NA
  
  compute_party_coverage <- function(pit_subset) {
    tibble(
      party = party_names,
      coverage_50 = map_dbl(seq_len(n_parties), ~mean(pit_subset[, .x] >= 0.25 & pit_subset[, .x] <= 0.75, na.rm = TRUE)),
      coverage_90 = map_dbl(seq_len(n_parties), ~mean(pit_subset[, .x] >= 0.05 & pit_subset[, .x] <= 0.95, na.rm = TRUE)),
      bias = map_dbl(seq_len(n_parties), ~mean(pit_subset[, .x], na.rm = TRUE) - 0.5),
      n_obs = map_int(seq_len(n_parties), ~sum(!is.na(pit_subset[, .x])))
    )
  }
  
  # Coverage by horizon bin
  coverage_by_horizon <- map(levels(horizon_bins), function(bin) {
    idx <- which(horizon_bins == bin)
    if (length(idx) == 0) return(NULL)
    compute_party_coverage(pit_values[idx, , drop = FALSE]) %>%
      mutate(horizon_bin = bin, .before = 1)
  }) %>%
    compact() %>%
    bind_rows()
  
  list(
    elpd = elpd_by_horizon,
    mse = mse_summary,
    coverage = coverage_by_horizon,
    pit_values = pit_values,
    log_lik = log_lik,
    horizon = tibble(survey_idx = seq_len(n_surveys), horizon = horizon, horizon_bin = horizon_bins)
  )
}
extract_profile_summary <- function(fit) {
  profiles <- fit$profiles()
  
  profile_df <- bind_rows(profiles, .id = "chain") %>%
    group_by(name) %>%
    summarize(
      total_time = sum(total_time),
      forward_time = sum(forward_time),
      reverse_time = sum(reverse_time),
      autodiff_calls = sum(autodiff_calls),
      .groups = "drop"
    ) %>%
    mutate(
      pct_total = total_time / sum(total_time) * 100,
      pct_forward = forward_time / sum(forward_time) * 100,
      pct_reverse = reverse_time / sum(reverse_time) * 100,
      time_per_call_us = total_time / autodiff_calls * 1e6
    ) %>%
    arrange(desc(total_time))
  
  profile_df
}



plot_voting_trends <- function(fit,
                               index_date,
                               df,
                               election_dates,
                               party_names,
                               cutoff_date,
                               save_path = "estimation/plt/trends/") {
  
  party_colors <- c(
    "CDU/CSU"  = "#000000",
    "SPD"      = "#E3000F",
    "GRÜNE"    = "#46962b",
    "FDP"      = "#FFED00",
    "LINKE"    = "#BE3075",
    "Sonstige" = "#A4A4A4",
    "REP"      = "#C8A050",
    "PIRATEN"  = "#FF8800",
    "AfD"      = "#009ee0"
  )
  
  trend_data <- fit$summary("trend_voteshares", ~quantile(., c(0.025, 0.25, 0.5, 0.75, 0.975))) %>%
    mutate(
      layer1_aggregate_idx = as.integer(str_match(variable, "(\\d+),")[, 2]),
      ix_party = as.integer(str_match(variable, ",(\\d+)")[, 2]),
      party = factor(party_names[ix_party], levels = names(party_colors))
    ) %>%
    right_join(index_date %>% select(date, layer1_aggregate_idx), relationship = "many-to-many")
  
  observed_data <- df %>%
    group_by(uuid, party, date) %>%
    summarize(vote_share = sum(vote_share) / 100, .groups = "drop")
  
  relevant_elections <- election_dates %>%
    filter(date >= min(trend_data$date), date <= max(trend_data$date))
  
  plot <- ggplot(trend_data, aes(x = date, y = `50%`, color = party, fill = party)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.125, color = NA) +
    geom_ribbon(aes(ymin = `25%`, ymax = `75%`), alpha = 0.25, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(data = observed_data, aes(y = vote_share), size = 2) +
    geom_hline(yintercept = 0.05, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = relevant_elections$date, linetype = "dashed", color = "darkred", alpha = 0.5) +
    geom_vline(xintercept = cutoff_date, linetype = "twodash", alpha = 0.75) +
    annotate("label", x = min(trend_data$date), y = 0.05, label = "5% threshold",
             hjust = 0, size = 3, fill = "white", alpha = 0.8) +
    scale_color_manual(values = party_colors) +
    scale_fill_manual(values = party_colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
    labs(
      title = "Overall Vote Shares Over Time",
      subtitle = "Combined long-term and short-term trends\nLines show median with 50% and 95% credible intervals",
      y = "Vote Share"
    ) +
    theme_light() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  ggsave(file.path(save_path, "overall_trend.png"), plot, width = 12, height = 8, dpi = 300)
}




























################################################################################
# Model Evaluation and Visualization
################################################################################

calculate_elpd(fit, data_list$n_remove)

calculate_ppc(fit, data_list$survey_y, n_remove)










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

fit$summary("election_trend_probabilities") %>%
  select(variable, mean) %>%
  mutate(
    election_idx = as.integer(str_match(variable, "\\[(\\d+)")[, 2]),
    party_idx = as.integer(str_match(variable, "(\\d+)\\]")[, 2]),
  ) %>%
  select(-variable) %>%
  pivot_wider(
    id_cols = election_idx,
    values_from = mean,
    names_from = party_idx
  )

plot_polling_error_trend

fit$summary("neg_logit_end_state")

## Visualize Trends
# Plot main vote share trends
plot_vote_share_trends(fit, 
                       input_data, 
                       date_index_list$index_date, 
                       df, 
                       party_names = PARTIES,
                       election_dates, 
                       cutoff_date = max(df$date) - 120)

# Plot trend volatility
plot_trend_volatility(fit, date_index_list$index_date, election_dates)

plot_transition_weights(fit, date_index_list$index_date, PARTIES_TRANS)


## Export Results
# Save daily vote share estimates
save_daily_vote_shares(fit,
                       index_date = index_date,
                       save_path = "web/public/estimated_trends/",
                       filename = "daily_vote_shares.csv")




plot_polling_error <- function(fit, 
                               input_data,
                               index_date,
                               df,
                               election_dates,
                               party_names,
                               cutoff_date,
                               save_path = "estimation/plt/trends/") {
  
  # Traditional German party colors
  party_colors <- c(
    "CDU/CSU" = "#000000",  # Black
    "SPD"     = "#E3000F",  # Red
    "GRÜNE"   = "#46962b",  # Green
    "FDP"     = "#FFED00",  # Yellow
    "LINKE"   = "#BE3075",  # Purple/Pink
    "Sonstige" = "#A4A4A4", # Grey
    "REP"     = "#C8A050"
  )
  
  # Prepare trend data
  trend_data <- fit$summary(
    "polling_error",
    ~quantile(., c(0.025, 0.25, 0.5, 0.75, 0.975))
  ) %>%
    mutate(
      layer1_aggregate_idx = as.integer(str_match(variable, "(\\d+),")[, 2]),
      ix_party = as.integer(str_match(variable, ",(\\d+)")[, 2]),
      party = factor(
        party_names[ix_party],
        levels = names(party_colors)
      )
    ) %>%
    right_join(index_date %>% 
                 select(date, layer1_aggregate_idx))
  
  # Get relevant elections
  relevant_elections <- election_dates %>%
    filter(
      date >= min(trend_data$date),
      date <= max(trend_data$date)
    )
  
  # Create plot
  plot <- ggplot(trend_data, aes(x = date, y = `50%`, color = party, fill = party)) +
    # Add uncertainty bands
    geom_ribbon(
      aes(ymin = `2.5%`, ymax = `97.5%`),
      alpha = 0.125,
      color = NA
    ) +
    geom_ribbon(
      aes(ymin = `25%`, ymax = `75%`),
      alpha = 0.25,
      color = NA
    ) +
    # Add trend line
    geom_line(linewidth = 1) +
    # Customize appearance
    scale_color_manual(values = party_colors) +
    scale_fill_manual(values = party_colors) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      name = "Polling Error",
      expand = expansion(mult = 0.05)
    ) +
    theme_light() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    # Add labels
    labs(
      title = "Polling Error Over Time",
      subtitle = "Lines show median predictions with 50% and 95% credible intervals",
      x = "Date",
      y = "Polling Error",
      caption = "Bands show 50% (darker) and 95% (lighter) credible intervals."
    ) +
    # Add election date markers and labels
    geom_vline(
      data = relevant_elections,
      aes(xintercept = date),
      linetype = "dashed",
      color = "darkred",
      alpha = 0.5
    ) +
    geom_text(
      data = relevant_elections,
      aes(
        x = date,
        y = max(trend_data$`97.5%`) -0.01,
        label = format(date, "%b %Y")
      ),
      angle = 90,
      hjust = 0.25,
      vjust = -0.45,
      size = 3,
      color = "darkred",
      inherit.aes = FALSE
    ) +
    # Add cutoff date marker and label
    geom_vline(
      data = data.frame(date = cutoff_date),
      aes(xintercept = date),
      linetype = "twodash",
      alpha = 0.75
    ) +
    geom_text(
      data = data.frame(date = cutoff_date),
      aes(
        x = date,
        y = max(trend_data$`97.5%`) -0.01,
        label = "Data cutoff"
      ),
      angle = 90,
      hjust = 0.25,
      vjust = -0.45,
      size = 3,
      inherit.aes = FALSE
    )
  
  # Save plot
  ggsave(
    filename = file.path(save_path, "polling_error.png"),
    plot = plot,
    width = 12,
    height = 8,
    dpi = 300
  )
  
  return(plot)
}


