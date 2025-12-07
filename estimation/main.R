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
END_DATE = as.Date("2015-09-27")

PARTIES_FIXED = c("CDU/CSU", "SPD", "FDP","GRÜNE", "LINKE")
RESIDUAL_CATEGORY = c("Sonstige")
PARTIES_TRANS = c("AfD")

PARTIES = c(PARTIES_FIXED, RESIDUAL_CATEGORY, PARTIES_TRANS)

PARTY_END_DATES = data.frame(
  party = c(
    "AfD"
  ),
  start_date = c(
    as.Date("2013-04-01")
  ),
  end_date = c(
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
  
  lfo_cutoff_index = 600
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


mod <- cmdstan_model(
  stan_file = "estimation/stan/gp_model_entering.stan",
  stanc_options = list("O1")
)

# Fit the model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  parallel_chains = 6,
  iter_warmup = 500,
  iter_sampling = 500,
  refresh = 500
)

starting_values = fit$summary("trend_voteshares") %>%
  filter(
    grepl("\\[2,", variable)
  )
plot_voting_trends(fit, index_date, df, election_dates, party_names = PARTIES,
                   cutoff_date = max(df$date) - 120)
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

function(fit, input_data, 
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
    "REP"     = "#C8A050",  # Brown
    "PIRATEN" = "#FF8800",  # Orange
    "AfD"     = "#009ee0"   # Light blue
  )
  
  # Helper function to prepare trend data
  prepare_trend_data <- function(trend_var) {
    fit$summary(trend_var, ~quantile(., c(0.025, 0.25, 0.5, 0.75, 0.975))) %>%
      mutate(
        layer1_aggregate_idx = as.integer(str_match(variable, "(\\d+),")[, 2]),
        ix_party = as.integer(str_match(variable, ",(\\d+)")[, 2]),
        party = factor(
          party_names[ix_party],
          levels = names(party_colors)
        )
      ) %>%
      right_join(index_date %>% 
                   select(date, layer1_aggregate_idx),
                 relationship = 'many-to-many')
  }
  
  # Helper function to filter relevant election dates
  get_relevant_elections <- function(trend_data) {
    election_dates %>%
      filter(
        date >= min(trend_data$date),
        date <= max(trend_data$date)
      )
  }
  
  # Prepare observed data (shared across all plots)
  observed_data <- df %>% 
    group_by(uuid, party, date) %>%
    summarize(vote_share = sum(vote_share)) %>%
    ungroup() %>%
    mutate(vote_share = vote_share / 100)
  
  # Helper function to create trend plot
  create_trend_plot <- function(trend_data, title_prefix, trend_description, trend_var) {
    relevant_elections <- get_relevant_elections(trend_data)
    
    # Define legend settings based on plot type
    legend_settings <- if (grepl("short_term", trend_var)) {
      theme(legend.position = "none")
    } else {
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )
    }
    
    ggplot(trend_data, aes(x = date, y = `50%`, color = party, fill = party)) +
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
      # Add trend lines and observed points (if applicable)
      geom_line(linewidth = 1) +
      {if (!grepl("short_term", trend_var)) 
        geom_point(
          data = observed_data,
          aes(y = vote_share),
          size = 2
        )
      } +
      # Customize appearance
      scale_color_manual(values = party_colors) +
      scale_fill_manual(values = party_colors) +
      {if (grepl("short_term", trend_var)) {
        facet_wrap(~party, ncol = 2)
      }} + 
      {if (grepl("short_term", trend_var)) {
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          name = "Percentage Difference from Long-term Average",
          # Let the limits be determined by the data
          expand = expansion(mult = 0.05)
        )
      } else {
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          limits = c(0, 0.5),
          name = "Vote Share"
        )
      }} +
      theme_light() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()
      ) +
      legend_settings +
      # Add 5% threshold line and annotation for non-short-term plots
      {if (!grepl("short_term", trend_var)) {
        list(
          geom_hline(
            yintercept = 0.05,
            linetype = "dashed",
            color = "black",
            alpha = 0.5
          ),
          annotate(
            "label",
            x = min(trend_data$date, na.rm = TRUE),
            y = 0.05,
            label = "5% electoral threshold",
            hjust = 0,
            size = 3,
            fill = "white",
            alpha = 0.8,
            label.r = unit(0.15, "lines")
          )
        )
      }} +
      # Add labels
      labs(
        title = paste0(title_prefix, " Vote Shares Over Time"),
        subtitle = paste0(trend_description, "\nLines show median predictions with 50% and 95% credible intervals"),
        x = "Date",
        y = "Vote Share",
        caption = "Points show observed data. Bands show 50% (darker) and 95% (lighter) credible intervals."
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
  }
  
  # Define trend components
  trend_components <- 
    list(
      var = "trend_voteshares",
      prefix = "Overall",
      desc = "Combined long-term and short-term trends",
      filename = "overall_trend_baseline.png"
    )
  
  index_date = date_index_list$index_date
  # Create and save all plots
  plots <- list()
  
    # Prepare trend data
    trend_data <- prepare_trend_data(trend_components$var)
    
    # Create plot
    plot <- create_trend_plot(
      trend_data = trend_data,
      title_prefix = trend_components$prefix,
      trend_description = trend_components$desc,
      trend_var = trend_components$var
    )
    
    # Save plot
    ggsave(
      filename = file.path(save_path, component$filename),
      plot = plot,
      width = 12,
      height = 8,
      dpi = 300
    )
    
    plots[[component$var]] <- plot
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


