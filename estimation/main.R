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
LAYER1_PERIOD <- "7 days"     # Weekly
LAYER2_PERIOD <- "14 days"    # Bi-weekly

START_DATE = as.Date("1998-01-01")
END_DATE = as.Date("2008-09-27")

PARTIES_FIXED = c("CDU/CSU", "SPD", "FDP","GRÜNE", "LINKE")
RESIDUAL_CATEGORY = c("Sonstige")
PARTIES_TRANS = c("REP")

PARTIES = c(PARTIES_FIXED, RESIDUAL_CATEGORY, PARTIES_TRANS)

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

df = load_institute_data("Allensbach", POLL_DATA_DIR) %>%
  mutate(
    uuid = paste0(publishing_date, "_", uuid)
  )

df %>%
  filter(vote_share > 0) %>% 
  group_by(party) %>%
  summarize(min_date = min(publishing_date)) %>%
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


list_party_presence = calculate_party_presence(input_data, PARTIES)

pattern_starts_ends = calculate_pattern_starts_ends(input_data)



# Calculate model dimensions
n_layer1 <- max(date_index_list$index_date$layer1_aggregate_idx)
n_layer2 <- max(date_index_list$index_date$layer2_aggregate_idx)
n_parties <- length(PARTIES) - 1  # Number of parties
n_surveys <- nrow(input_data)

# Extract indices
survey_layer1_idx <- input_data$layer1_aggregate_idx
trend_layer2_idx <- index_date_aggregate$layer2_aggregate_idx  # From index_date, not input_data

# Rounding vector
rounding_error_scale = input_data$rounding

# Create response matrix
survey_y <- input_data %>%
  select(all_of(PARTIES)) %>%
  as.matrix()

# elections
n_elections = df_elections %>% nrow
election_layer1_idx = df_elections$layer1_aggregate_idx
election_y = df_elections %>%
  select(all_of(PARTIES))

# spline
n_knots <- 5
spline_basis <- splines::bs(1:n_layer1, df = n_knots, degree = 3, intercept = TRUE)


mat = matrix(-99, nrow = nrow(list_party_presence$obs_idx), ncol = ncol(list_party_presence$obs_idx))
for (i in 1:nrow(mat)){
  for (j in 1:ncol(mat)){
    if (list_party_presence$obs_idx[i, j] != -99){
      mat[list_party_presence$obs_idx[i, j], j] = j
    }
  }
}
parties_included = apply(mat, 1, function(x) sum( x> 0))



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
  party_emergence_layer1_idx = 13 - 12,
  party_disappearance_layer1_idx = 267 + 1,

  
  # Rounding
  rounding_error_scale = rounding_error_scale,
  
  # Varying response options
  R = R,
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
  
  n_remove = 10
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
  iter_warmup = 200,
  iter_sampling = 50
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
                       index_date$index_date, 
                       df, 
                       party_names = PARTIES,
                       election_dates, 
                       cutoff_date = max(df$date) - 120)

# Plot trend volatility
plot_trend_volatility(fit, index_date$index_date, election_dates)

plot_transition_weights(fit, index_date$index_date, PARTIES_TRANS)


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


