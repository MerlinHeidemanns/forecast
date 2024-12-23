################################################################################
# Setup and Configuration
################################################################################

## Working Directory
setwd("/Users/merlinheidemanns/projects/forecast")

## Load Required Libraries
library(tidyverse)
library(cmdstanr)
library(lubridate)

## Source Custom Functions
source("estimation/utils.R")
source("estimation/plotting.R")

################################################################################
# Data Import
################################################################################

## Load Reference Data
election_dates <- read_csv("estimation/dta/reference/election_dates.csv")

################################################################################
# Global Constants
################################################################################

## Directory Paths
POLL_DATA_DIR <- "web/public/polling_data/"

## Time Period Constants
LAYER1_PERIOD <- "7 days"     # Weekly
LAYER2_PERIOD <- "14 days"    # Bi-weekly
LAYER3_PERIOD <- "quarter"    # Quarterly



df = load_institute_data("Allensbach", POLL_DATA_DIR)

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
    date < as.Date("2008-01-01"),
    !is.na(vote_share)
  ) %>%
  select(uuid, date, layer1_aggregate,
         survey_count, party, pollster, vote_share)

# Create party index
index_party <- data.frame(
  party = sort(unique(df$party))
) %>%
  mutate(ix_party = row_number())

# Create pollster index
index_pollster <- data.frame(
  pollster = sort(unique(df$pollster))
) %>%
  mutate(ix_pollster = row_number())

# Create date index with adjusted periods
index_date <- data.frame(
  date = seq(min(df$date), max(df$date), by = 1)
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
input_data <- df %>%
  filter(date < max(date) - 240) %>%
  mutate(y = floor(vote_share / 100 * survey_count)) %>%
  select(-vote_share) %>%
  pivot_wider(
    id_cols = c(uuid, date, survey_count, pollster),
    names_from = party,
    values_from = y
  ) %>%
  left_join(index_date, by = "date") %>%
  mutate(
    Sonstige = if_else(!is.na(REP), REP + Sonstige, Sonstige)
  ) %>%
  select(-REP) %>%
  filter(!is.na(Sonstige)) %>%
  left_join(df_pollster_rounding)


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


# Rounding vector
rounding_error_scale = input_data$rounding

# Create response matrix
y <- input_data %>%
  select(all_of(c("CDU/CSU", "FDP", "GRÜNE", "LINKE", "Sonstige", "SPD"))) %>%
  as.matrix()

# Create data list for Stan model
data_list <- list(
  # Dimensions
  n_layer1 = n_layer1,
  n_layer2 = n_layer2,
  n_layer3 = n_layer3,
  n_parties = n_parties,
  n_surveys = n_surveys,
  
  # Indices
  survey_layer1_idx = survey_layer1_idx,
  trend_layer2_idx  = trend_layer2_idx,
  trend_layer3_idx  = trend_layer3_idx,

  # Rounding
  rounding_error_scale = rounding_error_scale,
  
  # Response data
  y = y,
  
  # Prior parameters
  prior_volatility_short_term_mean_mu = 0.2,
  prior_trend_short_term_length_scale_mean = 0,
  prior_volatility_short_term_sigma = 0.2,
  prior_trend_short_term_mean_sigma = 2,
  prior_trend_mean_sigma = 4,
  
  # Model parameters
  volatility_short_term_length_scale_data = 14,
  trend_long_term_length_scale_data = 16,
  
  flag_inference = 1
)

mod <- cmdstan_model(
  stan_file = "estimation/stan/gp_model_sum.stan",
  stanc_options = list("O1")
)

# Fit the model
fit2 <- mod$sample(
  data = data_list,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 250
)



create_vote_share_ppc(fit2, data_list, 100)

compare_vote_shares(fit2, data_list)

compare_alpha_parameters(fit2, party_names = c("CDU/CSU", "FDP", "GRÜNE", "LINKE", "Sonstige", "SPD"))

plot_party_correlations(fit2, party_names = c("CDU/CSU", "FDP", "GRÜNE", "LINKE", "Sonstige", "SPD"),
                        n_draws = 10,
                        save_path =  "estimation/plt/prior_posterior/")

plot_length_scale_comparison(fit2, data_list$prior_trend_short_term_length_scale_mean)

plot_vote_share_trends(fit2, 
                       input_data, 
                       index_date, 
                       df, 
                       election_dates, 
                       cutoff_date = max(df$date) - 240)

plot_trend_volatility(fit2, index_date, election_dates)



save_daily_vote_shares(fit2,
                       index_date = index_date,
                       save_path = "web/public/estimated_trends/",
                       filename = "daily_vote_shares.csv")

The





