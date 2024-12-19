## Set working directory
setwd("/Users/merlinheidemanns/projects/forecast")
## Load libraries
library(tidyverse)
library(cmdstanr)
library(lubridate)

#' Directory and File Management Constants
POLL_DATA_DIR <- "web/public/polling_data/"


# Define the time period for grouping (can be adjusted)
TIME_PERIOD <- "7 days"
WEEK_PERIOD <- "14 days"  # Twice the daily rate

LEFT_PARTY_NAMES <- c(
  "LinkePDS" = "LINKE",
  "LINKE" = "LINKE",
  "PDS" = "LINKE"
)

df = load_institute_data("Allensbach", POLL_DATA_DIR)

# Apply party name standardization
df <- df %>%
  mutate(
    party = if_else(party %in% LEFT_PARTY_NAMES, "LINKE", party)
  )

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
    date_aggregate = floor_date(date, unit = TIME_PERIOD)
  ) %>%
  # Filter and select relevant columns
  filter(
    date < as.Date("2003-01-01"),
    !is.na(vote_share)
  ) %>%
  select(uuid, date, date_aggregate,
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
    date_aggregate = floor_date(date, unit = TIME_PERIOD),
    week_aggregate = floor_date(date, unit = WEEK_PERIOD),
    # First create a simple sequence of week numbers
    ix_week_aggregate = dense_rank(week_aggregate)
  )

index_date_aggregate = index_date %>%
  distinct(date_aggregate, week_aggregate, ix_week_aggregate) %>%
  arrange(date_aggregate) %>%
  mutate(ix_date_aggregate = row_number())

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

mod <- cmdstan_model(
  stan_file = "estimation/stan/gp_model.stan",
  stanc_options = list("O1")
)

# Calculate model dimensions
NTimePoints <- max(index_date$ix_date_aggregate)
NWeeks <- max(index_date$ix_week_aggregate)
NGroups <- 6  # Number of parties
NSurveys <- nrow(input_data)

# Extract indices
ix_week <- index_date_aggregate$ix_week_aggregate  # From index_date, not input_data
ix_date <- input_data$ix_date_aggregate

# Rounding vector
rounding_error_scale = input_data$rounding

# Create response matrix
y <- input_data %>%
  select(all_of(c("CDU/CSU", "FDP", "GRÜNE", "LINKE", "Sonstige", "SPD"))) %>%
  as.matrix()

# Create data list for Stan model
data_list <- list(
  # Dimensions
  NTimePoints = NTimePoints,
  NWeeks = NWeeks,
  NGroups = NGroups,
  NSurveys = NSurveys,
  
  # Indices
  ix_week = ix_week,
  ix_date = ix_date,
  
  # Rounding
  rounding_error_scale = rounding_error_scale,
  
  # Response data
  y = y,
  
  # Prior parameters
  prior_mu_sigma = 0.2,
  prior_length_scale = 7,
  period = NTimePoints,
  prior_sd_tau_sigma = 0.2,
  prior_mu_f_sigma = 2,
  
  # Model parameters
  chosen_length_scale_sigma = 7,
  flag_inference = 1
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

plot_length_scale_comparison(fit2, data_list$prior_length_scale)

plot_vote_share_trends(fit2, input_data, index_date, df)

plot_trend_volatility(fit2, index_date)












