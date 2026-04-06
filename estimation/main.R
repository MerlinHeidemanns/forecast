################################################################################
# Main Fitting Script — B-Spline Model
#
# Purpose: Load canonical data, fit B-spline model on log-odds,
#          run diagnostics, plot trends, save posterior.
#          Must be run from the project root directory.
#
# Usage:   source("estimation/main.R")
################################################################################

library(tidyverse)
library(cmdstanr)
library(lubridate)
library(splines)

source("estimation/canonicalize.R")
source("estimation/build_references.R")
source("estimation/theme.R")


################################################################################
# Constants
################################################################################

LAYER_PERIOD <- "14 days"
START_DATE   <- as.Date("1998-01-01")
END_DATE     <- Sys.Date()

PARTIES <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "AfD", "LINKE", "Sonstige")

## Canonical column name -> display name (for election results)
## Used with rename(): new_name = "old_name", so: `CDU/CSU` = "cdu_csu"
COL_TO_PARTY <- c(
  `CDU/CSU` = "cdu_csu", SPD = "spd", `GRÜNE` = "gruene",
  FDP = "fdp", AfD = "afd", LINKE = "linke", Sonstige = "sonstige"
)


################################################################################
# Data Generation — build canonical CSVs if missing
################################################################################

if (!file.exists("data/polls.csv")) {
  message("data/polls.csv not found — running canonicalize_polls()...")
  canonicalize_polls()
}

if (!file.exists("data/results.csv") || !file.exists("data/pollsters.csv")) {
  message("Reference data missing — running build_references()...")
  build_results()
  build_pollsters()
}


################################################################################
# Load & Prepare Polling Data
################################################################################

df_long <- load_canonical_polls(parties = PARTIES)

## Filter to modeling window
df_long <- df_long %>%
  filter(publishing_date >= START_DATE, publishing_date <= END_DATE)

## Require at least the major parties (CDU/CSU, SPD, GRÜNE, FDP)
MAJOR_PARTIES <- c("CDU/CSU", "SPD", "GRÜNE", "FDP")

viable_polls <- df_long %>%
  group_by(uuid) %>%
  summarize(
    n_major = sum(party %in% MAJOR_PARTIES & !is.na(vote_share) & vote_share > 0),
    .groups = "drop"
  ) %>%
  filter(n_major == length(MAJOR_PARTIES)) %>%
  pull(uuid)

df_long <- df_long %>% filter(uuid %in% viable_polls)

## Fill missing minor parties with 0
df_long <- df_long %>%
  mutate(vote_share = replace_na(vote_share, 0))

message(sprintf("Using %d polls (require %d major parties, fill minor with 0)",
                n_distinct(df_long$uuid), length(MAJOR_PARTIES)))


################################################################################
# Time Grid
################################################################################

period_breaks <- seq(START_DATE, END_DATE + 14, by = LAYER_PERIOD)

date_to_tidx <- function(d) {
  findInterval(d, period_breaks, rightmost.closed = TRUE)
}


################################################################################
# Pivot to Wide & Build Response Matrix
################################################################################

df_wide <- df_long %>%
  mutate(party = factor(party, levels = PARTIES)) %>%
  select(uuid, publishing_date, survey_count, party, vote_share) %>%
  pivot_wider(names_from = party, values_from = vote_share) %>%
  filter(!is.na(survey_count), survey_count > 0) %>%
  mutate(t_idx = date_to_tidx(publishing_date))

## Convert vote shares (%) to integer counts
y_matrix <- df_wide %>%
  select(all_of(PARTIES)) %>%
  mutate(across(everything(), ~ floor(. / 100 * df_wide$survey_count))) %>%
  as.matrix()

## Drop incomplete rows
keep <- complete.cases(y_matrix) & rowSums(y_matrix) > 0
y_matrix <- y_matrix[keep, ]
t_idx    <- df_wide$t_idx[keep]

T_max <- max(t_idx)
N     <- nrow(y_matrix)
P     <- ncol(y_matrix)

## Pre-aggregate polls by time period (sum counts within each period)
y_df <- as.data.frame(y_matrix)
y_df$t_idx <- t_idx
agg <- aggregate(. ~ t_idx, data = y_df, FUN = sum)
t_agg    <- agg$t_idx
y_agg    <- as.matrix(agg[, -1])  # drop t_idx column
N_agg    <- nrow(y_agg)

message(sprintf("Stan data: T=%d periods, N=%d surveys (N_agg=%d unique periods), P=%d parties",
                T_max, N, N_agg, P))


################################################################################
# Election Results (loaded before spline basis — knots placed at elections)
################################################################################

elections <- read_csv("data/results.csv", show_col_types = FALSE) %>%
  filter(election_date >= START_DATE, election_date <= END_DATE)

e_vote <- elections %>%
  select(all_of(unname(COL_TO_PARTY))) %>%
  rename(!!!COL_TO_PARTY) %>%
  select(all_of(PARTIES)) %>%
  as.matrix()

e_vote[is.na(e_vote)] <- 0
## Replace zeros with small epsilon (Dirichlet needs all elements > 0)
e_vote[e_vote == 0] <- 0.001
e_vote <- e_vote / rowSums(e_vote)

e_idx <- date_to_tidx(elections$election_date)

## Only keep elections within the time grid
keep_e <- e_idx >= 1 & e_idx <= T_max
e_vote <- e_vote[keep_e, , drop = FALSE]
e_idx  <- e_idx[keep_e]

message(sprintf("Elections: %d within modeling window", length(e_idx)))


################################################################################
# B-Spline Basis — knots at election dates
################################################################################

## Inner knots = election time indices (within grid bounds)
knot_positions <- sort(unique(e_idx))
## Ensure knots are strictly interior (not at boundaries)
knot_positions <- knot_positions[knot_positions > 1 & knot_positions < T_max]

B <- bs(1:T_max, knots = knot_positions, degree = 3, intercept = TRUE)
B <- as.matrix(B)
K <- ncol(B)

message(sprintf("B-spline basis: K=%d basis functions (%d inner knots at election dates, degree 3)",
                K, length(knot_positions)))


################################################################################
# Stan Data List — build observed-time lookups
################################################################################

## Unique observed time indices (union of poll periods + election periods)
obs_idx <- sort(unique(c(t_agg, e_idx)))
N_obs   <- length(obs_idx)

## Lookup tables: map poll/election indices → position in obs_idx
poll_obs_lookup <- match(t_agg, obs_idx)
e_obs_lookup    <- match(e_idx, obs_idx)

message(sprintf("Observed time points: %d of %d total periods (%.0f%% reduction in softmax calls)",
                N_obs, T_max, (1 - N_obs / T_max) * 100))

data_list <- list(
  T     = T_max,
  P     = P,
  K     = K,
  B     = B,
  N     = N_agg,
  t_idx = t_agg,
  y     = y_agg,
  N_obs = N_obs,
  obs_idx = obs_idx,
  poll_obs_lookup = poll_obs_lookup,
  E     = length(e_idx),
  e_obs_lookup = e_obs_lookup,
  e_vote = e_vote
)


################################################################################
# Compile & Fit
################################################################################

mod <- cmdstan_model("estimation/stan/spline.stan")

fit <- mod$sample(
  data            = data_list,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 500,
  iter_sampling   = 500,
  refresh         = 100,
  max_treedepth   = 10,
  adapt_delta     = 0.9
)

message("Fitting complete.")
fit$summary("tau")
fit$summary("phi")
fit$cmdstan_diagnose()

## Profiling
message("\n===== PROFILING =====")
prof <- fit$profiles()
prof_summary <- do.call(rbind, lapply(1:length(prof), function(chain_name) {
  df <- prof[[chain_name]]
  df$chain <- chain_name
  df
}))
prof_agg <- aggregate(
  cbind(total_time, forward_time, reverse_time) ~ name,
  data = prof_summary, FUN = mean
)
prof_agg <- prof_agg[order(-prof_agg$total_time), ]
print(prof_agg)
message("=====================\n")


################################################################################
# Extract Trends
################################################################################

pi_summary <- fit$summary(
  "pi", ~quantile(., c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95))
)

trend <- pi_summary %>%
  mutate(
    t     = as.integer(str_match(variable, "\\[(\\d+),")[, 2]),
    p     = as.integer(str_match(variable, ",(\\d+)\\]")[, 2]),
    party = factor(PARTIES[p], levels = names(PARTY_COLORS)),
    date  = period_breaks[t]
  ) %>%
  filter(!is.na(date))


################################################################################
# Plot
################################################################################

polls_overlay <- df_wide[keep, ] %>%
  select(publishing_date, all_of(PARTIES)) %>%
  pivot_longer(-publishing_date, names_to = "party", values_to = "pct") %>%
  mutate(
    party = factor(party, levels = names(PARTY_COLORS)),
    pct   = pct / 100
  )

p <- ggplot(trend, aes(x = date, color = party, fill = party)) +
  geom_ribbon(aes(ymin = `5%`, ymax = `95%`), alpha = 0.12, color = NA) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), alpha = 0.25, color = NA) +
  geom_line(aes(y = `50%`), linewidth = 0.8) +
  geom_point(data = polls_overlay,
             aes(x = publishing_date, y = pct),
             size = 0.3, alpha = 0.15) +
  geom_vline(xintercept = elections$election_date,
             linetype = "dotted", color = "grey50", linewidth = 0.3) +
  scale_color_parties() +
  scale_fill_parties() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.5)) +
  labs(
    title    = "B-spline polling model",
    subtitle = "Cubic B-splines on log-odds, Dirichlet-multinomial likelihood. Bands = 50%/90% CI.",
    y        = "Vote share"
  ) +
  theme_forecast()

PLT_DIR <- "estimation/plt/spline"
dir.create(PLT_DIR, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(PLT_DIR, "trend.png"), p, width = 14, height = 7, dpi = 200)
message("Saved: trend.png")


################################################################################
# Save Posterior
################################################################################

saveRDS(list(
  fit           = fit,
  data_list     = data_list,
  trend         = trend,
  parties       = PARTIES,
  dates         = period_breaks[1:T_max],
  period_breaks = period_breaks,
  elections     = elections,
  e_vote        = e_vote,
  e_idx         = e_idx,
  t_agg         = t_agg,
  y_agg         = y_agg,
  N_agg         = N_agg
), file = "data/spline_fit.rds")
message("Saved: data/spline_fit.rds")


################################################################################
# Export JSON for Frontend
################################################################################

source("estimation/export.R")
export_all(
  fit_path   = "data/spline_fit.rds",
  polls_path = "data/polls.csv",
  out_dir    = "web/public/data/"
)
message("JSON export complete.")
