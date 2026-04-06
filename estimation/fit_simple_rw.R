################################################################################
# Fit Simple Random Walk Model
#
# Minimal baseline: random walk on log-odds, multinomial likelihood.
# Filters to polls reporting all parties in PARTIES vector.
#
# Usage: source("estimation/fit_simple_rw.R")
################################################################################

library(tidyverse)
library(cmdstanr)
library(lubridate)

source("estimation/canonicalize.R")
source("estimation/build_references.R")
source("estimation/theme.R")

################################################################################
# Config
################################################################################

LAYER_PERIOD <- "7 days"    # weekly time grid
START_DATE   <- as.Date("1998-01-01")
END_DATE     <- Sys.Date()

PARTIES <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "AfD", "LINKE", "Sonstige")

################################################################################
# Generate data if missing
################################################################################

if (!file.exists("data/polls.csv")) canonicalize_polls()
if (!file.exists("data/results.csv")) { build_results(); build_pollsters() }

################################################################################
# Load & prepare data
################################################################################

## Load canonical polls (long format)
df_long <- load_canonical_polls(parties = PARTIES)

## Filter to modeling window
df_long <- df_long %>%
  filter(publishing_date >= START_DATE, publishing_date <= END_DATE)

## Identify polls that report ALL parties (non-NA, non-zero)
complete_polls <- df_long %>%
  group_by(uuid) %>%
  summarize(
    n_parties_reported = sum(!is.na(vote_share) & vote_share > 0),
    .groups = "drop"
  ) %>%
  filter(n_parties_reported == length(PARTIES)) %>%
  pull(uuid)

df_long <- df_long %>% filter(uuid %in% complete_polls)
message(sprintf("Using %d polls with all %d parties reported",
                n_distinct(df_long$uuid), length(PARTIES)))

## Time grid: weekly periods
all_dates <- seq(START_DATE, END_DATE, by = 1)
week_breaks <- seq(START_DATE, END_DATE + 7, by = LAYER_PERIOD)

date_to_tidx <- function(d) {
  findInterval(d, week_breaks, rightmost.closed = TRUE)
}

## Pivot to wide: one row per poll, columns = party counts
df_wide <- df_long %>%
  mutate(party = factor(party, levels = PARTIES)) %>%
  select(uuid, publishing_date, survey_count, party, vote_share) %>%
  pivot_wider(names_from = party, values_from = vote_share) %>%
  filter(!is.na(survey_count), survey_count > 0) %>%
  mutate(t_idx = date_to_tidx(publishing_date))

## Convert vote shares (%) to integer counts
party_cols <- PARTIES
y_matrix <- df_wide %>%
  select(all_of(party_cols)) %>%
  mutate(across(everything(), ~ floor(. / 100 * df_wide$survey_count))) %>%
  as.matrix()

## Drop any rows where conversion failed
keep <- complete.cases(y_matrix) & rowSums(y_matrix) > 0
y_matrix <- y_matrix[keep, ]
t_idx    <- df_wide$t_idx[keep]

T_max <- max(t_idx)
N     <- nrow(y_matrix)
P     <- ncol(y_matrix)

message(sprintf("Stan data: T=%d periods, N=%d surveys, P=%d parties", T_max, N, P))

################################################################################
# Election results
################################################################################

elections <- read_csv("data/results.csv", show_col_types = FALSE) %>%
  filter(election_date >= START_DATE, election_date <= END_DATE)

## Map canonical column names to display names
## rename(): new_name = "old_name"
col_map <- c(`CDU/CSU` = "cdu_csu", SPD = "spd", `GRÜNE` = "gruene",
             FDP = "fdp", AfD = "afd", LINKE = "linke", Sonstige = "sonstige")

e_vote <- elections %>%
  select(all_of(names(col_map))) %>%
  rename(!!!col_map) %>%
  select(all_of(PARTIES)) %>%
  as.matrix()

## Replace NA with 0 (parties that didn't exist yet)
e_vote[is.na(e_vote)] <- 0

## Renormalize rows to sum to 1
e_vote <- e_vote / rowSums(e_vote)

e_idx <- date_to_tidx(elections$election_date)

## Only keep elections within the time grid
keep_e <- e_idx >= 1 & e_idx <= T_max
e_vote <- e_vote[keep_e, , drop = FALSE]
e_idx  <- e_idx[keep_e]

message(sprintf("Elections: %d within modeling window", length(e_idx)))

################################################################################
# Stan data list
################################################################################

data_list <- list(
  T     = T_max,
  P     = P,
  N     = N,
  t_idx = t_idx,
  y     = y_matrix,
  E     = length(e_idx),
  e_idx = e_idx,
  e_vote = e_vote
)

################################################################################
# Compile & fit
################################################################################

mod <- cmdstan_model("estimation/stan/simple_rw.stan")

fit <- mod$sample(
  data            = data_list,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 500,
  iter_sampling   = 500,
  refresh         = 100
)

message("Fitting complete.")
fit$summary("sigma")
fit$cmdstan_diagnose()

################################################################################
# Extract & plot trends
################################################################################

pi_summary <- fit$summary("pi", ~quantile(., c(0.05, 0.25, 0.5, 0.75, 0.95)))

trend <- pi_summary %>%
  mutate(
    t = as.integer(str_match(variable, "\\[(\\d+),")[, 2]),
    p = as.integer(str_match(variable, ",(\\d+)\\]")[, 2]),
    party = factor(PARTIES[p], levels = names(PARTY_COLORS)),
    date  = week_breaks[t]
  ) %>%
  filter(!is.na(date))

## Raw poll data for overlay
polls_overlay <- df_wide[keep, ] %>%
  select(publishing_date, all_of(party_cols)) %>%
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
    title    = "Simple random walk model",
    subtitle = "Weekly log-odds random walk, multinomial likelihood. Bands = 50%/90% CI.",
    y        = "Vote share"
  ) +
  theme_forecast()

dir.create("estimation/plt/simple_rw", recursive = TRUE, showWarnings = FALSE)
ggsave("estimation/plt/simple_rw/trend.png", p, width = 14, height = 7, dpi = 200)
message("Saved: estimation/plt/simple_rw/trend.png")

################################################################################
# Save posterior draws for downstream use
################################################################################

pi_draws <- fit$draws("pi", format = "draws_matrix")
saveRDS(list(
  fit       = fit,
  data_list = data_list,
  trend     = trend,
  parties   = PARTIES,
  dates     = week_breaks[1:T_max]
), file = "data/simple_rw_fit.rds")
message("Saved: data/simple_rw_fit.rds")
