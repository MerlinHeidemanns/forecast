################################################################################
# Data Quality Plots
#
# Purpose: Visual checks on the raw canonical polling data.
# Usage:   source("estimation/plot_data_quality.R")
################################################################################

library(tidyverse)
source("estimation/theme.R")
source("estimation/canonicalize.R")   # for PARTY_COLS, load_canonical_polls

################################################################################
# Load data
################################################################################

polls <- read_csv("data/polls.csv", show_col_types = FALSE)
elections <- read_csv("data/results.csv", show_col_types = FALSE)

## Pivot polls to long format with display party names
polls_long <- polls %>%
  pivot_longer(
    cols      = all_of(PARTY_COLS),
    names_to  = "party_col",
    values_to = "vote_share"
  ) %>%
  mutate(
    party = factor(PARTY_LABELS[party_col], levels = names(PARTY_COLORS)),
    date  = date_published
  ) %>%
  filter(!is.na(vote_share), !is.na(date))

## Pivot elections the same way
elections_long <- elections %>%
  pivot_longer(
    cols      = all_of(PARTY_COLS),
    names_to  = "party_col",
    values_to = "vote_share"
  ) %>%
  mutate(
    party = factor(PARTY_LABELS[party_col], levels = names(PARTY_COLORS)),
    date  = election_date,
    vote_share = vote_share * 100   # results are 0–1, polls are 0–100
  ) %>%
  filter(!is.na(vote_share))


################################################################################
# Plot 1: Raw poll vote shares over time
################################################################################

dir.create("estimation/plt/data_quality", recursive = TRUE, showWarnings = FALSE)

p1 <- ggplot(polls_long, aes(x = date, y = vote_share, color = party)) +
  geom_point(size = 0.3, alpha = 0.25) +
  geom_point(
    data = elections_long,
    aes(x = date, y = vote_share),
    shape = 18, size = 3
  ) +
  geom_vline(
    xintercept = elections$election_date,
    linetype = "dotted", color = "grey50", linewidth = 0.3
  ) +
  scale_color_parties() +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(0, 50, 10),
    limits = c(0, 55)
  ) +
  labs(
    title    = "Raw polling data",
    subtitle = "All polls from 8 institutes. Diamonds = election results.",
    y        = "Vote share"
  ) +
  theme_forecast()

ggsave("estimation/plt/data_quality/raw_vote_shares.png",
       p1, width = 14, height = 7, dpi = 200)

message("Saved: estimation/plt/data_quality/raw_vote_shares.png")
