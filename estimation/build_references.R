################################################################################
# Step 3: Election Results & Pollster Registry
#
# Purpose: Build two canonical reference files:
#   1. data/results.csv   — Zweitstimme results, 2005–2025, canonical schema
#   2. data/pollsters.csv — pollster registry with method, active status, coverage
#
# Usage:   source("estimation/build_references.R")
#          build_results()
#          build_pollsters()
#
# Inputs:  estimation/dta/processed/germany_federal_elections_wide.csv
#          data/polls.csv (from canonicalize.R)
# Outputs: data/results.csv
#          data/pollsters.csv
################################################################################

library(tidyverse)

################################################################################
# Configuration
################################################################################

RESULTS_START_YEAR <- 1990   # reunification; matches polling data coverage
OUTPUT_DIR         <- "data"

## Canonical party columns — must match canonicalize.R
PARTY_COLS <- c("cdu_csu", "spd", "gruene", "fdp", "afd", "linke", "bsw", "sonstige")

## Column rename map: canonical name = old column name (for dplyr::rename)
ELECTION_PARTY_MAP <- c(
  cdu_csu        = "CDU/CSU",
  spd            = "SPD",
  gruene         = "GRÜNE",
  fdp            = "FDP",
  linke          = "LINKE",
  afd            = "AfD",
  sonstige_rep   = "REP",
  sonstige_other = "Sonstige"
)

## 2025 Bundestag election (Zweitstimme, Bundeswahlleiter official)
## Source: https://www.bundeswahlleiter.de/bundestagswahlen/2025.html
ELECTION_2025 <- tibble(
  election_year = 2025L,
  election_date = as.Date("2025-02-23"),
  cdu_csu  = 0.288,
  spd      = 0.167,
  gruene   = 0.117,
  fdp      = 0.041,
  afd      = 0.208,
  linke    = 0.087,
  bsw      = 0.047,
  sonstige = NA_real_   # computed as residual below
) %>% mutate(
  sonstige = 1 - rowSums(across(all_of(setdiff(PARTY_COLS, "sonstige"))), na.rm = TRUE)
)


################################################################################
# 1. Build results.csv
################################################################################

build_results <- function() {
  raw <- read_csv("estimation/dta/processed/germany_federal_elections_wide.csv",
                  show_col_types = FALSE)

  ## Rename to canonical party names
  results <- raw %>%
    rename(!!!ELECTION_PARTY_MAP) %>%
    mutate(
      ## Fold REP into sonstige (only relevant pre-2005)
      sonstige = sonstige_rep + sonstige_other,
      ## Parties that didn't exist get NA (not 0)
      afd   = ifelse(election_year < 2013, NA_real_, afd),
      linke = ifelse(election_year < 1990, NA_real_, linke),
      bsw   = NA_real_   # didn't exist before 2025
    ) %>%
    select(election_year, election_date, all_of(PARTY_COLS))

  ## Append 2025
  results <- bind_rows(results, ELECTION_2025)

  ## Filter to target range
  results <- results %>%
    filter(election_year >= RESULTS_START_YEAR) %>%
    arrange(election_date)

  ## Recompute sonstige as residual so rows sum to 1
  results <- results %>%
    mutate(
      known_sum = rowSums(across(all_of(setdiff(PARTY_COLS, "sonstige"))), na.rm = TRUE),
      sonstige  = 1 - known_sum
    ) %>%
    select(-known_sum)

  ## Validate: rows should sum to 1 (tolerance 0.001)
  results <- results %>%
    mutate(row_sum = rowSums(across(all_of(PARTY_COLS)), na.rm = TRUE))

  bad <- results %>% filter(abs(row_sum - 1) > 0.001)
  if (nrow(bad) > 0) {
    warning(sprintf("  %d elections have vote shares not summing to 1:", nrow(bad)))
    print(bad %>% select(election_year, row_sum))
  }

  results <- results %>% select(-row_sum)

  ## Write
  out_path <- file.path(OUTPUT_DIR, "results.csv")
  write_csv(results, out_path)
  message(sprintf("  Wrote %d elections to %s (years %d–%d)",
                  nrow(results), out_path,
                  min(results$election_year), max(results$election_year)))

  ## Cross-check: print summary
  message("  Party coverage by election:")
  results %>%
    mutate(across(all_of(PARTY_COLS), ~ !is.na(.))) %>%
    pivot_longer(all_of(PARTY_COLS), names_to = "party", values_to = "present") %>%
    group_by(party) %>%
    summarize(n_elections = sum(present), .groups = "drop") %>%
    arrange(desc(n_elections)) %>%
    { walk2(.$party, .$n_elections, ~ message(sprintf("    %-10s %d/%d elections",
                                                       .x, .y, nrow(results)))) }

  invisible(results)
}


################################################################################
# 2. Build pollsters.csv
################################################################################

build_pollsters <- function() {
  ## Load canonical polls
  if (!file.exists(file.path(OUTPUT_DIR, "polls.csv"))) {
    stop("data/polls.csv not found. Run canonicalize_polls() first.")
  }
  polls <- read_csv(file.path(OUTPUT_DIR, "polls.csv"), show_col_types = FALSE)

  ## Known pollster metadata — names must match directory names in polls.csv
  ## Sources: wahlrecht.de institute pages, Wikipedia
  pollster_meta <- tribble(
    ~pollster,              ~full_name,                          ~primary_method, ~notes,
    "Allensbach",           "Institut für Demoskopie Allensbach", "phone",        "Oldest German pollster, face-to-face historically",
    "Forsa",                "Forsa – Gesellschaft für Sozialforschung und statistische Analysen", "phone", "Largest weekly sample sizes (~2500)",
    "Forsch\u2019gr.Wahlen", "Forschungsgruppe Wahlen",            "phone",        "Conducts Politbarometer for ZDF",
    "GMS",                  "GMS Dr. Jung GmbH",                  "phone",        "Smaller institute, less frequent polling",
    "INSA",                 "INSA-Consulere GmbH",                "online",       "Online panel, high frequency since ~2014",
    "Infratestdimap",       "Infratest dimap",                    "phone",        "Conducts DeutschlandTrend for ARD",
    "Verian(Emnid)",        "Verian (formerly Kantar/Emnid)",     "phone",        "Weekly polls for Bild am Sonntag",
    "Yougov",               "YouGov Deutschland",                 "online",       "Online panel, MRP methodology"
  )

  ## Compute coverage stats from polls.csv
  pollster_stats <- polls %>%
    group_by(pollster) %>%
    summarize(
      n_polls      = n(),
      first_poll   = min(date_published, na.rm = TRUE),
      last_poll    = max(date_published, na.rm = TRUE),
      median_n     = median(n, na.rm = TRUE),
      pct_n_known  = mean(!is.na(n)) * 100,
      .groups      = "drop"
    ) %>%
    mutate(
      ## Active = published a poll within 2 years (generous to account for scraper lag)
      active = last_poll >= (Sys.Date() - 730)
    )

  ## Join metadata
  registry <- pollster_stats %>%
    left_join(pollster_meta, by = "pollster") %>%
    select(pollster, full_name, primary_method, active,
           n_polls, first_poll, last_poll, median_n, pct_n_known, notes) %>%
    arrange(pollster)

  ## Flag pollsters in polls.csv with no metadata
  missing_meta <- registry %>% filter(is.na(full_name))
  if (nrow(missing_meta) > 0) {
    warning("  Pollsters in polls.csv without metadata entry:")
    walk(missing_meta$pollster, ~ warning(sprintf("    - %s", .x)))
  }

  ## Write
  out_path <- file.path(OUTPUT_DIR, "pollsters.csv")
  write_csv(registry, out_path)
  message(sprintf("  Wrote %d pollsters to %s", nrow(registry), out_path))
  message(sprintf("    Active: %d | Inactive: %d",
                  sum(registry$active), sum(!registry$active)))

  invisible(registry)
}


################################################################################
# 3. Cross-check: every pollster in polls.csv has a registry entry
################################################################################

verify_coverage <- function() {
  results  <- read_csv(file.path(OUTPUT_DIR, "results.csv"), show_col_types = FALSE)
  polls    <- read_csv(file.path(OUTPUT_DIR, "polls.csv"), show_col_types = FALSE)
  registry <- read_csv(file.path(OUTPUT_DIR, "pollsters.csv"), show_col_types = FALSE)

  ## Check 1: every pollster in polls.csv is in the registry
  poll_pollsters <- unique(polls$pollster)
  reg_pollsters  <- registry$pollster
  missing <- setdiff(poll_pollsters, reg_pollsters)
  if (length(missing) > 0) {
    warning("  Pollsters in polls.csv but NOT in pollsters.csv: ",
            paste(missing, collapse = ", "))
  } else {
    message("  OK: all pollsters in polls.csv have registry entries")
  }

  ## Check 2: party columns match between results.csv and polls.csv
  result_parties <- intersect(names(results), PARTY_COLS)
  poll_parties   <- intersect(names(polls), PARTY_COLS)
  if (!setequal(result_parties, poll_parties)) {
    warning("  Party column mismatch:")
    warning("    In results but not polls: ", paste(setdiff(result_parties, poll_parties), collapse = ", "))
    warning("    In polls but not results: ", paste(setdiff(poll_parties, result_parties), collapse = ", "))
  } else {
    message("  OK: party columns match between results.csv and polls.csv")
  }

  ## Check 3: election dates have polling coverage
  message("  Election-to-poll coverage (polls within 30 days before election):")
  for (i in seq_len(nrow(results))) {
    edate <- results$election_date[i]
    n_polls_near <- polls %>%
      filter(date_published >= edate - 30, date_published <= edate) %>%
      nrow()
    message(sprintf("    %s (%d): %d polls in final 30 days",
                    edate, results$election_year[i], n_polls_near))
  }
}


################################################################################
# Run all if sourced directly
################################################################################

if (sys.nframe() == 0) {
  message("Step 3: Build reference data")
  message("============================")
  message("[1/3] Building results.csv...")
  build_results()
  message("[2/3] Building pollsters.csv...")
  build_pollsters()
  message("[3/3] Cross-checking coverage...")
  verify_coverage()
  message("Done.")
}
