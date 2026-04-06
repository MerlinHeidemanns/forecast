################################################################################
# Step 7: Posterior Export to JSON
#
# Purpose: Read spline_fit.rds and polls.csv, produce three JSON files
#          for the D3 frontend:
#            1. forecast_timeseries.json  — daily posterior trajectory
#            2. current_forecast.json     — latest snapshot per party
#            3. polls.json               — canonical poll data for scatter overlay
#
# Design:  docs/design/step7_json_export.md
#
# Usage:   source("estimation/export.R")
#          export_all()
#   — or — Rscript estimation/export.R
#
# Outputs: web/public/data/forecast_timeseries.json
#          web/public/data/current_forecast.json
#          web/public/data/polls.json
################################################################################

library(tidyverse)
library(jsonlite)

source("estimation/theme.R")

################################################################################
# Constants
################################################################################

## JSON key mapping: R column name -> JS-safe key
PARTY_KEY_MAP <- c(
  "CDU/CSU"  = "CDU_CSU",
  "SPD"      = "SPD",
  "GRÜNE"    = "GRUENE",
  "FDP"      = "FDP",
  "AfD"      = "AfD",
  "LINKE"    = "LINKE",
  "BSW"      = "BSW",
  "Sonstige" = "Sonstige"
)

## Reverse: canonical CSV column -> JS key
COL_KEY_MAP <- c(
  cdu_csu  = "CDU_CSU",
  spd      = "SPD",
  gruene   = "GRUENE",
  fdp      = "FDP",
  afd      = "AfD",
  linke    = "LINKE",
  bsw      = "BSW",
  sonstige = "Sonstige"
)

## Quantile labels expected from cmdstanr summary
QUANTILE_COLS <- c("5%", "10%", "25%", "50%", "75%", "90%", "95%")

## JSON quantile field names
QUANTILE_KEYS <- c("q05", "q10", "q25", "median", "q75", "q90", "q95")

PRECISION <- 3  # decimal places (0.1 pp)

################################################################################
# 1. Export forecast_timeseries.json
################################################################################

#' Export the full posterior trajectory at daily resolution
#'
#' Reads the pre-summarized trend tibble from the fit data (7 quantiles),
#' broadcasts biweekly periods to daily rows, and writes a hierarchical JSON.
#'
#' @param fit_data List loaded from spline_fit.rds
#' @param out_dir  Output directory (created if missing)
#' @return Path to written file (invisibly)
export_timeseries <- function(fit_data, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  trend         <- fit_data$trend
  parties       <- fit_data$parties
  period_breaks <- fit_data$period_breaks
  elections     <- fit_data$elections

  ## ---- Validate that all 7 quantile columns exist ----
  ## (Re-extraction is handled in export_all before calling this function)
  missing_q <- setdiff(QUANTILE_COLS, names(trend))
  if (length(missing_q) > 0) {
    stop("trend tibble missing quantile columns: ",
         paste(missing_q, collapse = ", "),
         ". Call export_all() or re-run main.R with 7 quantiles.")
  }

  ## ---- Build period -> date range mapping ----
  ## Each period t covers [period_breaks[t], period_breaks[t+1])
  T_max <- max(trend$t)

  if (length(period_breaks) < T_max + 1) {
    stop("period_breaks has ", length(period_breaks), " elements but need ",
         T_max + 1, " (T_max + 1). Check main.R time grid.")
  }

  period_dates <- tibble(
    t           = 1:T_max,
    period_start = period_breaks[1:T_max],
    period_end   = period_breaks[2:(T_max + 1)]
  )

  ## ---- Broadcast to daily resolution ----
  ## Build a daily date -> period lookup, then join quantiles onto it.
  ## Much faster than rowwise seq() + unnest for ~2,900 days × 7 parties.
  all_days <- tibble(date = seq(min(period_dates$period_start),
                                max(period_dates$period_end) - 1, by = "day"))
  all_days$t <- findInterval(all_days$date, period_dates$period_start,
                             rightmost.closed = TRUE)

  trend_slim <- trend %>%
    select(t, party, all_of(QUANTILE_COLS))

  daily <- all_days %>%
    inner_join(trend_slim, by = "t", relationship = "many-to-many") %>%
    select(date, party, all_of(QUANTILE_COLS))

  ## ---- Round to PRECISION decimals ----
  daily <- daily %>%
    mutate(across(all_of(QUANTILE_COLS), ~ round(., PRECISION)))

  ## ---- Pivot to wide-by-party nested structure ----
  ## Target: one row per date, with CDU_CSU: {q05, ..., q95}, SPD: {...}, ...
  daily_nested <- daily %>%
    mutate(party_key = PARTY_KEY_MAP[as.character(party)]) %>%
    pivot_wider(
      id_cols     = date,
      names_from  = party_key,
      values_from = all_of(QUANTILE_COLS),
      names_glue  = "{party_key}__{.value}"
    ) %>%
    arrange(date)

  ## Re-nest into per-party sub-objects
  party_keys <- unname(PARTY_KEY_MAP[parties])
  timeseries_list <- vector("list", nrow(daily_nested))

  for (i in seq_len(nrow(daily_nested))) {
    row <- list(date = format(daily_nested$date[i], "%Y-%m-%d"))
    for (pk in party_keys) {
      cols <- paste0(pk, "__", QUANTILE_COLS)
      vals <- as.numeric(daily_nested[i, cols, drop = TRUE])
      row[[pk]] <- setNames(as.list(vals), QUANTILE_KEYS)
    }
    timeseries_list[[i]] <- row
  }

  ## ---- Build party metadata ----
  parties_meta <- lapply(parties, function(p) {
    list(display = p, color = unname(PARTY_COLORS[p]))
  })
  names(parties_meta) <- PARTY_KEY_MAP[parties]

  ## ---- Build elections array ----
  ## Read results directly from the elections tibble (not e_vote, which is
  ## filtered to model-window elections and epsilon-replaced for Stan).
  ## Map display party name -> canonical CSV column name.
  DISPLAY_TO_COL <- c(
    "CDU/CSU" = "cdu_csu", "SPD" = "spd", "GRÜNE" = "gruene",
    "FDP" = "fdp", "AfD" = "afd", "LINKE" = "linke", "Sonstige" = "sonstige"
  )

  elections_list <- vector("list", nrow(elections))
  for (i in seq_len(nrow(elections))) {
    results <- list()
    for (p in parties) {
      col <- DISPLAY_TO_COL[p]
      val <- if (col %in% names(elections)) elections[[col]][i] else NA_real_
      results[[PARTY_KEY_MAP[p]]] <- if (!is.na(val)) round(val, PRECISION) else NULL
    }
    elections_list[[i]] <- list(
      date    = format(elections$election_date[i], "%Y-%m-%d"),
      year    = elections$election_year[i],
      results = results
    )
  }

  ## ---- Build sampler diagnostics ----
  fit_obj <- fit_data$fit
  diag <- tryCatch({
    diag_df <- fit_obj$diagnostic_summary(quiet = TRUE)
    list(
      n_divergent       = sum(diag_df$num_divergent),
      max_treedepth_hits = sum(diag_df$num_max_treedepth)
    )
  }, error = function(e) {
    list(n_divergent = NA, max_treedepth_hits = NA)
  })

  n_draws <- tryCatch(
    as.integer(fit_obj$metadata()$iter_sampling * fit_obj$num_chains()),
    error = function(e) NA_integer_
  )

  ## ---- Assemble top-level JSON ----
  date_range <- range(daily_nested$date)
  output <- list(
    metadata = list(
      model             = "spline",
      generated_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      n_draws           = n_draws,
      n_days            = nrow(daily_nested),
      date_range        = format(date_range, "%Y-%m-%d"),
      source_period_days = 14L,
      parties           = unname(party_keys),
      bsw_in_sonstige   = TRUE,
      quantiles         = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
      diagnostics       = diag
    ),
    parties    = parties_meta,
    elections  = elections_list,
    timeseries = timeseries_list
  )

  out_path <- file.path(out_dir, "forecast_timeseries.json")
  write_json(output, out_path, auto_unbox = TRUE, digits = PRECISION, pretty = FALSE)
  message(sprintf("  Wrote %s (%d daily rows, %.1f KB)",
                  out_path, nrow(daily_nested),
                  file.size(out_path) / 1024))

  invisible(out_path)
}


################################################################################
# 2. Export current_forecast.json
################################################################################

#' Export the latest-period snapshot for the summary panel
#'
#' @param fit_data   List loaded from spline_fit.rds
#' @param polls_df   Dataframe from data/polls.csv (canonical wide format)
#' @param out_dir    Output directory
#' @return Path to written file (invisibly)
export_current <- function(fit_data, polls_df, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  trend   <- fit_data$trend
  parties <- fit_data$parties

  ## ---- Get the last time period ----
  t_last <- max(trend$t)
  latest <- trend %>%
    filter(t == t_last) %>%
    mutate(across(all_of(QUANTILE_COLS), ~ round(., PRECISION)))

  ## ---- Build party array (sorted by median descending) ----
  party_list <- latest %>%
    arrange(desc(`50%`)) %>%
    rowwise() %>%
    mutate(
      id      = PARTY_KEY_MAP[as.character(party)],
      display = as.character(party),
      color   = unname(PARTY_COLORS[as.character(party)])
    ) %>%
    ungroup()

  parties_arr <- vector("list", nrow(party_list))
  for (i in seq_len(nrow(party_list))) {
    r <- party_list[i, ]
    parties_arr[[i]] <- list(
      id      = r$id,
      display = r$display,
      color   = r$color,
      median  = r$`50%`,
      q05     = r$`5%`,
      q10     = r$`10%`,
      q25     = r$`25%`,
      q75     = r$`75%`,
      q90     = r$`90%`,
      q95     = r$`95%`
    )
  }

  ## ---- Poll metadata ----
  last_30d    <- Sys.Date() - 30
  recent_polls <- polls_df %>%
    filter(date_published >= last_30d)

  last_poll <- polls_df %>%
    filter(date_published == max(date_published, na.rm = TRUE)) %>%
    slice(1)

  meta <- list(
    n_polls_total    = nrow(polls_df),
    n_polls_last_30d = nrow(recent_polls),
    last_poll_date   = format(max(polls_df$date_published, na.rm = TRUE), "%Y-%m-%d"),
    last_poll_pollster = as.character(last_poll$pollster[1])
  )

  ## ---- Assemble ----
  ## Use the last period's start date as the forecast date
  period_breaks <- fit_data$period_breaks
  forecast_date <- period_breaks[t_last]

  output <- list(
    date         = format(forecast_date, "%Y-%m-%d"),
    model        = "spline",
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    parties      = parties_arr,
    meta         = meta
  )

  out_path <- file.path(out_dir, "current_forecast.json")
  write_json(output, out_path, auto_unbox = TRUE, digits = PRECISION, pretty = TRUE)
  message(sprintf("  Wrote %s (%.1f KB)", out_path, file.size(out_path) / 1024))

  invisible(out_path)
}


################################################################################
# 3. Export polls.json
################################################################################

#' Export canonical poll data for the scatter overlay
#'
#' Reads data/polls.csv, converts percentages to proportions, renames columns
#' to JS-safe keys, and includes BSW as a separate column.
#'
#' @param polls_path Path to data/polls.csv
#' @param out_dir    Output directory
#' @return Path to written file (invisibly)
export_polls <- function(polls_path, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  polls <- read_csv(polls_path, show_col_types = FALSE)

  ## ---- Select and rename columns ----
  ## Canonical CSV columns: cdu_csu, spd, gruene, fdp, afd, linke, bsw, sonstige
  party_cols <- intersect(names(COL_KEY_MAP), names(polls))

  out_df <- polls %>%
    select(date = date_published, pollster, sample_size = n,
           all_of(party_cols)) %>%
    ## Convert percentages to proportions and round
    mutate(across(all_of(party_cols), ~ round(. / 100, PRECISION))) %>%
    ## Rename to JS keys: rename(new = old) needs inverted map
    rename(!!!setNames(names(COL_KEY_MAP[party_cols]), COL_KEY_MAP[party_cols])) %>%
    arrange(desc(date))

  ## ---- Build columns list (for schema documentation) ----
  js_party_keys <- unname(COL_KEY_MAP[party_cols])
  columns <- c("date", "pollster", "sample_size", js_party_keys)

  ## ---- Convert to list-of-lists for JSON ----
  ## Format dates as strings
  out_df <- out_df %>%
    mutate(date = format(date, "%Y-%m-%d"))

  ## Replace NA with NULL for JSON (jsonlite handles this)
  polls_list <- lapply(seq_len(nrow(out_df)), function(i) {
    row <- as.list(out_df[i, ])
    ## Convert NAs to NULL for clean JSON
    row <- lapply(row, function(x) if (is.na(x)) NULL else x)
    row
  })

  output <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    n_polls      = nrow(out_df),
    columns      = columns,
    polls        = polls_list
  )

  out_path <- file.path(out_dir, "polls.json")
  write_json(output, out_path, auto_unbox = TRUE, digits = PRECISION,
             null = "null", pretty = FALSE)
  message(sprintf("  Wrote %s (%d polls, %.1f KB)",
                  out_path, nrow(out_df), file.size(out_path) / 1024))

  invisible(out_path)
}


################################################################################
# 4. Orchestrator
################################################################################

#' Export all three JSON files for the frontend
#'
#' @param fit_path   Path to spline_fit.rds (default: "data/spline_fit.rds")
#' @param polls_path Path to polls.csv (default: "data/polls.csv")
#' @param out_dir    Output directory (default: "web/public/data/")
export_all <- function(fit_path   = "data/spline_fit.rds",
                       polls_path = "data/polls.csv",
                       out_dir    = "web/public/data/") {
  message("Step 7: Export posterior to JSON")
  message("================================")

  ## Load fit data
  if (!file.exists(fit_path)) {
    stop("Fit file not found: ", fit_path, ". Run main.R first.")
  }
  fit_data <- readRDS(fit_path)
  message(sprintf("  Loaded %s", fit_path))

  ## Load polls
  if (!file.exists(polls_path)) {
    stop("Polls file not found: ", polls_path, ". Run canonicalize.R first.")
  }
  polls_df <- read_csv(polls_path, show_col_types = FALSE)
  message(sprintf("  Loaded %s (%d polls)", polls_path, nrow(polls_df)))

  ## Ensure trend has all 7 quantiles (backward compat with old 5-quantile fits)
  missing_q <- setdiff(QUANTILE_COLS, names(fit_data$trend))
  if (length(missing_q) > 0) {
    message("  trend tibble missing columns: ", paste(missing_q, collapse = ", "))
    message("  Re-extracting 7 quantiles from fit object...")
    pi_summary <- fit_data$fit$summary(
      "pi", ~quantile(., c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95))
    )
    fit_data$trend <- pi_summary %>%
      mutate(
        t     = as.integer(str_match(variable, "\\[(\\d+),")[, 2]),
        p     = as.integer(str_match(variable, ",(\\d+)\\]")[, 2]),
        party = factor(fit_data$parties[p], levels = names(PARTY_COLORS)),
        date  = fit_data$period_breaks[t]
      ) %>%
      filter(!is.na(date))
    message("  Re-extraction complete.")
  }

  ## Export
  message("[1/3] forecast_timeseries.json...")
  export_timeseries(fit_data, out_dir)

  message("[2/3] current_forecast.json...")
  export_current(fit_data, polls_df, out_dir)

  message("[3/3] polls.json...")
  export_polls(polls_path, out_dir)

  message("================================")
  message("All JSON exports complete.")
}


################################################################################
# Run standalone
################################################################################

if (sys.nframe() == 0) {
  export_all()
}
