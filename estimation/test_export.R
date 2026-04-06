################################################################################
# Test: Step 7 JSON Export
#
# Purpose: Validate that export.R produces correct JSON files matching
#          the design doc schema (docs/design/step7_json_export.md).
#
# Usage:   Rscript estimation/test_export.R
#          (Run from project root after main.R has been run)
################################################################################

library(tidyverse)
library(jsonlite)

source("estimation/export.R")

################################################################################
# Config
################################################################################

OUT_DIR    <- "web/public/data/"
FIT_PATH   <- "data/spline_fit.rds"
POLLS_PATH <- "data/polls.csv"

pass <- 0
fail <- 0

check <- function(desc, condition) {
  if (isTRUE(condition)) {
    message(sprintf("  PASS: %s", desc))
    pass <<- pass + 1
  } else {
    message(sprintf("  FAIL: %s", desc))
    fail <<- fail + 1
  }
}

################################################################################
# 0. Run export
################################################################################

message("Running export_all()...")
export_all(FIT_PATH, POLLS_PATH, OUT_DIR)
message("")

################################################################################
# 1. Validate forecast_timeseries.json
################################################################################

message("=== forecast_timeseries.json ===")
ts <- fromJSON(file.path(OUT_DIR, "forecast_timeseries.json"), simplifyVector = FALSE)

## Metadata checks
check("metadata exists", !is.null(ts$metadata))
check("model is 'spline'", ts$metadata$model == "spline")
check("generated_at is non-empty", nchar(ts$metadata$generated_at) > 0)
check("n_days > 0", ts$metadata$n_days > 0)
check("source_period_days is 14", ts$metadata$source_period_days == 14)
check("bsw_in_sonstige is TRUE", ts$metadata$bsw_in_sonstige == TRUE)
check("7 quantiles listed", length(ts$metadata$quantiles) == 7)
check("7 parties listed", length(ts$metadata$parties) == 7)

## Party metadata
expected_parties <- c("CDU_CSU", "SPD", "GRUENE", "FDP", "AfD", "LINKE", "Sonstige")
check("all party keys in parties block",
      all(expected_parties %in% names(ts$parties)))

for (pk in expected_parties) {
  p <- ts$parties[[pk]]
  check(sprintf("party %s has display name", pk), !is.null(p$display))
  check(sprintf("party %s has color", pk), grepl("^#", p$color))
}

## Timeseries structure
check("timeseries is non-empty", length(ts$timeseries) > 0)
check("timeseries length matches n_days", length(ts$timeseries) == ts$metadata$n_days)

## Sample row check
row1 <- ts$timeseries[[1]]
check("first row has date", !is.null(row1$date))
check("first row date is valid", !is.na(as.Date(row1$date)))

for (pk in expected_parties) {
  party_data <- row1[[pk]]
  check(sprintf("row1 %s has 7 quantile fields", pk), length(party_data) == 7)
  check(sprintf("row1 %s quantile keys correct", pk),
        all(c("q05", "q10", "q25", "median", "q75", "q90", "q95") %in% names(party_data)))
  ## Monotonicity: q05 <= q10 <= ... <= q95
  vals <- c(party_data$q05, party_data$q10, party_data$q25,
            party_data$median, party_data$q75, party_data$q90, party_data$q95)
  check(sprintf("row1 %s quantiles monotonic", pk), all(diff(vals) >= 0))
  ## Precision: at most 3 decimal places
  check(sprintf("row1 %s values ≤ 3 decimals", pk),
        all(round(vals, 3) == vals))
}

## Date continuity: check for no gaps > 1 day
dates <- as.Date(sapply(ts$timeseries, `[[`, "date"))
gaps <- diff(dates)
check("no date gaps > 1 day", all(gaps == 1))
check("dates are sorted ascending", all(gaps >= 0))

## Elections
check("elections array exists", length(ts$elections) > 0)
e1 <- ts$elections[[1]]
check("election has date", !is.null(e1$date))
check("election has results", length(e1$results) > 0)

message("")

################################################################################
# 2. Validate current_forecast.json
################################################################################

message("=== current_forecast.json ===")
cur <- fromJSON(file.path(OUT_DIR, "current_forecast.json"), simplifyVector = FALSE)

check("date exists", !is.null(cur$date))
check("model is 'spline'", cur$model == "spline")
check("generated_at exists", nchar(cur$generated_at) > 0)
check("7 parties", length(cur$parties) == 7)

## Check party objects
for (i in seq_along(cur$parties)) {
  p <- cur$parties[[i]]
  check(sprintf("party %d has id", i), !is.null(p$id))
  check(sprintf("party %d has display", i), !is.null(p$display))
  check(sprintf("party %d has color", i), grepl("^#", p$color))
  check(sprintf("party %d has all quantile fields", i),
        all(c("median", "q05", "q10", "q25", "q75", "q90", "q95") %in% names(p)))
  ## Median should be between 0 and 1
  check(sprintf("party %d median in [0,1]", i), p$median >= 0 && p$median <= 1)
}

## Parties should be sorted by median descending
medians <- sapply(cur$parties, `[[`, "median")
check("parties sorted by median desc", all(diff(medians) <= 0))

## Meta
check("meta has n_polls_total", !is.null(cur$meta$n_polls_total))
check("meta has n_polls_last_30d", !is.null(cur$meta$n_polls_last_30d))
check("meta has last_poll_date", !is.null(cur$meta$last_poll_date))
check("meta has last_poll_pollster", !is.null(cur$meta$last_poll_pollster))

message("")

################################################################################
# 3. Validate polls.json
################################################################################

message("=== polls.json ===")
pj <- fromJSON(file.path(OUT_DIR, "polls.json"), simplifyVector = FALSE)

check("generated_at exists", nchar(pj$generated_at) > 0)
check("n_polls > 0", pj$n_polls > 0)
check("columns listed", length(pj$columns) > 0)
check("BSW in columns", "BSW" %in% pj$columns)
check("polls array length matches n_polls", length(pj$polls) == pj$n_polls)

## Sample poll
p1 <- pj$polls[[1]]
check("poll has date", !is.null(p1$date))
check("poll has pollster", !is.null(p1$pollster))
check("poll has CDU_CSU", !is.null(p1$CDU_CSU))

## Vote shares are proportions (0-1), not percentages
check("CDU_CSU is proportion (< 1)", p1$CDU_CSU < 1)

## Precision check
check("CDU_CSU ≤ 3 decimals", round(p1$CDU_CSU, 3) == p1$CDU_CSU)

message("")

################################################################################
# Summary
################################################################################

message(sprintf("==========================="))
message(sprintf("TOTAL: %d passed, %d failed", pass, fail))
if (fail > 0) {
  message("SOME TESTS FAILED")
  quit(status = 1)
} else {
  message("ALL TESTS PASSED")
}
