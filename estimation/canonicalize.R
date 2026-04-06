################################################################################
# Canonicalize Polling Data
#
# Purpose: Merge all per-pollster CSVs into a single validated polls.csv.
#          Rejected rows are written to rejected_polls.csv with reasons.
#
# Usage:   source("estimation/canonicalize.R")
#          Or: Rscript estimation/canonicalize.R
#
# Inputs:  web/public/polling_data/{pollster}/{current,historical}.csv (long format)
# Outputs: data/polls.csv           — validated, canonical, wide-format polls
#          data/rejected_polls.csv   — rows that failed validation with reasons
################################################################################

library(tidyverse)
library(digest)

################################################################################
# Configuration
################################################################################

POLL_DATA_DIR <- "web/public/polling_data"
OUTPUT_DIR    <- "data"

## Canonical party columns in output (wide format)
PARTY_COLS <- c("cdu_csu", "spd", "gruene", "fdp", "afd", "linke", "bsw", "sonstige")

## Party name mapping: raw label -> canonical column name
## Parties not in this map are folded into sonstige
PARTY_MAP <- c(
  "CDU/CSU"              = "cdu_csu",
  "SPD"                  = "spd",
  "GRÜNE"                = "gruene",
  "FDP"                  = "fdp",
  "AfD"                  = "afd",
  "LINKE"                = "linke",
  "LinkePDS"             = "linke",
  "PDS"                  = "linke",
  "BSW"                  = "bsw",
  "Sonstige"             = "sonstige",
  # Minor parties -> sonstige
  "FW"                   = "sonstige",
  "PIRATEN"              = "sonstige",
  "REP"                  = "sonstige",
  "REP/DVU"              = "sonstige",
  "Rechte"               = "sonstige"
)

## Method code mapping from Befragte prefix
METHOD_MAP <- c(
  "O"   = "online",
  "T"   = "phone",
  "TOM" = "mixed"
)

## Validation thresholds
VOTE_SHARE_SUM_MIN <- 95
VOTE_SHARE_SUM_MAX <- 105
SAMPLE_SIZE_MIN    <- 500
SAMPLE_SIZE_MAX    <- 50000


################################################################################
# 1. Load all per-pollster CSVs
################################################################################

load_all_pollster_data <- function(base_dir = POLL_DATA_DIR) {
  pollster_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)

  all_data <- map_dfr(pollster_dirs, function(dir) {
    pollster_name <- basename(dir)
    csv_files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)

    map_dfr(csv_files, function(f) {
      read_csv(f, show_col_types = FALSE) %>%
        mutate(
          Befragte = as.character(Befragte),
          mode     = as.character(mode),
          pollster  = pollster_name,
          source_file = basename(f)
        )
    })
  })

  return(all_data)
}


################################################################################
# 2. Harmonize party names and clean fields
################################################################################

harmonize_parties <- function(df) {
  df %>%
    # Drop non-vote-share rows (e.g. Nichtwähler/Unentschl.)
    filter(!party %in% c("Nichtwähler/Unentschl", "Nichtwähler/Unentschl.")) %>%
    # Map party names to canonical columns
    mutate(
      party_canonical = case_when(
        party %in% names(PARTY_MAP) ~ PARTY_MAP[party],
        TRUE                        ~ "sonstige"
      )
    ) %>%
    # Aggregate: if multiple raw parties map to the same canonical name in one poll,
    # sum their vote shares (e.g. FW + PIRATEN + Sonstige -> sonstige)
    group_by(uuid, publishing_date, Befragte, Zeitraum, mode, survey_count,
             sampling_start_day, sampling_end_day, pollster, source_file,
             party_canonical) %>%
    summarize(
      vote_share = sum(vote_share, na.rm = TRUE),
      .groups = "drop"
    )
}


################################################################################
# 3. Parse method codes from Befragte
################################################################################

parse_method <- function(befragte_str) {
  code <- str_extract(befragte_str, "^[A-Za-z]+")
  ifelse(is.na(code), "unknown", ifelse(code %in% names(METHOD_MAP), METHOD_MAP[code], "unknown"))
}


################################################################################
# 4. Pivot to wide format
################################################################################

pivot_to_wide <- function(df) {
  df_wide <- df %>%
    # Pivot party_canonical -> columns
    pivot_wider(
      id_cols = c(uuid, publishing_date, Befragte, Zeitraum,
                  sampling_start_day, sampling_end_day,
                  survey_count, pollster, source_file),
      names_from  = party_canonical,
      values_from = vote_share,
      values_fill = NA_real_
    )

  # Ensure all party columns exist
  for (col in PARTY_COLS) {
    if (!col %in% names(df_wide)) {
      df_wide[[col]] <- NA_real_
    }
  }

  # Parse method from Befragte and preserve raw fields for downstream R code
  df_wide <- df_wide %>%
    mutate(
      method       = parse_method(Befragte),
      n            = as.integer(survey_count),
      befragte_raw = Befragte,
      zeitraum_raw = Zeitraum
    ) %>%
    rename(
      date_published = publishing_date,
      date_start     = sampling_start_day,
      date_end       = sampling_end_day
    ) %>%
    # Compute raw vote share sum (before any sonstige recomputation)
    mutate(
      raw_vote_share_sum = rowSums(across(all_of(PARTY_COLS)), na.rm = TRUE)
    )

  return(df_wide)
}


################################################################################
# 5. Compute deterministic SHA-256 hash
################################################################################

compute_poll_hash <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      # Build the hash input: pollster|date_published|date_start|date_end|n|party=value pairs
      .hash_parties = paste(
        sort(
          map_chr(PARTY_COLS, function(col) {
            val <- get(col)
            paste0(col, "=", ifelse(is.na(val), "none", as.character(val)))
          })
        ),
        collapse = "|"
      ),
      .hash_input = paste(
        pollster,
        as.character(date_published),
        as.character(date_start),
        as.character(date_end),
        as.character(n),
        .hash_parties,
        sep = "|"
      ),
      poll_id = digest(.hash_input, algo = "sha256", serialize = FALSE)
    ) %>%
    ungroup() %>%
    select(-.hash_parties, -.hash_input)
}


################################################################################
# 6. Deduplicate by hash
################################################################################

deduplicate <- function(df) {
  n_before <- nrow(df)
  df_deduped <- df %>%
    distinct(poll_id, .keep_all = TRUE)
  n_after <- nrow(df_deduped)

  message(sprintf("  Deduplication: %d -> %d rows (%d duplicates removed)",
                  n_before, n_after, n_before - n_after))
  return(df_deduped)
}


################################################################################
# 7. Validation
################################################################################

validate_polls <- function(df) {
  # We'll collect rejection reasons per row. A row can fail multiple checks;
  # we record all of them separated by "; ".

  reasons <- character(nrow(df))

  # V1: Vote share sum in [95, 105]
  bad_sum <- which(
    df$raw_vote_share_sum < VOTE_SHARE_SUM_MIN |
    df$raw_vote_share_sum > VOTE_SHARE_SUM_MAX
  )
  reasons[bad_sum] <- paste0(
    reasons[bad_sum],
    ifelse(reasons[bad_sum] == "", "", "; "),
    sprintf("V1: vote_share_sum=%.1f outside [%d,%d]",
            df$raw_vote_share_sum[bad_sum],
            VOTE_SHARE_SUM_MIN, VOTE_SHARE_SUM_MAX)
  )

  # V2: Sample size in [500, 50000] (NA is allowed for historical polls)
  bad_n <- which(
    !is.na(df$n) & (df$n < SAMPLE_SIZE_MIN | df$n > SAMPLE_SIZE_MAX)
  )
  reasons[bad_n] <- paste0(
    reasons[bad_n],
    ifelse(reasons[bad_n] == "", "", "; "),
    sprintf("V2: n=%s outside [%d,%d]",
            as.character(df$n[bad_n]),
            SAMPLE_SIZE_MIN, SAMPLE_SIZE_MAX)
  )

  # V3: Date validity
  today <- Sys.Date()
  bad_date <- which(
    is.na(df$date_published) |
    df$date_published > today |
    (!is.na(df$date_start) & !is.na(df$date_end) & df$date_start > df$date_end) |
    (!is.na(df$date_end) & !is.na(df$date_published) & df$date_end > df$date_published + 7)
  )
  reasons[bad_date] <- paste0(
    reasons[bad_date],
    ifelse(reasons[bad_date] == "", "", "; "),
    "V3: date validity check failed"
  )

  # V5: Null check on required fields (n and party columns may be NA for historical polls)
  bad_null <- which(
    is.na(df$pollster) |
    is.na(df$date_published)
  )
  reasons[bad_null] <- paste0(
    reasons[bad_null],
    ifelse(reasons[bad_null] == "", "", "; "),
    "V5: null in required field (pollster/date_published)"
  )

  # Split into valid and rejected
  rejected_mask <- reasons != ""

  df_valid    <- df[!rejected_mask, ]
  df_rejected <- df[rejected_mask, ] %>%
    mutate(rejection_reason = reasons[rejected_mask])

  message(sprintf("  Validation: %d valid, %d rejected", nrow(df_valid), nrow(df_rejected)))

  # Log rejection breakdown
  if (nrow(df_rejected) > 0) {
    # Count how many times each check fired
    for (v in c("V1", "V2", "V3", "V5")) {
      count <- sum(str_detect(df_rejected$rejection_reason, v))
      if (count > 0) {
        message(sprintf("    %s: %d rows", v, count))
      }
    }
  }

  return(list(valid = df_valid, rejected = df_rejected))
}


################################################################################
# 8. Select and order output columns
################################################################################

finalize_output <- function(df) {
  df %>%
    select(
      poll_id,
      pollster,
      date_published,
      date_start,
      date_end,
      n,
      method,
      befragte_raw,
      zeitraum_raw,
      all_of(PARTY_COLS),
      raw_vote_share_sum
    ) %>%
    arrange(pollster, date_published)
}


################################################################################
# Main
################################################################################

canonicalize_polls <- function(poll_data_dir = POLL_DATA_DIR,
                               output_dir = OUTPUT_DIR) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  message("Step 1.2: Canonicalize polling data")
  message("===================================")

  # Step 1: Load
  message("\n[1/7] Loading all pollster CSVs...")
  df_raw <- load_all_pollster_data(poll_data_dir)
  message(sprintf("  Loaded %d rows from %d pollsters",
                  nrow(df_raw), n_distinct(df_raw$pollster)))

  # Step 2: Harmonize party names
  message("\n[2/7] Harmonizing party names...")
  df_harmonized <- harmonize_parties(df_raw)
  message(sprintf("  %d rows after harmonization (dropped non-vote rows)",
                  nrow(df_harmonized)))

  # Step 3: Pivot to wide
  message("\n[3/7] Pivoting to wide format...")
  df_wide <- pivot_to_wide(df_harmonized)
  message(sprintf("  %d polls in wide format", nrow(df_wide)))

  # Step 4: Compute hashes
  message("\n[4/7] Computing SHA-256 hashes...")
  df_hashed <- compute_poll_hash(df_wide)

  # Step 5: Deduplicate
  message("\n[5/7] Deduplicating...")
  df_deduped <- deduplicate(df_hashed)

  # Step 6: Validate
  message("\n[6/7] Validating...")
  validation_result <- validate_polls(df_deduped)

  # Step 7: Write outputs
  message("\n[7/7] Writing outputs...")

  df_valid <- finalize_output(validation_result$valid)
  write_csv(df_valid, file.path(output_dir, "polls.csv"))
  message(sprintf("  Wrote %d rows to %s/polls.csv",
                  nrow(df_valid), output_dir))

  if (nrow(validation_result$rejected) > 0) {
    df_rejected <- validation_result$rejected %>%
      select(
        poll_id, pollster, date_published, date_start, date_end, n, method,
        befragte_raw, zeitraum_raw,
        all_of(PARTY_COLS), raw_vote_share_sum, rejection_reason
      ) %>%
      arrange(pollster, date_published)
    write_csv(df_rejected, file.path(output_dir, "rejected_polls.csv"))
    message(sprintf("  Wrote %d rows to %s/rejected_polls.csv",
                    nrow(df_rejected), output_dir))
  } else {
    message("  No rejected rows.")
  }

  message("\nDone.")

  invisible(list(
    polls    = df_valid,
    rejected = validation_result$rejected
  ))
}


################################################################################
# Load canonical polls.csv for downstream use
#
# Drop-in replacement for the per-pollster loading in main.R.
# Returns long-format data matching what transform_polling_data() expects.
################################################################################

load_canonical_polls <- function(polls_path = file.path(OUTPUT_DIR, "polls.csv"),
                                 parties = c("CDU/CSU", "SPD", "FDP", "GRÜNE",
                                             "LINKE", "Sonstige", "AfD", "BSW")) {

  # Reverse mapping: canonical column name -> display party name
  col_to_party <- c(
    "cdu_csu"  = "CDU/CSU",
    "spd"      = "SPD",
    "gruene"   = "GRÜNE",
    "fdp"      = "FDP",
    "afd"      = "AfD",
    "linke"    = "LINKE",
    "bsw"      = "BSW",
    "sonstige" = "Sonstige"
  )

  df <- read_csv(polls_path, show_col_types = FALSE)

  # Pivot back to long format matching what main.R expects
  df_long <- df %>%
    pivot_longer(
      cols      = all_of(PARTY_COLS),
      names_to  = "party_col",
      values_to = "vote_share"
    ) %>%
    mutate(
      party              = col_to_party[party_col],
      publishing_date    = date_published,
      sampling_start_day = date_start,
      sampling_end_day   = date_end,
      survey_count       = n,
      uuid               = poll_id,
      # Use preserved raw fields from the original scrape
      Befragte           = befragte_raw,
      Zeitraum           = zeitraum_raw,
      mode               = method
    ) %>%
    filter(party %in% parties) %>%
    select(
      uuid, publishing_date, Befragte, Zeitraum, mode, survey_count,
      sampling_start_day, sampling_end_day, pollster, party, vote_share
    )

  return(df_long)
}


# Run if called as a script
if (sys.nframe() == 0) {
  canonicalize_polls()
}
