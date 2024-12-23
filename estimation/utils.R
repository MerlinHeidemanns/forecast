#' Detect Rounding Interval in Numeric Sequence
#' 
#' Analyzes a sequence of numbers to determine the rounding interval
#' used in their decimal places by examining patterns in decimal parts.
#' 
#' @param numbers A numeric vector containing the sequence to analyze
#' @return The detected rounding interval (e.g., 0.1, 0.2, 0.5), or NA if no consistent pattern found
#' 
#' @note Requires at least two different decimal values to detect the interval
detect_rounding <- function(numbers) {
  # Extract decimal parts
  decimals <- numbers - floor(numbers)
  
  # Get unique decimal values, excluding 0
  unique_decimals <- sort(unique(decimals[decimals > 0]))
  
  # Find the minimum difference between consecutive decimals
  min_diff <- min(diff(unique_decimals))
  
  # Check if all decimals are multiples of the minimum difference
  # Using round() with 3 decimal places to handle floating-point precision
  is_consistent <- all(round(round(decimals/min_diff) - decimals/min_diff, 3) == 0)
  
  if (is_consistent) {
    return(min_diff)
  } else {
    return(NA)  # If no consistent rounding interval is found
  }
}
################################################################################
# Data Loading Functions
################################################################################

#' Load and standardize polling data from an institute
#' @param institute_name Name of the polling institute
#' @param base_dir Base directory for polling data (defaults to POLL_DATA_DIR)
#' @return Dataframe of standardized polling data with:
#'   - Consistent party names (e.g., various left party names mapped to "LINKE")
#'   - Respondent counts as character
#'   - Added pollster column
load_institute_data <- function(institute_name, base_dir = POLL_DATA_DIR) {
  ## Party Name Mappings
  LEFT_PARTY_NAMES <- c(
    "LinkePDS" = "LINKE",
    "LINKE"    = "LINKE",
    "PDS"      = "LINKE"
  )
  # Construct full path to institute directory
  institute_path <- file.path(base_dir, institute_name)
  
  # Get all files for this institute
  data_files <- list.files(institute_path, full.names = TRUE)
  
  # Read and combine all files
  bind_rows(
    map(data_files, ~read_csv(.x) %>%
          mutate(Befragte = as.character(Befragte)))
  ) %>%
    mutate(
      pollster = institute_name,
      # Standardize party names
      party = if_else(party %in% names(LEFT_PARTY_NAMES), "LINKE", party)
    )
}




################################################################################
# Data Processing Functions
################################################################################

#' Process and Save Daily Vote Share Trends
#'
#' Takes a fitted model object and processes the trend shares data, 
#' saving daily vote shares to disk.
#'
#' @param fit Fitted model object containing posterior draws
#' @param index_date Data frame mapping indices to dates
#' @param save_path Directory path for saving data (default: "estimation/data/trends/")
#' @param filename Name of output file (default: "daily_vote_shares.csv")
#' @return Processed trend data frame (invisibly)
save_daily_vote_shares <- function(fit, 
                                   index_date, 
                                   save_path = "estimation/data/trends/",
                                   filename = "daily_vote_shares.csv") {
  
  # Create directory if it doesn't exist
  dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  
  # Process trend data
  trend_data <- fit$summary("trend_shares", ~quantile(., c(0.025, 0.25, 0.5, 0.75, 0.975))) %>%
    mutate(
      layer1_aggregate_idx = as.integer(str_match(variable, "(\\d+),")[, 2]),
      ix_party = as.integer(str_match(variable, ",(\\d+)")[, 2]),
      party = factor(
        c("CDU/CSU", "FDP", "GRÜNE", "LINKE", "Sonstige", "SPD")[ix_party],
        levels = c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "Sonstige")
      )
    ) %>%
    right_join(index_date %>% 
                 select(date, layer1_aggregate_idx))
  
  # Save to disk
  write_csv(
    trend_data,
    file.path(save_path, filename)
  )
  
  # Return processed data invisibly
  invisible(trend_data)
}
