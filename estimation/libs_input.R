create_date_indexes <- function(start_date, end_date, layer1_period, layer2_period) {
  # First create the basic date index
  index_date <- data.frame(
    date = seq(start_date, end_date, by = 1)
  ) %>%
    mutate(
      layer1_aggregate = floor_date(date, unit = layer1_period),
      layer2_aggregate = floor_date(date, unit = layer2_period),
      # Create a simple sequence of period numbers
      layer2_aggregate_idx = dense_rank(layer2_aggregate)
    )
  
  # Then create the aggregate index
  index_date_aggregate <- index_date %>%
    distinct(layer1_aggregate, layer2_aggregate, layer2_aggregate_idx) %>%
    arrange(layer1_aggregate) %>%
    mutate(layer1_aggregate_idx = row_number())
  
  # Join the aggregate index back to the main index
  index_date <- index_date %>%
    left_join(index_date_aggregate)
  
  # Return both indexes as a list
  return(list(
    index_date = index_date,
    index_date_aggregate = index_date_aggregate
  ))
}

transform_polling_data <- function(df, start_date, end_date, layer1_period, 
                                   parties) {
  df %>%
    rename(date = publishing_date) %>%
    mutate(
      # Handle survey count - replace zeros with NA, then impute with mean
      survey_count = ifelse(survey_count == 0, NA_real_, survey_count),
      survey_count = ifelse(is.na(survey_count), mean(survey_count, na.rm = TRUE), survey_count),
      # Group dates into periods
      layer1_aggregate = floor_date(date, unit = layer1_period)
    ) %>%
    # Apply filters
    filter(date < end_date, date > start_date, !is.na(vote_share)) %>%
    # Select only needed columns
    select(uuid, date, layer1_aggregate, survey_count, party, pollster, vote_share) %>%
    # Set up party factor levels
    mutate(party = factor(party, levels = parties))
}

# Function to create pollster index
create_pollster_index <- function(df) {
  # Create index dataframe
  index_pollster <- data.frame(
    pollster = sort(unique(df$pollster))
  ) %>%
    mutate(pollster_idx = row_number())
  
  return(index_pollster)
}

create_party_index <- function(party_levels) {
  # Create index dataframe
  index_party <- data.frame(
    party = party_levels
  ) %>%
    mutate(party_idx = as.integer(factor(party, levels = party_levels)))
  
  return(index_party)
}




detect_pollster_rounding <- function(df, index_pollster) {
  # Nested function to detect rounding patterns in a vector of numbers
  detect_rounding <- function(numbers) {
    # Extract decimal parts
    decimals <- numbers - floor(numbers)
    
    # Get unique decimal values, excluding 0
    unique_decimals <- sort(unique(decimals[decimals > 0]))
    
    # If no non-zero decimals, return 1 (whole numbers only)
    if (length(unique_decimals) <= 1) {
      return(ifelse(length(unique_decimals) == 0, 1, unique_decimals[1]))
    }
    
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
  
  # Apply the detection function to each pollster
  df_pollster_rounding <- lapply(index_pollster$pollster, function(i) {
    x <- df %>%
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
  
  return(df_pollster_rounding)
}







transform_input_data <- function(df, date_index_list, df_pollster_rounding, days_cutoff = 120) {
  # Find maximum date in the dataset
  max_date <- max(df$date)
  
  # Transform input data
  input_data <- df %>%
    # Filter out recent data based on cutoff period
    filter(date < (max_date - days_cutoff)) %>%
    # Convert vote shares to counts
    mutate(y = floor(vote_share / 100 * survey_count)) %>%
    # Remove original vote share column
    select(-vote_share) %>% 
    # Create survey index
    arrange(party_combination_idx, date, uuid) %>%
    group_by(party_combination_idx, uuid) %>%
    mutate(survey_idx = cur_group_id()) %>%
    ungroup() %>%
    # Pivot to wide format
    arrange(survey_idx) %>%
    pivot_wider(
      id_cols = c(uuid, survey_idx, date, survey_count, pollster, 
                  party_combination_idx, party_combination),
      names_from = party,
      values_from = y,
      values_fill = -99
    ) %>%
    # Join with date index and pollster rounding information
    left_join(date_index_list$index_date, by = "date") %>%
    left_join(df_pollster_rounding) %>%
    ungroup() %>%
    arrange(party_combination_idx)
  
  return(input_data)
}



calculate_party_presence <- function(input_data, parties) {
  # Initialize observation index matrix with missing value indicators (-99)
  obs_idx <- matrix(-99, nrow = nrow(input_data), ncol = length(parties))
  
  # For each party, collect survey indices where the party is present
  for (i in parties) {
    vec_tmp <- input_data %>%
      select(survey_idx, all_of(i)) %>%
      filter(get(i) > 0) %>%
      pull(survey_idx)
    
    # Populate the matrix with survey indices
    if (length(vec_tmp) > 0) {
      obs_idx[1:length(vec_tmp), match(i, parties)] <- vec_tmp
    }
  }
  
  # Calculate number of presences for each party
  n_parties_presence <- apply(obs_idx, 2, function(x) sum(x != -99))
  
  # Return both outputs as a list
  return(list(
    obs_idx = obs_idx,
    n_parties_presence = n_parties_presence
  ))
}



calculate_pattern_starts_ends <- function(input_data) {
  # Calculate the start and end survey indices for each party combination
  pattern_starts_ends <- input_data %>%
    arrange(survey_idx) %>%
    group_by(party_combination_idx) %>%
    summarize(
      min = min(survey_idx),
      max = max(survey_idx)
    ) %>%
    select(-party_combination_idx)
  
  return(pattern_starts_ends)
}
