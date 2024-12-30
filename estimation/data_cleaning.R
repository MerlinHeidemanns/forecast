################################################################################
# German Federal Election Analysis Script
# Purpose: Process and summarize German federal election data, calculate vote shares,
#          and merge with election dates
# Author: [Your Name]
# Last Updated: [Date]
################################################################################

################################################################################
# 1. Initial Setup
################################################################################

## 1.1 Set Working Directory
setwd("/Users/merlinheidemanns/projects/forecast")

## 1.2 Load Required Packages
library(tidyverse)  # For data manipulation and CSV operations

################################################################################
# 2. Data Import
################################################################################

## 2.1 Read Election Data
election_data <- read_csv("estimation/dta/raw/germany_federal_elections.csv")

################################################################################
# 3. Helper Functions
################################################################################

## 3.1 Percentage Calculator
# Calculates percentage with 2 decimal precision
# Args:
#   x: numerator
#   y: denominator
# Returns: percentage rounded to 2 decimal places
calc_percentage <- function(x, y) {
  x / y
}

################################################################################
# 4. Data Processing
################################################################################

## 4.1 Create Election Summary
election_summary <- election_data %>%
  group_by(election_year) %>%
  # Calculate voting statistics and party votes
  summarise(
    # General voting statistics
    Total_Eligible_Voters = sum(eligible_voters),
    Total_Voters = sum(number_voters),
    Total_Valid_Votes = sum(valid_votes),
    Turnout_Rate = calc_percentage(Total_Voters, Total_Eligible_Voters),
    Valid_Vote_Rate = calc_percentage(Total_Valid_Votes, Total_Voters),
    
    # Party vote counts
    CDU_Votes = sum(cdu, na.rm = TRUE),
    CSU_Votes = sum(csu, na.rm = TRUE),
    SPD_Votes = sum(spd, na.rm = TRUE),
    Greens_Votes = sum(gruene_comb, na.rm = TRUE),  # Combine Green party variations
    FDP_Votes = sum(fdp, na.rm = TRUE),
    Left_Votes = sum(linke_pds_comb, na.rm = TRUE),   # Combine Left party variations
    AfD_Votes = sum(afd, na.rm = TRUE)
  ) %>%
  # Calculate vote percentages
  mutate(
    CDU_Percent = calc_percentage(CDU_Votes, Total_Valid_Votes),
    CSU_Percent = calc_percentage(CSU_Votes, Total_Valid_Votes),
    SPD_Percent = calc_percentage(SPD_Votes, Total_Valid_Votes),
    Greens_Percent = calc_percentage(Greens_Votes, Total_Valid_Votes),
    FDP_Percent = calc_percentage(FDP_Votes, Total_Valid_Votes),
    Left_Percent = calc_percentage(Left_Votes, Total_Valid_Votes),
    AfD_Percent = calc_percentage(AfD_Votes, Total_Valid_Votes)
  ) %>%
  # Create simplified party columns
  mutate(
    `CDU/CSU` = CDU_Percent + CSU_Percent,  # Combined CDU/CSU percentage
    SPD = SPD_Percent,
    `GRÜNE` = Greens_Percent,
    FDP = FDP_Percent,
    LINKE = Left_Percent,
    AfD = AfD_Percent
  ) %>%
  mutate(
    Sonstige = 1 - (`CDU/CSU` + SPD + FDP + LINKE + `GRÜNE` + AfD)
  )

################################################################################
# 5. Election Dates Reference Data
################################################################################

## 5.1 Create Election Dates DataFrame
election_dates <- data.frame(
  election_year = c(1949, 1953, 1957, 1961, 1965, 1969, 1972, 1976, 1980, 
                    1983, 1987, 1990, 1994, 1998, 2002, 2005, 2009, 2013, 
                    2017, 2021),
  election_date = as.Date(c(
    "1949-08-14",  # First federal election
    "1953-09-06",
    "1957-09-15",
    "1961-09-17",
    "1965-09-19",
    "1969-09-28",
    "1972-11-19",
    "1976-10-03",
    "1980-10-05",
    "1983-03-06",
    "1987-01-25",
    "1990-12-02",  # First election after reunification
    "1994-10-16",
    "1998-09-27",
    "2002-09-22",
    "2005-09-18",
    "2009-09-27",
    "2013-09-22",
    "2017-09-24",
    "2021-09-26"
  ))
)

################################################################################
# 6. Export Processed Data
################################################################################

## 6.1 Merge and Export Final Dataset
election_summary %>%
  left_join(election_dates) %>%  # Merge with election dates
  write_csv("estimation/dta/processed/germany_federal_elections.csv")  # Export to CSV

election_summary %>%
  left_join(election_dates) %>%
  select(election_year, election_date, `CDU/CSU`, SPD, `GRÜNE`, FDP, LINKE, AfD, Sonstige) %>%
  pivot_longer(c(-election_year, -election_date),
               names_to = "party",
               values_to = "vote_share") %>%
  write_csv("estimation/dta/processed/germany_federal_elections_long.csv")  # Export to CSV

election_summary %>%
  left_join(election_dates) %>%
  select(election_year, election_date, `CDU/CSU`, SPD, `GRÜNE`, FDP, LINKE, AfD, Sonstige) %>%
  pivot_longer(c(-election_year, -election_date),
               names_to = "party",
               values_to = "vote_share") %>%
  pivot_wider(id_cols = c(election_year, election_date),
              names_from = party,
              values_from = vote_share) %>%
  write_csv("estimation/dta/processed/germany_federal_elections_wide.csv")  # Export to CSV

