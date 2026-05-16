### Area Median Income Calculator

# Load Packages
library(dplyr)
library(tidycensus)
library(tigris)
library(sf)
library(tidyr)
library(stringr)
library(mapview)

# Set working directory to folder location
setwd("/Users/S152973/Downloads/VMT_Rates")

# Function to interpolate median hh income from income brackets and total HHs
interpolate_median <- function(bracket_df, total_hh) {
  df <- bracket_df %>%
    arrange(lower) %>%
    mutate(
      cum_count = cumsum(estimate),
      cum_share = cum_count / total_hh
    )
  
  half <- total_hh / 2
  
  median_bracket <- df %>%
    filter(cum_count >= half) %>%
    slice(1)
  
  if (nrow(median_bracket) == 0) stop("Could not locate median bracket.")
  
  bracket_idx <- which(df$variable == median_bracket$variable)
  count_before <- if (bracket_idx == 1) 0 else df$cum_count[bracket_idx - 1]
  needed <- half - count_before
  bracket_width <- median_bracket$upper - median_bracket$lower
  
  median_est <- median_bracket$lower +
    (needed / median_bracket$estimate) * bracket_width
  
  list(
    median = median_est,
    median_bracket = median_bracket,
    distribution = df
  )
}


# Function to return estimated median household income for county combinations
estimate_combined_median_hhi <- function(county_names,
                                         year   = 2024,
                                         survey = "acs5") {
  
  STATE_FIPS <- "06"
  
  # Define income brackets
  brackets <- tribble(
    ~variable,      ~lower,   ~upper,
    "B19001_002",       0,    10000,
    "B19001_003",   10000,    15000,
    "B19001_004",   15000,    20000,
    "B19001_005",   20000,    25000,
    "B19001_006",   25000,    30000,
    "B19001_007",   30000,    35000,
    "B19001_008",   35000,    40000,
    "B19001_009",   40000,    45000,
    "B19001_010",   45000,    50000,
    "B19001_011",   50000,    60000,
    "B19001_012",   60000,    75000,
    "B19001_013",   75000,   100000,
    "B19001_014",  100000,   125000,
    "B19001_015",  125000,   150000,
    "B19001_016",  150000,   200000,
    "B19001_017",  200000,   250000 
  )
  
  # Resolve county names -> FIPS codes via the fips_codes reference table
  ca_counties <- tidycensus::fips_codes %>%
    filter(state_code == STATE_FIPS) %>%
    mutate(
      full_name = paste0(county, ", ", state_name),
      short_name = str_remove(county, " County$")
    )
  
  matched <- lapply(county_names, function(nm) {
    hit <- ca_counties %>%
      filter(
        str_detect(full_name, regex(nm, ignore_case = TRUE)) |
          str_detect(short_name, regex(nm, ignore_case = TRUE)) |
          str_detect(county, regex(nm, ignore_case = TRUE))
      )
    if (nrow(hit) == 0) stop(sprintf("Could not match county name: '%s'", nm))
    if (nrow(hit) > 1)  stop(sprintf("Ambiguous county name '%s' matched: %s",
                                     nm, paste(hit$county, collapse = ", ")))
    hit
  }) %>%
    bind_rows()
  
  county_fips <- matched$county_code
  area_label <- paste(matched$short_name, collapse = " / ")
  
  # Get B19001 bracket data
  raw <- get_acs(
    geography = "county",
    variables = brackets$variable,
    state = STATE_FIPS,
    county = county_fips,
    year = year,
    survey = survey,
    output = "tidy"
  )
  
  # Aggregate bracket counts across counties
  combined_brackets <- raw %>%
    filter(variable != "B19001_001") %>%
    group_by(variable) %>%
    summarise(
      estimate = sum(estimate, na.rm = TRUE),
      moe = sqrt(sum(moe^2, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    left_join(brackets, by = "variable") %>%
    arrange(lower)
  
  total_households <- sum(combined_brackets$estimate)
  
  # Interpolate the median
  result <- interpolate_median(combined_brackets, total_households)

  # Approximate MOE on the median
  low_result <- interpolate_median(
    combined_brackets %>% mutate(estimate = pmax(estimate - moe / 1.645, 0)),
    total_households
  )
  high_result <- interpolate_median(
    combined_brackets %>% mutate(estimate = estimate + moe / 1.645),
    total_households
  )
  
  approx_moe <- 1.645 * (high_result$median - low_result$median) / 2
  
  # Get results
  results <- data.frame(median = result$median)
  
  return(results)
}


# Read county crosswalk
county_crosswalk <- read.csv("Data/Geography/county_crosswalk.csv") %>%
  select(County, Region)

# Get a unique list of regions
regions <- unique(county_crosswalk$Region)

out = NULL

for (i in regions) {
  
  county_list <- county_crosswalk %>%
    filter(Region == i)
  
  county_vector <- county_list$County
  
  county_results <- estimate_combined_median_hhi(
    county_names = county_vector,
    year = 2024,
    survey = "acs5"
  )
  
  results_df <- data_frame(region = i,
                           median_hh_income = county_results$median) %>%
    mutate(li_threshold = median_hh_income * .8)
  
  out <- rbind(out, results_df)
  
  print(i)
}

write.csv(out, "estimated_median_hh_income.csv")
