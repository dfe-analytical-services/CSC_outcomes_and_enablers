# This function implements the data pipeline for transforming input csv & xlsx files into curated datasets
# The resulting datasets are saved to ./data/ as rds files for reading in on startup

run_data_pipeline <- function(clear_environment = FALSE) {
  # we need to clear everything from the environment first so that when the function completes we only have
  # the curated datasets in the environment
  if (clear_environment == FALSE) stop() else rm(list = ls())

  # Library calls ---------------------------------------------------------------------------------
  shhh <- suppressPackageStartupMessages # It's a library, so shhh!
  shhh(library(dplyr))
  shhh(library(reshape2))
  shhh(library(tidyverse))
  shhh(library(readODS))
  shhh(library(readxl))
  shhh(library(janitor))
  shhh(library(data.table))

  # source supporting functions to prepare data
  source("R/read_data.R")
  source("R/stats_neighbours.R")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare all datasets ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Stats Neighbours ----
  ## Read in the stats_neighbours and generate a long table for all stats neighbour aggregations
  stats_neighbours <- get_statistical_neighbours() # head(statistical_neighbours(), 152)
  stats_neighbours_long <- get_stats_neighbours_long(stats_neighbours)

  ## Read in the workforce data ----
  workforce_data <- suppressWarnings(read_workforce_data(sn_long = stats_neighbours_long))
  location_data <- GET_location() # fact table linking LA to its region
  location_data_workforce <- GET_location_workforce() # fact table linking LA to its region

  ## Read in the workforce characteristics data (Enabler 2) ----
  workforce_eth <- suppressWarnings(read_workforce_eth_data(sn_long = stats_neighbours_long))
  workforce_eth_seniority <- suppressWarnings(read_workforce_eth_seniority_data())
  population_eth <- suppressWarnings(read_ethnic_population_data())
  combined_ethnicity_data <- suppressWarnings(merge_eth_dataframes(sn_long = stats_neighbours_long))

  ## Read in ofsted leadership data (Enabler 3) ----
  spending_data <- suppressWarnings(read_spending_data(sn_long = stats_neighbours_long))
  spending_data_no_cla <- suppressWarnings(read_spending_data2(sn_long = stats_neighbours_long))
  spending_per_capita <- suppressWarnings(read_per_capita_spending(sn_long = stats_neighbours_long))
  ofsted_leadership_data <- suppressWarnings(read_ofsted_leadership_data(sn_long = stats_neighbours_long))
  ofsted_leadership_data_long <- suppressWarnings(pivot_ofsted_data(ofsted_leadership_data))

  ## Read in the CLA data (outcome 1) ----
  cla_rates <- suppressWarnings(read_cla_rate_data(sn_long = stats_neighbours_long))
  cla_placements <- suppressWarnings(read_cla_placement_data(sn_long = stats_neighbours_long))
  combined_cla_data <- suppressWarnings(merge_cla_dataframes(sn_long = stats_neighbours_long))
  combined_cla_31_march_data <- suppressWarnings(merge_cla_31_march_dataframes(sn_long = stats_neighbours_long))

  ## Read in the CIN  data (outcome 1) ----
  cin_rates <- suppressWarnings(read_cin_rate_data(sn_long = stats_neighbours_long))
  cin_referrals <- suppressWarnings(read_cin_referral_data(sn_long = stats_neighbours_long))

  ## Read in the outcomes data (outcome 1) ----
  outcomes_absence <- suppressWarnings(read_outcomes_absence_data(sn_long = stats_neighbours_long))
  outcomes_ks2 <- suppressWarnings(read_outcomes_ks2_data(sn_long = stats_neighbours_long))
  outcomes_ks4 <- suppressWarnings(read_outcomes_ks4_data(sn_long = stats_neighbours_long))

  ## Read in outcome 2 data ----
  ceased_cla_data <- suppressWarnings(read_outcome2(sn_long = stats_neighbours_long))

  ## Read in outcome 3 data ----
  repeat_cpp <- suppressWarnings(read_cpp_in_year_data(sn_long = stats_neighbours_long))
  duration_cpp <- suppressWarnings(read_cpp_by_duration_data(sn_long = stats_neighbours_long))
  assessment_factors <- suppressWarnings(read_assessment_factors(sn_long = stats_neighbours_long))
  hospital_admissions <- suppressWarnings(read_a_and_e_data(sn_long = stats_neighbours_long))

  ## Read in outcome 4 data ----
  placement_data <- suppressWarnings(read_placement_info_data(sn_long = stats_neighbours_long))
  placement_changes_data <- suppressWarnings(read_number_placements_data(sn_long = stats_neighbours_long))

  care_leavers_activity_data <- suppressWarnings(read_care_leavers_activity_data(sn_long = stats_neighbours_long))
  care_leavers_accommodation_data <- suppressWarnings(read_care_leavers_accommodation_suitability(sn_long = stats_neighbours_long))

  wellbeing_sdq_data <- suppressWarnings(read_wellbeing_child_data(sn_long = stats_neighbours_long))

  placement_order_match_data <- suppressWarnings(read_placement_order_match_data())


  ## Summary Data ----
  summary_data <- collect_summary_data_all()

  dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

  for (dataset_name in names(dfs)) {
    saveRDS(object = dfs[[dataset_name]], file = paste0("./data/", print(dataset_name), ".rds"))
  }
}

# x = profvis::profvis({ run_data_pipeline(clear_environment = TRUE)})
