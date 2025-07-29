# **** Functions required to run the data pipeline ****

# First it is necessary to clear the environment and source all of the functions in this file

if (TRUE == FALSE) {
  # clear the environment and source the necessary pipeline functions
  rm(list = ls())
  source("./R/data_pipeline.R")

  # now run the first step of the pipeline to generate the new datasets and comparisons with current app data
  pipeline_run <- run_data_pipeline_step_1(clear_environment = TRUE)


  # now compare the current and old with the diagnostics provided
  print(pipeline_run$pipeline_comparison)


  # if the diagnostics are ok then record the necessary parameters in order to run the second step of the pipeline
  pipeline_update_template <- list(
    username <- Sys.getenv("USERNAME"),
    run_datetime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    reason_for_pipeline_run <- ""
  )

  # finally run the update to bring the new data through the pipeline into the app (i.e. copy to RDS files in ./data/ folder)
  # run_data_pipeline_step_2(pipeline_run, pipeline_update_template)
}



# This function implements the first data pipeline step of transforming input csv & xlsx files into curated datasets
# The resulting datasets are returned in a list at this point along with the parameters used to generate them.
# Additional validation takes place to provide feedback on what has been generated and what differs to the rds datasets in the ./data/ folder of the repo
# This is the first stage of the pipeline, to build the data in a temporary fashion with nothing in the main application being changed yet, that follows in step 2
run_data_pipeline_step_1 <- function(clear_environment = FALSE) {
  datasets_new <- pipeline_generate_datasets(clear_environment = TRUE)
  datasets_rds <- pipeline_read_rds()


  meta_rds <- pipeline_dataset_metadata(datasets_rds)
  meta_new <- pipeline_dataset_metadata(datasets_new)

  pipeline_comparison <- pipeline_compare_datasets(meta_rds, meta_new)

  return(list(datasets_new = datasets_new, pipeline_comparison = pipeline_comparison))
}


# This function is run by the user directly and handles the
# saved to ./data/ as rds files for reading in on startup
run_data_pipeline_step_2 <- function(pipeline_run) {
  if (class(dfs)) {
    for (dataset_name in names(dfs)) {
      saveRDS(object = dfs[[dataset_name]], file = paste0("./data/", print(dataset_name), ".rds"))
    }
  }
}


# Supporting functions for running the data pipeline ----


pipeline_generate_datasets <- function(clear_environment = FALSE) {
  # we need to clear everything from the environment first so that when the function completes we only have
  # the curated datasets in the environment
  if (clear_environment == FALSE) {
    return()
  } else {
    rm(list = ls())
  }

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
  source("R/data_pipeline.R")
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
  list2env(Filter(function(x) is(x, "data.frame"), mget(ls())), envir = .GlobalEnv)
  summary_data <- collect_summary_data_all()

  datasets_new <- Filter(function(x) is(x, "data.frame"), mget(ls()))

  return(datasets_new)
}

# Helper function in the pipeline to compare the datasets (current v new). Various tests are applied and an output text generated
pipeline_compare_datasets <- function(meta_rds, meta_new) {
  user_feedback <- glue::glue("CSC Public Dashboard: Data pipeline diagnostics
  Run date: { Sys.Date() }")

  # compare datasets
  dataset_name_comparison <- dcast(
    rbindlist(list(
      data.table(rds_or_new = "CURRENT", check_type = "dataset_name", dataset_name = meta_rds$dataset_names),
      data.table(rds_or_new = "NEW", check_type = "dataset_name", dataset_name = meta_new$dataset_names)
    )),
    dataset_name ~ check_type + rds_or_new
  )[, match_dataset_name := dataset_name_NEW == dataset_name_CURRENT]

  dataset_class_comparison <- dcast(rbindlist(list(
    data.table(rds_or_new = "CURRENT", check_type = "dataset_class", dataset_name = names(meta_rds$dataset_class), dataset_class = sapply(meta_rds$dataset_class, paste, collapse = ",")),
    data.table(rds_or_new = "NEW", check_type = "dataset_class", dataset_name = names(meta_new$dataset_class), dataset_class = sapply(meta_new$dataset_class, paste, collapse = ","))
  )), dataset_name ~ check_type + rds_or_new)[, match_dataset_class := dataset_class_NEW == dataset_class_CURRENT]

  dataset_nrow_comparison <- dcast(rbindlist(list(
    data.table(rds_or_new = "CURRENT", check_type = "num_rows", dataset_name = names(meta_rds$dataset_nrow), dataset_nrow = sapply(meta_rds$dataset_nrow, paste, collapse = ",")),
    data.table(rds_or_new = "NEW", check_type = "num_rows", dataset_name = names(meta_new$dataset_class), dataset_nrow = sapply(meta_new$dataset_nrow, paste, collapse = ","))
  )), dataset_name ~ check_type + rds_or_new)[, match_dataset_num_rows := num_rows_NEW == num_rows_CURRENT]

  dataset_columns_comparison <- dcast(rbindlist(list(
    data.table(rds_or_new = "CURRENT", check_type = "dataset_columns", dataset_name = names(meta_rds$dataset_columns), dataset_columns = sapply(meta_rds$dataset_columns, paste, collapse = ",")),
    data.table(rds_or_new = "NEW", check_type = "dataset_columns", dataset_name = names(meta_new$dataset_columns), dataset_columns = sapply(meta_new$dataset_columns, paste, collapse = ","))
  )), dataset_name ~ check_type + rds_or_new)[, match_dataset_columns := dataset_columns_NEW == dataset_columns_CURRENT]

  return(list(
    "dataset_name_comparison" = dataset_name_comparison,
    "dataset_class_comparison" = dataset_class_comparison,
    "dataset_nrow_comparison" = dataset_nrow_comparison,
    "dataset_columns_comparison" = dataset_columns_comparison
  ))
}

# Helper function to get all of the datasets currently in data folder into a list comparable with the new datasets
pipeline_read_rds <- function() {
  # first get the names of the RDS files in the ./data directory
  rds_files_to_read <- dir("./data/", pattern = "rds")

  # Read all RDS datasets into a list
  datasets_rds <- lapply(rds_files_to_read, function(rds_file) {
    rds_file <- paste0("./data/", rds_file)
    readRDS(rds_file)
  })

  # rename the list elements to the object name for comparison
  names(datasets_rds) <- sapply(rds_files_to_read, function(rds_file) {
    object_name <- gsub(pattern = ".rds", "", rds_file)
  })

  return(datasets_rds)
}

# helper function to get metadata for the datasets in a list (works for current/rds and new)
pipeline_dataset_metadata <- function(datasets_list) {
  dataset_names <- names(datasets_list)
  dataset_class <- sapply(datasets_list, class)
  dataset_nrow <- sapply(datasets_list, nrow)
  dataset_columns <- lapply(datasets_list, function(x) names(x))
  return(list(
    "dataset_names" = dataset_names,
    "dataset_class" = dataset_class,
    "dataset_nrow" = dataset_nrow,
    "dataset_columns" = dataset_columns
  ))
}
