# **** Run the data pipeline ****
# The section below which should be run step by step

if (TRUE == FALSE) {
  # BEGIN PIPELINE ----

  ## 1. First it is necessary to clear the environment and source all of the functions in this file -----
  rm(list = ls())
  source("./R/data_pipeline.R")


  ## 2. Now run the first step of the pipeline to generate the new datasets and comparisons with current app data ----
  pipeline_run <- run_data_pipeline_step_1()


  ## 3. Investigate the output from above to compare the current and old data using the diagnostics provided ----
  print(pipeline_run$pipeline_comparison)


  ## 4. If the diagnostics are ok then record the necessary parameters in order to run the second step of the pipeline ----

  # this must be entered, minimum 10 characters, please be verbose with explanation
  reason_for_pipeline_run <- "REASON GOES HERE" # <---- EDIT HERE

  # this must be updated to "Y" to signify the comparison has been checked
  comparison_checked <- "N" # <---- EDIT HERE



  ## 5. verify the update parameters ----
  pipeline_run_parameters <- list(
    "username" = Sys.getenv("USERNAME"),
    "run_datetime" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "reason_for_pipeline_run" = reason_for_pipeline_run,
    "comparison_checked" = comparison_checked
  )

  print(pipeline_run_parameters)


  ## FINAL STEP - proceed with caution having completed steps 1-6 above


  ## 6. Finally run the update to bring the new data through the pipeline into the app (i.e. copy to RDS files in ./data/ folder) ----
  print(run_data_pipeline_step_2(pipeline_run, pipeline_run_parameters))

  # END PIPELINE ----
}

# ===========================================================================================================
# DO NOT EDIT THE CODE BELOW HERE OR ATTEMPT TO RUN ANY OF THE FUNCTIONS DIRECTLY ----
# ===========================================================================================================

# Main Functions for the Data Pipeline ----

# This function implements the first data pipeline step of transforming input csv & xlsx files into curated datasets
# The resulting datasets are returned in a list at this point along with the parameters used to generate them.
# Additional validation takes place to provide feedback on what has been generated and what differs to the rds datasets in the ./data/ folder of the repo
# This is the first stage of the pipeline, to build the data in a temporary fashion with nothing in the main application being changed yet, that follows in step 2
run_data_pipeline_step_1 <- function() {
  datasets_new <- pipeline_generate_datasets()
  datasets_rds <- pipeline_read_rds()

  meta_rds <- pipeline_dataset_metadata(datasets_rds)
  meta_new <- pipeline_dataset_metadata(datasets_new)

  pipeline_comparison <- pipeline_compare_datasets(meta_rds, meta_new, datasets_rds, datasets_new)

  return(list(datasets_new = datasets_new, pipeline_comparison = pipeline_comparison))
}


# This function is run as the final step in the pipeline and handles the saving of
# the datasets generated into ./data/ as rds files for reading in on startup

run_data_pipeline_step_2 <- function(pipeline_run, pipeline_run_parameters) {
  print("* Pipeline step 2 running ----")
  # first we perform various checks on the input to this function
  print("* Checking input parameters")
  # do some checks on the pipeline_parameters
  if (class(pipeline_run_parameters) != "list") {
    return("pipeline_run_parameters is not a list")
  }
  # expect elements with specific names
  if (!identical(names(pipeline_run_parameters), c("username", "run_datetime", "reason_for_pipeline_run", "comparison_checked"))) {
    return("pipeline_parameters does not contain the correct elements")
  }
  # expect a username with one or more chars
  if (!nchar(pipeline_run_parameters$username) > 0) {
    return("pipeline_run_parameters must have a username with more than zero chars")
  }
  # expect a run_date_time in the right format
  if (!is(lubridate::as_datetime(pipeline_run_parameters$run_datetime), "POSIXct")) {
    return("pipeline_run_parameters must have a run_datetime in the right format")
  }
  # expect a pipeline_run_reason with > 10 chars
  if (!nchar(pipeline_run_parameters$reason_for_pipeline_run) >= 10) {
    return("pipeline_run_parameters must have a reason with more than 10 chars")
  }
  # expect the checked elemnt to say "Y"
  if (pipeline_run_parameters$comparison_checked != "Y") {
    return("pipeline_run_parameters comparison_checked must be equal to 'Y'")
  }

  # do some checks on the pipeline_run
  if (class(pipeline_run) != "list") {
    return("pipeline_run must be a list")
  }
  if (!identical(names(pipeline_run), c("datasets_new", "pipeline_comparison"))) {
    return("pipeline_run must")
  }
  # expect pipeline_run$datasets_new to be a list of named data objects
  if (class(pipeline_run$datasets_new) != "list") {
    return("pipeline_run datasets_new must be a list")
  }
  if (length(which(sapply(pipeline_run$datasets_new, function(x) !is(x, "data.frame")))) != 0) {
    return("all pipeline_run datasets_new must be a data.frame")
  }
  if (sum(nchar(names(pipeline_run$datasets_new)) == 0) > 0) {
    return("pipeline_run datasets_new must all be named")
  }


  # now check inside the function that the data should be overwritten
  user_input <- readline(prompt = "Data pipeline step 2 checks passed, are you sure you wish to finalise the update? Type 'Yes' to proceed: ")
  if (user_input != "Yes") {
    return(paste0("Pipeline step 2 aborted.  Dashboard datasets not overwritten.  You entered '", user_input, "', to proceed you are required to enter (case-sensitive) 'Yes'."))
  }

  # this step is the final step to overwrite the data!!!
  for (dataset_name in names(pipeline_run$datasets_new)) {
    # write out to the console what is happening
    print(paste(dataset_name, " ---copied to---> ", paste0("./data/", dataset_name, ".rds")))
    # write the dataset out to rds file
    saveRDS(object = pipeline_run$datasets_new[[dataset_name]], file = paste0("./data/", print(dataset_name), ".rds"))
  }

  # now check that the data in the rds files matches the data we have generated ??

  # now log the pipeline_run in the history, getting the sequentially next ID
  print("* Saving pipeline run to history")
  pipeline_history <- readRDS("./data/pipeline/data_pipeline_run_history.rds")
  last_pipeline_id <- if (length(pipeline_history) == 0) {
    0
  } else {
    # loop through the list to get the maximum_id (ie. the latest run)
    max(sapply(pipeline_history, function(x) x$pipeline_run_id))
  }

  # append this run to the pipeline_run_history list
  pipeline_history[[length(pipeline_history) + 1]] <- list(
    "pipeline_run_id" = last_pipeline_id + 1,
    "parameters" = pipeline_run_parameters,
    "pipeline_comparison" = pipeline_run$pipeline_comparison
  )
  # now save the updated history
  saveRDS(pipeline_history, "./data/pipeline/data_pipeline_run_history.rds")

  print("* Saving pipeline run to history")

  return("Step 2 successful: applicaion datasets updated.")
}


# Supporting functions for running the data pipeline ----

pipeline_generate_datasets <- function() {
  # Library calls ---------------------------------------------------------------------------------
  shhh <- suppressPackageStartupMessages # It's a library, so shhh!
  shhh(library(dplyr))
  shhh(library(reshape2))
  shhh(library(tidyverse))
  shhh(library(readODS))
  shhh(library(readxl))
  shhh(library(janitor))
  shhh(library(data.table, pos = 3))

  # source supporting functions to prepare data
  # source("R/data_pipeline.R")
  source("R/read_data.R")
  source("R/stats_neighbours.R")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare all datasets ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Stats Neighbours ----
  ## Read in the stats_neighbours and generate a long table for all stats neighbour aggregations
  stats_neighbours <- get_statistical_neighbours() # head(statistical_neighbours(), 152)
  stats_neighbours_long <- get_stats_neighbours_long(stats_neighbours)

  ## Read in CLA placements data first as this is used in GET_location called in the next section
  cla_placements <- suppressWarnings(read_cla_placement_data(sn_long = stats_neighbours_long))
  .GlobalEnv$cla_placements <- cla_placements

  ## Read in the workforce data ----
  workforce_data <- suppressWarnings(read_workforce_data(sn_long = stats_neighbours_long))
  workforce_headline_measures <- suppressWarnings(read_workforce_headline_measures())
  location_data <- GET_location(cla_placements) # fact table linking LA to its region
  location_data_workforce <- GET_location_workforce(workforce_headline_measures) # fact table linking LA to its region

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
pipeline_compare_datasets <- function(meta_rds, meta_new, datasets_rds, datasets_new) {
  user_feedback <- glue::glue("CSC Public Dashboard: Data pipeline diagnostics
  Run date: { Sys.Date() }")

  # compare datasets
  dataset_name_comparison <- dcast.data.table(
    rbindlist(list(
      data.table(rds_or_new = "CURRENT", check_type = "dataset_name", dataset_name = meta_rds$dataset_names),
      data.table(rds_or_new = "NEW", check_type = "dataset_name", dataset_name = meta_new$dataset_names)
    )),
    dataset_name ~ check_type + rds_or_new
  )[, match_dataset_name := dataset_name_NEW == dataset_name_CURRENT]

  dataset_class_comparison <- dcast.data.table(rbindlist(list(
    data.table(rds_or_new = "CURRENT", check_type = "dataset_class", dataset_name = names(meta_rds$dataset_class), dataset_class = sapply(meta_rds$dataset_class, paste, collapse = ",")),
    data.table(rds_or_new = "NEW", check_type = "dataset_class", dataset_name = names(meta_new$dataset_class), dataset_class = sapply(meta_new$dataset_class, paste, collapse = ","))
  )), dataset_name ~ check_type + rds_or_new)[, match_dataset_class := dataset_class_NEW == dataset_class_CURRENT]

  dataset_nrow_comparison <- dcast.data.table(rbindlist(list(
    data.table(rds_or_new = "CURRENT", check_type = "num_rows", dataset_name = names(meta_rds$dataset_nrow), dataset_nrow = sapply(meta_rds$dataset_nrow, paste, collapse = ",")),
    data.table(rds_or_new = "NEW", check_type = "num_rows", dataset_name = names(meta_new$dataset_class), dataset_nrow = sapply(meta_new$dataset_nrow, paste, collapse = ","))
  )), dataset_name ~ check_type + rds_or_new)[, match_dataset_num_rows := num_rows_NEW == num_rows_CURRENT]

  dataset_columns_comparison <- dcast.data.table(rbindlist(list(
    data.table(rds_or_new = "CURRENT", check_type = "dataset_columns", dataset_name = names(meta_rds$dataset_columns), dataset_columns = sapply(meta_rds$dataset_columns, paste, collapse = ",")),
    data.table(rds_or_new = "NEW", check_type = "dataset_columns", dataset_name = names(meta_new$dataset_columns), dataset_columns = sapply(meta_new$dataset_columns, paste, collapse = ","))
  )), dataset_name ~ check_type + rds_or_new)[, match_dataset_columns := dataset_columns_NEW == dataset_columns_CURRENT]

  # build the summary of dataset comparisons for easy reference
  dataset_comparison_summary <- merge.data.table(
    dataset_name_comparison[, .(dataset_name, match_dataset_name)],
    dataset_class_comparison[, .(dataset_name, match_dataset_class)],
    all.x = TRUE
  )

  dataset_comparison_summary <- merge.data.table(
    dataset_comparison_summary,
    dataset_nrow_comparison[, .(dataset_name, match_dataset_num_rows)],
    all.x = TRUE
  )

  dataset_comparison_summary <- merge.data.table(
    dataset_comparison_summary,
    dataset_columns_comparison[, .(dataset_name, match_dataset_columns)],
    all.x = TRUE
  )

  dataset_comparison_summary[, match_summary := ((match_dataset_name + match_dataset_class + match_dataset_num_rows + match_dataset_columns) == 4)]

  # objects in both
  equal_datasets <- dataset_comparison_summary[match_summary == TRUE]$dataset_name

  # add in a data comparison (setdiffs)
  df_setdiffs <- lapply(equal_datasets, function(df_name) {
    list(
      old_v_new = setdiff(datasets_new[[df_name]], datasets_rds[[df_name]]),
      new_v_old = setdiff(datasets_rds[[df_name]], datasets_new[[df_name]])
    )
  })
  names(df_setdiffs) <- equal_datasets


  return(list(
    "dataset_comparison_summary" = dataset_comparison_summary,
    "dataset_name_comparison" = dataset_name_comparison,
    "dataset_class_comparison" = dataset_class_comparison,
    "dataset_nrow_comparison" = dataset_nrow_comparison,
    "dataset_columns_comparison" = dataset_columns_comparison,
    "dataset_setdiffs" = df_setdiffs
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
