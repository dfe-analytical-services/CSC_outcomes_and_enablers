# **** Run the data pipeline ****
# Please refer to the pipeline documentation found at www.XXXXXXX.tbc
# The section below within the IF statement should be run step by step

if (TRUE == FALSE) { # this IF statement is to prevent the following block of code from running if sourced

  # BEGIN PIPELINE ----

  ## 1. First it is necessary to clear the environment and source all of the functions in this file -----
  rm(list = ls())
  source("./R/data_pipeline.R")
  shhh <- suppressPackageStartupMessages # It's a library, so shhh!
  shhh(library(dplyr))
  shhh(library(reshape2))
  shhh(library(tidyverse))
  shhh(library(readODS))
  shhh(library(readxl))
  shhh(library(janitor))
  shhh(library(data.table, pos = 3))



  ## 2. Preliminary diagnostics: before running the pipeline and triggering errors do some comparisons between the csv files for consistency year on year
  path_new <- "~/CSC Private dashboard/data-comparisons/2025/"
  path_old <- "~/CSC Private dashboard/data-comparisons/2024/"

  path_old <- "~/CSC shiny dashboard/Data QA/cla_2025/data-comparisons/2025/"
  path_new <- "~/CSC shiny dashboard/Data QA/cla_2025/data-comparisons/2025_v2/"

  pipeline_prelim <- get_pipeline_prelim(path_new, path_old)

  saveRDS(pipeline_prelim$pipeline_comparison, file = "~/CSC shiny dashboard/Data QA/cla_2025/pipeline_comparison_prelim_v2.rds")
  print(pipeline_prelim)


  rmarkdown::render("./inst/pipeline_prelim.Rmd", params = list(
    pipeline_comparison = pipeline_prelim$dataset_comparison
  ))


  ## 2. Now run the first step of the pipeline to generate the new datasets and comparisons with current dashboard data ----
  pipeline_run <- run_data_pipeline_step_1()

  saveRDS(pipeline_run$pipeline_comparison, file = "~/CSC shiny dashboard/Data QA/cla_2025/pipeline_comparison_step_1_v2.rds")

  pipeline_run <- run_data_pipeline_step_1(datasets_new = pipeline_run$datasets_new)

  ## 3. Investigate the output from above to compare the current and old data using the diagnostics provided ----
  print(pipeline_run$pipeline_comparison)

  rmarkdown::render("./inst/pipeline_diagnostics.Rmd", params = list(
    pipeline_comparison = pipeline_run$dataset_comparison
  ))

  deltas_to_export <- rlang::flatten(pipeline_run$pipeline_comparison$consolidated_setdiffs)
  names(deltas_to_export)
  writexl::write_xlsx(deltas_to_export, "./pipeline_consolidated_setdiffs.xlsx")


  ## 4. If the diagnostics are ok then record the necessary parameters in order to run the second step of the pipeline ----

  # this must be entered, minimum 10 characters, please be verbose with explanation
  reason_for_pipeline_run <- "Revision to CLA 2025 Data update.  Relates to revision of percentage figures in la_cla_on_31_march_by_characteristics.csv" # <---- EDIT HERE

  # this must be updated to "Y" to signify the comparison has been checked
  comparison_checked <- "Y" # <---- EDIT HERE



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

# preliminary comparisons of the two sets of csv files (old and new)

get_pipeline_prelim <- function(path_new, path_old) {
  # the two sets of files in the publication for the new and old/current year can be placed in a location to allow them to be compared.
  # comparisons include field names, data types, values and specifically geographies.
  path_raw <- "./data-raw/"
  files_new <- dir(path_new, pattern = "csv")
  files_old <- dir(path_old, pattern = "csv")
  files_raw <- dir(path_raw, pattern = "csv")

  file_name_matches <- files_raw[which(files_raw %in% files_old)]
  file_name_mismatches <- file_name_matches[which(!(file_name_matches %in% files_new))]

  # get the old files read in from csv to a data.table
  old <- lapply(file_name_matches, function(file_name) fread(paste0(path_old, file_name)))
  names(old) <- file_name_matches

  # get the new files read in from csv to a data.table
  new <- lapply(file_name_matches, function(file_name) fread(paste0(path_new, file_name)))
  names(new) <- file_name_matches

  # compare the old and new datasets
  pc1 <- pipeline_compare_datasets(
    meta_rds = pipeline_dataset_metadata(old),
    meta_new = pipeline_dataset_metadata(new),
    datasets_rds = old,
    datasets_new = new
  )

  return(datasets = list(old = old, new = new, pipeline_comparison = pc1))
}


# Main Functions for the Data Pipeline ----

# This function implements the first data pipeline step of transforming input csv & xlsx files into curated datasets
# The resulting datasets are returned in a list at this point along with the parameters used to generate them.
# Additional validation takes place to provide feedback on what has been generated and what differs to the rds datasets in the ./data/ folder of the repo
# This is the first stage of the pipeline, to build the data in a temporary fashion with nothing in the main application being changed yet, that follows in step 2
run_data_pipeline_step_1 <- function(datasets_new = NULL) {
  if (is.null(datasets_new)) datasets_new <- pipeline_generate_datasets()
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

# Helper function in the pipeline to compare the datasets (current v new). Various tests are applied and an output list generated
pipeline_compare_datasets <- function(meta_rds, meta_new, datasets_rds, datasets_new, Geography_column = NULL) {
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
    data.table(rds_or_new = "CURRENT", check_type = "dataset_class", dataset_name = names(meta_rds$dataset_class), dataset_class = sapply(meta_rds$dataset_class, paste, collapse = ", ")),
    data.table(rds_or_new = "NEW", check_type = "dataset_class", dataset_name = names(meta_new$dataset_class), dataset_class = sapply(meta_new$dataset_class, paste, collapse = ", "))
  )), dataset_name ~ check_type + rds_or_new)[, match_dataset_class := dataset_class_NEW == dataset_class_CURRENT]

  dataset_nrow_comparison <- dcast.data.table(rbindlist(list(
    data.table(rds_or_new = "CURRENT", check_type = "num_rows", dataset_name = names(meta_rds$dataset_nrow), dataset_nrow = sapply(meta_rds$dataset_nrow, paste, collapse = ", ")),
    data.table(rds_or_new = "NEW", check_type = "num_rows", dataset_name = names(meta_new$dataset_class), dataset_nrow = sapply(meta_new$dataset_nrow, paste, collapse = ", "))
  )), dataset_name ~ check_type + rds_or_new)[, match_dataset_num_rows := num_rows_NEW == num_rows_CURRENT]

  dataset_columns_comparison <- dcast.data.table(rbindlist(list(
    data.table(rds_or_new = "CURRENT", check_type = "dataset_columns", dataset_name = names(meta_rds$dataset_columns), dataset_columns = sapply(meta_rds$dataset_columns, paste, collapse = ", ")),
    data.table(rds_or_new = "NEW", check_type = "dataset_columns", dataset_name = names(meta_new$dataset_columns), dataset_columns = sapply(meta_new$dataset_columns, paste, collapse = ", "))
  )), dataset_name ~ check_type + rds_or_new)[, match_dataset_columns := dataset_columns_NEW == dataset_columns_CURRENT]

  dataset_geographies_comparison <- rbindlist(
    lapply(1:length(datasets_new), function(x, datasets_new, datasets_rds) {
      if ("geo_breakdown" %in% names(datasets_new[[x]]) & "geo_breakdown" %in% names(datasets_rds[[x]])) {
        geo_column <- "geo_breakdown"
      } else if ("la_name" %in% names(datasets_new[[x]]) & "la_name" %in% names(datasets_rds[[x]])) {
        geo_column <- "la_name"
      } else {
        geo_column <- NULL
      }
      if (!is.null(geo_column)) {
        las_added <- paste0(setdiff(unique(datasets_new[[x]][[geo_column]]), unique(datasets_rds[[x]][[geo_column]])), collapse = ", ")
        las_removed <- paste0(setdiff(unique(datasets_rds[[x]][[geo_column]]), unique(datasets_new[[x]][[geo_column]])), collapse = ", ")
        data.table(
          dataset_name = names(datasets_new)[x],
          las_added = las_added,
          las_removed = las_removed
        )
      }
    }, datasets_new, datasets_rds)
  )

  # Function to find common elements between two delimited strings
  compare_elements <- function(str1, str2, delimiter = ",", common = TRUE) {
    # Validate inputs
    if (!is.character(str1) || !is.character(str2)) {
      stop("Both inputs must be character strings.")
    }

    # Split strings into vectors
    vec1 <- trimws(unlist(strsplit(str1, delimiter, fixed = TRUE)))
    vec2 <- trimws(unlist(strsplit(str2, delimiter, fixed = TRUE)))

    # Find common elements
    if (common == TRUE) ret_val <- paste(intersect(vec1, vec2), collapse = ", ")
    if (common == FALSE) ret_val <- paste(setdiff(vec1, vec2), collapse = ", ")

    return(ret_val)
  }

  dataset_columns_comparison[, columns_retained := mapply(compare_elements, dataset_columns_CURRENT, dataset_columns_NEW, common = TRUE)]
  dataset_columns_comparison[, columns_added := mapply(compare_elements, dataset_columns_CURRENT, dataset_columns_NEW, common = FALSE)]
  dataset_columns_comparison[, columns_removed := mapply(compare_elements, dataset_columns_NEW, dataset_columns_CURRENT, common = FALSE)]





  collapse_function <- function(x) {
    if (length(x) == 0) {
      return("")
    } # handle empty lists
    paste(x, collapse = ", ")
  }

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

  # objects consistent in both (i.e. setdiff will not fail)
  diff_datasets <- dataset_comparison_summary[match_dataset_name + match_dataset_class + match_dataset_columns == 3]$dataset_name

  # add in a data comparison (setdiffs)
  df_setdiffs <- lapply(diff_datasets, function(df_name, datasets_new, datasets_rds) {
    print(df_name)
    list(
      old_v_new = setdiff(datasets_new[[df_name]], datasets_rds[[df_name]]),
      new_v_old = setdiff(datasets_rds[[df_name]], datasets_new[[df_name]])
    )
  }, datasets_new, datasets_rds)
  names(df_setdiffs) <- diff_datasets

  # datasets which have deltas
  changed_datasets <- names(which(lapply(df_setdiffs, function(x) nrow(x$old_v_new) + nrow(x$new_v_old)) > 0))

  # add in the column comparison of values
  dataset_column_values_comparison <- list(
    new = summarise_columns_over_datasets(datasets_new[changed_datasets]), # [changed_datasets[changed_datasets != "summary_data"]]
    old = summarise_columns_over_datasets(datasets_rds[changed_datasets]) # [changed_datasets[changed_datasets != "summary_data"]]
  )

  candidate_key_cols <- c("time_period", "old_la_code", dataset_column_values_comparison$new[number_count == 0]$column_name)


  consolidated_setdiffs <- lapply(changed_datasets, function(dataset_name, df_setdiffs, candidate_key_cols) {
    added <- setDT(df_setdiffs[[dataset_name]]$old_v_new)[, new := "NEW"]
    removed <- setDT(df_setdiffs[[dataset_name]]$new_v_old)[, old := "OLD"]

    join_cols <- intersect(intersect(names(added), names(removed)), candidate_key_cols)

    dataset_compare <- merge(added, removed, by = join_cols, all = TRUE, suffixes = c("_newval", "_oldval"))

    new_order <- c(join_cols, sort(setdiff(names(dataset_compare), join_cols)))
    setcolorder(dataset_compare, neworder = new_order)
    dataset_compare
  }, df_setdiffs, candidate_key_cols)

  names(consolidated_setdiffs) <- changed_datasets

  dataset_column_values_comparison$new[number_count > 0]
  return(list(
    "dataset_comparison_summary" = dataset_comparison_summary,
    "dataset_name_comparison" = dataset_name_comparison,
    "dataset_class_comparison" = dataset_class_comparison,
    "dataset_nrow_comparison" = dataset_nrow_comparison,
    "dataset_columns_comparison" = dataset_columns_comparison,
    "dataset_geographies_comparison" = dataset_geographies_comparison,
    "dataset_setdiffs" = df_setdiffs,
    "dataset_column_values_comparison" = dataset_column_values_comparison,
    "consolidated_setdiffs" = consolidated_setdiffs,
    "changed_datasets" = changed_datasets
  ))
}

summarise_columns_over_datasets <- function(dataset_list, label = "my_datasets") {
  # get a summary of column names by table and the number of unique values within the column in the dataset
  column_names <- rbindlist(lapply(names(dataset_list), function(dataset_name, dataset_list) {
    data.table(
      dataset_name = dataset_name,
      num_rows = nrow(dataset_list[[dataset_name]]),
      column_name = names(dataset_list[[dataset_name]]),
      unique_value_count = sapply(dataset_list[[dataset_name]], function(x) length(unique(x)))
    )
  }, dataset_list))

  # now get the actual value counts by dataset and column
  all_data <- rbindlist(dataset_list, fill = TRUE)
  column_values <- rbindlist(lapply(names(all_data), function(column_name, all_data) data.table(column_name = column_name, column_value = unique(all_data[[column_name]])), all_data))
  column_values$numeric_value <- sapply(column_values$column_value, function(x) !is.na(as.numeric(x)))

  column_summary <- merge(
    column_values[, .(unique_value_count = .N, number_count = sum(numeric_value)), by = .(column_name)],
    column_names[, .(occurences = .N, row_count = sum(num_rows), sum_unique_value_count = sum(unique_value_count)), by = column_name]
  )[order(-unique_value_count)]
  column_summary[, number_percentage := number_count / unique_value_count]
  column_summary[, occurences_per_value := as.integer(row_count / unique_value_count)]

  return(column_summary)
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
  dataset_class <- sapply(datasets_list, function(x) class(x)[1])
  dataset_nrow <- sapply(datasets_list, nrow)
  dataset_columns <- lapply(datasets_list, function(x) names(x))
  return(list(
    "dataset_names" = dataset_names,
    "dataset_class" = dataset_class,
    "dataset_nrow" = dataset_nrow,
    "dataset_columns" = dataset_columns
  ))
}
