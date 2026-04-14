# **** Run the data pipeline ****

# Please refer to the pipeline documentation found at www.XXXXXXX.tbc
# The section below within the IF statement should be run step by step

if (TRUE == FALSE) { # this IF statement is to prevent the following block of code from running if sourced

  # BEGIN PIPELINE ----

  ## 1. First it is necessary to clear the environment and source all of the functions in this file -----
  rm(list = ls())
  source("./R/data_pipeline_functions.R")
  shhh <- suppressPackageStartupMessages # It's a library, so shhh!
  shhh(library(dplyr))
  shhh(library(reshape2))
  shhh(library(tidyverse))
  shhh(library(readODS))
  shhh(library(readxl))
  shhh(library(janitor))
  shhh(library(data.table, pos = 3))

  ## 2. Set Common parameters ----
  YOUR_LOCAL_PATH <- "C:/Users/npaterson/OneDrive - Department for Education/Documents/CSC shiny dashboard/Data QA/" # <--- REPLACE WITH YOUR USERNAME and create the folders on your device
  TASK_NAME <- "outcomes_2025" # <--- REPLACE WITH YOUR FOLDER and ensure there are data files pasted into two subfolders for the new data and the old data, note that the file names must match up





  ## 3. Preliminary diagnostics (OPTIONAL): before running the pipeline for a modified raw dataset and potentially triggering errors do some comparisons between the csv files for consistency year on year ----
  pipeline_prelim <- get_pipeline_prelim(
    path_new = paste0(YOUR_LOCAL_PATH, TASK_NAME, "/data-comparisons/2024/"),
    path_old = paste0(YOUR_LOCAL_PATH, TASK_NAME, "/data-comparisons/2025/")
  )

  print(pipeline_prelim)

  # if you wish to save artifacts from the preliminary pipeline run then execute these steps
  saveRDS(pipeline_prelim$pipeline_comparison, file = paste0(YOUR_LOCAL_PATH, "pipeline_comparison_prelim.rds")) 

  # produce a diagnostic report of the differences in the files.
  rmarkdown::render(
    input = "./inst/pipeline_prelim.Rmd",
    output_dir  = "inst",
    output_file = "pipeline_prelim.html",
        params = list(
      pipeline_comparison = pipeline_prelim$dataset_comparison,
      pipeline_comparison_file = paste0(YOUR_LOCAL_PATH, "pipeline_comparison_prelim.rds")
    )
  )

  ## 4. Now run the first step of the pipeline to generate the new datasets and comparisons with current dashboard data ----
  PIPELINE_RUN_VERSION <- "v5a"


  pipeline_run <- run_data_pipeline_step_1(
    datasets_new = NULL, datasets_rds = NULL,
    save_datasets = FALSE, save_comparison = TRUE,
    YOUR_LOCAL_PATH, TASK_NAME, PIPELINE_RUN_VERSION
  )



  # Note that it is possible to rerun the pipeline WITHOUT generating the new datasets by running all of the read data functions.
  # This requires an existing set of datasets to be fed in for datasets_new
  # this is a useful way of doing a comparison between two batches of datasets, or the current datasets in memory to identify the differences
  # pipeline_run <- run_data_pipeline_step_1(datasets_new = read_environment_datasets(), save_comparison = TRUE, YOUR_LOCAL_PATH = YOUR_LOCAL_PATH, TASK_NAME = TASK_NAME, PIPELINE_RUN_VERSION = PIPELINE_RUN_VERSION)
  # pr <- run_data_pipeline_step_1(datasets_new = pipeline_read_rds("./data/"), datasets_rds = pipeline_read_rds(rds_file_path = "C:/Users/mweller1/OneDrive - Department for Education/Documents/CSC shiny dashboard/Data QA/cla_2025/data-comparisons/rds_2024/"))

  # saveRDS(pr$pipeline_comparison, file = "~/CSC shiny dashboard/Data QA/sw_stability/pipeline_comparison_2004_v_2005.rds")





  ## 5. Investigate the output from above to compare the current and old data using the diagnostics provided ----
  print(pipeline_run$pipeline_comparison)
  
  saveRDS(pipeline_run$pipeline_comparison, file = paste0(YOUR_LOCAL_PATH, "pipeline_comparison.rds")) 

  rmarkdown::render(  
    input = "./inst/pipeline_diagnostics.Rmd", 
    output_dir  = "inst",
    output_file = "pipeline_diagnostics.html",
    params = list(
    pipeline_comparison = pipeline_run$dataset_comparison,
    pipeline_comparison_file = paste0(YOUR_LOCAL_PATH, "pipeline_comparison.rds")
  ))


  # more indepth analysis of the diffs
  names(pipeline_run$pipeline_comparison$consolidated_field_diffs)

  pipeline_run$pipeline_comparison$consolidated_field_diffs$workforce_eth_seniority[!(variable_clean %in% c("Percentage", "inpost_headcount"))]
  pipeline_run$pipeline_comparison$consolidated_field_diffs$workforce_data # [!(variable_clean %in% c("Count", "Denominator"))]

  pipeline_run$pipeline_comparison$consolidated_field_diffs



  ## 6. If the diagnostics are ok then record the necessary parameters in order to run the second step of the pipeline ----

  # this must be entered, minimum 10 characters, please be verbose with explanation
  reason_for_pipeline_run <- "Update outcomes data" # <---- EDIT HERE

  # this must be updated to "Y" to signify the comparison has been checked
  comparison_checked <- "Y" # <---- EDIT HERE



  ## 7. verify the update parameters ----
  pipeline_run_parameters <- list(
    "username" = Sys.getenv("USERNAME"),
    "run_datetime" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "reason_for_pipeline_run" = reason_for_pipeline_run,
    "comparison_checked" = comparison_checked
  )

  print(pipeline_run_parameters)


  ## FINAL STEP - proceed with caution having completed steps 1-6 above

  ## 8. Finally run the update to bring the new data through the pipeline into the app (i.e. copy to RDS files in ./data/ folder) ----
  # note that you will be prompted in the Console window
  # upon completion refer to the guide regarding the git-related steps which follow

  print(run_data_pipeline_step_2(pipeline_run, pipeline_run_parameters))

  # END PIPELINE ----
}
