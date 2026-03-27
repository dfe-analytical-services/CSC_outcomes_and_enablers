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



  ## 2. Preliminary diagnostics: before running the pipeline for a modified raw dataset and potentially triggering errors do some comparisons between the csv files for consistency year on year ----

  PRELIM_PATH <- "C:/Users/mweller1/OneDrive - Department for Education/Documents/CSC shiny dashboard/Data QA/workforce_2025" # <--- REPLACE WITH YOUR FOLDER and ensure there are data files pasted into two subfolders for the new data and the old data, note that the file names must match up
  path_old <- paste0(PRELIM_PATH, "/data-comparisons/2025/")
  path_new <- paste0(PRELIM_PATH, "/data-comparisons/2024/")

  pipeline_prelim <- get_pipeline_prelim(path_new, path_old)
  print(pipeline_prelim)

  # if you wish to save artifacts from the preliminary pipeline run then execute these steps
  saveRDS(pipeline_prelim$pipeline_comparison, file = paste0(PRELIM_PATH, "/data-comparisons/pipeline_comparison_prelim_v1.rds")) # <--- REPLACE FILENAME AS REQUIRED

  # produce a diagnostic report of the differences in the files.
  rmarkdown::render(
    input = "./inst/pipeline_prelim.Rmd",
    output_dir = "",
    output_file = "",
    params = list(
      pipeline_comparison = pipeline_prelim$dataset_comparison,
      pipeline_comparison_file = paste0(PRELIM_PATH, "/data-comparisons/pipeline_comparison_prelim_v1.rds")
    )
  )


  ## 3. Now run the first step of the pipeline to generate the new datasets and comparisons with current dashboard data ----
  pipeline_run <- run_data_pipeline_step_1()

  saveRDS(pipeline_run$pipeline_comparison, file = "~/CSC shiny dashboard/Data QA/workforce_2025/pipeline_comparison_workforce_after_unrevert.rds")


  # this is a useful way of doing a comparison between two batches of datasets to identify the differences
  pipeline_run <- run_data_pipeline_step_1(datasets_new = pipeline_run$datasets_new)
  # pr <- run_data_pipeline_step_1(datasets_new = pipeline_read_rds("./data/"), datasets_rds = pipeline_read_rds(rds_file_path = "C:/Users/mweller1/OneDrive - Department for Education/Documents/CSC shiny dashboard/Data QA/cla_2025/data-comparisons/rds_2024/"))
  # saveRDS(pr$pipeline_comparison, file = "~/CSC shiny dashboard/Data QA/sw_stability/pipeline_comparison_2004_v_2005.rds")



  ## 4. Export the files as required
  writexl::write_xlsx(x = pipeline_run$pipeline_comparison$consolidated_setdiffs_summary, "~/CSC shiny dashboard/Data QA/workforce_2025/Consolidated SetDiff workforce after unrevert.xlsx")

  # detailed setdiffs
  deltas_to_export <- rlang::flatten(pipeline_run$pipeline_comparison$consolidated_setdiffs)
  names(deltas_to_export)
  writexl::write_xlsx(deltas_to_export, "~/CSC shiny dashboard/Data QA/workforce_2025/pipeline_consolidated_setdiffs_workforce_after_unrevert.xlsx")

  # setdiffs -> field_diffs
  deltas_to_export <- rlang::flatten(pipeline_run$pipeline_comparison$consolidated_field_diffs)
  names(deltas_to_export)
  writexl::write_xlsx(deltas_to_export, "~/CSC shiny dashboard/Data QA/workforce_2025/pipeline_consolidated_field_diffs_workforce_after_unrevert.xlsx")



  ## 5. Investigate the output from above to compare the current and old data using the diagnostics provided ----
  print(pipeline_run$pipeline_comparison)

  rmarkdown::render("./inst/pipeline_diagnostics.Rmd", params = list(
    pipeline_comparison = pr$dataset_comparison,
    pipeline_comparison_file = "~/CSC shiny dashboard/Data QA/workforce_2025/pipeline_comparison_v1.rds"
  ))


  # more indepth analysis of the diffs
  pipeline_run$pipeline_comparison$consolidated_field_diffs$hospital_admissions[!(variable_clean %in% c("Count", "Denominator"))]
  pipeline_run$pipeline_comparison$consolidated_field_diffs$workforce_data # [!(variable_clean %in% c("Count", "Denominator"))]





  ## 6. If the diagnostics are ok then record the necessary parameters in order to run the second step of the pipeline ----

  # this must be entered, minimum 10 characters, please be verbose with explanation
  reason_for_pipeline_run <- "Re-run pipeline for summary data after re-revert caused rds file conflicts on summary_data" # <---- EDIT HERE

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
