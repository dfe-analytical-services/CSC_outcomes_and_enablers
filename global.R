# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------

# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(rsconnect))
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(shinytitle))
shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(plotly))
shhh(library(DT))
shhh(library(xfun))
shhh(library(metathis))
shhh(library(shinyalert))
shhh(library(shinytest2))
shhh(library(diffviewer))
shhh(library(rstudioapi))
shhh(library(bslib))
shhh(library(reshape2))
shhh(library(tidyverse))
shhh(library(dfeshiny))
shhh(library(shinyvalidate))
shhh(library(reactable))
shhh(library(readODS))
shhh(library(readxl))
shhh(library(janitor))
shhh(library(scales))
shhh(library(data.table))

# shhh(library(shinya11y)) # used to test the accessibility of the dashboard

shhh(library(htmltools))

lapply(list.files("R/ui_panels/", full.names = TRUE, recursive = TRUE), source)

# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# This line enables bookmarking such that input choices are shown in the url.
enableBookmarking("url")

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# Source scripts ---------------------------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets to the server file.
# It's best to do this here instead of the server file, to improve performance.

source("R/read_data.R")

# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

site_title <- "Children's Social Care - Outcomes and Enablers"
site_primary <- "https://department-for-education.shinyapps.io/csc-outcomes-enablers/"
site_overflow <- "https://department-for-education.shinyapps.io/csc-outcomes-enablers-overflow/"
sites_list <- c(site_primary, site_overflow) # We can add further mirrors where necessary. Each one can generally handle about 2,500 users simultaneously
ees_pub_name <- "Statistical publication" # Update this with your parent publication name (e.g. the EES publication)
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/" # Update with parent publication link
google_analytics_key <- "Q13T4ENF6C"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare all datasets ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Stats Neighbours ----
## Read in the stats_neighbours and generate a long table for all stats neighbour aggregations
stats_neighbours <- head(statistical_neighbours(), 152)
stats_neighbours_long <- get_stats_neighbours_long()
source("R/stats_neighbours.R")

## Read in the workforce data ----
workforce_data <- suppressWarnings(read_workforce_data())
location_data <- GET_location() # fact table linking LA to its region
location_data_workforce <- GET_location_workforce() # fact table linking LA to its region

## Read in the workforce characteristics data (Enabler 2) ----
workforce_eth <- suppressWarnings(read_workforce_eth_data())
workforce_eth_seniority <- suppressWarnings(read_workforce_eth_seniority_data())
population_eth <- suppressWarnings(read_ethnic_population_data())
combined_ethnicity_data <- suppressWarnings(merge_eth_dataframes())

## Read in ofsted leadership data (Enabler 3) ----
spending_data <- suppressWarnings(read_spending_data())
spending_data_no_cla <- suppressWarnings(read_spending_data2())
spending_per_capita <- suppressWarnings(read_per_capita_spending())
ofsted_leadership_data <- suppressWarnings(read_ofsted_leadership_data())
ofsted_leadership_data_long <- suppressWarnings(pivot_ofsted_data())

## Read in the CLA data (outcome 1) ----
cla_rates <- suppressWarnings(read_cla_rate_data())
cla_placements <- suppressWarnings(read_cla_placement_data())
combined_cla_data <- suppressWarnings(merge_cla_dataframes())
combined_cla_31_march_data <- suppressWarnings(merge_cla_31_march_dataframes())

## Read in the CIN  data (outcome 1) ----
cin_rates <- suppressWarnings(read_cin_rate_data())
cin_referrals <- suppressWarnings(read_cin_referral_data())

## Read in the outcomes data (outcome 1) ----
outcomes_absence <- suppressWarnings(read_outcomes_absence_data())
outcomes_ks2 <- suppressWarnings(read_outcomes_ks2_data())
outcomes_ks4 <- suppressWarnings(read_outcomes_ks4_data())

## Read in outcome 2 data ----
ceased_cla_data <- suppressWarnings(read_outcome2())

## Read in outcome 3 data ----
repeat_cpp <- suppressWarnings(read_cpp_in_year_data())
duration_cpp <- suppressWarnings(read_cpp_by_duration_data())
assessment_factors <- suppressWarnings(read_assessment_factors())
af_child_abuse_extra_filter <- assessment_factors %>%
  filter(str_detect(assessment_factor, "Abuse|abuse|Neglect|neglect")) %>%
  select(assessment_factor) %>%
  pull("assessment_factor")

extra_familial_harm_af <- c(
  "Going missing",
  "Child sexual exploitation",
  "Trafficking",
  "Gangs",
  "Child criminal exploitation"
)
# "Alcohol Misuse child", "Drug Misuse child", "Missing", "Child sexual exploitation", "Trafficking", "Gangs", "Child criminal exploitation"

hospital_admissions <- suppressWarnings(read_a_and_e_data())

## Read in outcome 4 data ----
placement_data <- suppressWarnings(read_placement_info_data())

# Define the custom order
custom_order <- c(
  "Foster placements",
  "Secure homes and children's homes",
  "Independent and semi-independent living arrangements/supported accommodation"
)

# Sort the values based on the custom order
placement_type_filter <- placement_data %>%
  filter(str_detect(
    characteristic,
    "Semi|semi|Foster|foster|Settings|settings|Secure|secure"
  )) %>%
  select(characteristic) %>%
  pull("characteristic") %>%
  unique() %>%
  factor(levels = custom_order) %>%
  sort()

placement_changes_data <- suppressWarnings(read_number_placements_data())

care_leavers_activity_data <- suppressWarnings(read_care_leavers_activity_data())
care_leavers_accommodation_data <- suppressWarnings(read_care_leavers_accommodation_suitability())

wellbeing_sdq_data <- suppressWarnings(read_wellbeing_child_data())

placement_order_match_data <- suppressWarnings(read_placement_order_match_data())


# Download button --------------------
# Function to create a download button for reactable
csvDownloadButton <- function(
    id,
    filename = "data.csv",
    label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("customDownloadDataCSV('%s', '%s')", id, filename),
    class = "btn btn-default"
  )
}


# Expandable section ------------------
expandable <- function(inputId, label, contents) {
  govDetails <- shiny::tags$details(
    class = "govuk-details",
    id = inputId,
    shiny::tags$summary(
      class = "govuk-details__summary",
      shiny::tags$span(
        class = "govuk-details__summary-text",
        label
      )
    ),
    shiny::tags$div(contents)
  )
}
