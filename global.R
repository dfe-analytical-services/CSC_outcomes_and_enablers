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
shhh(library(htmltools))

# shhh(library(shinya11y)) # used to test the accessibility of the dashboard

# source the ui files
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
source("R/stats_neighbours.R")

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
# Read all RDS datasets into the Global environment ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get the names of the RDS files in the ./data directory
rds_files_to_read <- dir("./data/", pattern = "rds")

for (rds_file in rds_files_to_read) {
  object_name <- gsub(pattern = ".rds", "", rds_file)
  rds_file <- paste0("./data/", rds_file)
  assign(object_name, readRDS(rds_file), envir = .GlobalEnv)
}


# create some lookup lists from various tables to populate dropdowns
af_child_abuse_extra_filter <- assessment_factors %>%
  filter(str_detect(assessment_factor, "Abuse|abuse|Neglect|neglect")) %>%
  select(assessment_factor) %>%
  pull("assessment_factor") %>%
  unique()
extra_familial_harm_af <- c(
  "Going missing",
  "Child sexual exploitation",
  "Trafficking",
  "Gangs",
  "Child criminal exploitation"
)
# "Alcohol Misuse child", "Drug Misuse child", "Missing", "Child sexual exploitation", "Trafficking", "Gangs", "Child criminal exploitation"

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


# Download button --------------------
# Function to create a download button for reactable
csvDownloadButton <- function(
    id,
    filename = "data.csv",
    label = "Download as CSV") {
  gov_row(
    tags$button(
      tagList(icon("download"), label),
      onclick = sprintf("customDownloadDataCSV('%s', '%s')", id, filename),
      class = "govuk-button"
    )
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


# Reactable global settings ---------------
options(reactable.language = reactableLang(searchPlaceholder = "Search within table"))
