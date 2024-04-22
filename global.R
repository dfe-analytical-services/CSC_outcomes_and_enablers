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
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinydashboard))
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

# shhh(library(shinya11y))

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

# source("R/filename.r")


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

site_title <- "DfE Shiny Template"
site_primary <- "https://department-for-education.shinyapps.io/dfe-shiny-template/"
site_overflow <- "https://department-for-education.shinyapps.io/dfe-shiny-template-overflow/"
sites_list <- c(site_primary, site_overflow) # We can add further mirrors where necessary. Each one can generally handle about 2,500 users simultaneously
ees_pub_name <- "Statistical publication" # Update this with your parent publication name (e.g. the EES publication)
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/" # Update with parent publication link
google_analytics_key <- "Q13T4ENF6C"


source("R/read_data.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in the workforce data

workforce_data <- read_workforce_data()
location_data <- GET_location() # fact table linking LA to its region
location_data_workforce <- GET_location_workforce() # fact table linking LA to its region

# Read in the workforce characteristics data (Enabler 2)
# workforce_char <- read_workforce_char_data()

workforce_eth <- suppressWarnings(read_workforce_eth_data())
workforce_eth_seniority <- suppressWarnings(read_workforce_eth_seniority_data())
population_eth <- suppressWarnings(read_ethnic_population_data())
combined_ethnicity_data <- suppressWarnings(merge_eth_dataframes())

# Read in the CLA data (outcome 1)
cla_rates <- suppressWarnings(read_cla_rate_data())
cla_placements <- suppressWarnings(read_cla_placement_data())
combined_cla_data <- suppressWarnings(merge_cla_dataframes())
# uasc_data <- test_uasc()

# Read in the CIN  data (outcome 1)
cin_rates <- suppressWarnings(read_cin_rate_data())
cin_referrals <- suppressWarnings(read_cin_referral_data())


# Read in the outcomes data (outcome 1)
outcomes_absence <- suppressWarnings(read_outcomes_absence_data())
outcomes_ks2 <- suppressWarnings(read_outcomes_ks2_data())
outcomes_ks4 <- suppressWarnings(read_outcomes_ks4_data())

# Read in outcome 2 data
ceased_cla_data <- suppressWarnings(read_outcome2())

# Read in outcome 3 data
repeat_cpp <- suppressWarnings(read_cpp_in_year_data())
assessment_factors <- suppressWarnings(read_assessment_factors())
af_child_abuse_extra_filter <- assessment_factors %>%
  filter(str_detect(assessment_factor, "Abuse|abuse|Neglect|neglect")) %>%
  select(assessment_factor) %>%
  pull("assessment_factor")

extra_familial_harm_af <- c("Missing", "Child sexual exploitation", "Trafficking", "Gangs", "Child criminal exploitation")
# "Alcohol Misuse child", "Drug Misuse child", "Missing", "Child sexual exploitation", "Trafficking", "Gangs", "Child criminal exploitation"

# Read in stats neighbours
stats_neighbours <- head(statistical_neighbours(), 152)

# Dropdowns
# choice_breakdown_level <- workforce_data %>% select(geographic_level) %>% filter(geographic_level != "National")%>% distinct()
# choices_LA <- workforce_data %>% filter(geographic_level == "Local authority") %>% select()

dropdown_choices <- cla_rates # %>%
#   mutate(geo_breakdown = case_when(
#     geographic_level == "National" ~ "National",#NA_character_,
#     geographic_level == "Regional" ~ region_name,
#     geographic_level == "Local authority" ~ la_name
#   )) %>%
#   select(geographic_level, geo_breakdown,turnover_rate_fte_perc,time_period,"time_period","turnover_rate_fte_perc", "absence_rate_fte_perc",
#          "agency_worker_rate_fte_perc", "agency_cover_rate_fte_perc", "vacancy_rate_fte_perc", "vacancy_agency_cover_rate_fte_perc",
#          "turnover_rate_headcount_perc", "agency_worker_rate_headcount_perc", "caseload_fte") %>% distinct()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TEMPLATE code
# Read in the data
# dfRevBal <- read_revenue_data()
# # Get geographical levels from data
# dfAreas <- dfRevBal %>%
#   select(
#     geographic_level, country_name, country_code,
#     region_name, region_code,
#     la_name, old_la_code, new_la_code
#   ) %>%
#   distinct()
#
# choicesLAs <- dfAreas %>%
#   filter(geographic_level == "Local authority") %>%
#   select(geographic_level, area_name = la_name) %>%
#   arrange(area_name)
#
# choicesAreas <- dfAreas %>%
#   filter(geographic_level == "National") %>%
#   select(geographic_level, area_name = country_name) %>%
#   rbind(dfAreas %>% filter(geographic_level == "Regional") %>% select(geographic_level, area_name = region_name)) %>%
#   rbind(choicesLAs)
#
# choicesYears <- unique(dfRevBal$time_period)
#
# choicesPhase <- unique(dfRevBal$school_phase)

expandable <- function(inputId, label, contents) {
  govDetails <- shiny::tags$details(
    class = "govuk-details", id = inputId,
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
