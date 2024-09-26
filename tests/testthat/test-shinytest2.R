test_that("{shinytest2} recording: CSC_outcomes_and_enablers_error_test", {
  app <- AppDriver$new(
    name = "CSC_outcomes_and_enablers_error_test",
    load_timeout = 32 * 1000,
    timeout = 16 * 1000,
    wait = TRUE,
    height = 886, width = 1203,
    variant = NULL,
    expect_values_screenshot_args = FALSE
  )
  app$set_inputs(navlistPanel = "outcome1_page")
  app$set_inputs(select_geography_o1 = "")
  app$set_inputs(select_geography_o1 = "Regional")
  app$set_inputs(geographic_breakdown_o1 = "East Midlands")
  app$wait_for_idle(500)
  Sys.sleep(2)
  app$expect_values(input = c("navlistPanel"), output = c("outcome1_choice_text1", "outcome1_choice_text2"))

  # Demo of using input and output flags to limit the range of shiny elements being
  # captured by a test.
  # Test 2: Switch to Outcomes page 2, select regional and check the national
  # comparison checkbox
  app$set_inputs(
    navlistPanel = "outcome2_page",
    select_geography_o2 = "Regional",
    national_comparison_checkbox_o2 = TRUE,
    outcome2_panels = "Families engaging and receiving support from their family network"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o2", "outcome2_panels",
      "national_comparison_checkbox_o2", "regional_comparison_checkbox_o2"
    ),
    output = c(
      "outcome2_choice_text1", "outcome2_choice_text2",
      "SGO_headline_txt", "CAO_headline_txt"
    )
  )

  # Outcome 3 - looking at local authority level and no checkboxes
  app$set_inputs(
    navlistPanel = "outcome3_page",
    select_geography_o3 = "Local authority",
    geographic_breakdown_o3 = "Barking and Dagenham",
    national_comparison_checkbox_o3 = FALSE,
    regional_comparison_checkbox_o3 = FALSE,
    outcome3_panels = "Child safety - general"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o3", "geographic_breakdown_o3", "outcome3_panels",
      "national_comparison_checkbox_o3", "regional_comparison_checkbox_o3"
    ),
    output = c(
      "outcome3_choice_text1", "outcome3_choice_text2", "cpp_in_year_txt", "cpp_duration_txt",
      "hosp_admissions_txt"
    )
  )

  # Outcome 4 - Looking at local authority level with national checkbox
  app$set_inputs(
    navlistPanel = "outcome4_page",
    select_geography_o4 = "Local authority",
    geographic_breakdown_o4 = "Bedford",
    national_comparison_checkbox_o4 = FALSE,
    regional_comparison_checkbox_o4 = TRUE,
    outcome4_panels = "Child wellbeing"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o3", "geographic_breakdown_o3", "outcome4_panels",
      "national_comparison_checkbox_o3", "regional_comparison_checkbox_o3"
    ),
    output = c("outcome4_choice_text1", "outcome4_choice_text2", "wellbeing_score_stat")
  )

  # Enabler 1 - Nothing here

  # Enabler 2 - Local authority (this page has no checkboxes)
  app$set_inputs(
    navlistPanel = "outcome4_page",
    select_geography_e2 = "Local authority",
    geographic_breakdown_e2 = "Bexley",
    enabler2_panels = "Spending"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_e2", "geographic_breakdown_e2", "enabler2_panels"
    ),
    output = c(
      "enabler2_choice_text1", "enabler2_choice_text2",
      "total_spending_txt", "avg_spend_per_child", "spend_minus_cla_txt"
    )
  )

  # Enabler 3 - Local authority with regional checkbox and national checkbox
  app$set_inputs(
    navlistPanel = "outcome4_page",
    select_geography_e2 = "Local authority",
    geographic_breakdown_e2 = "Medway",
    national_comparison_checkbox_e3 = TRUE,
    regional_comparison_checkbox_e3 = TRUE,
    enabler3_panels = "Social worker ethnicity"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_e3", "geographic_breakdown_e3", "enabler3_panels",
      "national_comparison_checkbox_e3", "regional_comparison_checkbox_e3"
    ),
    output = c("enabler3_choice_text1", "enabler3_choice_text2", "non_white_txt")
  )
})
