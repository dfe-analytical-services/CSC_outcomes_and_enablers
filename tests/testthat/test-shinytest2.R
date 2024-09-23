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
  app$expect_values()

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
})
