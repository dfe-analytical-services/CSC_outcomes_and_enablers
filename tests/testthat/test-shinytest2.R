test_that("{shinytest2} recording: CSC_outcomes_and_enablers_error_test", {
  app <- AppDriver$new(
    name = "CSC_outcomes_and_enablers_error_test",
    load_timeout = 50 * 1000,
    timeout = 25 * 1000,
    wait = TRUE,
    height = 886, width = 1203,
    variant = NULL,
    expect_values_screenshot_args = FALSE
  )
  app$set_inputs(navlistPanel = "outcome1_page")
  app$set_window_size(width = 1203, height = 886)
  app$set_inputs(select_geography_o1 = "")
  app$set_inputs(select_geography_o1 = "Regional")
  app$set_inputs(geographic_breakdown_o1 = "East Midlands")
  app$wait_for_idle(500)
  app$expect_values()
})
