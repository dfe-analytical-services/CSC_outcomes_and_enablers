library(shinytest2)

test_that("{shinytest2} recording: CSC_outcomes_and_enablers_error_test", {
  app <- AppDriver$new(
    variant = platform_variant(), name = "CSC_outcomes_and_enablers_error_test",
    height = 886, width = 1203
  )
  app$set_inputs(navlistPanel = "outcome1_page")
  app$set_window_size(width = 1203, height = 886)
  app$set_inputs(select_geography_o1 = "")
  app$set_inputs(select_geography_o1 = "Regional")
  app$set_inputs(geographic_breakdown_o1 = "")
  app$expect_values()
  app$expect_screenshot()
})
