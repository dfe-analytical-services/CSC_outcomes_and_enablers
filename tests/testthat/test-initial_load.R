test_that("{shinytest2} recording: CSC_outcomes_and_enablers_error_test", {
  app <- AppDriver$new(
    name = "CSC_outcomes_and_enablers_initial_load",
    load_timeout = 240 * 1000,
    timeout = 120 * 1000,
    wait = FALSE,
    height = 886,
    width = 1203,
    variant = NULL,
    expect_values_screenshot_args = FALSE
  )

  # Wait until Shiny is not busy for 500ms
  app$wait_for_idle(500)

  # Screenshots are left on for this script to help with troubleshooting
  # They will not cause any failures if there's changes
  app$expect_values(
    input = c("navlistPanel"),
    output = c("outcome1_choice_text1", "outcome1_choice_text2")
  )
})
