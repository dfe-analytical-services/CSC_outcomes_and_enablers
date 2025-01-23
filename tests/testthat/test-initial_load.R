app <- AppDriver$new(
  name = "basic_load",
  height = 846, width = 1445,
  load_timeout = 240 * 1000,
  timeout = 120 * 1000,
  wait = TRUE
)

# Wait until Shiny is not busy for 500ms
app$wait_for_idle(500)

# Screenshots are left on for this script to help with troubleshooting
# They will not cause any failures if there's changes

test_that("App loads", {
  # Capture initial values
  app$expect_values(input = c("navlistPanel"), output = c("outcome1_choice_text1", "outcome1_choice_text2"))
})
