library(data.table)
source("../../R/data_pipeline_functions.R", local = TRUE)

test_that("field_diffs returns added and removed rows when there are no value columns", {
  csd <- data.table(
    la_name = c("Barnsley", "Barnsley"),
    new_la_code = c("E08000038", "E08000016"),
    new = c("NEW", NA),
    old = c(NA, "OLD")
  )

  result <- field_diffs(csd)

  expect_equal(nrow(result), 2)
  expect_setequal(result$new_or_old, c("NEW", "OLD"))
  expect_false("new_or_old" %in% names(csd))
})

test_that("field_diffs still compares old and new values when value columns exist", {
  csd <- data.table(
    la_name = "Barnsley",
    count_newval = 10,
    count_oldval = 8,
    new = "NEW",
    old = "OLD",
    new_or_old = "BOTH"
  )

  result <- field_diffs(csd)

  expect_equal(nrow(result), 1)
  expect_equal(result$variable_clean, "count")
  expect_equal(result$NEW, 10)
  expect_equal(result$OLD, 8)
})
