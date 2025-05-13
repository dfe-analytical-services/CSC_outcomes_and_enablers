source("./R/read_data.R")

test_that("Clean date tests", {
  m1 <- data.frame()
  expect_error(clean_date(m1))
  m2 <- data.frame(time_period = "202223")
  expect_equal(clean_date(m2)$time_period[1], "2022/23")
})

test_that("Decimal rounding", {
  expect_warning(decimal_rounding("x", 0))
  expect_equal(decimal_rounding("x", 0), "x")
  expect_equal(decimal_rounding("100.5", 0), "101")
  expect_equal(decimal_rounding("101.5", 0), "102")
  expect_equal(decimal_rounding("101.4", 0), "101")
  expect_equal(decimal_rounding("100.55", 1), "100.6")
  expect_equal(decimal_rounding("100.54", 1), "100.5")
  expect_equal(decimal_rounding(100.111, 1), "100.1") # test with a numeric passed in
  expect_equal(decimal_rounding(NA, 1), NA)
  # need to add error handling in the function
  expect_error(decimal_rounding(100.11))
  expect_error(decimal_rounding(100, NULL))
})

test_that("insert_geo_breakdown", {
  m1 <- data.table(
    geographic_level = c("National", "Regional", "Local authority", "XX"),
    region_name = c("", "Region_1", "Region_1", "YY"),
    la_name = c("", "", "LA_1", "ZZ")
  )
  expect_equal(insert_geo_breakdown(m1)$geo_breakdown, c("National", "Region_1", "LA_1", NA))
  m2 <- data.table()
  expect_error(insert_geo_breakdown(m2))
})


test_that("redacted_to_negative", {
  m1 <- data.table(a = c("c", "23", "22.22", "NA", NA, "XXX"))
  # redacted_to_negative(m1, col_old <- "a", col_new = "b", )
})
