## these are the tests for Stats Neighbours

select_geographic_level <- "Local authority"
select_geo_breakdown <- "Barking and Dagenham"


# TESTS: SN wrapper function

# test for cin_rates dataset
x <- {
  test_sn(stats_neighbours_long,
    dataset = cin_rates,
    select_geographic_level = select_geographic_level,
    select_geo_breakdown = select_geo_breakdown,
    sum_cols = c("CIN_number"),
    mean_cols = c("CIN_rate"),
    group_cols = c("LA.number", "time_period"),
    dimensional_filters = list(),
    verbose = FALSE
  )
}
x
names(x)
x

# test for cla_rates dataset
x <- {
  test_sn(stats_neighbours_long,
    dataset = cla_rates,
    mean_cols = c("Rate Per 10000"),
    sum_cols = c("population_estimate", "Number"),
    group_cols = c("LA.number", "time_period", "population_count"),
    select_geographic_level = select_geographic_level,
    select_geo_breakdown = select_geo_breakdown,
    dimensional_filters = list("population_count" = "Children looked after at 31 March each year"),
    verbose = FALSE
  )
}
x
names(x)

# test for ceased_cla_data dataset
dimensional_filters <- list("characteristic" = "Special guardianship orders")
x <- {
  test_sn(stats_neighbours_long,
    dataset = ceased_cla_data,
    mean_cols = c("Ceased (%)"),
    sum_cols = c("Number ceased", "Total_num"),
    group_cols = c("LA.number", "time_period", "characteristic"),
    select_geographic_level = select_geographic_level,
    select_geo_breakdown = select_geo_breakdown,
    dimensional_filters = dimensional_filters,
    verbose = FALSE
  )
}
x
names(x)




# ===================================================================================
# TESTS: sn aggregation function

# dataset = cin_rates
# sum_cols = c("CIN_number"),
# mean_cols = c("CIN_rate"),
# group_cols = c("LA.number", "time_period")


## testing the metrics creation/agregation ----
### CIN ----
sn_metrics <- sn_aggregations(stats_neighbours_long,
  dataset = cin_rates,
  sum_cols = c("CIN_number"),
  mean_cols = c("CIN_rate"),
  group_cols = c("LA.number", "time_period")
)

# CLA rates ----
aggregated_calc <- parse(text = paste0("10000 * Number/population_estimate"))
calc_name <- "calc_rate"
sn_metrics <- sn_aggregations(stats_neighbours_long,
  dataset = cla_rates,
  mean_cols = c("Rate Per 10000"),
  sum_cols = c("population_estimate", "Number"),
  group_cols = c("LA.number", "time_period", "population_count"),
  aggregated_calc = "",
  calc_name = ""
)
if (calc_name != "") sn_metrics[, eval(quote(calc_name)) := round(eval(aggregated_calc))]
sn_metrics


# ceased_CLA-data ----
aggregated_calc <- parse(text = paste0("100 * `Number ceased`/Total_num"))
calc_name <- "calc_perc"
sn_metrics <- sn_aggregations(stats_neighbours_long,
  dataset = ceased_cla_data,
  mean_cols = c("Ceased (%)"),
  sum_cols = c("Number ceased", "Total_num"),
  group_cols = c("LA.number", "time_period", "characteristic")
) # ,
# aggregated_calc,
# calc_name)
if (calc_name != "") sn_metrics[, eval(quote(calc_name)) := round(eval(aggregated_calc))]
sn_metrics[characteristic == "Special guardianship orders"]
sn_metrics[characteristic == "Residence order or child arrangement order granted"]


# ===============================================================================

# TESTS: filtering datasets ----

# default values for testing
select_geographic_level <- "Local authority"
select_geo_breakdown <- "Merton"
dimensional_filters <- list("characteristic" = "Special guardianship orders")

### CLA rates ----

filter_time_series_data(
  dataset = cla_rates,
  select_geographic_level = select_geographic_level,
  select_geo_breakdown = select_geo_breakdown,
  check_compare_national = TRUE,
  check_compare_regional = TRUE,
  check_compare_sn = TRUE,
  dimensional_filters = list("population_count" = "Children starting to be looked after each year")
)
### CIN filtering ----
filter_time_series_data(
  dataset = cin_rates,
  select_geographic_level = select_geographic_level,
  select_geo_breakdown = select_geo_breakdown,
  check_compare_national = TRUE,
  check_compare_regional = TRUE,
  check_compare_sn = TRUE,
  dimensional_filters = list()
)

### ceased CLA filtering ----
filter_time_series_data(
  dataset = ceased_cla_data,
  select_geographic_level = select_geographic_level,
  select_geo_breakdown = select_geo_breakdown,
  check_compare_national = TRUE,
  check_compare_regional = TRUE,
  check_compare_sn = TRUE,
  dimensional_filters = list(characteristic = "Special guardianship orders")
)


### Repeat referrals
filter_time_series_data(
  dataset = cin_referrals,
  select_geographic_level = select_geographic_level,
  select_geo_breakdown = select_geo_breakdown,
  check_compare_national = TRUE,
  check_compare_regional = TRUE,
  check_compare_sn = TRUE,
  dimensional_filters = list()
)

### Repeat referrals
filter_time_series_data(
  dataset = outcomes_absence,
  select_geographic_level = select_geographic_level,
  select_geo_breakdown = select_geo_breakdown,
  check_compare_national = TRUE,
  check_compare_regional = TRUE,
  check_compare_sn = TRUE,
  dimensional_filters = list("school_type" = "", "social_care_group" = "")
)






mean(c(1, 2, 3, NA), na.rm = TRUE)



# write.csv(sn_metrics, file = "c:/Users/mweller1/Documents/projects/CSC_outcomes_and_enablers/sn_metrics.csv")

sn_metrics[LA.number == 207]
