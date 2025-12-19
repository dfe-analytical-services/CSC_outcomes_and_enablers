# source("./global.R")
# source("./R/stats_neighbours.R")

## these are the tests for Stats Neighbours
if (TRUE == FALSE) {
  select_geographic_level <- "Local authority"
  select_geo_breakdown <- "Barking and Dagenham"


  # TESTS: SN wrapper function

  # 1 test for cin_rates dataset ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = cin_rates,
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      sum_cols = c("CIN_number"),
      median_cols = c("CIN_rate"),
      group_cols = c("LA.number", "time_period"),
      dimensional_filters = list(),
      verbose = FALSE
    )
  }
  x
  names(x)


  # 2 test for cin_referral_data ----
  dimensional_filters <- list()
  x <- {
    test_sn(stats_neighbours_long,
      dataset = cin_referrals,
      median_cols = c("Re_referrals_percent"),
      sum_cols = c("Referrals", "Re_referrals"),
      group_cols = c("LA.number", "time_period"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = dimensional_filters,
      verbose = FALSE
    )
  }
  x
  names(x)


  # 3,4 test for cla_rates dataset ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = cla_rates,
      mean_cols = c("Rate Per 10000"),
      sum_cols = c("population_estimate", "Number"),
      group_cols = c("LA.number", "time_period", "population_count"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      # dimensional_filters = list("population_count" = "Children looked after at 31 March each year"),
      dimensional_filters = list("population_count" = "Children starting to be looked after each year"),
      verbose = FALSE
    )
  }
  x
  names(x)

  # 5,6 test for outcomes_absence dataset ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = outcomes_absence,
      mean_cols = c(),
      sum_cols = c("Total pupils", "t_sess_overall", "t_sess_possible"),
      group_cols = c("LA.number", "time_period", "school_type", "social_care_group"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("social_care_group" = "CINO at 31 March", "school_type" = "Total"),
      # dimensional_filters = list("social_care_group" = "CINO at 31 March", "school_type" = "Total"),
      # dimensional_filters = list("social_care_group" = "CINO at 31 March", "school_type" = "Total"),
      verbose = FALSE
    )
  }
  x
  names(x)

  # 7 test for outcomes_ks2 ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = outcomes_ks2,
      mean_cols = c(),
      sum_cols = c("t_rwm_met_expected_standard", "t_rwm_eligible_pupils"),
      group_cols = c("LA.number", "time_period", "social_care_group"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("social_care_group" = "CINO at 31 March"),
      verbose = FALSE
    )
  }
  x
  names(x)

  # 8 test for outcomes_ks4 ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = outcomes_ks4,
      mean_cols = c(),
      sum_cols = c("t_pupils", "t_att8"),
      group_cols = c("LA.number", "time_period", "social_care_group"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("social_care_group" = "CINO at 31 March"),
      verbose = FALSE
    )
  }
  x
  names(x)


  # 9,10 test for ceased_cla_data ----
  dimensional_filters <- list("characteristic" = "Special guardianship order")
  dimensional_filters <- list("characteristic" = "Residence order or child arrangement order granted")
  x <- {
    test_sn(stats_neighbours_long,
      dataset = ceased_cla_data,
      mean_cols = c(),
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

  # 11 test for repeat_cpp ----

  x <- {
    test_sn(stats_neighbours_long,
      dataset = repeat_cpp,
      mean_cols = c(),
      sum_cols = c("CPP_start", "CPP_subsequent"),
      group_cols = c("LA.number", "time_period"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list(),
      verbose = FALSE
    )
  }
  x
  names(x)

  # 12 test for duration_cpp ----

  x <- {
    test_sn(stats_neighbours_long,
      dataset = duration_cpp,
      mean_cols = c(),
      sum_cols = c("CPP_At31", "X2_years_or_more"),
      group_cols = c("LA.number", "time_period"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list(),
      verbose = FALSE
    )
  }
  x
  names(x)



  # 13,14 test for assessment_factors ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = assessment_factors,
      mean_cols = c(),
      sum_cols = c("Number", "population_estimate"),
      group_cols = c("LA.number", "time_period", "assessment_factor"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("assessment_factor" = "Neglect"),
      verbose = FALSE
    )
  }
  x
  names(x)

  # 15 test for placement_changes_data (placement_stability)----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = placement_changes_data,
      mean_cols = c(),
      sum_cols = c("Number", "total_number"),
      group_cols = c("LA.number", "time_period", "cla_group", "placement_stability"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("placement_stability" = "With 3 or more placements during the year"),
      verbose = FALSE
    )
  }
  x
  names(x)

  # 16 test for placement_data (placed > 20 miles from home) ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = placement_data,
      mean_cols = c(),
      sum_cols = c("number", "total_number"),
      group_cols = c("LA.number", "time_period", "cla_group", "characteristic"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("cla_group" = "Distance between home and placement", "characteristic" = "Placed more than 20 miles from home"),
      verbose = FALSE
    )
  }
  x
  names(x)

  # 17 test for placement_data (Children living in selected placement type (%)) ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = placement_data,
      mean_cols = c(),
      sum_cols = c("number", "total_number"),
      group_cols = c("LA.number", "time_period", "cla_group", "characteristic"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("cla_group" = "Placement", "characteristic" = "Foster placements"),
      verbose = FALSE
    )
  }
  x
  names(x)


  # 18 test for wellbeing_sdq_data ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = wellbeing_sdq_data,
      mean_cols = c(),
      sum_cols = c("sdq_score_recd", "sdq_score_recd_x_score"),
      group_cols = c("LA.number", "time_period", "cla_group", "characteristic"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("characteristic" = "SDQ average score"),
      verbose = FALSE
    )
  }
  cols_to_update <- c("number", "number_num")
  x[geographic_level == "Statistical neighbours (median)", (cols_to_update) := eval(parse(text = "round(sdq_score_recd_x_score / sdq_score_recd, digits = 1)"))]
  x
  names(x)

  # 19 test for care_leavers_activity_data ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = care_leavers_activity_data,
      mean_cols = c(),
      sum_cols = c("number", "total_number"),
      group_cols = c("LA.number", "time_period", "age", "activity"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("age" = "19 to 21 years", "activity" = "Total in education, employment or training"),
      verbose = FALSE
    )
  }

  cols_to_update <- c("percentage", "percent")
  x[geographic_level == "Statistical neighbours (median)", (cols_to_update) := eval(parse(text = "round(100 * as.numeric(number) / total_number, digits = 0)"))]
  x
  names(x)

  # 20 test for care_leavers_accommodation_data ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = care_leavers_accommodation_data,
      mean_cols = c(),
      sum_cols = c("number", "total_number"),
      group_cols = c("LA.number", "time_period", "age", "accommodation_suitability"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("age" = "19 to 21 years", "accommodation_suitability" = "Accommodation considered suitable"),
      verbose = FALSE
    )
  }

  cols_to_update <- c("percentage", "percent")
  x[geographic_level == "Statistical neighbours (median)", (cols_to_update) := eval(parse(text = "round(100 * as.numeric(number) / total_number, digits = 0)"))]
  x
  names(x)

  # 21 test for workforce_data ----
  x <- {
    test_sn(stats_neighbours_long,
      dataset = workforce_data,
      mean_cols = c(),
      sum_cols = c("number", "total_number"),
      group_cols = c("LA.number", "time_period", "age", "accommodation_suitability"),
      select_geographic_level = select_geographic_level,
      select_geo_breakdown = select_geo_breakdown,
      dimensional_filters = list("age" = "19 to 21 years", "accommodation_suitability" = "Accommodation considered suitable"),
      verbose = FALSE
    )
  }

  cols_to_update <- c("percentage", "percent")
  x[geographic_level == "Statistical neighbours (median)", (cols_to_update) := eval(parse(text = "round(100 * as.numeric(number) / total_number, digits = 0)"))]
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
    aggregated_calc = aggregated_calc,
    calc_name = calc_name
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
  sn_metrics[characteristic == "Special guardianship order"]
  sn_metrics[characteristic == "Residence order or child arrangement order granted"]


  # ===============================================================================

  # TESTS: filtering datasets ----

  # default values for testing
  select_geographic_level <- "Local authority"
  select_geo_breakdown <- "Merton"
  dimensional_filters <- list("characteristic" = "Special guardianship order")

  ### CLA rates ----

  filter_time_series_data(
    dataset_in = cla_rates,
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
    dimensional_filters = list(characteristic = "Special guardianship order")
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

  ### placement stability
  filtered_data <- filter_time_series_data(
    dataset_in = placement_changes_data,
    select_geographic_level = "Local authority",
    select_geo_breakdown = "Merton",
    check_compare_national = TRUE,
    check_compare_regional = TRUE,
    check_compare_sn = TRUE,
    dimensional_filters = list("placement_stability" = "With 3 or more placements during the year")
  )




  mean(c(1, 2, 3, NA), na.rm = TRUE)



  # write.csv(sn_metrics, file = "c:/Users/mweller1/Documents/projects/CSC_outcomes_and_enablers/sn_metrics.csv")

  sn_metrics[LA.number == 207]
}
