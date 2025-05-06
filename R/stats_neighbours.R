sn_aggregations <- function(sn_long,
                            dataset,
                            sum_cols,
                            median_cols,
                            group_cols = c("LA.number", "time_period"),
                            aggregated_calc = "",
                            calc_name = "") {
  # tests


  # 1 get the mappings for SN -----
  # merge the datasetwith the SN mappings for LAs to aggregate
  sn_dataset <- merge(
    sn_long,
    dataset[geographic_level == "Local authority"],
    by.x = "SN_LA_number",
    by.y = "old_la_code",
    all.x = TRUE,
    allow.cartesian = TRUE
  )

  # 2 Defining functions and columns pertaining to aggregations -----
  agg_cols <- list(median_cols, sum_cols)
  funs <- rep(c(median, sum), lengths(agg_cols))
  new_cols <- unlist(agg_cols)

  # 3 now perform the aggregations to the LA and time_period level across the SNs -----
  # summary function to aggegate the dataset of metrics for SNs and calculate sums, means as required
  sn_agg <- sn_dataset[
    j = Map(function(f, x) f(as.numeric(x), na.rm = TRUE), funs, .SD),
    by = mget(group_cols),
    .SDcols = unlist(agg_cols)
  ]
  # we need to make sure the columns names with V1...V3 are renamed to the aggregated metrics we have calculated
  setnames(sn_agg, old = paste0("V", 1:length(new_cols)), new = new_cols)
  sn_agg[, geographic_level := "Statistical neighbours (median)"]

  # 4. final step to tidy up the dataset to return with certain key fields
  by.y <- c("old_la_code", group_cols[-1])
  cols_to_keep <- unique(c("geo_breakdown", "time_period", "old_la_code", by.y))
  sn_finalised <- merge(sn_agg, dataset[, .SD, .SDcols = cols_to_keep], by.x = group_cols, by.y = by.y)
  setnames(sn_finalised, c("LA.number"), "old_la_code")
  sn_finalised[, geo_breakdown_sn := geo_breakdown]
  sn_finalised[, geo_breakdown := "Statistical neighbours (median)"]

  # tidy up the data points before and after the LA values for special cases
  # North Northamptonshire start 2023
  # West Northamptonshire start 2023
  # Cumbria end 2023
  sn_finalised <- sn_finalised[!(geo_breakdown_sn %in% c("North Northamptonshire", "West Northamptonshire") & time_period < "2022")]
  sn_finalised <- sn_finalised[!(geo_breakdown_sn %in% c("Cumbria") & time_period > "2023")]

  return(sn_finalised)
}




## Examples need to move to testing
# TESTS: putting it all together
test_sn <- function(sn_long,
                    dataset,
                    sum_cols,
                    median_cols,
                    group_cols = c("LA.number", "time_period"),
                    select_geographic_level,
                    select_geo_breakdown,
                    check_compare_national = TRUE,
                    check_compare_regional = TRUE,
                    check_compare_sn = TRUE,
                    dimensional_filters = list(),
                    verbose = TRUE) {
  setDT(dataset)
  sn_metrics <- sn_aggregations(stats_neighbours_long,
    dataset = dataset,
    sum_cols = sum_cols,
    median_cols = median_cols,
    group_cols = group_cols
  )


  if (verbose == TRUE) print(sn_metrics)

  # now add the computed metrics to the original dataset
  dataset_with_sn <- rbindlist(l = list(dataset, sn_metrics), fill = TRUE)


  filter_time_series_data(
    dataset = dataset_with_sn,
    select_geographic_level = select_geographic_level,
    select_geo_breakdown = select_geo_breakdown,
    check_compare_national = check_compare_national,
    check_compare_regional = check_compare_regional,
    check_compare_sn = check_compare_sn,
    dimensional_filters = dimensional_filters
  )
}

## Filtering logic for the dataset to aid time series plotting (and tables)
filter_time_series_data <- function(dataset_in,
                                    select_geographic_level,
                                    select_geo_breakdown,
                                    check_compare_national,
                                    check_compare_regional,
                                    check_compare_sn,
                                    dimensional_filters = list()) {
  # default values for testing
  # elect_geographic_level <- "Local authority"
  # select_geo_breakdown <- "Merton"
  # dimensional_filters <- list("characteristic" = "Special guardianship orders")

  # make the dataset into a data.table and take a copy so we don't tamper with underlying data
  setDT(dataset_in)
  dataset <- copy(dataset_in)

  # apply any dimensional filters for this dataset (e.g. characteristic, placement type, assessment factor)
  if (length(dimensional_filters) > 0) {
    dataset <- dataset[eval(AndEQUAL(dimensional_filters))]
  }

  # now filter using the selects geo_breakdown and level - these dropdowns are the primary filtering geography, others may be added later
  filtered_data <- dataset[geographic_level == select_geographic_level & geo_breakdown == select_geo_breakdown]

  # now add in the rows for national, regional, SN comparisons
  if (!is.null(check_compare_national)) {
    filtered_data <- rbindlist(l = list(filtered_data, dataset[geographic_level == "National"]))
  }
  if (!is.null(check_compare_regional)) {
    # get the region from the LA and apply additional filtering
    location <- location_data %>%
      filter(la_name %in% select_geo_breakdown) %>%
      pull(region_name)
    if (length(location == 1)) {
      filtered_data <- rbindlist(l = list(filtered_data, dataset[geographic_level == "Regional" & geo_breakdown == location]))
    }
  }
  if (!is.null(check_compare_sn)) {
    filtered_data <- rbindlist(
      l = list(
        filtered_data,
        dataset[geographic_level == "Statistical neighbours (median)" & geo_breakdown_sn == select_geo_breakdown]
      )
    )
  }

  filtered_data[geographic_level == "Statistical neighbours (median)", geo_breakdown := "Statistical neighbours (median)"]
  filtered_data <- filtered_data[order(-time_period, factor(geographic_level, levels = c("National", "Regional", "Local authority", "Statistical neighbours (median)")))]

  return(filtered_data)
}




AndIN <- function(cond) {
  Reduce(
    function(x, y) call("&", call("(", x), call("(", y)),
    lapply(names(cond), function(var) call("%in%", as.name(var), cond[[var]]))
  )
}
AndEQUAL <- function(cond) {
  Reduce(
    function(x, y) call("&", call("(", x), call("(", y)),
    lapply(names(cond), function(var) call("==", as.name(var), cond[[var]]))
  )
}


AndISNA <- function(cond) {
  Reduce(
    function(x, y) call("&", call("(", x), call("(", y)),
    lapply(names(cond), function(var) call("is.na", as.name(var), cond[[var]]))
  )
}
