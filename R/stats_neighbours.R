#
# 1. rounding? where to do the rounding and what would be the effect of rounding if we go down the lay route of averaging rounded numbers....
# 2. which LA fields and datasets to map with? Assumption is on old_la_code
# 3. what to do with SN LA codes which are not in the dataset. for example, Dag includes Barnet as a SN but we have no data for that LA
# 4. what about numbers stored as characters? we can't aggregate these!


sn_aggregations <- function(stats_neighbours_long,
                            dataset,
                            sum_cols,
                            median_cols,
                            group_cols = c("LA.number", "time_period"),
                            aggregated_calc = "",
                            calc_name = "") {
  # setDT(dataset)

  # 1 get the mapping for SN -----
  # merge the SN definitions with the dataset for LAs to aggregate
  sn_dataset <- merge(
    stats_neighbours_long,
    dataset[geographic_level == "Local authority"],
    by.x = "SN_LA_number",
    by.y = "old_la_code",
    all.x = TRUE,
    allow.cartesian = TRUE
  )

  # TASK: we should check for missing data here: what if an LA listed as a SN doesn't have metrics?
  # QUESTION: ceiling or round for the numbers where decimals are introduced through averaging

  # 2 Defining functions and columns pertaining to aggregations -----
  agg_cols <- list(median_cols, sum_cols)
  funs <- rep(c(median, sum), lengths(agg_cols))
  new_cols <- unlist(agg_cols)

  ## 3 managing redacted values -----


  # step to remove an LA from SN calculations if it has ANY redacted values
  # then report the redactions
  # then look for how much of an impact losing only 1 would be
  # redacted_las <- get_redacted_las(sn_dataset, new_cols, group_cols)


  # 4 now perform the aggregations to the LA and time_period level across the SNs -----
  # summary function to aggegate the dataset of metrics for SNs and calculate sums, means as required
  sn_agg <- sn_dataset[
    j = Map(function(f, x) f(as.numeric(x), na.rm = TRUE), funs, .SD),
    by = mget(group_cols),
    .SDcols = unlist(agg_cols)
  ]

  # we need to make sure the columns names with V1...V3 are renamed to the aggregated metrics we have calculated
  setnames(sn_agg, old = paste0("V", 1:length(new_cols)), new = new_cols)

  # TODO: populate the character columns which are displayed in the table

  # now perform any additional calculations, defining the calculation and the field name (ie LHS and RHS of `:=`)
  # if (calc_name != "") sn_metrics[, eval(quote(calc_name)) := round(eval(aggregated_calc)), verbose = TRUE]

  sn_agg[, geographic_level := "Statistical neighbours"]
  by.y <- c("old_la_code", group_cols[-1])
  cols_to_keep <- unique(c("geo_breakdown", "time_period", "old_la_code", by.y))
  dataset[, .SD, .SDcols = cols_to_keep]

  sn_finalised <- merge(sn_agg, dataset[, .SD, .SDcols = cols_to_keep], by.x = group_cols, by.y = by.y)
  setnames(sn_finalised, c("LA.number"), "old_la_code")
  sn_finalised[, geo_breakdown_sn := geo_breakdown]
  sn_finalised[, geo_breakdown := "Statistical neighbours"]

  sn_finalised
}




## Examples
# TESTS: putting it all together
test_sn <- function(stats_neighbours_long,
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



## Function to prepare the data for appending to the existing dataset
## Filtering logic for the dataset to aid plotting (and tables hopefully)
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
    location <- location_data %>%
      filter(la_name %in% select_geo_breakdown)
    filtered_data <- rbindlist(l = list(filtered_data, dataset[geographic_level == "Regional" & geo_breakdown == location$region_name]))
  }
  if (!is.null(check_compare_sn)) {
    filtered_data <- rbindlist(l = list(filtered_data, dataset[geographic_level == "Statistical neighbours" & geo_breakdown_sn == select_geo_breakdown]))
  }

  filtered_data[geographic_level == "Statistical neighbours", geo_breakdown := "Statistical neighbours"]

  return(filtered_data)
}



# Analysis of redacted values in SNs -----


get_redacted_las <- function(sn_dataset, new_cols, group_cols) {
  la_redactions <- sn_dataset[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = new_cols, by = LA.number] # by = mget(group_cols)
  la_redactions[, row_sums := rowSums(.SD), .SDcols = new_cols]
  la_redactions[row_sums > 0]$LA.number
}

# sn_dataset[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = new_cols, by = mget(group_cols)]
#
#
# sn_dataset[, Reduce(is.na, .SD), .SDcols = new_cols]
# for (col in new_cols) print(table(redacted_las[[col]]))



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
# cond <- list(x = 0, z = 0)
# AndEQUAL(cond)
