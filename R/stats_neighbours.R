library(data.table)

source("global.R")
# lookup <- stats_neighbours %>% select(SN_LA_name = "LA.Name", SN_LA_number = "LA.number")

stats_neighbours_long <- stats_neighbours %>%
  pivot_longer(
    cols = starts_with("SN"),
    names_to = c("SN_rank"),
    names_pattern = "(\\d+)",
    values_to = "SN_LA_name"
  ) %>%
  left_join(stats_neighbours %>% select(SN_LA_name = "LA.Name", SN_LA_number = "LA.number")) %>%
  setDT()


#
# 1. rounding?
# 2. which LA fields and datasets to map with? Assumption is on old_la_code
# 3. what to do with SN LA codes which are not in the dataset. for example, Dag includes Barnet as a SN but we have no data for that LA
# 4. what about numbers stored as characters? we can't aggregate these!


sn_aggregations <- function(stats_neighbours_long,
                            dataset,
                            sum_cols,
                            mean_cols,
                            group_cols = c("LA.number", "time_period"),
                            aggregated_calc = "",
                            calc_name = "") {
  setDT(dataset)

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
  agg_cols <- list(mean_cols, sum_cols)
  funs <- rep(c(mean, sum), lengths(agg_cols))
  new_cols <- unlist(agg_cols)

  # 3 managing redacted values -----


  # step to remove an LA from SN calculations if it has ANY redacted values
  # then report the redactions
  # then look for how much of an impact losing only 1 would be
  # redacted_las <- get_redacted_las(sn_dataset, new_cols, group_cols)


  # 4 now perform the aggregations to the LA and time_period level across the SNs -----
  # summary function to aggegate the dataset of metrics for SNs and calculate sums, means as required
  sn_agg <- sn_dataset[
    j = Map(function(f, x) ceiling(f(as.numeric(x), na.rm = TRUE)), funs, .SD),
    by = mget(group_cols),
    .SDcols = unlist(agg_cols)
  ]

  # we need to make sure the columns names with V1...V3 are renamed to the aggregated metrics we have calculated
  setnames(sn_agg, old = paste0("V", 1:length(new_cols)), new = new_cols)

  # TODO: populate the character columns which are displayed in the table

  # now perform any additional calculations, defining the calculation and the field name (ie LHS and RHS of `:=`)
  if (calc_name != "") sn_metrics[, eval(quote(calc_name)) := round(eval(aggregated_calc))]

  sn_agg[, geographic_level := "Statistical neighbours"]

  cols_to_keep <- ""
  sn_finalised <- merge(sn_agg, dataset[, .(geo_breakdown, time_period, old_la_code, new_la_code, la_name)], by.x = group_cols, by.y = c("old_la_code", group_cols[-1]))
  setnames(sn_finalised, c("LA.number"), "old_la_code")

  sn_finalised
}


## Function to prepare the data for appending to the existing dataset
names(dataset)
names(sn_metrics)

sn_metrics[]

## Examples



# TESTS: SN Aggregations

# CIN ----
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




mean(c(1, 2, 3, NA), na.rm = TRUE)



write.csv(sn_metrics, file = "c:/Users/mweller1/Documents/projects/CSC_outcomes_and_enablers/sn_metrics.csv")

sn_metrics[LA.number == 207]


## Filtering logic for the dataset to aid plotting (and tables hopefully)
filter_time_series_data <- function(dataset, select_geographic_level, select_geo_breakdown, check_compare_national, check_compare_regional, check_compare_sn, dimensional_filters = list()) {
  # default values for testing
  # elect_geographic_level <- "Local authority"
  # select_geo_breakdown <- "Merton"
  # dimensional_filters <- list("characteristic" = "Special guardianship orders")

  # make the dataset into a data.table and take a copy so we don't tamper with underlying data
  setDT(dataset)
  dataset <- copy(dataset)

  # apply any dimensional filters for this dataset (e.g. characteristic, placement type, assessment factor)
  if (length(dimensional_filters) > 0) {
    dataset <- dataset[eval(AndEQUAL(dimensional_filters))]
  }

  # now filter using the selects geo_breakdown and level - these dropdowns are the primary filtering geography, others may be added later
  filtered_data <- dataset[geographic_level == select_geographic_level & geo_breakdown == select_geo_breakdown]

  # now add in the rows for national, regional, SN comparisons
  if (check_compare_national == TRUE) {
    filtered_data <- rbindlist(l = list(filtered_data, dataset[geographic_level == "National"]))
  }
  if (check_compare_regional == TRUE) {
    location <- location_data %>%
      filter(la_name %in% select_geo_breakdown)
    filtered_data <- rbindlist(l = list(filtered_data, dataset[geographic_level == "Regional" & geo_breakdown == location$region_name]))
  }
  if (check_compare_sn == TRUE) {
    # to note here, the geo_breakdown for SN records do not contain the value 'Statistical neighbour' rather the LA name
    filtered_data <- rbindlist(l = list(filtered_data, dataset[geographic_level == "Statistical neighbours" & geo_breakdown == select_geo_breakdown]))
  }

  return(filtered_data)
}

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

### CLA filtering ----
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



### OLD CODE

output$outcome1_choice_text2 <- renderText({
  # Checking to see if they picked national average comparison
  if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
    paste0("You have also selected to compare with the ", tags$b("National Average."))
    # If they picked regional comparison
  } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
    paste0("You have also selected to compare with the ", tags$b("Regional average."))
    # Picked both national and regional comparison
  } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
    paste0("You have also selected to compare with the ", tags$b("National average"), " and the ", tags$b("Regional average."))
  }
})



# Analysis of redacted values in SNs -----


get_redacted_las <- function(sn_dataset, new_cols, group_cols) {
  la_redactions <- sn_dataset[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = new_cols, by = LA.number] # by = mget(group_cols)
  la_redactions[, row_sums := rowSums(.SD), .SDcols = new_cols]
  la_redactions[row_sums > 0]$LA.number
}

sn_dataset[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = new_cols, by = mget(group_cols)]


sn_dataset[, Reduce(is.na, .SD), .SDcols = new_cols]
for (col in new_cols) print(table(redacted_las[[col]]))



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
cond <- list(characteristic = "Special guardianship orders")
cond <- list(new_la_code = "E09000024")


dataset[eval(AndEQUAL(cond))]


AndISNA <- function(cond) {
  Reduce(
    function(x, y) call("&", call("(", x), call("(", y)),
    lapply(names(cond), function(var) call("is.na", as.name(var), cond[[var]]))
  )
}
cond <- list(x = 0, z = 0)
AndEQUAL(cond)
dataset
