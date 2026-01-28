## Filtering logic for the dataset to aid time series plotting (and tables)
filter_time_series_data <- function(dataset_in,
                                    select_geographic_level,
                                    select_geo_breakdown,
                                    check_compare_national,
                                    check_compare_regional,
                                    check_compare_sn,
                                    dimensional_filters = list()) {
  # default values for testing
  # select_geographic_level <- "Local authority"
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



# Function designed to produce a filtered dataset for the LA View chart and table

filter_la_toggle_dataset <- function(dataset_in,
                                     select_geographic_level,
                                     select_geo_breakdown,
                                     select_time_period = NULL,
                                     dimensional_filters = list()) {
  # make the dataset into a data.table and take a copy so we don't tamper with underlying data
  setDT(dataset_in)
  dataset <- copy(dataset_in)

  # apply any dimensional filters for this dataset (e.g. characteristic, placement type, assessment factor)
  if (length(dimensional_filters) > 0) {
    dataset <- dataset[eval(AndEQUAL(dimensional_filters))]
  }

  if (is.null(select_time_period)) select_time_period <- max(dataset_in$time_period)

  # need logic for the 3 selections National, Regional, LA here
  if (select_geographic_level %in% c("Local authority", "National")) {
    dataset[geographic_level == "Local authority" & time_period == select_time_period]
  } else if (select_geographic_level == "Regional") {
    # get the location data
    # Check if the selected region is London
    if (select_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == select_geo_breakdown) %>%
        pull(la_name)
    }
    dataset[geographic_level == "Local authority" & geo_breakdown %in% location & time_period == select_time_period]
  }
}



filter_sn_toggle_dataset <- function(dataset_in,
                                     select_geographic_level,
                                     select_geo_breakdown,
                                     select_time_period = NULL,
                                     dimensional_filters = list()) {
  # test for LA selected


  # make the dataset into a data.table and take a copy so we don't tamper with underlying data
  setDT(dataset_in)
  dataset <- copy(dataset_in)

  # apply any dimensional filters for this dataset (e.g. characteristic, placement type, assessment factor)
  if (length(dimensional_filters) > 0) {
    dataset <- dataset[eval(AndEQUAL(dimensional_filters))]
  }

  if (is.null(select_time_period)) select_time_period <- max(dataset_in$time_period)

  # only one piece of logic here: select the chosen LA and the 10 Stat Neighbours
  dataset_in %>%
    filter(
      geographic_level == "Local authority",
      time_period == select_time_period,
      geo_breakdown %in% c(select_geo_breakdown, stat_neighbours_for_la(select_geo_breakdown))
    )
}


stat_neighbours_for_la <- function(selected_geo_breakdown) {
  sn_names <- stats_neighbours %>%
    filter(stats_neighbours$LA.Name == selected_geo_breakdown) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.character()

  return(sn_names)
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






# ===== for the LA table



#
#
# if (input$select_geography_e3 == "Regional") {
#   if (input$geographic_breakdown_e3 == "London") {
#     # Include both Inner London and Outer London
#     location <- location_data %>%
#       filter(region_name %in% c("Inner London", "Outer London")) %>%
#       pull(la_name)
#   } else {
#     # Get the la_name values within the selected region_name
#     location <- location_data %>%
#       filter(region_name == input$geographic_breakdown_e3) %>%
#       pull(la_name)
#   }
#
#   data <- workforce_data %>%
#     filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
#     select(time_period, geo_breakdown, "Caseload Fte") %>%
#     arrange(desc(`Caseload Fte`)) %>%
#     rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")
# } else if (input$select_geography_e3 %in% c("Local authority", "National")) {
#   data <- workforce_data %>%
#     filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
#     select(time_period, geo_breakdown, "Caseload Fte") %>%
#     arrange(desc(`Caseload Fte`)) %>%
#     rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")
# }
