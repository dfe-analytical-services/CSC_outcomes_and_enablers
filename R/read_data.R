# Script where we provide functions to read in the data file(s).

# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.

# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.

dropList <- c("E10000009", "E10000021", "E06000028", "E06000029")

# Function to clean column names
colClean <- function(x) {
  colnames(x) <- gsub("\\.", "perc", colnames(x))
  x
}

# function to convert str columns into numerical columns
convert_perc_cols_to_numeric <- function(x) {
  suppressWarnings({
    perc_cols <- grep("fte", colnames(x))
    x[, perc_cols] <- apply(x[, perc_cols], 2, function(x) as.numeric(as.character(x)))
  })
  return(x)
}

clean_date <- function(dataset) {
  dataset <- dataset %>%
    mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
  return(dataset)
}

decimal_rounding <- function(value, digits) {
  if (!is.na(as.numeric(value))) {
    # value_round <- format(as.numeric(as.character(value)), nsmall = digits)
    value_round <- format(true_round(value, digits = digits), nsmall = digits)
  } else {
    value_round <- value
  }
  return(value_round)
}

true_round <- function(number, digits) {
  number <- as.numeric(number)
  posneg <- sign(number)
  number <- abs(number) * 10^digits
  number <- number + 0.5 + sqrt(.Machine$double.eps)
  number <- trunc(number)
  number <- number / 10^digits
  number * posneg
}

insert_geo_breakdown <- function(dataset) {
  dataset <- dataset %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    ))
  return(dataset)
}

# function to remove CUmbria from a dataset
remove_cumbria_data <- function(dataset) {
  if ("geo_breakdown" %in% names(dataset)) dataset %>% filter(geo_breakdown != "Cumbria")
}

redacted_to_negative <- function(dataset, col_old, col_new, copy_numeric_vals = TRUE) {
  # dataset <- data.table(a = c("c", "23", "22.22", "NA", NA))
  # col_old <- "a"
  # col_new <- "b"
  if (copy_numeric_vals == TRUE) {
    dataset <- dataset %>%
      mutate(!!sym(col_new) := case_when(
        !!sym(col_old) == "c" ~ -100,
        !!sym(col_old) == "low" ~ -200,
        !!sym(col_old) == "k" ~ -200,
        !!sym(col_old) == "u" ~ -250,
        !!sym(col_old) == "x" ~ -300,
        !!sym(col_old) == "z" ~ -400,
        TRUE ~ as.numeric(!!sym(col_old))
      ))
  } else {
    dataset <- dataset %>%
      mutate(!!sym(col_new) := case_when(
        !!sym(col_old) == "c" ~ -100,
        !!sym(col_old) == "low" ~ -200,
        !!sym(col_old) == "k" ~ -200,
        !!sym(col_old) == "u" ~ -250,
        !!sym(col_old) == "x" ~ -300,
        !!sym(col_old) == "z" ~ -400,
        TRUE ~ as.numeric(!!sym(col_new))
      ))
  }
  return(dataset)
}

redacted_to_na <- function(dataset, col_old, col_new) {
  dataset <- dataset %>%
    mutate(!!sym(col_new) := case_when(
      !!sym(col_old) %in% c("c", "x", "Z") ~ NA,
      TRUE ~ as.numeric(!!sym(col_old))
    ))
  return(dataset)
}

# Need a fact table for the LA's and their Regions
# cla_placements replaces raw file = "./data-raw/la_children_who_started_to_be_looked_after_during_the_year.csv" as a default
GET_location <- function(dataset = NULL) {
  if (is.null(dataset)) dataset <- copy(cla_placements)
  FACT_location <- dataset %>%
    filter(geographic_level == "Local authority") %>%
    select(region_name, geo_breakdown, new_la_code, old_la_code) %>%
    rename(la_name = geo_breakdown) %>%
    unique()
}

# Need a fact table for the LA's and their Regions for workforce data as they have LAs combined
GET_location_workforce <- function(dataset = NULL) { # file = "./data-raw/csww_indicators_2017_to_2024.csv"

  if (is.null(dataset)) stop()
  FACT_Location_workforce <- dataset %>%
    filter(geographic_level == "Local authority") %>%
    select(region_name, geo_breakdown) %>%
    rename(la_name = geo_breakdown) %>%
    unique() %>%
    setDF()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #### Statistical Neighbours read and convert to long format ------------

get_statistical_neighbours <- function(file = "./data-raw/sn_model_2025_wide.csv") {
  stats_neighbours_raw <- fread(file)

  # one-off hack to correct St Helens
  stats_neighbours_raw[la_name == "St Helens", la_name := "St. Helens"]

  # Create a lookup table
  setnames(stats_neighbours_raw, old = c("old_la_code", "la_name"), new = c("LA.number", "LA.Name"))
  lookup <- stats_neighbours_raw %>% select("LA.Name", "LA.number")

  setnames(stats_neighbours_raw, gsub(pattern = "SN_", "SN", names(stats_neighbours_raw)))

  df <- stats_neighbours_raw %>% select("LA.Name", "LA.number", "SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10")

  for (col in c("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10")) {
    df[[col]] <- lookup$LA.Name[match(df[[col]], lookup$"LA.number")]
  }
  # return a data.frame
  setDF(df)

  return(df)
}

get_stats_neighbours_long <- function(stats_neighbours) {
  stats_neighbours_long <- stats_neighbours %>%
    pivot_longer(
      cols = starts_with("SN"),
      names_to = c("SN_rank"),
      names_pattern = "(\\d+)",
      values_to = "SN_LA_name" # SN_LA_name
    ) %>%
    left_join(stats_neighbours %>% select(SN_LA_name = "LA.Name", SN_LA_number = "LA.number")) %>%
    as.data.table()

  return(stats_neighbours_long)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Summary Page ----
#### Summary Page dataset build ----

# this function will retrieve the summary_datafor a single indicator from a named dataset, metric and filtering
collect_summary_data_metric <- function(sort_order, dataset_name, dimensional_filters = list(), tab_name, accordion_text, heading_text, metric_display_text, value_column, value_format, ass_factor = NULL) {
  # function to pull current year data for all geographies (including stat neighbours)
  dataset_in <- copy(get0(dataset_name))
  setDT(dataset_in)

  # apply any dimensional filters for this dataset (e.g. characteristic, placement type, assessment factor)
  if (length(dimensional_filters) > 0) {
    dataset_in <- dataset_in[eval(AndEQUAL(dimensional_filters))]
  }

  # get the latest date
  time_period_latest <- max(dataset_in$time_period)

  # we need to add the geo_breakdown_sn if it doesn't exist (some headline stats do not appear in timeseries charts/tables
  # so have not been through the calculation for stat neighbours)
  if (!("geo_breakdown_sn" %in% names(dataset_in))) {
    dataset_in[, geo_breakdown_sn := NA]
  }

  # filter by time_period, extract the metric and build the text values for the table headings & accordions
  dataset_out <- dataset_in[
    i = time_period == time_period_latest,
    j = .(
      sort_order = sort_order,
      tab_name = tab_name,
      accordion_text = accordion_text,
      heading_text = heading_text,
      time_period, geographic_level, geo_breakdown, geo_breakdown_sn,
      metric_text = metric_display_text,
      value_format = value_format,
      value = get(value_column)
    )
  ]

  percent_format <- function(x) {
    if (is.na(as.numeric(x))) {
      return(x)
    }
    if (is.numeric(as.numeric(x))) {
      x_formatted <- paste0(x, "%")
      return(x_formatted)
    }
  }
  curreny_per_capita_format <- function(x) {
    if (is.na(as.numeric(x))) {
      return(x)
    }
    if (is.numeric(as.numeric(x))) {
      x_formatted <- paste0("~£", prettyNum(x, big.mark = ",", scientific = FALSE))
      return(x_formatted)
    }
  }
  # quick and dirty method to add the percentage sign where required
  dataset_out[value_format == "percent", value := sapply(value, percent_format)]
  # quick and dirty method to add the currency where required
  dataset_out[value_format == "currency_per_capita", value := sapply(value, curreny_per_capita_format)]

  return(dataset_out)
}


# this function is the controller of summary_data build.  It takes the input metadata from Excel and processes each indicator (i.e. row of the metadata table)
# data for all indicators is combined into a single data.table for use in the summary page
collect_summary_data_all <- function() {
  metric_parameters <- data.table(read_excel(path = "./data/summary_page_metadata.xlsx", sheet = 1))

  summary_data <- rbindlist(
    lapply(
      metric_parameters$sort_order,
      function(x, metric_parameters) {
        this_metric <- metric_parameters[sort_order == x]
        tab_name <- this_metric$tab_name
        accordion_text <- this_metric$accordion_text
        heading_text <- this_metric$heading_text
        metric_display_text <- this_metric$metric_display_text
        value_column <- this_metric$value_column
        value_format <- this_metric$value_format
        dataset_name <- this_metric$dataset_name
        dimensional_filters <- eval(parse(text = this_metric$dimensional_filters))
        assessment_factor <- this_metric$assessment_factor
        collect_summary_data_metric(
          sort_order = x,
          tab_name = tab_name,
          accordion_text = accordion_text,
          heading_text = heading_text,
          metric_display_text = metric_display_text,
          value_column = value_column,
          value_format = value_format,
          dataset_name = dataset_name,
          dimensional_filters = dimensional_filters,
          ass_factor = assessment_factor
        )
      }, metric_parameters
    )
  )

  # if any NA have crept in we need to display "na"
  summary_data[is.na(value), value := "na"]

  # before the date functions we need to clean the 6-digit dates - this should move into the data loading for one time correction everywhere!
  summary_data[nchar(time_period) == 6, time_period := paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))]

  # now for each section heading/domain (apart from Ofsted leadership) we need to move the date into the header, determining which date to use!
  date_frequency <- unique(summary_data[, .(tab_name, accordion_text, heading_text, time_period, metric_text)])[, .N, by = .(tab_name, accordion_text, heading_text, time_period)]
  date_per_heading <- merge(date_frequency, date_frequency[, .(N = max(N)), by = .(tab_name, accordion_text, heading_text)])
  setnames(date_per_heading, "time_period", "header_time_period")
  date_per_heading[, N := NULL]
  # there may be discrepancies on dates within a heading so individual exceptions to the norm are corrected by adding the date in brackets within the indicator text.....
  summary_data <- merge(summary_data, date_per_heading)
  summary_data[header_time_period != time_period, metric_text := paste0(metric_text, " (", time_period, ")")]
  summary_data[, heading_text := paste0(heading_text, " (", time_period, ")")]

  return(summary_data)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Outcome 1 -------------------
# CLA rate per 10k children data
read_cla_rate_data <- function(sn_long, file = "./data-raw/cla_number_and_rate_per_10k_children.csv") {
  cla_rate_data <- fread(file)

  cla_rate_data <- cla_rate_data %>%
    colClean() %>%
    # removing old Dorset, Poole, Bournemouth, Northamptonshire
    filter(!(new_la_code %in% c("E10000009", "E10000021", "E06000028", "E06000029"))) %>%
    insert_geo_breakdown()

  cla_rate_data <- remove_cumbria_data(cla_rate_data)

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = cla_rate_data,
    median_cols = c("rate_per_10000"),
    sum_cols = c("number"),
    group_cols = c("LA.number", "time_period", "population_count")
  )
  cla_rate_data <- rbindlist(l = list(cla_rate_data, sn_metrics), fill = TRUE, use.names = TRUE)

  cla_rate_data <- cla_rate_data %>%
    mutate(rate_per_10000 = sapply(rate_per_10000, decimal_rounding, 0)) %>%
    redacted_to_negative(col_old = "rate_per_10000", col_new = "Rate Per 10000") %>%
    redacted_to_negative(col_old = "number", col_new = "Number") %>%
    select(geographic_level, geo_breakdown, geo_breakdown_sn, time_period, region_code, region_name, new_la_code, old_la_code, la_name, population_count, population_estimate, number, Number, rate_per_10000, `Rate Per 10000`) %>%
    distinct()

  return(cla_rate_data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_cla_placement_data <- function(sn_long, file = "./data-raw/la_children_who_started_to_be_looked_after_during_the_year.csv") {
  cla_placement_data <- read.csv(file)
  cla_placement_data <- colClean(cla_placement_data) %>%
    insert_geo_breakdown() %>%
    mutate(Percentage = case_when(
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      TRUE ~ as.numeric(percentage)
    )) %>%
    setDT() %>%
    filter(!(new_la_code %in% c("E10000009", "E10000021", "E06000028", "E06000029"))) %>%
    remove_cumbria_data() %>%
    select(geographic_level, geo_breakdown, time_period, region_code, region_name, new_la_code, old_la_code, la_name, cla_group, characteristic, number, percentage) %>%
    distinct() %>%
    setDF() # just to make sure we aren't breaking anything though it would be preferable to stick to data.table everywhere!

  return(cla_placement_data)
}


read_cla_31_march_data <- function(file = "./data-raw/la_cla_on_31_march_by_characteristics.csv") {
  cla_31_march_data <- read.csv(file)
  cla_31_march_data <- colClean(cla_31_march_data) %>%
    insert_geo_breakdown() %>%
    mutate(Percentage = case_when(
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      TRUE ~ as.numeric(percentage)
    )) %>%
    filter(!(new_la_code %in% c("E10000009", "E10000021", "E06000028", "E06000029"))) %>%
    setDT() %>%
    remove_cumbria_data() %>%
    select(geographic_level, geo_breakdown, time_period, region_code, region_name, new_la_code, old_la_code, la_name, cla_group, characteristic, number, percentage) %>%
    distinct() %>%
    setDF()

  return(cla_31_march_data)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merge_cla_dataframes <- function(sn_long) {
  # Read the data of just get it from the global environment if it already exists
  if (exists(x = "cla_rates")) {
    cla_rates_local <- copy(cla_rates)
  } else {
    cla_rates_local <- suppressWarnings(read_cla_rate_data(sn_long = sn_long))
  }

  if (exists("cla_placements")) {
    cla_placements_local <- copy(cla_placements)
  } else {
    cla_placements_local <- suppressWarnings(read_cla_placement_data())
  }
  setDT(cla_placements_local)
  setDT(cla_rates_local)


  # Rename the columns to make it clear which dataset they come from
  cla_rates_local <- rename(cla_rates_local,
    rates_number = number
  )

  cla_placements_local <- rename(cla_placements_local,
    placements_number = number
  )

  # merge two data frames
  merged_data <- merge(
    cla_rates_local[geographic_level != "Statistical neighbours (median)"],
    cla_placements_local[geographic_level != "Statistical neighbours (median)"],
    by.x = c("geo_breakdown", "time_period", "geographic_level", "region_code", "region_name", "new_la_code", "old_la_code", "la_name"),
    by.y = c("geo_breakdown", "time_period", "geographic_level", "region_code", "region_name", "new_la_code", "old_la_code", "la_name"),
    allow.cartesian = TRUE
  )

  merged_data <- merged_data %>%
    mutate(placement_per_10000 = round(as.numeric(placements_number) / as.numeric(population_estimate) * 10000)) %>%
    mutate(`Placement Rate Per 10000` = case_when(
      placements_number == "c" ~ -100,
      placements_number == "low" ~ -200,
      placements_number == "k" ~ -200,
      placements_number == "u" ~ -250,
      placements_number == "x" ~ -300,
      placements_number == "z" ~ -400,
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      rate_per_10000 == "c" ~ -100,
      rate_per_10000 == "low" ~ -200,
      rate_per_10000 == "k" ~ -200,
      rate_per_10000 == "u" ~ -250,
      rate_per_10000 == "x" ~ -300,
      rate_per_10000 == "z" ~ -400,
      TRUE ~ as.numeric(placement_per_10000)
    )) %>%
    mutate("placement_per_10000" = case_when(
      placements_number == "c" ~ "c",
      placements_number == "low" ~ "low",
      placements_number == "k" ~ "k",
      placements_number == "u" ~ "u",
      placements_number == "x" ~ "x",
      placements_number == "z" ~ "z",
      percentage == "c" ~ "c",
      percentage == "low" ~ "low",
      percentage == "k" ~ "k",
      percentage == "u" ~ "u",
      percentage == "x" ~ "x",
      percentage == "z" ~ "z",
      rate_per_10000 == "c" ~ "c",
      rate_per_10000 == "low" ~ "low",
      rate_per_10000 == "k" ~ "k",
      rate_per_10000 == "u" ~ "u",
      rate_per_10000 == "x" ~ "x",
      rate_per_10000 == "z" ~ "z",
      TRUE ~ as.character(placement_per_10000)
    )) %>%
    mutate(characteristic = case_when(
      characteristic == "Unaccompanied asylum-seeking children" ~ "UASC",
      characteristic == "Non-unaccompanied asylum-seeking children" ~ "Non-UASC",
      TRUE ~ as.character(characteristic)
    ))

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = merged_data,
    median_cols = c("placement_per_10000"),
    sum_cols = c(),
    group_cols = c("LA.number", "time_period", "population_count", "characteristic")
  )
  merged_data <- rbindlist(l = list(merged_data, sn_metrics), fill = TRUE, use.names = TRUE)

  merged_data <- merged_data %>%
    mutate(placement_per_10000 = sapply(placement_per_10000, decimal_rounding, 0)) %>%
    return(merged_data)
}

merge_cla_31_march_dataframes <- function(sn_long) {
  # Read the data
  cla_rates_local <- read_cla_rate_data(sn_long)
  cla_31_march_data <- read_cla_31_march_data()

  setDT(cla_31_march_data)

  # Rename the columns to make it clear which dataset they come from
  cla_rates_local <- rename(cla_rates_local,
    rates_number = number
  )

  cla_31_march_data <- rename(cla_31_march_data,
    cla_31_march_number = number
  )

  # merge two data frames
  merged_31_march_data <- merge(cla_rates_local[geographic_level != "Statistical neighbours (median)"], cla_31_march_data[geographic_level != "Statistical neighbours (median)"],
    by.x = c("geo_breakdown", "time_period", "geographic_level", "region_code", "region_name", "new_la_code", "old_la_code", "la_name"),
    by.y = c("geo_breakdown", "time_period", "geographic_level", "region_code", "region_name", "new_la_code", "old_la_code", "la_name"),
    allow.cartesian = TRUE
  )

  merged_31_march_data <- merged_31_march_data %>%
    mutate(placement_per_10000 = round(as.numeric(cla_31_march_number) / as.numeric(population_estimate) * 10000)) %>%
    mutate(`Placement Rate Per 10000` = case_when(
      cla_31_march_number == "c" ~ -100,
      cla_31_march_number == "low" ~ -200,
      cla_31_march_number == "k" ~ -200,
      cla_31_march_number == "u" ~ -250,
      cla_31_march_number == "x" ~ -300,
      cla_31_march_number == "z" ~ -400,
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      rate_per_10000 == "c" ~ -100,
      rate_per_10000 == "low" ~ -200,
      rate_per_10000 == "k" ~ -200,
      rate_per_10000 == "u" ~ -250,
      rate_per_10000 == "x" ~ -300,
      rate_per_10000 == "z" ~ -400,
      TRUE ~ as.numeric(placement_per_10000)
    )) %>%
    mutate("placement_per_10000" = case_when(
      cla_31_march_number == "c" ~ "c",
      cla_31_march_number == "low" ~ "low",
      cla_31_march_number == "k" ~ "k",
      cla_31_march_number == "u" ~ "u",
      cla_31_march_number == "x" ~ "x",
      cla_31_march_number == "z" ~ "z",
      percentage == "c" ~ "c",
      percentage == "low" ~ "low",
      percentage == "k" ~ "k",
      percentage == "u" ~ "u",
      percentage == "x" ~ "x",
      percentage == "z" ~ "z",
      rate_per_10000 == "c" ~ "c",
      rate_per_10000 == "low" ~ "low",
      rate_per_10000 == "k" ~ "k",
      rate_per_10000 == "u" ~ "u",
      rate_per_10000 == "x" ~ "x",
      rate_per_10000 == "z" ~ "z",
      TRUE ~ as.character(placement_per_10000)
    )) %>%
    mutate(characteristic = case_when(
      characteristic == "Unaccompanied asylum-seeking children" ~ "UASC",
      characteristic == "Non-unaccompanied asylum-seeking children" ~ "Non-UASC",
      TRUE ~ as.character(characteristic)
    ))

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = merged_31_march_data,
    median_cols = c("placement_per_10000"),
    sum_cols = c(),
    group_cols = c("LA.number", "time_period", "population_count", "characteristic")
  )
  merged_31_march_data <- rbindlist(l = list(merged_31_march_data, sn_metrics), fill = TRUE, use.names = TRUE)

  merged_31_march_data <- merged_31_march_data %>%
    mutate(placement_per_10000 = sapply(placement_per_10000, decimal_rounding, 0))

  return(merged_31_march_data)
}

# TODO: redundant code?
# a <- merge_cla_31_march_dataframes()

# CIN rate per 10k children data
read_cin_rate_data <- function(sn_long, file = "./data-raw/b1_children_in_need_2013_to_2024.csv") {
  cin_rate_data <- fread(file)

  # initial cleansing steps
  cin_rate_data <- cin_rate_data %>%
    colClean() %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = cin_rate_data,
    median_cols = c("At31_episodes_rate"),
    sum_cols = c("At31_episodes"),
    group_cols = c("LA.number", "time_period")
  )

  cin_rate_data <- rbindlist(l = list(cin_rate_data, sn_metrics), fill = TRUE, use.names = TRUE)

  # handle the redactions by creating numeric columns from char columns with arbitrary values to aid table sort order
  cin_rate_data <- cin_rate_data %>%
    mutate(At31_episodes_rate = sapply(At31_episodes_rate, decimal_rounding, 0)) %>%
    mutate(At31_episodes = sapply(At31_episodes, decimal_rounding, 0)) %>%
    redacted_to_negative(col_old = "At31_episodes", col_new = "CIN_number") %>%
    redacted_to_negative(col_old = "At31_episodes_rate", col_new = "CIN_rate")

  # finalise the dataset by rounding rates and selecting relevant columns
  cin_rate_data <- cin_rate_data %>%
    select(geographic_level, geo_breakdown, geo_breakdown_sn, time_period, region_code, region_name, new_la_code, old_la_code, la_name, CIN_number, At31_episodes, CIN_rate, At31_episodes_rate) %>%
    distinct()

  return(cin_rate_data)
}

# CIN referrals data
read_cin_referral_data <- function(sn_long, file = "./data-raw/c1_children_in_need_referrals_and_rereferrals_2013_to_2024.csv") {
  cin_referral_data <- fread(file)

  # initial cleansing steps
  cin_referral_data <- cin_referral_data %>%
    colClean() %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = cin_referral_data,
    median_cols = c("Re_referrals_percent"), # what medians are we taking
    sum_cols = c(), # "Referrals", "Re_referrals"),
    group_cols = c("LA.number", "time_period")
  )

  cin_referral_data <- rbindlist(l = list(cin_referral_data, sn_metrics), fill = TRUE, use.names = TRUE)

  # handle the redactions by creating numeric columns from char columns with arbitrary values to aid table sort order
  cin_referral_data <- cin_referral_data %>%
    mutate(Re_referrals_percent = sapply(Re_referrals_percent, decimal_rounding, 1)) %>%
    redacted_to_negative(col_old = "Referrals", col_new = "Referrals_num") %>%
    redacted_to_negative(col_old = "Re_referrals", col_new = "Re_referrals_num") %>%
    redacted_to_na(col_old = "Re_referrals_percent", col_new = "Re_referrals_percentage") %>%
    redacted_to_negative(col_old = "Re_referrals_percent", col_new = "Re-referrals (%)")

  # finalise the dataset by rounding rates and selecting relevant columns
  cin_referral_data <- cin_referral_data %>%
    select(
      time_period, geographic_level, geo_breakdown, geo_breakdown_sn, region_code, region_name, new_la_code, old_la_code, la_name,
      Referrals, Re_referrals, Re_referrals_percent, Referrals_num, Re_referrals_num, Re_referrals_percentage, `Re-referrals (%)`
    ) %>%
    distinct()

  return(cin_referral_data)
}



# Outcome 1 Outcomes absence data for child well being and development
read_outcomes_absence_data <- function(sn_long, file = "./data-raw/absence_six_half_terms_la.csv") {
  # Notes: there is no removal of old LAs here
  outcomes_absence_data <- fread(file)

  # due to a modified field name in the input dataset (10/04/2025) we perform a rename
  outcomes_absence_data <- outcomes_absence_data %>%
    rename(school_type = phase_type_grouping)

  # Select only columns we want
  outcomes_absence_data <- outcomes_absence_data %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = outcomes_absence_data,
    median_cols = c("pt_overall", "pt_pupils_pa_10_exact", "pt_pupils_pa_50_exact"), # "pt_sess_authorised", "pt_sess_unauthorised"),
    sum_cols = c("t_pupils"),
    group_cols = c("LA.number", "time_period", "school_type", "social_care_group"),
  )
  outcomes_absence_data <- rbindlist(l = list(outcomes_absence_data, sn_metrics), fill = TRUE, use.names = TRUE)

  # manual step to ensure COVID years are redacted to X
  cols_to_update <- c("pt_overall", "t_pupils", "pt_pupils_pa_10_exact", "pt_pupils_pa_50_exact")
  time_periods_to_update <- c(201920)
  outcomes_absence_data[time_period %in% time_periods_to_update, (cols_to_update) := lapply(.SD, function(x) "x"), .SDcols = cols_to_update]

  # Make % columns numeric
  outcomes_absence_data <- outcomes_absence_data %>%
    select(
      geographic_level, geo_breakdown, geo_breakdown_sn, country_code, region_code, new_la_code, old_la_code, time_period,
      "time_period", "geographic_level", "region_name", social_care_group,
      school_type, t_pupils, t_sess_possible, t_sess_overall, pt_overall, t_sess_authorised,
      pt_sess_authorised, t_sess_unauthorised, pt_sess_unauthorised, t_pupils_pa_10_exact, pt_pupils_pa_10_exact,
      t_pupils_pa_50_exact, pt_pupils_pa_50_exact
    ) %>%
    mutate(pt_overall = sapply(pt_overall, decimal_rounding, 1)) %>%
    mutate(pt_pupils_pa_10_exact = sapply(pt_pupils_pa_10_exact, decimal_rounding, 1)) %>%
    mutate(pt_pupils_pa_50_exact = sapply(pt_pupils_pa_50_exact, decimal_rounding, 1)) %>%
    mutate(pt_sess_authorised = sapply(pt_sess_authorised, decimal_rounding, 1)) %>%
    mutate(pt_sess_unauthorised = sapply(pt_sess_unauthorised, decimal_rounding, 1)) %>%
    redacted_to_negative(col_old = "pt_overall", col_new = "Overall absence (%)") %>%
    redacted_to_negative(col_old = "pt_pupils_pa_10_exact", col_new = "Persistent absentees (%)") %>%
    redacted_to_negative(col_old = "pt_pupils_pa_50_exact", col_new = "Severe absentees (%)") %>%
    redacted_to_negative(col_old = "pt_sess_authorised", col_new = "Authorised absence (%)") %>%
    redacted_to_negative(col_old = "pt_sess_unauthorised", col_new = "Unauthorised absence (%)") %>%
    redacted_to_negative(col_old = "t_pupils", col_new = "Total pupils")

  return(outcomes_absence_data)
}


# Outcome 1 Outcomes KS2 data for education attainment
read_outcomes_ks2_data <- function(sn_long, file = "./data-raw/ks2_la.csv") {
  outcomes_ks2_data <- fread(file)

  outcomes_ks2_data <- outcomes_ks2_data %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = outcomes_ks2_data,
    median_cols = c("pt_rwm_met_expected_standard"),
    sum_cols = c("t_rwm_eligible_pupils"), # TODO: modify?
    group_cols = c("LA.number", "time_period", "social_care_group"),
  )
  outcomes_ks2_data <- rbindlist(l = list(outcomes_ks2_data, sn_metrics), fill = TRUE, use.names = TRUE)

  # manual step to ensure COVID years are redacted to X
  cols_to_update <- c("pt_rwm_met_expected_standard", "t_rwm_eligible_pupils")
  time_periods_to_update <- c(201920, 202021)
  outcomes_ks2_data[time_period %in% time_periods_to_update, (cols_to_update) := lapply(.SD, function(x) "x"), .SDcols = cols_to_update]


  outcomes_ks2_data <- outcomes_ks2_data %>%
    # TODO: reduce the number of columns
    # Select only columns we want
    select(
      geographic_level, geo_breakdown, geo_breakdown_sn, country_code, region_code, new_la_code, old_la_code, time_period,
      "time_period", "geographic_level", "region_name", social_care_group,
      version, t_read_eligible_pupils, t_read_met_expected_standard, pt_read_met_expected_standard, t_writta_eligible_pupils,
      t_writta_met_expected_standard, pt_writta_met_expected_standard, t_mat_eligible_pupils, t_mat_met_expected_standard,
      pt_mat_met_expected_standard, t_rwm_eligible_pupils, t_rwm_met_expected_standard, pt_rwm_met_expected_standard,
      t_gps_eligible_pupils, t_gps_met_expected_standard, pt_gps_met_expected_standard, t_scita_eligible_pupils,
      t_scita_met_expected_standard, pt_scita_met_expected_standard, t_read_progress_eligible_pupils,
      t_read_progress_score, avg_read_progress_score, avg_read_progress_score_lower_CI, avg_read_progress_score_upper_CI,
      t_writta_progress_eligible_pupils, t_writta_progress_score, avg_writta_progress_score, avg_writta_progress_score_lower_CI,
      avg_writta_progress_score_upper_CI, t_mat_progress_eligible_pupils, t_mat_progress_score, avg_mat_progress_score,
      avg_mat_progress_score_lower_CI, avg_mat_progress_score_upper_CI
    ) %>%
    mutate(pt_rwm_met_expected_standard = sapply(pt_rwm_met_expected_standard, decimal_rounding, 0)) %>%
    # Make % columns numeric
    redacted_to_negative(col_old = "pt_rwm_met_expected_standard", col_new = "Expected standard reading writing maths (%)")
  # redacted_to_negative(col_old = "t_rwm_eligible_pupils", col_new = "?")



  return(outcomes_ks2_data)
}



# Outcome 1 Outcomes KS4 data for education attainment
read_outcomes_ks4_data <- function(sn_long, file = "./data-raw/ks4_la.csv") {
  outcomes_ks4_data <- fread(file)

  # Select only columns we want
  outcomes_ks4_data <- outcomes_ks4_data %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = outcomes_ks4_data,
    median_cols = c("avg_att8"),
    sum_cols = c("t_pupils"),
    group_cols = c("LA.number", "time_period", "social_care_group"),
  )

  outcomes_ks4_data <- rbindlist(l = list(outcomes_ks4_data, sn_metrics), fill = TRUE, use.names = TRUE)

  outcomes_ks4_data <- outcomes_ks4_data %>%
    select(
      geographic_level, geo_breakdown, geo_breakdown_sn, country_code, region_code, new_la_code, old_la_code, time_period,
      "time_period", "geographic_level", "region_name", social_care_group,
      version, t_pupils, t_att8, avg_att8, t_l2basics_95, pt_l2basics_95, t_l2basics_94, pt_l2basics_94,
      t_ebacc_e_ptq_ee, pt_ebacc_e_ptq_ee, t_ebaccaps, avg_ebaccaps, t_inp8calc,
      t_p8score, avg_p8score, p8score_CI_low, p8score_CI_upp
    ) %>%
    mutate(avg_att8 = sapply(avg_att8, decimal_rounding, 1)) %>%
    # Make number columns numeric
    redacted_to_negative(col_old = "avg_att8", col_new = "Average Attainment 8") %>%
    redacted_to_negative(col_old = "t_pupils", col_new = "Total pupils")

  return(outcomes_ks4_data)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Outcome 2 ----
# read outcome 2 function but without manual calculation of the percentages.
read_outcome2 <- function(sn_long, file = "./data-raw/la_children_who_ceased_during_the_year.csv") {
  # drop old LA's
  ceased_cla_data <- fread(file)

  # TODO: remove make this logic a function as we have various implementations of it
  las_to_remove <- c("Poole", "Bournemouth", "Northamptonshire")
  ceased_cla_data <- ceased_cla_data %>%
    filter(!(new_la_code %in% dropList), !la_name %in% las_to_remove) %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = ceased_cla_data,
    median_cols = c("percentage"),
    sum_cols = c(),
    group_cols = c("LA.number", "time_period", "cla_group", "characteristic"),
  )
  ceased_cla_data <- rbindlist(l = list(ceased_cla_data, sn_metrics), fill = TRUE, use.names = TRUE)

  ceased_cla_data <- ceased_cla_data %>%
    mutate(percentage = sapply(percentage, decimal_rounding, 0)) %>%
    # Make number columns numeric
    redacted_to_negative(col_old = "percentage", col_new = "Ceased (%)") %>%
    redacted_to_negative(col_old = "number", col_new = "Number ceased")

  totals <- ceased_cla_data %>%
    filter(characteristic == "Total") %>%
    rename("Total_num" = "Number ceased") %>%
    mutate("Total" = number) %>%
    select(time_period, geographic_level, geo_breakdown, geo_breakdown_sn, cla_group, Total_num, Total)

  joined <- left_join(ceased_cla_data, totals, by = c("time_period", "geographic_level", "geo_breakdown", "geo_breakdown_sn", "cla_group"))
  joined <- joined %>%
    select("time_period", "geographic_level", "geo_breakdown", "geo_breakdown_sn", "old_la_code", "new_la_code", "cla_group", "characteristic", "number", "Number ceased", "Total_num", "Total", "percentage", "Ceased (%)")
  # For tables, we want to show the suppressed letters so use columns "percentage" and "number".
  # For charts, char values do not work so use column "Ceased (%)"

  return(joined)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Outcome 3 ----
### Outcome 3 Child Safety General ----
##### Child Protection Plans starting during year, which were second or subsequent plans (accordion 1) ----
read_cpp_in_year_data <- function(sn_long, file = "./data-raw/d3_cpps_subsequent_plan_2013_to_2024.csv") {
  cpp_in_year_data <- fread(file)

  # add geo_breakdown
  cpp_in_year_data <- cpp_in_year_data %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data() %>%
    mutate(CPP_subsequent_percent = sapply(CPP_subsequent_percent, decimal_rounding, 1))


  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = cpp_in_year_data,
    median_cols = c("CPP_subsequent_percent"),
    sum_cols = c(),
    group_cols = c("LA.number", "time_period", "category"),
  )
  cpp_in_year_data <- rbindlist(l = list(cpp_in_year_data, sn_metrics), fill = TRUE, use.names = TRUE)

  # round and create the sort order numeric column
  cpp_in_year_data <- cpp_in_year_data %>%
    select(
      time_period, geographic_level, geo_breakdown, geo_breakdown_sn, country_code, region_code, region_name, new_la_code, old_la_code, la_name, CPP_start, CPP_subsequent, CPP_subsequent_percent
    ) %>%
    mutate(CPP_subsequent_percent = sapply(CPP_subsequent_percent, decimal_rounding, 1)) %>%
    redacted_to_negative(col_old = "CPP_subsequent_percent", col_new = "Repeat_CPP_percent")

  return(cpp_in_year_data)
}

### CPP by duration (accordion 2)
read_cpp_by_duration_data <- function(sn_long, file = "./data-raw/d5_cpps_at31march_by_duration_2013_to_2024.csv") {
  cpp_by_duration_data <- read.csv(file) %>% data.table()

  cpp_by_duration_data <- cpp_by_duration_data %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = cpp_by_duration_data,
    median_cols = c("X2_years_or_more_percent"),
    sum_cols = c(),
    group_cols = c("LA.number", "time_period"),
  )
  cpp_by_duration_data <- rbindlist(l = list(cpp_by_duration_data, sn_metrics), fill = TRUE, use.names = TRUE)

  cpp_by_duration_data <- cpp_by_duration_data %>%
    mutate(X2_years_or_more_percent = sapply(X2_years_or_more_percent, decimal_rounding, digits = 1)) %>%
    select(
      time_period, geographic_level, geo_breakdown, geo_breakdown_sn, country_code, region_code, region_name, old_la_code,
      CPP_At31, `X3_months_or_less`, `X3_months_or_less_percent`, more_than_3_months_6_months, more_than_3_months_6_months_percent, more_than_6_months_less_than_1_year,
      more_than_6_months_less_than_1_year_percent, `X1_year_less_than_2_years`, `X1_year_less_than_2_years_percent`, `X2_years_or_more`, `X2_years_or_more_percent`
    ) %>%
    redacted_to_negative(col_old = "X2_years_or_more_percent", col_new = "CPP_2_years_or_more_percent")


  return(cpp_by_duration_data)
}

### Hospital admissions (accordion 3) ------
# Data for this indicator is from an API and only shows the latest data
# LA data from here: https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/pat/15/par/E92000001/ati/502/are/E09000002/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/tre-ao-0_car-do-0
# Region level data from here: https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/6/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/tre-ao-0_car-do-0

read_a_and_e_data <- function(sn_long, la_file = "./data-raw/la_hospital_admissions_2324.csv", region_file = "./data-raw/region_hospital_admissions_2324.csv") {
  # read the raw data from 2 csv files
  la_admissions <- read.csv("./data-raw/la_hospital_admissions_2324.csv") # la_file)
  region_admissions <- read.csv("./data-raw/region_hospital_admissions_2324.csv") # region_file)

  # remove North and West Northamptonshire for pre 2022
  # remove Cumberland & Westmorland and Furness for pre 2023
  # remove BCP and Dorset for pre 2019
  # remove Buckinghamshire for pre 2020
  la_admissions <- la_admissions %>%
    filter(!(Area.Name %in% c("North Northamptonshire", "West Northamptonshire") & Time.period < "2022")) %>%
    filter(!(Area.Name %in% c("Cumberland", "Westmorland and Furness") & Time.period < "2023")) %>%
    filter(!(Area.Name %in% c("Dorset", "Bournemouth, Christchurch and Poole") & Time.period < "2019")) %>%
    filter(!(Area.Name %in% c("Buckinghamshire UA") & Time.period < "2020"))


  # additional step to clean dots out of the coumn names
  setnames(la_admissions, "Area.Name", "AreaName")
  setnames(region_admissions, "Area.Name", "AreaName")
  la_admissions$AreaName <- sub(" UA$", "", la_admissions$AreaName)
  region_admissions$AreaName <- sub(" region \\(statistical\\)$", "", region_admissions$AreaName)


  # note the hard-coded cleansing here as the input files provided require a couple of hacks
  admissions_data_joined <- rbind(
    la_admissions[la_admissions$Category == "" & la_admissions$Sex == "Persons", c(1:13, 18, 19)],
    region_admissions[region_admissions$Parent.Code == "E92000001" & region_admissions$Category == "" & region_admissions$Sex == "Persons", c(1:13, 18, 19)]
  ) %>%
    select("Time.period", "Area.Type", "AreaName", "Area.Code", "Value", "Count", "Denominator") %>%
    rename(`time_period` = `Time.period`, `geographic_level` = `Area.Type`, `geo_breakdown` = `AreaName`, `new_la_code` = `Area.Code`) %>%
    distinct()

  admissions_data_joined["geographic_level"][admissions_data_joined["geographic_level"] == "Regions (statistical)"] <- "Regional"
  admissions_data_joined["geographic_level"][admissions_data_joined["geographic_level"] == "Counties & UAs (from Apr 2023)"] <- "Local authority"
  admissions_data_joined["geographic_level"][admissions_data_joined["geographic_level"] == "England"] <- "National"
  admissions_data_joined["geo_breakdown"][admissions_data_joined["geo_breakdown"] == "England"] <- "National"

  admissions_data_joined <- remove_cumbria_data(admissions_data_joined)

  # For the stats neighbours charts we need to have old la codes, not available in this data so just get it from another dataset
  la_codes <- suppressWarnings(read_workforce_data(sn_long = sn_long)) %>%
    filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
    select(old_la_code, new_la_code) %>%
    distinct() %>%
    separate_rows(c("old_la_code", "new_la_code"), sep = " / ")

  admissions_data2 <- left_join(admissions_data_joined, la_codes, by = c("new_la_code"))
  # admissions_data2$Count <- as.numeric(gsub(",", "", admissions_data2$Count))
  # admissions_data2$rate_per_10000 <- as.numeric(admissions_data2$rate_per_10000)

  # Name changes

  admissions_data2 <- admissions_data2 %>%
    mutate(geo_breakdown = case_when(
      geo_breakdown == "Yorkshire and the Humber" ~ "Yorkshire and The Humber",
      geo_breakdown == "Bristol" ~ "Bristol, City of",
      geo_breakdown == "Herefordshire" ~ "Herefordshire, County of",
      geo_breakdown == "Kingston upon Hull" ~ "Kingston upon Hull, City of",
      TRUE ~ as.character(geo_breakdown)
    ))

  # Inner London Data

  inner_london <- c(
    "Westminster",
    "Tower Hamlets",
    "Camden",
    "Hackney",
    "Kensington and Chelsea",
    "Southwark",
    "Lewisham",
    "Islington",
    "Wandsworth",
    "Hammersmith and Fulham",
    "Haringey",
    "Lambeth",
    "Newham",
    "City of London"
  )
  inner_london_data <- admissions_data2 %>%
    filter(geo_breakdown %in% inner_london)

  inner_london_stat <- inner_london_data %>%
    group_by(time_period) %>%
    summarise(
      geographic_level = "Regional",
      geo_breakdown = "Inner London",
      new_la_code = "E13000001",
      Value = sum(Value, na.rm = TRUE),
      Count = sum(Count[Denominator >= 0], na.rm = TRUE),
      Denominator = sum(Denominator[Denominator >= 0], na.rm = TRUE),
      old_la_code = NA
    )

  # Outer London Data

  Outer_london <- c(
    "Bexley",
    "Greenwich",
    "Harrow",
    "Brent",
    "Waltham Forest",
    "Ealing",
    "Richmond upon Thames",
    "Hillingdon",
    "Kingston upon Thames",
    "Hounslow",
    "Bromley",
    "Barnet",
    "Croydon",
    "Enfield",
    "Merton",
    "Sutton",
    "Barking and Dagenham",
    "Redbridge",
    "Havering"
  )

  Outer_london_data <- admissions_data2 %>%
    filter(geo_breakdown %in% Outer_london)

  Outer_london_stat <- Outer_london_data %>%
    group_by(time_period) %>%
    summarise(
      geographic_level = "Regional",
      geo_breakdown = "Outer London",
      new_la_code = "E13000002",
      Value = sum(Value, na.rm = TRUE),
      Count = sum(Count[Denominator >= 0], na.rm = TRUE),
      Denominator = sum(Denominator[Denominator >= 0], na.rm = TRUE),
      old_la_code = NA
    )

  # Inner and Outer London

  inner_and_outer_london <- rbind(inner_london_stat, Outer_london_stat)

  # rate per 10000

  inner_and_outer_london <- inner_and_outer_london %>%
    # mutate(rate_per_10000 = Count / (Denominator / 10000)) %>%
    mutate(Value = Count / (Denominator / 10000))

  # Add Inner and Outer London to the data frame
  admissions_data2 <- rbind(admissions_data2, inner_and_outer_london)


  # Convert rate_per_10000 to a character for all rows
  admissions_data2 <- admissions_data2 %>%
    mutate(rate_per_10000 = case_when(
      is.na(Value) ~ "x",
      TRUE ~ as.character(Value)
    ))


  # add stats neighbours
  # now calculate SN metrics and append to the bottom of the dataset
  setDT(admissions_data2)
  admissions_data2[, old_la_code := as.numeric(old_la_code)]
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = admissions_data2,
    median_cols = c("rate_per_10000", "Value"),
    sum_cols = c(),
    group_cols = c("LA.number", "time_period"),
  )
  admissions_data3 <- rbindlist(l = list(admissions_data2, sn_metrics), fill = TRUE, use.names = TRUE)

  admissions_data3 <- admissions_data3 %>%
    mutate(rate_per_10000 = sapply(rate_per_10000, decimal_rounding, 0)) %>%
    mutate(Value = sapply(Value, true_round, 0)) %>%
    mutate(Value = case_when(
      is.na(Value) ~ -300,
      TRUE ~ as.numeric(Value)
    ))

  return(admissions_data3)
}



## Child abuse/Neglect / Harms outside the home ----
### Assessment Factors ------
read_assessment_factors <- function(sn_long, file = "./data-raw/c3_factors_identified_at_end_of_assessment_2018_to_2024.csv") {
  ass_fac_data_raw <- fread(file)
  ass_fac_data_raw <- ass_fac_data_raw %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  columns <- c(
    "Episodes_with_assessment_factor",
    "Alcohol_Misuse_child", "Alcohol_Misuse_parent", "Alcohol_Misuse_person", "Drug_Misuse_child",
    "Drug_Misuse_parent", "Drug_Misuse_person", "Domestic_Abuse_child", "Domestic_Abuse_parent",
    "Domestic_Abuse_person", "Mental_Health_child", "Mental_Health_parent", "Mental_Health_person", "Learning_Disability_child",
    "Learning_Disability_parent", "Learning_Disability_person", "Physical_Disability_child",
    "Physical_Disability_parent", "Physical_Disability_person", "Young_Carer", "Privately_fostered",
    "Unaccompanied_asylum_seeker", "Going_missing", "Child_sexual_exploitation", "Trafficking", "Gangs",
    "Socially_unacceptable_behaviour", "Self_harm", "Neglect", "Emotional_Abuse", "Physical_Abuse_unknown",
    "Physical_Abuse_child_on_child", "Physical_Abuse_adult_on_child", "Sexual_Abuse_unknown", "Sexual_Abuse_child_on_child",
    "Sexual_Abuse_adult_on_child", "Female_Genital_Mutilation", "Faith_linked_abuse", "Child_criminal_exploitation", "Other"
  )

  # this helps us to narrow down to only the AFs we are going to report
  af_to_keep <- c(
    "Domestic Abuse child", "Domestic Abuse parent", "Domestic Abuse person",
    "Neglect", "Emotional Abuse", "Physical Abuse unknown", "Physical Abuse child on child",
    "Physical Abuse adult on child", "Sexual Abuse unknown", "Sexual Abuse child on child",
    "Sexual Abuse adult on child", "Faith linked abuse", "Going missing",
    "Child sexual exploitation", "Trafficking", "Gangs", "Child criminal exploitation"
  )

  # original steps to pivot and clean columns
  ass_fac_data <- ass_fac_data_raw %>%
    pivot_longer(
      cols = columns,
      names_to = "assessment_factor",
      values_to = "value"
    ) %>%
    data.table() %>%
    mutate(assessment_factor = gsub("_", " ", assessment_factor)) %>%
    filter(assessment_factor %in% af_to_keep)

  # Using the population data from CLA rates data, because Data needs to be rates per 10,000
  if (exists(x = "cla_rates")) {
    populations <- copy(cla_rates)
  } else {
    populations <- suppressWarnings(read_cla_rate_data(sn_long = sn_long))
  }
  populations <- populations %>%
    filter(geo_breakdown != "Statistical neighbours (median)") %>%
    select(time_period, geo_breakdown, new_la_code, old_la_code, population_estimate) %>%
    distinct()
  ass_fac_data <- left_join(ass_fac_data, populations, by = c("time_period", "geo_breakdown", "new_la_code", "old_la_code"), relationship = "many-to-many") %>%
    mutate(`rate_per_10000` = (as.numeric(value) / as.numeric(population_estimate)) * 10000) %>%
    filter(time_period != 2018)

  # now we need to tidy up, creating the character column and populating it appropriately
  ass_fac_data <- ass_fac_data %>%
    mutate(rate_per_10000_char = as.character(rate_per_10000))

  ass_fac_data[is.na(as.numeric(value)), rate_per_10000_char := value]
  ass_fac_data[is.na(rate_per_10000_char), rate_per_10000_char := "z"]

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = ass_fac_data,
    median_cols = c("rate_per_10000_char", "rate_per_10000"),
    sum_cols = c("value"),
    group_cols = c("LA.number", "time_period", "assessment_factor"),
  )
  sn_metrics <- sn_metrics %>%
    mutate(rate_per_10000_char = as.character(rate_per_10000))

  sn_metrics[is.na(rate_per_10000_char), rate_per_10000_char := "z"]

  ass_fac_data <- rbindlist(l = list(ass_fac_data, sn_metrics), fill = TRUE, use.names = TRUE)

  ass_fac_data <- ass_fac_data %>%
    mutate(`rate_per_10000_char` = sapply(rate_per_10000_char, decimal_rounding, digits = 0)) %>%
    redacted_to_negative(col_old = "rate_per_10000_char", col_new = "rate_per_10000") %>%
    redacted_to_negative(col_old = "value", col_new = "Number")


  return(ass_fac_data)
}



# Outcome 4 -----
## Number of placements (placement_changes_data) -----

read_number_placements_data <- function(sn_long, file = "./data-raw/la_cla_placement_stability.csv") {
  placement_chg_data <- fread(file)

  placement_chg_data <- placement_chg_data %>%
    filter(!(new_la_code %in% dropList)) %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data() %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, cla_group, placement_stability, number, percentage)

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = placement_chg_data,
    median_cols = c("percentage"),
    sum_cols = c(),
    group_cols = c("LA.number", "time_period", "cla_group", "placement_stability"),
  )
  placement_chg_data <- rbindlist(l = list(placement_chg_data, sn_metrics), fill = TRUE, use.names = TRUE)

  placement_chg_data <- placement_chg_data %>%
    mutate(percentage = sapply(percentage, decimal_rounding, 0)) %>%
    redacted_to_negative(col_old = "percentage", col_new = "Percent") %>%
    rename("Percentage" = "percentage", "Number" = "number")

  return(placement_chg_data)
}

## Placement type and distance----
read_placement_info_data <- function(sn_long, file = "./data-raw/la_cla_on_31_march_by_characteristics.csv") {
  placement_info_data <- fread(file)

  placement_info_data <- placement_info_data %>%
    filter(!(new_la_code %in% dropList)) %>%
    # custom filter to reduce dataset :-)
    filter(cla_group %in% c("Placement", "Distance between home and placement")) %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = placement_info_data,
    median_cols = c("percentage"),
    sum_cols = c(),
    group_cols = c("LA.number", "time_period", "cla_group", "characteristic"),
  )
  placement_info_data <- rbindlist(l = list(placement_info_data, sn_metrics), fill = TRUE, use.names = TRUE)

  placement_info_data <- placement_info_data %>%
    mutate(percentage = sapply(percentage, decimal_rounding, 0)) %>%
    select(time_period, geographic_level, geo_breakdown, geo_breakdown_sn, new_la_code, old_la_code, cla_group, characteristic, number, percentage) %>%
    redacted_to_negative(col_old = "percentage", col_new = "Percent")

  return(placement_info_data)
}

# Need to do some aggregation so that placement types is aggregated to these: "foster placements", "secure units, childrens's homes or semi-independent living", "other"

## Care leavers activity -----
read_care_leavers_activity_data <- function(sn_long, file = "./data-raw/la_care_leavers_activity.csv") {
  cl_activity_data <- fread(file) %>%
    # filter out old dorset code
    filter(!(new_la_code %in% dropList)) %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = cl_activity_data,
    median_cols = c("percentage"),
    sum_cols = c(), # "value", "population_estimate"
    group_cols = c("LA.number", "time_period", "age", "activity"),
  )
  cl_activity_data <- rbindlist(l = list(cl_activity_data, sn_metrics), fill = TRUE, use.names = TRUE)

  cl_activity_data <- cl_activity_data %>%
    select(time_period, geographic_level, geo_breakdown, geo_breakdown_sn, new_la_code, old_la_code, age, activity, number, percentage) %>%
    mutate(percentage = sapply(percentage, decimal_rounding, 0)) %>%
    redacted_to_negative(col_old = "percentage", col_new = "percent")

  # Age column needs to be uniform with the accommodation data as they share the same age range filter
  # "17 to 18 years" sounds better than "aged 17 to 18" but this can be swapped around if needed
  cl_activity_data[age == "Aged 17 to 18", age := "17 to 18 years"]
  cl_activity_data[age == "Aged 19 to 21", age := "19 to 21 years"]

  return(cl_activity_data)
}

## Care leavers accommodation -----
read_care_leavers_accommodation_suitability <- function(sn_long, file = "./data-raw/la_care_leavers_accommodation_suitability.csv") {
  cl_accom_data <- fread(file) %>%
    # filter out old dorset code
    filter(!(new_la_code %in% dropList)) %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data()

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = cl_accom_data,
    median_cols = c("percentage"),
    sum_cols = c(), # "value", "population_estimate"
    group_cols = c("LA.number", "time_period", "age", "accommodation_suitability"),
  )
  cl_accom_data <- rbindlist(l = list(cl_accom_data, sn_metrics), fill = TRUE, use.names = TRUE)

  cl_accom_data <- cl_accom_data %>%
    mutate(percentage = sapply(percentage, decimal_rounding, 0)) %>%
    select(
      time_period, geographic_level, geo_breakdown, geo_breakdown_sn, new_la_code, old_la_code,
      age, accommodation_suitability, number, percentage
    ) %>%
    redacted_to_negative(col_old = "percentage", col_new = "percent")

  # TODO: in care_leavers_activity_data we clean the text in the age field, check whether this is necessary in this file

  return(cl_accom_data)
}


## Wellbeing of child -(SDQ) ----

read_wellbeing_child_data <- function(sn_long, file = "./data-raw/la_conviction_health_outcome_cla.csv") {
  data <- fread(file)

  data2 <- data %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data() %>%
    filter(cla_group == "Ages 5 to 16 years with SDQ score") %>%
    filter(!(new_la_code %in% dropList)) %>%
    mutate(percentage = ifelse(!is.na(as.numeric(percentage)),
      format(as.numeric(as.character(percentage)), nsmall = 1),
      percentage
    )) %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, cla_group, characteristic, number, percentage)

  data3 <- data2 %>%
    redacted_to_negative(col_old = "number", col_new = "number_num") %>%
    redacted_to_negative(col_old = "percentage", col_new = "percentage_num")

  # pull out the SDQ_scores_received column
  data_totals <- data3 %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, cla_group, characteristic, number) %>%
    filter(characteristic == "SDQ score was received") %>%
    rename(sdq_score_recd = "number") %>%
    mutate(sdq_score_recd = as.numeric(sdq_score_recd)) %>%
    select(-characteristic)

  # join the sdq_score_recd totals to the original dataset and calculate the weighted score
  data4 <- data3 %>%
    inner_join(data_totals, by = join_by(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, cla_group)) %>%
    mutate(sdq_score_recd_x_score = case_when(
      characteristic == "SDQ average score" ~ (as.numeric(sdq_score_recd) * as.numeric(number)),
      TRUE ~ NA
    ))

  data5 <- data4 %>%
    mutate(score_label = case_when(
      (number_num >= 0 & number_num < 14) ~ "Normal",
      (number_num >= 14 & number_num < 17) ~ "Borderline",
      (number_num >= 17 & number_num <= 40) ~ "Cause for concern",
      (number_num < 0) ~ "Supressed Score",
      TRUE ~ as.character("Error")
    ))

  # add stats neighbours
  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = data5,
    median_cols = c("number"),
    sum_cols = c(), # "value", "population_estimate"
    group_cols = c("LA.number", "time_period", "cla_group", "characteristic"),
  )
  final_dataset <- rbindlist(l = list(data5, sn_metrics), fill = TRUE, use.names = TRUE)
  final_dataset <- final_dataset %>%
    mutate(number = sapply(number, decimal_rounding, 1))

  return(final_dataset)
}

## Placement order and match data ----
read_placement_order_match_data <- function(file = "./data-raw/national_cla_adopted_average_time_between_adoption_process_stages.csv") {
  data <- read.csv(file)

  data <- data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National"
    )) %>%
    filter(stage_of_adoption_process == "2. Average time between decision that child should be placed for adoption and matching of child and adopters")

  data <- data %>%
    mutate(number_num = case_when(
      number == "c" ~ -100,
      number == "low" ~ -200,
      number == "k" ~ -200,
      number == "u" ~ -250,
      number == "x" ~ -300,
      number == "z" ~ -400,
      TRUE ~ as.numeric(number)
    ))

  # total(all ages)

  data <- data %>%
    mutate(age_start_poc = case_when(
      age_start_poc == "Total" ~ "Total (all ages)",
      TRUE ~ as.character(age_start_poc)
    ))

  data$months <- sapply(strsplit(data$number, ":"), function(x) {
    years <- as.numeric(x[1])
    months <- as.numeric(x[2])
    months <- years * 12 + months
    return(months)
  })


  return(data)
}










# Enabler 2 -----------------
# For filters to work nicely, we want to have two levels of grouping: geographic level (national, regional, LA)
# and level breakdown (region names and la names)

# firstly make a basic dataset to be usedfor the purposes of the GET_location calls
# the raw data file is not used anywhere else in read_data.R so we assume it's not in the pipeline

read_workforce_headline_measures <- function() {
  raw_file <- "data-raw/csww_headline_measures_2017_to_2022.csv"
  dataset <- fread(raw_file) %>%
    insert_geo_breakdown()
}

read_workforce_data <- function(sn_long, file = "./data-raw/csww_indicators_2017_to_2024.csv") {
  workforce_data <- fread(file)
  workforce_data <- workforce_data %>%
    colClean() %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data() %>%
    select(
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, old_la_code, turnover_rate_fte, time_period, "time_period", "turnover_rate_fte", "absence_rate_fte",
      "agency_rate_fte", "agency_cover_rate_fte", "vacancy_rate_fte", "vacancy_agency_cover_rate_fte",
      "turnover_rate_headcount", "agency_rate_headcount", "caseload_fte"
    )

  # old_la_code is a critical field and it's stored as a character in this dataset (with some exceptions e.g. 314 / 318 and 240 / 941)
  workforce_data[, original_old_la_code := old_la_code]
  workforce_data[, old_la_code := as.numeric(old_la_code)]

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = workforce_data,
    median_cols = c("turnover_rate_fte", "agency_rate_fte", "vacancy_rate_fte", "caseload_fte"),
    sum_cols = c(),
    group_cols = c("LA.number", "time_period")
  )
  workforce_data <- rbindlist(l = list(workforce_data, sn_metrics), fill = TRUE, use.names = TRUE)
  workforce_data[, old_la_code := (original_old_la_code)]

  workforce_data <- workforce_data %>%
    # removing old Dorset
    filter(!(new_la_code %in% dropList)) %>%
    mutate(turnover_rate_fte = sapply(turnover_rate_fte, decimal_rounding, 1)) %>%
    mutate(agency_rate_fte = sapply(agency_rate_fte, decimal_rounding, 1)) %>%
    mutate(vacancy_rate_fte = sapply(vacancy_rate_fte, decimal_rounding, 1)) %>%
    mutate(caseload_fte = sapply(caseload_fte, decimal_rounding, 1)) %>%
    distinct()

  workforce_data <- suppressWarnings(workforce_data %>%
    mutate(across(
      .cols = grep("fte", colnames(workforce_data)),
      .fns = ~ case_when(
        . == "c" ~ -100,
        . == "low" ~ -200,
        . == "k" ~ -200,
        . == "u" ~ -250,
        . == "x" ~ -300,
        . == "z" ~ -400,
        TRUE ~ as.numeric(.)
      ),
      .names = "{str_to_title(str_replace_all(.col, '_', ' '))}"
    )))

  return(workforce_data)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce ethnicity data
read_workforce_eth_data <- function(sn_long, file = "./data-raw/csww_role_by_characteristics_inpost_2019_to_2024.csv") {
  workforce_ethnicity_data <- fread(file)
  # Select only columns we want
  workforce_ethnicity_data <- workforce_ethnicity_data %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data() %>%
    select(
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, old_la_code, time_period,
      "time_period", "geographic_level", "region_name", "role", breakdown_topic, breakdown,
      inpost_FTE, inpost_FTE_percentage, inpost_headcount, inpost_headcount_percentage
    ) %>%
    # removing old Dorset
    filter(!(new_la_code %in% dropList))

  workforce_ethnicity_data$new_la_code[workforce_ethnicity_data$new_la_code == ""] <- NA
  workforce_ethnicity_data$region_code[workforce_ethnicity_data$region_code == ""] <- NA
  workforce_ethnicity_data <- mutate(workforce_ethnicity_data, code = coalesce(new_la_code, region_code, country_code))

  workforce_ethnicity_data <- convert_perc_cols_to_numeric(as.data.frame(workforce_ethnicity_data))

  # now we need to create a Non-white set of metrics
  setDT(workforce_ethnicity_data)
  non_white_data <- workforce_ethnicity_data[breakdown == "White" & role == "Total"]
  non_white_data[is.numeric(as.numeric(inpost_headcount_percentage)), inpost_headcount_percentage := as.character(100 - as.numeric(inpost_headcount_percentage))]
  non_white_data[, breakdown := "Non-white"]

  # and then append these to the dataset
  workforce_ethnicity_data <- rbindlist(list(workforce_ethnicity_data, non_white_data))



  # add stat neighbours

  workforce_ethnicity_data[, old_la_code := as.numeric(old_la_code)]
  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = workforce_ethnicity_data,
    median_cols = c("inpost_headcount_percentage"),
    sum_cols = c(), # "value", "population_estimate"
    group_cols = c("LA.number", "time_period", "role", "breakdown_topic", "breakdown")
  )

  final_dataset <- rbindlist(l = list(workforce_ethnicity_data, sn_metrics), fill = TRUE, use.names = TRUE)

  final_dataset <- final_dataset %>%
    mutate(`inpost_headcount_percentage` = sapply(`inpost_headcount_percentage`, decimal_rounding, 1))

  # this line has been removed from near the top
  # mutate(inpost_headcount_percentage = as.numeric(inpost_headcount_percentage)) %>%

  return(final_dataset)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce ethnicity by seniority data
read_workforce_eth_seniority_data <- function(file = "./data-raw/csww_role_by_characteristics_inpost_2019_to_2024.csv") {
  workforce_ethnicity_seniority_data <- read.csv(file)
  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    insert_geo_breakdown() %>%
    remove_cumbria_data() %>%
    select(
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, old_la_code, time_period,
      "time_period", "geographic_level", "region_name", "role", breakdown_topic, breakdown,
      inpost_FTE, inpost_FTE_percentage, inpost_headcount, inpost_headcount_percentage
    ) %>%
    filter(breakdown_topic == "Ethnicity major") %>%
    # removing old Dorset
    filter(!(new_la_code %in% dropList))

  workforce_ethnicity_seniority_data$new_la_code[workforce_ethnicity_seniority_data$new_la_code == ""] <- NA
  workforce_ethnicity_seniority_data$region_code[workforce_ethnicity_seniority_data$region_code == ""] <- NA
  workforce_ethnicity_seniority_data <- mutate(workforce_ethnicity_seniority_data, code = coalesce(new_la_code, region_code, country_code))

  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    mutate(seniority = case_when(
      role == "Total" ~ "Total",
      role == "Case holder" ~ "Case holder",
      role == "Qualified without cases" ~ "Qualified without cases",
      role == "Senior practitioner" ~ "Senior practitioner",
      role %in% c("First line manager", "Senior manager", "Middle manager") ~ "Manager"
    ))

  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    mutate(inpost_headcount = case_when(
      inpost_headcount == "Z" ~ 0,
      inpost_headcount == "x" ~ 0,
      TRUE ~ as.numeric(inpost_headcount)
    ))


  # #sum ethnicity counts to create grouped manager percents
  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    group_by(geographic_level, geo_breakdown, time_period, region_name, code, seniority, breakdown) %>%
    summarise_at(c("inpost_headcount"), sum) %>%
    filter(!(breakdown %in% c("Total", "Not known", "Known")))


  # sum ethnicity headcount to create total
  total_observation <- workforce_ethnicity_seniority_data %>%
    group_by(geographic_level, geo_breakdown, time_period, region_name, code, seniority) %>%
    summarise(Observation = sum(inpost_headcount), .groups = "keep")

  # Join the total observation back to the original dataframe
  workforce_ethnicity_seniority_data <- left_join(workforce_ethnicity_seniority_data, total_observation,
    by = c("geographic_level", "geo_breakdown", "time_period", "region_name", "code", "seniority")
  )

  # Create ethnicity percentages
  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    mutate(Percentage = round(inpost_headcount / Observation * 100, 1))

  # Filter to include only the latest year of data
  latest_year <- max(workforce_ethnicity_seniority_data$time_period)
  workforce_ethnicity_seniority_data <- subset(workforce_ethnicity_seniority_data, time_period == latest_year)

  return(workforce_ethnicity_seniority_data)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# General population ethnicity data


read_ethnic_population_data <- function(file1 = "./data-raw/ons-ethnic-population-reg.csv", file2 = "./data-raw/ons-ethnic-population-nat.csv", file3 = "./data-raw/ons-ethnic-population-la.csv") {
  # Read the csv files
  df_regions <- read.csv(file1, check.names = FALSE)
  df_countries <- read.csv(file2, check.names = FALSE)
  df_authorities <- read.csv(file3, check.names = FALSE)
  df_Inner_London <- read.csv(file3, check.names = FALSE)
  df_Outer_London <- read.csv(file3, check.names = FALSE)
  df_Kingston_upon_Thames <- read.csv(file3, check.names = FALSE)
  df_North_Northamptonshire <- read.csv(file3, check.names = FALSE)

  # Rename the columns to make them consistent across all data frames
  names(df_regions) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_countries) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_authorities) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_Inner_London) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_Outer_London) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_Kingston_upon_Thames) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_North_Northamptonshire) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")

  # Add 'geographic_level' column to each dataframe
  df_regions$geographic_level <- "Regional"
  df_countries$geographic_level <- "National"
  df_authorities$geographic_level <- "Local authority"
  df_Kingston_upon_Thames$geographic_level <- "Local authority"
  df_North_Northamptonshire$geographic_level <- "Local authority"
  df_Inner_London$geographic_level <- "Regional"
  df_Outer_London$geographic_level <- "Regional"

  # include just England to make national data
  df_countries <- df_countries[df_countries$Code %in% "E92000001", ]

  # create England data
  df_countries <- df_countries %>%
    mutate(Name = "National", Code = "E92000001")

  # select just Richmond upon Thames and Kingston upon Thames
  df_Kingston_upon_Thames <- df_Kingston_upon_Thames[df_Kingston_upon_Thames$Code %in% c("E09000021", "E09000027"), ]

  # select just North Northamptonshire and West Northamptonshire
  df_North_Northamptonshire <- df_North_Northamptonshire[df_North_Northamptonshire$Code %in% c("E06000061", "E06000062"), ]

  # remove Richmond upon Thames, Kingston upon Thames, North Northmptonshire & West Northamptonshire from LA file
  df_authorities <- df_authorities[!(df_authorities$Code %in% c("E09000021", "E09000027", "E06000061", "E06000062")), ]

  # include just inner London LAs to make inner London data
  df_Inner_London <- df_Inner_London[df_Inner_London$Code %in% c(
    "E09000001",
    "E09000007",
    "E09000012",
    "E09000013",
    "E09000014",
    "E09000019",
    "E09000020",
    "E09000022",
    "E09000023",
    "E09000025",
    "E09000028",
    "E09000030",
    "E09000032",
    "E09000033"
  ), ]

  # include just outer London LAs to make outer London data
  df_Outer_London <- df_Outer_London[df_Outer_London$Code %in% c(
    "E09000002",
    "E09000003",
    "E09000004",
    "E09000005",
    "E09000006",
    "E09000008",
    "E09000009",
    "E09000010",
    "E09000011",
    "E09000015",
    "E09000016",
    "E09000017",
    "E09000018",
    "E09000021",
    "E09000024",
    "E09000026",
    "E09000027",
    "E09000029",
    "E09000031"
  ), ]


  # create Kingston upon Thames/Richmond data (they submit a joint workforce return)
  df_Kingston_upon_Thames <- df_Kingston_upon_Thames %>%
    mutate(Name = "Kingston upon Thames / Richmond upon Thames", Code = "E09000021 / E09000027") %>%
    group_by(Code, Name, EthnicGroupCode, EthnicGroup, geographic_level) %>%
    summarise(Observation = sum(Observation), .groups = "drop")

  # create N/W Northamptonshire data (they submit a joint workforce return)
  df_North_Northamptonshire <- df_North_Northamptonshire %>%
    mutate(Name = "North Northamptonshire / West Northamptonshire", Code = "E06000061 / E06000062") %>%
    group_by(Code, Name, EthnicGroupCode, EthnicGroup, geographic_level) %>%
    summarise(Observation = sum(Observation), .groups = "drop")

  # create outer London data
  df_Inner_London <- df_Inner_London %>%
    mutate(Name = "Inner London", Code = "E13000001") %>%
    group_by(Code, Name, EthnicGroupCode, EthnicGroup, geographic_level) %>%
    summarise(Observation = sum(Observation), .groups = "drop")

  # create inner London data
  df_Outer_London <- df_Outer_London %>%
    mutate(Name = "Outer London", Code = "E13000002") %>%
    group_by(Code, Name, EthnicGroupCode, EthnicGroup, geographic_level) %>%
    summarise(Observation = sum(Observation), .groups = "drop")

  # Combine the data frames
  ethnic_population_data <- rbind(df_regions, df_countries, df_authorities, df_Inner_London, df_Outer_London, df_Kingston_upon_Thames, df_North_Northamptonshire)
  ethnic_population_data <- subset(ethnic_population_data, select = -c(EthnicGroupCode))

  # Remove rows where 'EthnicGroup' equals 'Does not apply'
  ethnic_population_data <- ethnic_population_data[ethnic_population_data$EthnicGroup != "Does not apply", ]

  # Convert 'EthnicGroup' to character type
  ethnic_population_data$EthnicGroup <- as.character(ethnic_population_data$EthnicGroup)

  # Add a new column 'EthnicGroupShort' that has a value of the first 5 characters in the 'EthnicGroup' column
  ethnic_population_data <- ethnic_population_data %>%
    mutate(EthnicGroupShort = substr(EthnicGroup, 1, 5))

  # Calculate the total observation for each 'Name'
  total_observation <- ethnic_population_data %>%
    group_by(Name, geographic_level) %>%
    summarise(TotalObservation = sum(Observation), .groups = "drop")

  # Join the total observation back to the original dataframe
  ethnic_population_data <- left_join(ethnic_population_data, total_observation, by = c("Name", "geographic_level"))

  # Group by 'Name', 'geographic_level' and 'EthnicGroupShort', and calculate the percentage, select unique values
  ethnic_population_data <- ethnic_population_data %>%
    group_by(Code, Name, geographic_level, EthnicGroupShort) %>%
    summarise(Percentage = round(sum(Observation) / TotalObservation * 100, 1), .groups = "drop") %>%
    unique()

  return(ethnic_population_data)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merge_eth_dataframes <- function(sn_long) {
  # Read the data
  workforce_eth <- read_workforce_eth_data(sn_long = sn_long)[geographic_level != "Statistical neighbours (median)"]
  population_eth <- read_ethnic_population_data()


  # Filter to include only the latest year of data
  latest_year <- max(workforce_eth$time_period)
  workforce_eth <- subset(workforce_eth, time_period == latest_year)

  # Filter to only include the ethnicity groups for all social workers
  workforce_eth <- workforce_eth %>%
    filter(breakdown_topic == "Ethnicity major", role == "Total") %>%
    filter(!(breakdown %in% c("Total", "Not known", "Known")))

  # Amend names of ethnic groups to match ONS data
  workforce_eth <- workforce_eth %>%
    mutate(breakdown = case_when(
      breakdown == "Mixed / Multiple ethnic groups" ~ "Mixed",
      breakdown == "Asian / Asian British" ~ "Asian",
      breakdown == "Black / African / Caribbean / Black British" ~ "Black",
      breakdown == "Other ethnic group" ~ "Other",
      TRUE ~ breakdown
    ))

  # Merge the two data frames
  merged_data <- left_join(workforce_eth, population_eth, by = c("code" = "Code", "breakdown" = "EthnicGroupShort"))

  return(merged_data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Enabler 3 -------------------
# Spending

read_spending_data <- function(sn_long, file = "./data-raw/RSX_LA_Data_2024-25_data_by_LA.ods") {
  data <- read_ods(file, sheet = "RSX_LA_Data_202425", range = "A7:DA430")
  data2 <- data %>% select("ONS Code", "Local authority", "Notes", "Class", "Detailed Class", "Certification", "Children Social Care - Total Expenditure (C3 = C1 + C2)", "Total Service Expenditure - Total Expenditure (C3 = C1 + C2)")

  data3 <- data2 %>%
    filter(data2$Class %in% c("UA", "MD", "LB", "SC", "Eng")) %>%
    rename(`CS Expenditure` = "Children Social Care - Total Expenditure (C3 = C1 + C2)", `Total Expenditure` = "Total Service Expenditure - Total Expenditure (C3 = C1 + C2)") %>%
    # replace "[x]" values with x
    mutate_all(~ gsub("\\[x\\]", "x", .)) %>%
    # replace & in local authority names with "and"
    mutate_all(~ gsub("\\&", "and", .)) %>%
    mutate(`Local authority` = gsub(" UA", "", `Local authority`)) %>%
    mutate(exp = case_when(
      `CS Expenditure` == "x" ~ 0,
      TRUE ~ as.numeric(`CS Expenditure`)
    )) %>%
    mutate(total_exp = case_when(
      `Total Expenditure` == "x" ~ 0,
      TRUE ~ as.numeric(`Total Expenditure`)
    ))


  # calculate the share of the
  # data3$cs_share <- janitor::round_half_up(((data3$exp) / (data3$total_exp)) * 100)
  data3$cs_share <- ((data3$exp) / (data3$total_exp)) * 100
  data3 <- data3 %>%
    mutate(cs_share = case_when(
      `CS Expenditure` == "x" ~ 0,
      TRUE ~ as.numeric(cs_share)
    ))

  merged_data <- merge(GET_location(cla_placements), data3, by.x = "new_la_code", by.y = "ONS Code", all = FALSE)
  merged_data$geographic_level <- "Local authority"
  merged_data$geo_breakdown <- merged_data$la_name
  merged_data$time_period <- "2024/25"
  merged_data <- merged_data %>%
    remove_cumbria_data() %>%
    select(time_period, geographic_level, geo_breakdown, region_name, new_la_code, old_la_code, "CS Expenditure", "Total Expenditure", exp, total_exp, cs_share) %>%
    # removing old Dorset
    filter(!(new_la_code %in% dropList))

  # get national level data
  national_data <- data3 %>% filter(data3$Class == "Eng")
  national_data$geographic_level <- "National"
  national_data$geo_breakdown <- "National"
  national_data$time_period <- "2024/25"
  national_data$new_la_code <- as.character("")
  national_data$old_la_code <- as.numeric("")
  national_data <- national_data %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, "CS Expenditure", "Total Expenditure", exp, total_exp, cs_share)

  regional_spending <- merged_data %>%
    group_by(region_name) %>%
    summarise(exp = sum(exp), total_exp = sum(total_exp), cs_share = ((exp / total_exp) * 100)) %>%
    rename("geo_breakdown" = "region_name")
  # regional_spending$cs_share <- janitor::round_half_up(regional_spending$cs_share)
  regional_spending$geographic_level <- "Regional"
  regional_spending$time_period <- "2024/25"
  regional_spending$new_la_code <- as.character("")
  regional_spending$old_la_code <- as.numeric("")

  london_com <- regional_spending %>%
    filter(geo_breakdown == "Inner London" | geo_breakdown == "Outer London") %>%
    summarise(exp = sum(exp), total_exp = sum(total_exp), cs_share = ((exp / total_exp) * 100)) %>%
    mutate(
      "time_period" = "2024/25",
      "geographic_level" = "Regional",
      "geo_breakdown" = "London",
      "new_la_code" = "",
      "old_la_code" = as.numeric("")
    )
  # london_com$cs_share <- janitor::round_half_up(london_com$cs_share)

  regional_spending <- rbind(regional_spending, london_com)

  df <- full_join(merged_data, national_data, by = c("time_period", "geographic_level", "geo_breakdown", "new_la_code", "old_la_code", "CS Expenditure", "Total Expenditure", "exp", "total_exp", "cs_share"))
  df2 <- full_join(df, regional_spending, by = c("time_period", "geographic_level", "geo_breakdown", "new_la_code", "old_la_code", "exp", "total_exp", "cs_share"))


  df2$cs_share <- round(df2$cs_share, 1)

  final_dataset <- df2 %>%
    mutate(exp = case_when(
      `CS Expenditure` == "x" ~ -300,
      TRUE ~ as.numeric(exp)
    )) %>%
    mutate(total_exp = case_when(
      `Total Expenditure` == "x" ~ -300,
      TRUE ~ as.numeric(total_exp)
    )) %>%
    mutate(cs_share = case_when(
      `Total Expenditure` == "x" ~ -300,
      TRUE ~ as.numeric(cs_share)
    )) %>%
    mutate(`CS Share` = case_when(
      cs_share == -300 ~ "x",
      TRUE ~ as.character(cs_share)
    )) %>%
    mutate(`CS Share` = ifelse(!is.na(as.numeric(`CS Share`)),
      format(as.numeric(as.character(`CS Share`)), nsmall = 1),
      `CS Share`
    )) %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, "CS Expenditure", "Total Expenditure", exp, total_exp, cs_share, "CS Share")
  # final_dataset$cs_share <- janitor::round_half_up(final_dataset$cs_share)

  # add stat neighbours
  setDT(final_dataset)

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = final_dataset,
    median_cols = c("CS Share"),
    sum_cols = c(), # "value", "population_estimate"
    group_cols = c("LA.number", "time_period"),
  )

  final_dataset <- rbindlist(l = list(final_dataset, sn_metrics), fill = TRUE, use.names = TRUE)

  final_dataset <- final_dataset %>%
    mutate(`CS Share` = sapply(`CS Share`, decimal_rounding, 1))

  return(final_dataset)
}

read_per_capita_spending <- function(sn_long, file = "./data-raw/mye24tablesew.xlsx") {
  population_estimates <- read_excel(file, sheet = "MYE2 - Persons", range = "A8:V412")
  test_df <- population_estimates
  test_df$under18 <- rowSums(test_df[, c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17")])
  test_df2 <- test_df[grepl("^E10|^E06|^E08|^E09", test_df$Code), ]
  local_authority_pop <- test_df2 %>% select("Code", "Name", "Geography", "under18")

  regional_pop <- test_df[grepl("^E12", test_df$Code), ]
  regional_pop$Name <- str_to_title((regional_pop$Name))
  regional_pop$Name[regional_pop$Name == "Yorkshire And The Humber"] <- "Yorkshire and The Humber"
  regional_pop$Name[regional_pop$Name == "East"] <- "East of England"
  regional_pop <- regional_pop %>% select("Code", "Name", "Geography", "under18")

  # Join local authority and regional pop together
  population <- rbind(local_authority_pop, regional_pop)
  # Get national data
  national_pop <- sum(population[population$Geography == "Region", "under18"], na.rm = FALSE)
  new_row <- data.frame(Code = "E92000001", Name = "National", Geography = "National", under18 = national_pop)
  population2 <- rbind(population, new_row)
  # Correct formatting
  population3 <- population2 %>%
    mutate(geographic_level = case_when(
      Geography == "Region" ~ "Regional",
      Geography == "County" ~ "Local authority",
      Geography == "London Borough" ~ "Local authority",
      Geography == "Metropolitan District" ~ "Local authority",
      Geography == "Unitary Authority" ~ "Local authority",
      TRUE ~ as.character(Geography)
    )) %>%
    select("Code", "Name", "geographic_level", "under18") %>%
    rename("new_la_code" = "Code", "geo_breakdown" = "Name")

  # Need to make sure the LA's in population estimates are the same as the spending data LA's
  # Joining west moreland + cumberland to get cumbria
  # cumbria_pop <- sum(population3[(population3$geo_breakdown == "Westmorland and Furness" | population3$geo_breakdown == "Cumberland"), "under18"], na.rm = FALSE)
  # new_row1 <- data.frame(new_la_code = "E10000006", geographic_level = "Local authority", geo_breakdown = "Cumbria", under18 = cumbria_pop)
  # population4 <- rbind(population3, new_row1)

  # Joining up LA's to get population Estimates for inner and outer london
  outer_london_list <- c("E09000002", "E09000003", "E09000004", "E09000005", "E09000006", "E09000008", "E09000009", "E09000010", "E09000011", "E09000015", "E09000016", "E09000017", "E09000018", "E09000021", "E09000024", "E09000026", "E09000027", "E09000029", "E09000031")
  inner_london_list <- c("E09000007", "E09000001", "E09000012", "E09000013", "E09000014", "E09000019", "E09000020", "E09000022", "E09000023", "E09000025", "E09000028", "E09000030", "E09000032", "E09000033")
  inner_london <- sum(population3[(population3$new_la_code %in% inner_london_list), "under18"], na.rm = FALSE)
  outer_london <- sum(population3[(population3$new_la_code %in% outer_london_list), "under18"], na.rm = FALSE)
  inner_row <- data.frame(new_la_code = "", geographic_level = "Regional", geo_breakdown = "Inner London", under18 = inner_london)
  outer_row <- data.frame(new_la_code = "", geographic_level = "Regional", geo_breakdown = "Outer London", under18 = outer_london)

  population4 <- rbind(population3, inner_row) %>%
    rbind(outer_row) %>%
    select(-c(new_la_code))

  spending_data <- suppressWarnings(read_spending_data(sn_long = sn_long))
  joined_data <- left_join(spending_data[geographic_level != "Statistical neighbours (median)"], population4, by = c("geographic_level", "geo_breakdown"))
  joined_data$`Cost per child` <- format((joined_data$exp / joined_data$under18) * 1000, digits = 1)
  joined_data$cost_per_capita <- format((joined_data$exp / joined_data$under18) * 1000, digits = 1)

  joined_data2 <- joined_data %>%
    mutate(`Cost per child` = case_when(
      exp == -300 ~ "x",
      TRUE ~ as.character(`Cost per child`)
    )) %>%
    mutate(cost_per_capita = case_when(
      exp == -300 ~ -300,
      TRUE ~ as.numeric(cost_per_capita)
    ))

  joined_data2$`Cost per child` <- formatC(joined_data2$`Cost per child`, format = "f", big.mark = ",", digits = 0)
  joined_data2$cost_per_capita <- janitor::round_half_up(joined_data2$cost_per_capita)

  # add stat neighbours
  setDT(joined_data2)
  joined_data2 <- remove_cumbria_data(joined_data2)

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = joined_data2,
    median_cols = c("Cost per child"),
    sum_cols = c(), # "value", "population_estimate"
    group_cols = c("LA.number", "time_period")
  )

  final_dataset <- rbindlist(l = list(joined_data2, sn_metrics), fill = TRUE, use.names = TRUE)

  final_dataset <- final_dataset %>%
    mutate(`Cost per child` = sapply(`Cost per child`, decimal_rounding, 0)) %>%
    return(final_dataset)
}

read_spending_data2 <- function(sn_long, file = "./data-raw/RO3_LA_DATA_2024-25_data_by_LA.ods") {
  data <- read_ods(file, sheet = "RO3_LA_Data_202425", range = "A7:SC418")
  data2 <- data %>% select("ONS Code", "Local authority", "Notes", "Class", "Detailed Class", "Certification", "TOTAL CHILDREN SOCIAL CARE - Total Expenditure (C3 = C1 + C2)", "Children's social care - Children Looked After [note 3] - Total Expenditure (C3 = C1 + C2)")

  data3 <- data2 %>%
    filter(data2$Class %in% c("UA", "MD", "LB", "SC", "Eng")) %>%
    rename(`CLA Expenditure` = "Children's social care - Children Looked After [note 3] - Total Expenditure (C3 = C1 + C2)", `Total Expenditure` = "TOTAL CHILDREN SOCIAL CARE - Total Expenditure (C3 = C1 + C2)") %>%
    # replace "[x]" values with x
    mutate_all(~ gsub("\\[x\\]", "x", .)) %>%
    # replace & in local authority names with "and"
    mutate_all(~ gsub("\\&", "and", .)) %>%
    mutate(`Local authority` = gsub(" UA", "", `Local authority`)) %>%
    mutate(cla_exp = case_when(
      `CLA Expenditure` == "x" ~ 0,
      TRUE ~ as.numeric(`CLA Expenditure`)
    )) %>%
    mutate(total_exp = case_when(
      `Total Expenditure` == "x" ~ 0,
      TRUE ~ as.numeric(`Total Expenditure`)
    ))
  # calculate the share of the expenditure not for CLA
  # data3$minus_cla_share <- janitor::round_half_up(((data3$total_exp - data3$cla_exp) / (data3$total_exp)) * 100)
  data3$minus_cla_share <- ((data3$total_exp - data3$cla_exp) / (data3$total_exp)) * 100

  data3 <- data3 %>%
    mutate(minus_cla_share = case_when(
      `CLA Expenditure` == "x" ~ 0,
      TRUE ~ as.numeric(minus_cla_share)
    ))

  merged_data <- merge(GET_location(cla_placements), data3, by.x = "new_la_code", by.y = "ONS Code", all = FALSE)
  merged_data$geographic_level <- "Local authority"
  merged_data$geo_breakdown <- merged_data$la_name
  merged_data$time_period <- "2024/25"
  merged_data <- merged_data %>%
    select(time_period, geographic_level, geo_breakdown, region_name, new_la_code, old_la_code, "CLA Expenditure", "Total Expenditure", cla_exp, total_exp, minus_cla_share)

  # get national level data
  national_data <- data3 %>% filter(data3$Class == "Eng")
  national_data$geographic_level <- "National"
  national_data$geo_breakdown <- "National"
  national_data$time_period <- "2024/25"
  national_data$new_la_code <- as.character("")
  national_data$old_la_code <- as.numeric("")
  national_data <- national_data %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, "CLA Expenditure", "Total Expenditure", cla_exp, total_exp, minus_cla_share)

  regional_spending <- merged_data %>%
    group_by(region_name) %>%
    summarise(cla_exp = sum(cla_exp), total_exp = sum(total_exp), minus_cla_share = (((total_exp - cla_exp) / total_exp) * 100)) %>%
    rename("geo_breakdown" = "region_name")
  # regional_spending$minus_cla_share <- janitor::round_half_up(regional_spending$minus_cla_share)
  regional_spending$geographic_level <- "Regional"
  regional_spending$time_period <- "2024/25"
  regional_spending$new_la_code <- as.character("")
  regional_spending$old_la_code <- as.numeric("")

  london_com <- regional_spending %>%
    filter(geo_breakdown == "Inner London" | geo_breakdown == "Outer London") %>%
    summarise(cla_exp = sum(cla_exp), total_exp = sum(total_exp), minus_cla_share = (((total_exp - cla_exp) / total_exp) * 100)) %>%
    mutate(
      "time_period" = "2024/25",
      "geographic_level" = "Regional",
      "geo_breakdown" = "London",
      "new_la_code" = as.character(""),
      "old_la_code" = as.numeric(""),
    )
  # london_com$minus_cla_share <- janitor::round_half_up(london_com$minus_cla_share)

  regional_spending <- rbind(regional_spending, london_com)

  df <- full_join(merged_data, national_data, by = c("time_period", "geographic_level", "geo_breakdown", "new_la_code", "old_la_code", "CLA Expenditure", "Total Expenditure", "cla_exp", "total_exp", "minus_cla_share"))
  df2 <- full_join(df, regional_spending, by = c("time_period", "geographic_level", "geo_breakdown", "new_la_code", "old_la_code", "cla_exp", "total_exp", "minus_cla_share"))

  setDT(df2)
  df2 <- remove_cumbria_data(df2)
  df2[`CLA Expenditure` == "x" | `Total Expenditure` == "x", minus_cla_share := NA]
  df2[, minus_cla_share := true_round(minus_cla_share, 1)]

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = df2,
    median_cols = c("minus_cla_share"),
    sum_cols = c(), # "value", "population_estimate"
    group_cols = c("LA.number", "time_period")
  )

  final_dataset <- rbindlist(l = list(df2, sn_metrics), fill = TRUE, use.names = TRUE)

  final_dataset <- final_dataset %>%
    mutate(minus_cla_share = sapply(as.character(minus_cla_share), decimal_rounding, 1)) %>%
    mutate(`Excluding CLA Share` = case_when(
      is.na(minus_cla_share) ~ "x",
      TRUE ~ as.character(minus_cla_share)
    )) %>%
    mutate(exp = case_when(
      `CLA Expenditure` == "x" ~ -300,
      TRUE ~ as.numeric(cla_exp)
    )) %>%
    mutate(total_exp = case_when(
      `Total Expenditure` == "x" ~ -300,
      TRUE ~ as.numeric(total_exp)
    )) %>%
    mutate(minus_cla_share = case_when(
      `Total Expenditure` == "x" ~ -300,
      TRUE ~ as.numeric(minus_cla_share)
    )) %>%
    select(time_period, geographic_level, geo_breakdown, geo_breakdown_sn, new_la_code, old_la_code, "CLA Expenditure", "Total Expenditure", cla_exp, total_exp, minus_cla_share, "Excluding CLA Share")

  return(final_dataset)
}

# Ofsted leadership data
read_ofsted_leadership_data <- function(sn_long, file = "./data-raw/LA_Inspection_Outcomes_as_at_31_March_2025.ods") {
  # Import data and drop top 3 rows to ensure headers are correct
  file <- "./data-raw/LA_Inspection_Outcomes_as_at_31_March_2025.ods"
  ofsted_leadership_data <- read_ods(file, sheet = "Inspections_as_at_31_March", skip = 2)

  # Remove authorities that aren't yet inspected
  ofsted_leadership_data <- ofsted_leadership_data %>%
    filter(`Inspection date` != "Not yet inspected")

  # Convert "Inspection date" column to date format and copy the year into new "time_period" column
  ofsted_leadership_data$`Inspection date` <- as.Date(ofsted_leadership_data$`Inspection date`, format = "%d/%m/%Y")
  ofsted_leadership_data$inspection_year <- format(ofsted_leadership_data$`Inspection date`, "%Y")
  ofsted_leadership_data$published_year <- year(as.Date(ofsted_leadership_data$`Publication date`, format = "%d/%m/%Y"))
  ofsted_leadership_data$time_period <- max(format(ofsted_leadership_data$`Inspection date`, "%Y"))

  ofsted_leadership_data <- ofsted_leadership_data %>%
    select(-c(
      `Web link`,
      `Overall effectiveness`,
      `The experiences and progress of children who need help and protection`,
      `The experiences and progress of children in care`,
      `The experiences and progress of care leavers`
    ))

  # Tidy column names
  ofsted_leadership_data <- ofsted_leadership_data %>%
    rename(
      "geo_breakdown" = `Local authority name`,
      "region" = `Ofsted region`,
      "inspection_date" = `Inspection date`,
      "impact_of_leaders" = `The impact of leaders on social work practice with children and families`
    ) %>%
    mutate(geo_breakdown = case_when(
      geo_breakdown == "Bristol" ~ "Bristol, City of",
      geo_breakdown == "Durham" ~ "County Durham",
      geo_breakdown == "Bournemouth, Christchurch & Poole" ~ "Bournemouth, Christchurch and Poole",
      geo_breakdown == "Herefordshire" ~ "Herefordshire, County of",
      geo_breakdown == "Hammersmith & Fulham" ~ "Hammersmith and Fulham",
      geo_breakdown %in% c("Kingston Upon Hull", "Kingston upon Hull") ~ "Kingston upon Hull, City of",
      geo_breakdown == "Telford & Wrekin" ~ "Telford and Wrekin",
      geo_breakdown == "Richmond Upon Thames" ~ "Richmond upon Thames",
      geo_breakdown == "St Helens" ~ "St. Helens",
      TRUE ~ as.character(geo_breakdown)
    )) %>%
    remove_cumbria_data()

  setDT(ofsted_leadership_data)
  # we now need a step to correct the ofsted regions using a csv file with the correct mappings
  ofsted_region_corrections <- fread("./data-raw/ofsted_region_mapping_corrections.csv")

  ofsted_leadership_data <- merge(ofsted_leadership_data, ofsted_region_corrections, by.x = "geo_breakdown", by.y = "la_name", all.x = TRUE)
  ofsted_leadership_data[!is.na(region_name_correct), region := region_name_correct][, region_name_correct := NULL]


  # Assign all current values as "Local authority" (before combining data to get Regional and National values)
  ofsted_leadership_data$geographic_level <- "Local authority"

  # Get old_la_code values from cla_rates
  cla_rates_selected <- read_cla_rate_data(sn_long = sn_long) %>% select(geo_breakdown, old_la_code)
  cla_rates_selected <- cla_rates_selected %>% distinct(geo_breakdown, old_la_code, .keep_all = TRUE)
  ofsted_leadership_data <- left_join(ofsted_leadership_data, cla_rates_selected, by = c("geo_breakdown" = "geo_breakdown"))

  ofsted_leadership_data <- ofsted_leadership_data %>%
    mutate(
      inadequate_count = ifelse(impact_of_leaders == "Inadequate", 1, 0),
      requires_improvement_count = ifelse(impact_of_leaders == "Requires improvement to be good", 1, 0),
      good_count = ifelse(impact_of_leaders == "Good", 1, 0),
      outstanding_count = ifelse(impact_of_leaders == "Outstanding", 1, 0)
    )

  # Create a new dataframe with 'geo_breakdown' column as the region name from 'region' column
  region_counts <- ofsted_leadership_data %>% mutate(geo_breakdown = region)

  # Summarise the counts for each region
  region_counts <- region_counts %>%
    group_by(geo_breakdown) %>%
    summarise(
      inadequate_count = sum(inadequate_count),
      requires_improvement_count = sum(requires_improvement_count),
      good_count = sum(good_count),
      outstanding_count = sum(outstanding_count),
      time_period = max(time_period)
    )

  region_counts$geographic_level <- "Regional"

  # Create a new dataframe for the London counts
  london_counts <- region_counts %>%
    filter(geo_breakdown %in% c("Inner London", "Outer London")) %>%
    summarise(
      geo_breakdown = "London",
      time_period = max(time_period),
      geographic_level = "Regional",
      requires_improvement_count = sum(requires_improvement_count),
      inadequate_count = sum(inadequate_count),
      good_count = sum(good_count),
      outstanding_count = sum(outstanding_count)
    )

  # Create a new dataframe for the national counts
  national_counts <- region_counts %>%
    summarise(
      geo_breakdown = "National",
      time_period = max(time_period),
      geographic_level = "National",
      requires_improvement_count = sum(requires_improvement_count),
      inadequate_count = sum(inadequate_count),
      good_count = sum(good_count),
      outstanding_count = sum(outstanding_count)
    )

  # Combine the new data with the existing data
  ofsted_leadership_data <- bind_rows(ofsted_leadership_data, region_counts, london_counts, national_counts)

  # add stat neighbours
  setDT(ofsted_leadership_data)

  # now calculate SN metrics and append to the bottom of the dataset
  sn_metrics <- sn_aggregations(
    sn_long = sn_long,
    dataset = ofsted_leadership_data,
    median_cols = c(),
    sum_cols = c("requires_improvement_count", "inadequate_count", "good_count", "outstanding_count"), # "value", "population_estimate"
    group_cols = c("LA.number", "time_period")
  )
  # sn_metrics[, time_period := max(time_period)]

  ofsted_leadership_data <- rbindlist(l = list(ofsted_leadership_data, sn_metrics), fill = TRUE, use.names = TRUE)

  # Flip the data so the geographic_levels are in order for the dropdown
  ofsted_leadership_data <- ofsted_leadership_data[nrow(ofsted_leadership_data):1, ]

  # trim off any extra columns
  ofsted_leadership_data <- ofsted_leadership_data[, .(geo_breakdown, region, inspection_date, impact_of_leaders, inspection_year, published_year, time_period, geographic_level, old_la_code, inadequate_count, requires_improvement_count, good_count, outstanding_count, geo_breakdown_sn)]

  return(ofsted_leadership_data)
}


pivot_ofsted_data <- function(ofsted_leadership_data) {
  # Pivoted version
  ofsted_leadership_data_long <- ofsted_leadership_data %>%
    pivot_longer(
      cols = c(inadequate_count, requires_improvement_count, good_count, outstanding_count),
      names_to = "Rating",
      values_to = "Count"
    )

  return(ofsted_leadership_data_long)
}
