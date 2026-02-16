# TECHDEBT: this can be somewhat tidied up
stats_neighbours_table <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, selectedcolumn = NULL, yvalue = NULL) {
  sn_names <- stats_neighbours %>%
    filter(stats_neighbours$LA.Name == selected_geo_breakdown) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.character()

  if (is.null(selectedcolumn)) {
    data2 <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, sn_names)) %>%
      select(time_period, geo_breakdown, `yvalue`) %>%
      mutate(
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "statistical neighbours")
      ) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`)))) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(`yvalue`, ~ str_to_title(str_replace_all(., "_", " "))) %>%
      mutate_at(str_to_title(str_replace_all(yvalue, "_", " ")), ~ case_when(
        . == "z" ~ -400,
        . == "c" ~ -100,
        . == "k" ~ -200,
        . == "x" ~ -300,
        TRUE ~ as.numeric(.)
      )) %>%
      arrange(desc(!!sym(str_to_title(str_replace_all(yvalue, "_", " ")))))
  } else {
    data2 <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, sn_names)) %>%
      select(all_of(c("time_period", "geo_breakdown", selectedcolumn, yvalue))) %>%
      mutate(
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "statistical neighbours")
      ) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(`yvalue`, ~ str_to_title(str_replace_all(., "_", " "))) %>%
      mutate_at(str_to_title(str_replace_all(yvalue, "_", " ")), ~ case_when(
        . == "z" ~ -400,
        . == "c" ~ -100,
        . == "k" ~ -200,
        . == "x" ~ -300,
        TRUE ~ as.numeric(.)
      )) %>%
      arrange(desc(!!sym(str_to_title(str_replace_all(yvalue, "_", " ")))))
  }
}

# Now a couple of custom tabulation functions for the UASC and Ofsted tables ----

stats_neighbours_table_uasc <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue) {
  sn_names <- stats_neighbours %>%
    filter(stats_neighbours$LA.Name == selected_geo_breakdown) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.character()

  data2 <- dataset %>%
    filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, sn_names)) %>%
    select(geo_breakdown, characteristic, `yvalue`) %>%
    mutate(
      is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "statistical neighbours")
    ) %>%
    rename(`Local authority` = `geo_breakdown`, `UASC status` = `characteristic`, `Selection` = `is_selected`) %>%
    rename_at(`yvalue`, ~ str_to_title(str_replace_all(., "_", " "))) %>%
    mutate_at(str_to_title(str_replace_all(yvalue, "_", " ")), ~ case_when(
      . == "z" ~ -400,
      . == "c" ~ -100,
      . == "k" ~ -200,
      . == "x" ~ -300,
      TRUE ~ as.numeric(.)
    )) %>%
    arrange(desc(!!sym(str_to_title(str_replace_all(yvalue, "_", " ")))))
}

stats_neighbours_table_ofsted <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, selectedcolumn = NULL, yvalue = NULL) {
  sn_names <- stats_neighbours %>%
    filter(stats_neighbours$LA.Name == selected_geo_breakdown) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.character()

  if (is.null(selectedcolumn)) {
    data2 <- dataset %>%
      filter(geographic_level == "Local authority", geo_breakdown %in% c(selected_geo_breakdown, sn_names), Count == 1) %>%
      select(latest_rating, geo_breakdown, `yvalue`) %>%
      mutate(
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "statistical neighbours")
      ) %>%
      rename(`Last rated` = `latest_rating`, `Local authority` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(`yvalue`, ~ str_to_title(str_replace_all(., "_", " "))) %>%
      mutate_at(str_to_title(str_replace_all(yvalue, "_", " ")), ~ case_when(
        . == "z" ~ "-400",
        . == "c" ~ "-100",
        . == "k" ~ "-200",
        . == "x" ~ "-300",
        TRUE ~ as.character(.)
      )) %>%
      arrange(desc(!!sym(str_to_title(str_replace_all(yvalue, "_", " ")))))
  } else {
    data2 <- dataset %>%
      filter(geographic_level == "Local authority", geo_breakdown %in% c(selected_geo_breakdown, sn_names)) %>%
      select(all_of(c("latest_rating", "geo_breakdown", selectedcolumn, yvalue))) %>%
      mutate(
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "statistical neighbours")
      ) %>%
      rename(`Last rated` = `latest_rating`, `Local authority` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(`yvalue`, ~ str_to_title(str_replace_all(., "_", " "))) %>%
      mutate_at(str_to_title(str_replace_all(yvalue, "_", " ")), ~ case_when(
        . == "z" ~ "-400",
        . == "c" ~ "-100",
        . == "k" ~ "-200",
        . == "x" ~ "-300",
        TRUE ~ as.character(.)
      )) %>%
      arrange(desc(!!sym(str_to_title(str_replace_all(yvalue, "_", " ")))))
  }
}


# Tabulation helper functions ----

# Ordering tables with suppression
cellfunc <- function(value) {
  if (is.na(value)) {
    return("z")
  } else if (value == -100) {
    return("c")
  } else if (value == -200) {
    return("k")
  } else if (value == -250) {
    return("u")
  } else if (value == -300) {
    return("x")
  } else if (value == -400) {
    return("z")
  } else {
    return(value)
  }
}

# Ordering tables with suppression
cellfunc_decimal_percent <- function(value) {
  if (is.na(value)) {
    return("z")
  } else if (value == -100) {
    return("c")
  } else if (value == -200) {
    return("k")
  } else if (value == -250) {
    return("u")
  } else if (value == -300) {
    return("x")
  } else if (value == -400) {
    return("z")
  } else {
    return(format(value, nsmall = 1))
  }
}

# Ordering tables with suppression
cellfunc_social_ethnicity <- function(value) {
  if (is.na(value)) {
    return("NA")
  } else if (value == -100) {
    return("c")
  } else if (value == -200) {
    return("k")
  } else if (value == -250) {
    return("u")
  } else if (value == -300) {
    return("x")
  } else if (value == -400) {
    return("z")
  } else {
    return(format(value, nsmall = 1))
  }
}
