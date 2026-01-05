generate_choice_text1 <- function(select_geography, geographic_breakdown, region_name) {
  if (select_geography == "National") {
    paste0("You have selected ", tags$b(select_geography), " level statistics on ", tags$b("England"), ".")
  } else if (select_geography == "Regional") {
    paste0("You have selected ", tags$b(select_geography), " level statistics for ", tags$b(geographic_breakdown), ".")
  } else if (select_geography == "Local authority") {
    paste0("You have selected ", tags$b(select_geography), " level statistics for ", tags$b(geographic_breakdown), ", in ", region_name, ".")
  }
}

generate_choice_text2 <- function(national_comparison_checkbox = NULL, region_comparison_checkbox = NULL, sn_comparison_checkbox = NULL, summary_page = NULL, select_geography = NULL) {
  comparisons <- c()
  choice_text2 <- ""

  if (is.null(summary_page)) {
    # Checking to see if they picked national average comparison
    if (!is.null(national_comparison_checkbox)) comparisons <- c(comparisons, "National average")
    if (!is.null(region_comparison_checkbox)) comparisons <- c(comparisons, "Regional average")
    if (!is.null(sn_comparison_checkbox)) comparisons <- c(comparisons, "Statistical neighbours average")
    comparison_text <- "You have also selected to compare with the "
  } else {
    if (select_geography == "Regional") comparisons <- c("National average")
    if (select_geography == "Local authority") comparisons <- c("National average", "Regional average", "Statistical neighbours average")
    comparison_text <- "You will also be shown comparisons with the "
  }

  if (length(comparisons) == 0) {
    return(choice_text2)
  }
  if (length(comparisons) == 1) {
    choice_text2 <- paste0(comparison_text, tags$b(comparisons[1]), ".")
    return(choice_text2)
  }
  if (length(comparisons) == 2) {
    choice_text2 <- paste0(comparison_text, tags$b(comparisons[1]), " and the ", tags$b(comparisons[2]), ".")
    return(choice_text2)
  }
  if (length(comparisons) == 3) {
    choice_text2 <- paste0(comparison_text, tags$b(comparisons[1]), ", the ", tags$b(comparisons[2]), " and the ", tags$b(comparisons[3]), ".")
    return(choice_text2)
  }
  return(choice_text2)
}
#
#
# output$caseload_txt <- renderText({
#   if (input$geographic_breakdown_e3 == "") {
#     stat <- "NA"
#     paste0(stat, "<br>")
#   } else {
#     previous_year_value <- workforce_data %>%
#       filter(time_period == (max(workforce_data$time_period) - 1) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
#       select(caseload_fte)
#
#     current_year_value <- workforce_data %>%
#       filter(time_period == (max(workforce_data$time_period)) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
#       select(caseload_fte)
#
#     if (nrow(previous_year_value) < 1) {
#       context <- ""
#     } else if ((current_year_value < previous_year_value)) {
#       context <- paste0(" down from ", previous_year_value, " ", (max(workforce_data$time_period) - 1))
#     } else if ((current_year_value > previous_year_value)) {
#       context <- paste0(" up from ", previous_year_value, " ", (max(workforce_data$time_period) - 1))
#     } else {
#       context <- "No change"
#     }
#     stat <- format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e3) %>% select(caseload_fte), nsmall = 1)
#     paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "in ", max(workforce_data$time_period), context, "</p>")
#   }
# })


generate_headline_box_text <- function(dataset, column_name, geo_breakdown, nsmall, format_style) {
  # check for a dataset, column name, geo_breakdown

  # get the max period
  max_period <- max(dataset[[column_name]])
  prior_period <- max_period - 1

  # get the stat for CY

  if (geo_breakdown == "" || nrow(stat) == 0) {
    stat <- "NA"
  }

  # get the stat for PY

  # generate a formatted text output to be rendered - percentage here, any other extra text?

  stat <- format(
    dataset %>%
      filter(time_period == max_period & geo_breakdown %in% geo_breakdown) %>%
      select(column_name),
    nsmall = nsmall
  )

  paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "in ", max_period, context, "</p>")
  headline_text <- paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max_period, ")", "</p>")
}
