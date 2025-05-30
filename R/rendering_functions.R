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
  # browser()
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
