generate_choice_text1 <- function(select_geography, geographic_breakdown, region_name) {
  if (select_geography == "National") {
    paste0("You have selected ", tags$b(select_geography), " level statistics on ", tags$b("England"), ".")
  } else if (select_geography == "Regional") {
    paste0("You have selected ", tags$b(select_geography), " level statistics for ", tags$b(geographic_breakdown), ".")
  } else if (select_geography == "Local authority") {
    paste0("You have selected ", tags$b(select_geography), " level statistics for ", tags$b(geographic_breakdown), ", in ", region_name, ".")
  }
}

generate_choice_text2 <- function(national_comparison_checkbox, region_comparison_checkbox, sn_comparison_checkbox) {
  comparisons <- c()
  choice_text2 <- ""

  # Checking to see if they picked national average comparison
  if (!is.null(national_comparison_checkbox)) comparisons <- c(comparisons, "National average")
  if (!is.null(region_comparison_checkbox)) comparisons <- c(comparisons, "Regional average")
  if (!is.null(sn_comparison_checkbox)) comparisons <- c(comparisons, "Statistical neighbours average")

  if (length(comparisons) == 0) {
    return(choice_text2)
  }
  if (length(comparisons) == 1) {
    choice_text2 <- paste0("You have also selected to compare with the ", tags$b(comparisons[1]), ".")
    return(choice_text2)
  }
  if (length(comparisons) == 2) {
    choice_text2 <- paste0("You have also selected to compare with the ", tags$b(comparisons[1]), " and the ", tags$b(comparisons[2]), ".")
    return(choice_text2)
  }
  if (length(comparisons) == 3) {
    choice_text2 <- paste0("You have also selected to compare with the ", tags$b(comparisons[1]), ", the ", tags$b(comparisons[2]), " and the ", tags$b(comparisons[3]), ".")
    return(choice_text2)
  }
  return(choice_text2)
}
