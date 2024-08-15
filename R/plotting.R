# CSC charts

# Time series repeat function ----
# This is a repeat use function for all of the time series plots in this dashboard.

plotly_time_series_custom_scale <- function(dataset, level, breakdown, yvalue, yaxis_title, ylim_upper, add_rect = FALSE, percentage = FALSE) {
  # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
  ylim_upper <- (ceiling(ylim_upper / 20) * 20) + (ylim_upper * 0.05)

  # add_rect is only true for graphs with boundaries - Wellbeing SDQ score charts
  if (add_rect == FALSE) {
    filtered_data <- dataset %>%
      select(time_period, geo_breakdown, `yvalue`) %>%
      mutate(`Time period` = as.character(`time_period`)) %>%
      rename(`Location` = `geo_breakdown`) %>%
      rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))

    p <- ggplot(filtered_data, aes(
      x = `Time period`, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), color = `Location`,
      text = if (percentage) {
        paste0(
          str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
          "Location: ", `Location`, "<br>",
          "Time period: ", `Time period`
        )
      } else {
        paste0(
          str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), "<br>",
          "Location: ", `Location`, "<br>",
          "Time period: ", `Time period`
        )
      }
    )) +
      # geom_path(group = 1) +
      ylab(yaxis_title) +
      xlab("Time period") +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(size = 1.0)
      ) +
      scale_y_continuous(
        limits = c(0, ylim_upper)
      ) +
      labs(color = "Location") +
      scale_color_manual(
        "Location",
        values = gss_colour_pallette
      )
    p <- p +
      geom_path(group = 1) +
      geom_point()
  } else {
    filtered_data <- dataset %>%
      select(time_period, geo_breakdown, score_label, `yvalue`) %>%
      mutate(`Time period` = as.character(`time_period`)) %>%
      rename(`Location` = `geo_breakdown`) %>%
      rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))

    p <- ggplot(filtered_data, aes(
      x = `Time period`, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), color = `Location`,
      text = paste0(
        str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), "<br>",
        "SDQ score: ", `score_label`, "<br>",
        "Location: ", `Location`, "<br>",
        "Time period: ", `Time period`
      )
    )) +
      ylab(yaxis_title) +
      xlab("Time period") +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(size = 1.0)
      ) +
      scale_y_continuous(
        limits = c(0, ylim_upper)
      ) +
      labs(color = "Location") +
      scale_color_manual(
        "Location",
        values = gss_colour_pallette
      )
    max_xaxis <- length(unique(dataset$time_period)) + 1
    suppressWarnings(
      p <- p +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 0, ymax = 14, text = paste("Normal SDQ score: 0-13"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 14, ymax = 17, text = paste("Borderline SDQ score: 14-16"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 17, ymax = ylim_upper, text = paste("Cause for concern SDQ score: 17-40"))) +
        geom_path(group = 1) +
        geom_hline(linetype = "dashed", colour = "#F46A25", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
        geom_hline(linetype = "dot", colour = "red", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) +
        geom_point()
    )
  }
  return(p)
}

# By LA bar chart repeat function ----
by_la_bar_plot <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue, yaxis_title, yupperlim = NULL, add_rect = FALSE, percentage = FALSE) {
  if (selected_geo_lvl == "Local authority") {
    if (add_rect == "FALSE") {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected")
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    } else {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`, score_label) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected")
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    }
  } else if (selected_geo_lvl == "National") {
    if (add_rect == FALSE) {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = "Not Selected"
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    } else {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`, score_label) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = "Not Selected"
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    }
  } else if (selected_geo_lvl == "Regional") {
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    if (add_rect == FALSE) {
      la_data <- dataset %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = "Selected"
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    } else {
      la_data <- dataset %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`, score_label) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = "Selected"
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    }
  }

  if (add_rect == FALSE) {
    p <- ggplot(la_data, aes(
      x = Breakdown, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = if (percentage) {
        paste0(
          str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
          "Local authority: ", Breakdown, "<br>",
          "Time period: ", time_period, "<br>",
          "Selection: ", Selection
        )
      } else {
        paste0(
          str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), "<br>",
          "Local authority: ", Breakdown, "<br>",
          "Time period: ", time_period, "<br>",
          "Selection: ", Selection
        )
      }
    )) +
      ylab(yaxis_title) +
      xlab("") +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(size = 1.0)
      ) +
      scale_fill_manual(
        "LA Selection",
        values = c("Selected" = "#12436D", "Not Selected" = "#88A1B5")
      )

    if (is.null(yupperlim)) {
      p <- p + scale_y_continuous(limits = c(0, 100))
    } else {
      # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
      yupperlim <- (ceiling(yupperlim / 10) * 10) + (yupperlim * 0.05)
      p <- p + scale_y_continuous(limits = c(0, yupperlim))
    }

    p1 <- p +
      geom_col(position = position_dodge())
  } else {
    p <- ggplot(la_data, aes(
      x = Breakdown, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = paste0(
        str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), "<br>",
        "SDQ score: ",
        "Local authority: ", Breakdown, "<br>",
        "Time period: ", time_period, "<br>",
        "Selection: ", Selection
      )
    )) +
      ylab(yaxis_title) +
      xlab("") +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(size = 1.0)
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_fill_manual(
        "LA Selection",
        values = c("Selected" = "#12436D", "Not Selected" = "#88A1B5")
      )

    max_xaxis <- length(unique(la_data$`Breakdown`)) + 1

    suppressWarnings(
      p1 <- p +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 0, ymax = 14, text = paste("Normal SDQ score: 0-13"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 14, ymax = 17, text = paste("Borderline SDQ score: 14-16"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 17, ymax = yupperlim, text = paste("Cause for concern SDQ score: 17-40"))) +
        geom_hline(linetype = "dashed", colour = "#F46A25", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
        geom_hline(linetype = "dot", colour = "red", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) +
        geom_col(position = position_dodge())
    )
  }

  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p2 <- p1 + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p2 <- p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  return(p2)
}

# By Region bar chart repeat function -----
by_region_bar_plot <- function(dataset, yvalue, yaxis_title, yupperlim, add_rect = FALSE, percentage = FALSE) {
  if (add_rect == FALSE) {
    reg_data <- dataset %>%
      filter(geographic_level == "Regional", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, `yvalue`) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`)))) %>% # Order by turnover rate
      rename(`Breakdown` = `geo_breakdown`) %>%
      rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

    p <- ggplot(reg_data, aes(
      x = `Breakdown`, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = factor(time_period),
      text = if (percentage) {
        paste0(
          str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_title(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
          "Region: ", `Breakdown`, "<br>",
          "Time period: ", `time_period`
        )
      } else {
        paste0(
          str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), "<br>",
          "Region: ", `Breakdown`, "<br>",
          "Time period: ", `time_period`
        )
      }
    ))

    # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
    yupperlim <- (ceiling(yupperlim / 10) * 10) + (yupperlim * 0.05)

    p2 <- p +
      geom_col(position = position_dodge()) +
      ylab(yaxis_title) +
      xlab("Region") +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(size = 1.0)
      ) +
      scale_y_continuous(limits = c(0, yupperlim)) +
      scale_fill_manual(
        "Time Period",
        values = "#12436D" # gss_colour_pallette
      )
  } else {
    reg_data <- dataset %>%
      filter(geographic_level == "Regional", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, `yvalue`, score_label) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`)))) %>%
      rename(`Breakdown` = `geo_breakdown`) %>%
      rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

    p <- ggplot(reg_data, aes(
      x = `Breakdown`, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = factor(time_period),
      text = paste0(
        str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), "<br>",
        "SDQ score: ", `score_label`, "<br>",
        "Region: ", `Breakdown`, "<br>",
        "Time period: ", `time_period`
      )
    ))

    max_xaxis <- length(unique(reg_data$`Breakdown`)) + 1

    suppressWarnings(
      p2 <- p +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 0, ymax = 14, text = paste("Normal SDQ score: 0-13"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 14, ymax = 17, text = paste("Borderline SDQ score: 14-16"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 17, ymax = yupperlim, text = paste("Cause for concern SDQ score: 17-40"))) +
        geom_hline(linetype = "dashed", colour = "#F46A25", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
        geom_hline(linetype = "dot", colour = "red", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) +
        ylab(yaxis_title) +
        xlab("Region") +
        theme_classic() +
        theme(
          text = element_text(size = 12),
          axis.text.x = element_text(angle = 45),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(r = 12)),
          axis.line = element_line(size = 1.0)
        ) +
        scale_y_continuous(limits = c(0, yupperlim)) +
        scale_fill_manual(
          "Time Period",
          values = "#12436D" # gss_colour_pallette
        ) +
        geom_col(position = position_dodge(), alpha = 1, fill = "#12436D")
    )
  }
}

# Ethnicity Rate ----

plot_ethnicity_rate <- function(geo_breakdown, geographic_level) {
  ethnicity_data <- workforce_eth[
    workforce_eth$geo_breakdown %in% geo_breakdown &
      workforce_eth$role == "Total" &
      workforce_eth$breakdown_topic == "Ethnicity major" &
      !(workforce_eth$breakdown %in% c("Known", "Total", "Not known")) &
      workforce_eth$time_period >= (max(workforce_eth$time_period) - 3),
    c(
      "time_period", "geo_breakdown", "breakdown_topic", "breakdown",
      "inpost_headcount_percentage"
    )
  ]

  # Ensure 'percentage' is numeric
  ethnicity_data$percentage <- as.numeric(ethnicity_data$inpost_headcount_percentage)

  custom_x_order <- c("White", "Mixed / Multiple ethnic groups", "Asian / Asian British", "Black / African / Caribbean / Black British", "Other ethnic group")

  p <- ggplot(ethnicity_data, aes(
    x = breakdown, y = percentage, fill = factor(time_period),
    text = paste0(
      "Ethnic group: ", breakdown, "<br>",
      "Percentage of workforce: ", format(percentage, nsmall = 1), "<br>",
      "Time period: ", time_period
    )
  )) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylab("Percentage") +
    xlab("Ethnicity") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_fill_manual(
      "Year", # Change legend title
      values = gss_colour_pallette
    ) +
    scale_x_discrete(
      limits = custom_x_order,
      labels = c("White" = "White", "Mixed / Multiple ethnic groups" = "Mixed", "Asian / Asian British" = "Asian", "Black / African / Caribbean / Black British" = "Black", "Other ethnic group" = "Other")
    )
  if (all(is.na(ethnicity_data$percentage))) {
    p <- p + annotate(x = 3, y = 50, geom = "text", label = "There is no available data due to zero social workers with known ethnicity", color = "red")
  }
  return(p)
}


plot_population_ethnicity_rate <- function(geo_breakdown, geographic_level.x) {
  # Filter the data based on 'geo_breakdown', 'geographic_level'
  combined_ethnicity_data <- combined_ethnicity_data[
    combined_ethnicity_data$geo_breakdown %in% geo_breakdown,
    c(
      "time_period", "geo_breakdown", "geographic_level.x", "breakdown",
      "inpost_headcount_percentage", "Percentage"
    )
  ] %>%
    rename(Ethnicity = breakdown, Workforce = inpost_headcount_percentage, Population = Percentage)


  # Ensure 'percentage' is numeric
  combined_ethnicity_data$Workforce <- as.numeric(combined_ethnicity_data$Workforce)

  # Reshape the dataframe to a long format
  combined_ethnicity_data_long <- melt(combined_ethnicity_data,
    id.vars = c("geo_breakdown", "geographic_level.x", "time_period", "Ethnicity"),
    measure.vars = c("Workforce", "Population"),
    variable.name = "Data",
    value.name = "Percentage"
  )

  custom_x_order <- c("White", "Black", "Asian", "Mixed", "Other")

  p <- ggplot(combined_ethnicity_data_long, aes(x = Ethnicity, y = Percentage, fill = Data)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Percentage") +
    xlab("Ethnicity") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_fill_manual(
      "Data", # Change legend title
      values = c("#F46A25", "#3D3D3D")
    ) +
    scale_x_discrete(
      limits = custom_x_order,
      labels = c("White" = "White", "Mixed" = "Mixed", "Asian" = "Asian", "Black" = "Black", "Other" = "Other")
    )

  return(p)
}

plot_seniority_eth <- function(geo_breakdown, geographic_level) {
  ethnicity_data_sen <- workforce_eth_seniority[
    workforce_eth_seniority$geo_breakdown %in% geo_breakdown & workforce_eth_seniority$seniority != "Total",
    c("time_period", "geo_breakdown", "breakdown", "Percentage", "seniority")
  ]

  custom_x_order <- c("White", "Mixed / Multiple ethnic groups", "Asian / Asian British", "Black / African / Caribbean / Black British", "Other ethnic group")
  custom_fill_order <- c("Manager", "Senior practitioner", "Case holder", "Qualified without cases")


  p <- ggplot(ethnicity_data_sen, aes(
    x = breakdown, y = Percentage, fill = factor(seniority, levels = custom_fill_order),
    text = paste0(
      "Ethnic group: ", breakdown, "<br>",
      "Percentage: ", format(Percentage, nsmall = 1), "<br>",
      "Seniority level: ", factor(seniority, levels = custom_fill_order)
    )
  )) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylab("Percentage") +
    xlab("Ethnicity") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_fill_manual(
      "Seniority Level", # Change legend title
      values = gss_colour_pallette
    ) +
    scale_x_discrete(
      limits = custom_x_order,
      labels = c("White" = "White", "Mixed / Multiple ethnic groups" = "Mixed", "Asian / Asian British" = "Asian", "Black / African / Caribbean / Black British" = "Black", "Other ethnic group" = "Other")
    )
  if (all(is.na(ethnicity_data_sen$Percentage))) {
    p <- p + annotate(x = 3, y = 50, geom = "text", label = "There is no available data due to zero social workers with known ethnicity", color = "red")
  }
  return(p)
}


# Outcome 1 - Access to support getting help charts ----
plot_uasc <- function(geo_break, geo_lvl) {
  uasc_data <- combined_cla_data %>%
    filter(geographic_level %in% geo_lvl & geo_breakdown %in% geo_break &
      characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children") &
      population_count == "Children starting to be looked after each year") %>%
    select(time_period, geo_breakdown, `Placement Rate Per 10000`, characteristic)


  # Set the max y-axis scale based on the data
  max_rate <- max(
    combined_cla_data$`Placement Rate Per 10000`[combined_cla_data$population_count == "Children starting to be looked after each year" &
      combined_cla_data$characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")],
    na.rm = TRUE
  )

  # Round the max_rate to the nearest 20 then multiply by 1.05 (this will be used for the upper y-axis limit)
  max_rate <- (ceiling(max_rate / 20) * 20) + (max_rate * 0.05)

  ggplot(uasc_data, aes(`time_period`, `Placement Rate Per 10000`,
    fill = factor(characteristic, levels = c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")),
    text = paste0(
      "Placement rate per 10,000: ", `Placement Rate Per 10000`, "<br>",
      "UASC status: ", factor(characteristic, levels = c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")), "<br>",
      "Location: ", geo_breakdown, "<br>",
      "Time period: ", `time_period`
    )
  )) +
    geom_bar(stat = "identity") +
    ylab("Rate per 10,000 children") +
    xlab("Time period") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_x_continuous(breaks = seq(min(uasc_data$time_period), max(uasc_data$time_period), by = 1)) +
    scale_y_continuous(
      limits = c(0, max_rate),
    ) +
    scale_fill_manual(
      "UASC status",
      # breaks = unique(c("England", inputArea)),
      values = c("Unaccompanied asylum-seeking children" = "#28A197", "Non-unaccompanied asylum-seeking children" = "#12436D")
    )
}

# bar chart by region
plot_uasc_reg <- function() {
  uasc_data <- combined_cla_data %>%
    filter(geographic_level == "Regional" &
      characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children") &
      population_count == "Children starting to be looked after each year" & time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, `Placement Rate Per 10000`, characteristic) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -`Placement Rate Per 10000`))

  # Set the max y-axis scale based on the data
  max_rate <- max(
    combined_cla_data$`Placement Rate Per 10000`[combined_cla_data$population_count == "Children starting to be looked after each year" &
      combined_cla_data$characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children") &
      combined_cla_data$time_period == max(combined_cla_data$time_period) &
      combined_cla_data$geographic_level == "Regional"],
    na.rm = TRUE
  )

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  ggplot(uasc_data, aes(`geo_breakdown`, `Placement Rate Per 10000`,
    fill = factor(characteristic, levels = c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")),
    text = paste0(
      "Placement rate per 10,000: ", `Placement Rate Per 10000`, "<br>",
      "UASC status: ", factor(characteristic, levels = c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")), "<br>",
      "Region: ", geo_breakdown, "<br>",
      "Time period: ", `time_period`
    )
  )) +
    geom_bar(stat = "identity") +
    ylab("Rate per 10,000 children") +
    xlab("Region") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "UASC Status",
      # breaks = unique(c("England", inputArea)),
      values = c("Unaccompanied asylum-seeking children" = "#28A197", "Non-unaccompanied asylum-seeking children" = "#12436D")
    )
}

plot_uasc_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL) {
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")


  colors <- c(
    "Unaccompanied asylum-seeking children (Selected)" = "#28A197",
    "Non-unaccompanied asylum-seeking children (Selected)" = "#12436D",
    "Unaccompanied asylum-seeking children (Not Selected)" = "#28A1977F",
    "Non-unaccompanied asylum-seeking children (Not Selected)" = "#12436D7F"
  )

  if (selected_geo_lvl == "Local authority") {
    cla_data <- combined_cla_data %>%
      filter(
        geographic_level == "Local authority", time_period == max(time_period), population_count == "Children starting to be looked after each year",
        characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")
      ) %>%
      select(time_period, geo_breakdown, `Placement Rate Per 10000`, characteristic) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -`Placement Rate Per 10000`), # Order by placement_per_10000
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected"),
        characteristic_selected = ifelse(is_selected == "Selected", paste0(characteristic, " (Selected)"), paste0(characteristic, " (Not Selected)"))
      )
  } else if (selected_geo_lvl == "National") {
    cla_data <- combined_cla_data %>%
      filter(
        geographic_level == "Local authority", time_period == max(time_period), population_count == "Children starting to be looked after each year",
        characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")
      ) %>%
      select(time_period, geo_breakdown, `Placement Rate Per 10000`, characteristic) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -`Placement Rate Per 10000`), # Order by placement_per_10000
        is_selected = "Not Selected",
        characteristic_selected = paste0(characteristic, " (Not Selected)")
      )
  } else if (selected_geo_lvl == "Regional") {
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }

    cla_data <- combined_cla_data %>%
      filter(
        geo_breakdown %in% location, time_period == max(time_period), population_count == "Children starting to be looked after each year", rate_per_10000 != "NA",
        characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")
      ) %>%
      select(time_period, geo_breakdown, `Placement Rate Per 10000`, characteristic) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -`Placement Rate Per 10000`), # Order by placement_per_10000
        is_selected = "Selected",
        characteristic_selected = ifelse(is_selected == "Selected", paste0(characteristic, " (Selected)"), paste0(characteristic, " (Not Selected)"))
      )
  }

  # Set the max y-axis scale based on rhe data
  max_rate <- max(
    combined_cla_data$`Placement Rate Per 10000`[combined_cla_data$population_count == "Children starting to be looked after each year" &
      combined_cla_data$characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children") &
      combined_cla_data$time_period == max(combined_cla_data$time_period) &
      combined_cla_data$geographic_level == "Local authority"],
    na.rm = TRUE
  )

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  # Use the new variable in the plot
  p <- ggplot(cla_data, aes(
    x = geo_breakdown, y = `Placement Rate Per 10000`, fill = factor(characteristic_selected,
      levels = c(
        "Unaccompanied asylum-seeking children (Selected)",
        "Non-unaccompanied asylum-seeking children (Selected)",
        "Unaccompanied asylum-seeking children (Not Selected)",
        "Non-unaccompanied asylum-seeking children (Not Selected)"
      )
    ),
    text = paste0(
      "Placement rate per 10,000: ", `Placement Rate Per 10000`, "<br>",
      "UASC status: ", factor(characteristic, levels = c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")), "<br>",
      "Local authority: ", geo_breakdown, "<br>",
      "Selection: ", is_selected, "<br>",
      "Time period: ", `time_period`
    )
  )) +
    geom_bar(stat = "identity") +
    ylab("Rate per 10,000 children") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "UASC Status",
      values = colors,
      labels = c(
        "Unaccompanied asylum-seeking children (Selected)",
        "Non-unaccompanied asylum-seeking children (Selected)",
        "Unaccompanied asylum-seeking children (Not Selected)",
        "Non-unaccompanied asylum-seeking children (Not Selected)"
      )
    )

  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  return(p)
}


# CLA Rates ----
# bar chart by region
plot_cla_rate_reg <- function() {
  cla_reg_data <- cla_rates %>%
    filter(geographic_level == "Regional", time_period == max(time_period), population_count == "Children starting to be looked after each year") %>%
    select(time_period, geo_breakdown, `Rate Per 10000`) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -`Rate Per 10000`)) # Order by cla rate

  # Set the max y-axis scale based on the data
  max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year" &
    cla_rates$time_period == max(cla_rates$time_period) &
    cla_rates$geographic_level == "Regional"], na.rm = TRUE)

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  ggplot(cla_reg_data, aes(`geo_breakdown`, `Rate Per 10000`,
    fill = factor(time_period),
    text = paste0(
      "Rate per 10,000: ", `Rate Per 10000`, "<br>",
      "Region: ", `geo_breakdown`, "<br>",
      "Time period: ", `time_period`
    )
  )) +
    geom_col(position = position_dodge()) +
    ylab("Rate per 10,000 children") +
    xlab("Region") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "Time period",
      values = "#12436D" # gss_colour_pallette
    )
}

plot_cla_rate_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL) {
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")

  if (selected_geo_lvl == "Local authority") {
    cla_data <- cla_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), population_count == "Children starting to be looked after each year") %>%
      select(time_period, geo_breakdown, `Rate Per 10000`) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -`Rate Per 10000`), # Order by rate_per_10000
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected")
      )
  } else if (selected_geo_lvl == "National") {
    cla_data <- cla_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), population_count == "Children starting to be looked after each year") %>%
      select(time_period, geo_breakdown, `Rate Per 10000`) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -`Rate Per 10000`), # Order by rate_per_10000
        is_selected = "Not Selected"
      )
  } else if (selected_geo_lvl == "Regional") {
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }

    cla_data <- cla_rates %>%
      filter(geo_breakdown %in% location, time_period == max(time_period), population_count == "Children starting to be looked after each year", rate_per_10000 != "NA") %>%
      select(time_period, geo_breakdown, `Rate Per 10000`) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -`Rate Per 10000`), # Order by rate_per_10000
        is_selected = "Selected"
      )
  }

  # Set the max y-axis scale based on the data
  max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year" &
    cla_rates$time_period == max(cla_rates$time_period) &
    cla_rates$geographic_level == "Local authority"], na.rm = TRUE)

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  p <- ggplot(cla_data, aes(`geo_breakdown`, `Rate Per 10000`,
    fill = `is_selected`,
    text = paste0(
      "Rate per 10,000: ", `Rate Per 10000`, "<br>",
      "Local authority: ", `geo_breakdown`, "<br>",
      "Selection: ", `is_selected`
    )
  )) +
    geom_col(position = position_dodge()) +
    ylab("Rate per 10,000 children") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = "#12436D", "Not Selected" = "#88A1B5")
    )

  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  return(p)
}

# CLA Rates ----
# bar chart by region
plot_cla_march_reg <- function() {
  cla_reg_data <- cla_rates %>%
    filter(geographic_level == "Regional", time_period == max(time_period), population_count == "Children looked after at 31 March each year") %>%
    select(time_period, geo_breakdown, `Rate Per 10000`) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -`Rate Per 10000`)) # Order by cla rate

  # Set the max y-axis scale based on the data
  max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children looked after at 31 March each year" &
    cla_rates$time_period == max(cla_rates$time_period) &
    cla_rates$geographic_level == "Regional"], na.rm = TRUE)

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  ggplot(cla_reg_data, aes(`geo_breakdown`, `Rate Per 10000`,
    fill = factor(time_period),
    text = paste0(
      "Rate per 10,000: ", `Rate Per 10000`, "<br>",
      "Region: ", `geo_breakdown`, "<br>",
      "Time period: ", time_period
    )
  )) +
    geom_col(position = position_dodge()) +
    ylab("Rate per 10,000 children") +
    xlab("Region") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "Time period",
      values = "#12436D" # gss_colour_pallette
    )
}

plot_cla_march_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL) {
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")

  if (selected_geo_lvl == "Local authority") {
    cla_data <- cla_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), population_count == "Children looked after at 31 March each year") %>%
      select(time_period, geo_breakdown, `Rate Per 10000`) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -`Rate Per 10000`), # Order by rate_per_10000
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected")
      )
  } else if (selected_geo_lvl == "National") {
    cla_data <- cla_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), population_count == "Children looked after at 31 March each year") %>%
      select(time_period, geo_breakdown, `Rate Per 10000`) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -`Rate Per 10000`), # Order by rate_per_10000
        is_selected = "Not Selected"
      )
  } else if (selected_geo_lvl == "Regional") {
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }

    cla_data <- cla_rates %>%
      filter(geo_breakdown %in% location, time_period == max(time_period), population_count == "Children looked after at 31 March each year", rate_per_10000 != "NA") %>%
      select(time_period, geo_breakdown, `Rate Per 10000`) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -`Rate Per 10000`), # Order by rate_per_10000
        is_selected = "Selected"
      )
  }

  # Set the max y-axis scale based on the data
  max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children looked after at 31 March each year" &
    cla_rates$time_period == max(cla_rates$time_period) &
    cla_rates$geographic_level == "Local authority"], na.rm = TRUE)

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  p <- ggplot(cla_data, aes(`geo_breakdown`, `Rate Per 10000`,
    fill = `is_selected`,
    text = paste0(
      "Rate per 10,000: ", `Rate Per 10000`, "<br>",
      "Local authority: ", `geo_breakdown`, "<br>",
      "Selection: ", `is_selected`, "<br>",
      "Time period: ", time_period
    )
  )) +
    geom_col(position = position_dodge()) +
    ylab("Rate per 10,000 children") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = "#12436D", "Not Selected" = "#88A1B5")
    )

  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  return(p)
}

# CIN rates -------

# cin rate chart by region
plot_cin_rate_reg <- function() {
  cin_reg_data <- cin_rates %>%
    filter(geographic_level == "Regional", time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, CIN_rate) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -CIN_rate)) # Order by turnover rate

  # Set the max y-axis scale based on the data
  max_rate <- max(cin_rates$CIN_rate[cin_rates$time_period == max(cin_rates$time_period) &
    cin_rates$geographic_level == "Regional"], na.rm = TRUE)

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  ggplot(cin_reg_data, aes(`geo_breakdown`, `CIN_rate`,
    fill = factor(time_period),
    text = paste0(
      "CIN rate per 10,000: ", `CIN_rate`, "<br>",
      "Region: ", geo_breakdown, "<br>",
      "Time period: ", time_period
    )
  )) +
    geom_col(position = position_dodge()) +
    ylab("CIN rates per 10,000") +
    xlab("Region") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "Time period",
      values = "#12436D" # gss_colour_pallette
    )
}


# cin rate chart by la
plot_cin_rates_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL) {
  location_data <- GET_location("data/b1_children_in_need_2013_to_2023.csv")

  if (selected_geo_lvl == "Local authority") {
    cin_data <- cin_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, CIN_rate) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -CIN_rate), # Order by cin rate
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected")
      ) %>%
      rename("CIN rate per 10,000" = CIN_rate)
  } else if (selected_geo_lvl == "National") {
    cin_data <- cin_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, CIN_rate) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -CIN_rate), # Order by cin rate
        is_selected = "Not Selected"
      ) %>%
      rename("CIN rate per 10,000" = CIN_rate)
  } else if (selected_geo_lvl == "Regional") {
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }

    cin_data <- cin_rates %>%
      filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, CIN_rate) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -CIN_rate), # Order by cin rate
        is_selected = "Selected"
      ) %>%
      rename("CIN rate per 10,000" = CIN_rate)
  }

  # Set the max y-axis scale based on the data
  max_rate <- max(cin_rates$CIN_rate[cin_rates$time_period == max(cin_rates$time_period) &
    cin_rates$geographic_level == "Local authority"], na.rm = TRUE)

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  p <- ggplot(cin_data, aes(`geo_breakdown`, `CIN rate per 10,000`,
    fill = `is_selected`,
    text = paste0(
      "CIN rate per 10,000: ", `CIN rate per 10,000`, "<br>",
      "Local authority: ", geo_breakdown, "<br>",
      "Time period: ", time_period, "<br>",
      "Selection: ", `is_selected`
    )
  )) +
    geom_col(position = position_dodge()) +
    ylab("CIN rates per 10,000") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = "#12436D", "Not Selected" = "#88A1B5")
    )

  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  return(p)
}

# CIN referrals ----

# bar chart by region
plot_cin_referral_reg <- function() {
  referral_reg_data <- cin_referrals %>%
    filter(geographic_level == "Regional", time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, Re_referrals_percentage) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -Re_referrals_percentage)) # Order by turnover rate

  # Set the max_rate based on the data
  max_rate <- max(cin_referrals$Re_referrals_percentage[cin_referrals$time_period == max(cin_referrals$time_period) &
    cin_referrals$geographic_level == "Regional"], na.rm = TRUE)

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  ggplot(referral_reg_data, aes(`geo_breakdown`, `Re_referrals_percentage`,
    fill = factor(time_period),
    text = paste0(
      "Re-referrals (%): ", format(`Re_referrals_percentage`, nsmall = 1), "<br>",
      "Region: ", geo_breakdown, "<br>",
      "Time period: ", time_period
    )
  )) +
    geom_col(position = position_dodge()) +
    ylab("Re-referrals (%)") +
    xlab("Region") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "Time period",
      values = "#12436D" # gss_colour_pallette
    )
}


# bar chart by LA
plot_cin_referral_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL) {
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")

  if (selected_geo_lvl == "Local authority") {
    LA_referral_data <- cin_referrals %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, Re_referrals_percentage) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -Re_referrals_percentage), # Order by vacancy rate
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected")
      )
  } else if (selected_geo_lvl == "National") {
    LA_referral_data <- cin_referrals %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, Re_referrals_percentage) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -Re_referrals_percentage), # Order by vacancy rate
        is_selected = "Not Selected"
      )
  } else if (selected_geo_lvl == "Regional") {
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }

    LA_referral_data <- cin_referrals %>%
      filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, Re_referrals_percentage) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -Re_referrals_percentage),
        is_selected = "Selected"
      )
  }

  max_rate <- max(cin_referrals$Re_referrals_percentage[cin_referrals$time_period == max(cin_referrals$time_period) &
    cin_referrals$geographic_level == "Local authority"], na.rm = TRUE)

  # Round the max_rate to the nearest 10
  max_rate <- ceiling(max_rate / 10) * 10


  p <- ggplot(LA_referral_data, aes(`geo_breakdown`, `Re_referrals_percentage`,
    fill = `is_selected`,
    text = paste0(
      "Re-referrals (%): ", format(Re_referrals_percentage, nsmall = 1), "<br>",
      "Local authority: ", geo_breakdown, "<br>",
      "Time period: ", time_period, "<br>",
      "Selection: ", is_selected
    )
  )) +
    geom_col(position = position_dodge()) +
    ylab("Re-referrals  (%)") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = "#12436D", "Not Selected" = "#88A1B5")
    )

  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  return(p)
}

# Outcome 3 ----

all_assessment_factors_plot <- function(dataset, factorslist, selected_geo_breakdown = NULL) {
  data <- assessment_factors %>%
    filter(assessment_factor %in% (factorslist)) %>%
    filter(geo_breakdown == selected_geo_breakdown, time_period == max(time_period))

  p <- ggplot(data, aes(
    x = reorder(assessment_factor, rate_per_10000), y = rate_per_10000,
    text = paste(
      "Assessment factor: ", assessment_factor, "<br>",
      "Rate per 10,000: ", rate_per_10000, "<br>",
      "Location: ", geo_breakdown, "<br>",
      "Time period: ", time_period
    )
  )) +
    geom_bar(stat = "identity", fill = "#12436D") +
    scale_y_continuous(limits = c(0, NA)) +
    coord_flip() +
    xlab("Assessment factor") +
    ylab("Rate per 10,000")

  # logic to check if the table is empty or not
  annotate_x <- length(unique(data$assessment_factor)) / 2
  annotate_y <- 0
  if (max(data$rate_per_10000) == 0) {
    p <- p + annotate(x = annotate_x, y = annotate_y, geom = "text", label = "There is no data to plot, view the table alternative below for more details.", color = "red")
  }
  return(p)
}
factors_by_la_bar_plot <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue, yaxis_title, yupperlim = NULL, add_rect = FALSE, percentage = FALSE) {
  if (selected_geo_lvl == "Local authority") {
    if (add_rect == "FALSE") {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected")
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    } else {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`, score_label) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected")
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    }
  } else if (selected_geo_lvl == "National") {
    if (add_rect == FALSE) {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = "Not Selected"
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    } else {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`, score_label) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = "Not Selected"
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    }
  } else if (selected_geo_lvl == "Regional") {
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    if (add_rect == FALSE) {
      la_data <- dataset %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = "Selected"
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    } else {
      la_data <- dataset %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`, score_label) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = "Selected"
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    }
  }

  if (add_rect == FALSE) {
    p <- ggplot(la_data, aes(
      x = Breakdown, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = if (percentage) {
        paste0(
          str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
          "Local authority: ", Breakdown, "<br>",
          "Time period: ", time_period, "<br>",
          "Selection: ", Selection
        )
      } else {
        paste0(
          str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), "<br>",
          "Local authority: ", Breakdown, "<br>",
          "Time period: ", time_period, "<br>",
          "Selection: ", Selection
        )
      }
    )) +
      ylab(yaxis_title) +
      xlab("") +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(size = 1.0)
      ) +
      scale_fill_manual(
        "LA Selection",
        values = c("Selected" = "#12436D", "Not Selected" = "#88A1B5")
      )

    if (is.null(yupperlim)) {
      p <- p + scale_y_continuous(limits = c(0, 100))
    } else {
      # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
      yupperlim <- (ceiling(yupperlim / 10) * 10) + (yupperlim * 0.05)
      p <- p + scale_y_continuous(limits = c(0, yupperlim))
    }

    p1 <- p +
      geom_col(position = position_dodge())
  } else {
    p <- ggplot(la_data, aes(
      x = Breakdown, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = paste0(
        str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), "<br>",
        "SDQ score: ",
        "Local authority: ", Breakdown, "<br>",
        "Time period: ", time_period, "<br>",
        "Selection: ", Selection
      )
    )) +
      ylab(yaxis_title) +
      xlab("") +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(size = 1.0)
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_fill_manual(
        "LA Selection",
        values = c("Selected" = "#12436D", "Not Selected" = "#88A1B5")
      )

    max_xaxis <- length(unique(la_data$`Breakdown`)) + 1

    suppressWarnings(
      p1 <- p +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 0, ymax = 14, text = paste("Normal SDQ score: 0-13"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 14, ymax = 17, text = paste("Borderline SDQ score: 14-16"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 17, ymax = yupperlim, text = paste("Cause for concern SDQ score: 17-40"))) +
        geom_hline(linetype = "dashed", colour = "#F46A25", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
        geom_hline(linetype = "dot", colour = "red", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) +
        geom_col(position = position_dodge())
    )
  }

  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p2 <- p1 + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p2 <- p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  # label if no cases for selected factor

  if (selected_geo_lvl == "Local authority") {
    selected_row <- la_data %>% filter(Selection == "Selected")
    selected_rate <- selected_row$`Rate per 10000`
    if (length(selected_rate) > 0 && selected_rate < 1) {
      y_max <- max(la_data$`Rate per 10000`)
      x_len <- length(la_data$Breakdown)
      p2 <- p2 + annotate(y = y_max, x = x_len / 2, geom = "text", label = "Selected local authority not highlighted, as factor was not recorded or the volume of data is too low to be published.", color = "red")
    }
  }


  return(p2)
}

# Enabler 3 ----
plot_ofsted <- function() {
  ofsted_data <- ofsted_leadership_data_long %>%
    filter(geographic_level == "National", time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, Rating, Count)

  # Set the factor levels for Rating in the desired order
  ofsted_data$Rating <- factor(ofsted_data$Rating, levels = c(
    "outstanding_count",
    "good_count",
    "requires_improvement_count",
    "inadequate_count"
  ))

  ofsted_data <- ofsted_data %>%
    mutate(Rating = recode(Rating,
      "inadequate_count" = "Inadequate",
      "requires_improvement_count" = "Requires Improvement",
      "good_count" = "Good",
      "outstanding_count" = "Outstanding"
    ))

  # Set the max y-axis scale based on the data
  max_rate <- max(ofsted_data$Count, na.rm = TRUE)

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  p <- ggplot(ofsted_data, aes(
    x = Rating, y = Count, fill = factor(Rating),
    text = paste(
      "Breakdown:", "National", "<br>",
      "Ofsted leadership rating:", Rating, "<br>",
      "Count:", Count, "<br>",
      "Latest data publication:", time_period
    )
  )) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_classic() +
    coord_flip() +
    scale_fill_manual(
      "Ofsted leadership rating", # Change legend title
      values = gss_colour_pallette,
      breaks = c("Outstanding", "Good", "Requires Improvement", "Inadequate")
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    scale_x_discrete(limits = c("Inadequate", "Requires Improvement", "Good", "Outstanding")) +
    xlab("Ofsted leadership rating") +
    ylab("Count")

  return(p)
}


plot_ofsted_reg <- function() {
  ofsted_data <- ofsted_leadership_data_long %>%
    filter(geographic_level == "Regional", time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, Rating, Count)

  # Set the factor levels for Rating in the desired order
  ofsted_data$Rating <- factor(ofsted_data$Rating, levels = c(
    "outstanding_count",
    "good_count",
    "requires_improvement_count",
    "inadequate_count"
  ))

  ofsted_data <- ofsted_data %>%
    mutate(Rating = recode(Rating,
      "inadequate_count" = "Inadequate",
      "requires_improvement_count" = "Requires Improvement",
      "good_count" = "Good",
      "outstanding_count" = "Outstanding"
    ))

  # Set the max y-axis scale based on the data
  max_rate <- max(ofsted_data$Count, na.rm = TRUE)

  # Round the max_rate to the nearest 10 (this will be used for the upper y-axis limit)
  max_rate <- ceiling(max_rate / 10) * 10

  p <- ggplot(ofsted_data, aes(
    x = geo_breakdown, y = Count, fill = factor(Rating),
    text = paste(
      "Breakdown:", "National", "<br>",
      "Ofsted leadership rating:", Rating, "<br>",
      "Count:", Count, "<br>",
      "Latest data publication:", time_period
    )
  )) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_classic() +
    scale_fill_manual(
      "Ofsted leadership rating", # Change legend title
      values = gss_colour_pallette,
      breaks = c("Outstanding", "Good", "Requires Improvement", "Inadequate")
    ) +
    scale_y_continuous(limits = c(0, max_rate)) +
    xlab("Region") +
    ylab("Count")

  return(p)
}


# Statistical Neighbours function ----
statistical_neighbours_plot <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue, yaxis_title, ylim_upper, add_rect = FALSE, percentage = FALSE) {
  # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
  ylim_upper <- (ceiling(ylim_upper / 10) * 10) + (ylim_upper * 0.05)

  selected_la <- dataset %>%
    filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown == selected_geo_breakdown) %>%
    select(geo_breakdown, old_la_code)

  selected_la$old_la_code <- as.numeric(selected_la$old_la_code)

  neighbours_list <- stats_neighbours %>%
    filter(stats_neighbours$LA.number == selected_la$old_la_code) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.list()

  if (add_rect == FALSE) {
    filtered_data <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, neighbours_list)) %>%
      select(geo_breakdown, `yvalue`) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours")
      ) %>%
      rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

    ggplot(filtered_data, aes(
      x = Breakdown, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = if (percentage) {
        paste0(
          str_to_title(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_title(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
          "Local authority: ", `Breakdown`, "<br>",
          "Time period: ", max(dataset$time_period), "<br>",
          "Selection: ", `Selection`
        )
      } else {
        paste0(
          str_to_title(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), "<br>",
          "Local authority: ", `Breakdown`, "<br>",
          "Time period: ", max(dataset$time_period), "<br>",
          "Selection: ", `Selection`
        )
      }
    )) +
      geom_col(position = position_dodge()) +
      ylab(yaxis_title) +
      xlab("") +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(size = 1.0),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(limits = c(0, ylim_upper)) +
      scale_fill_manual(
        "LA Selection",
        values = c("Selected" = "#12436D", "Statistical Neighbours" = "#88A1B5")
      )
  } else {
    filtered_data <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, neighbours_list)) %>%
      select(geo_breakdown, `yvalue`, score_label) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours")
      ) %>%
      rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

    max_xaxis <- 11 # ten neighbours and selected LA
    suppressWarnings(
      p <- ggplot(filtered_data, aes(
        x = Breakdown, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
        text = paste0(
          str_to_title(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), "<br>",
          "SDQ score: ", `score_label`, "<br>",
          "Local authority: ", `Breakdown`, "<br>",
          "Time period: ", max(dataset$time_period), "<br>",
          "Selection: ", `Selection`
        )
      )) +
        geom_col(position = position_dodge()) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 0, ymax = 14, text = paste("Normal SDQ score: 0-13"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 14, ymax = 17, text = paste("Borderline SDQ score: 14-16"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 17, ymax = ylim_upper, text = paste("Cause for concern SDQ score: 17-40"))) +
        geom_hline(linetype = "dashed", colour = "#F46A25", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
        geom_hline(linetype = "dot", colour = "red", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) +
        ylab(yaxis_title) +
        xlab("") +
        theme_classic() +
        theme(
          text = element_text(size = 12),
          axis.title.y = element_text(margin = margin(r = 12)),
          axis.line = element_line(size = 1.0),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_y_continuous(limits = c(0, ylim_upper)) +
        scale_fill_manual(
          "LA Selection",
          values = c("Selected" = "#12436D", "Statistical Neighbours" = "#88A1B5")
        )
    )
  }
}

statistical_neighbours_plot_factors <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue, yaxis_title, ylim_upper, add_rect = FALSE, percentage = FALSE) {
  # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
  ylim_upper <- (ceiling(ylim_upper / 10) * 10) + (ylim_upper * 0.05)

  selected_la <- dataset %>%
    filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown == selected_geo_breakdown) %>%
    select(geo_breakdown, old_la_code)

  selected_la$old_la_code <- as.numeric(selected_la$old_la_code)

  neighbours_list <- stats_neighbours %>%
    filter(stats_neighbours$LA.number == selected_la$old_la_code) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.list()

  if (add_rect == FALSE) {
    filtered_data <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, neighbours_list)) %>%
      select(geo_breakdown, `yvalue`) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours")
      ) %>%
      rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

    p <- ggplot(filtered_data, aes(
      x = Breakdown, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = if (percentage) {
        paste0(
          str_to_title(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_title(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
          "Local authority: ", `Breakdown`, "<br>",
          "Time period: ", max(dataset$time_period), "<br>",
          "Selection: ", `Selection`
        )
      } else {
        paste0(
          str_to_title(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), "<br>",
          "Local authority: ", `Breakdown`, "<br>",
          "Time period: ", max(dataset$time_period), "<br>",
          "Selection: ", `Selection`
        )
      }
    )) +
      geom_col(position = position_dodge()) +
      ylab(yaxis_title) +
      xlab("") +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(size = 1.0),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(limits = c(0, ylim_upper)) +
      scale_fill_manual(
        "LA Selection",
        values = c("Selected" = "#12436D", "Statistical Neighbours" = "#88A1B5")
      )
  } else {
    filtered_data <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, neighbours_list)) %>%
      select(geo_breakdown, `yvalue`, score_label) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours")
      ) %>%
      rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

    max_xaxis <- 11 # ten neighbours and selected LA
    suppressWarnings(
      p <- ggplot(filtered_data, aes(
        x = Breakdown, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
        text = paste0(
          str_to_title(str_replace_all(yvalue, "_", " ")), ": ", !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), "<br>",
          "SDQ score: ", `score_label`, "<br>",
          "Local authority: ", `Breakdown`, "<br>",
          "Time period: ", max(dataset$time_period), "<br>",
          "Selection: ", `Selection`
        )
      )) +
        geom_col(position = position_dodge()) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 0, ymax = 14, text = paste("Normal SDQ score: 0-13"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 14, ymax = 17, text = paste("Borderline SDQ score: 14-16"))) +
        geom_rect(colour = NA, fill = NA, alpha = 0.1, aes(xmin = 0, xmax = max_xaxis, ymin = 17, ymax = ylim_upper, text = paste("Cause for concern SDQ score: 17-40"))) +
        geom_hline(linetype = "dashed", colour = "#F46A25", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
        geom_hline(linetype = "dot", colour = "red", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) +
        ylab(yaxis_title) +
        xlab("") +
        theme_classic() +
        theme(
          text = element_text(size = 12),
          axis.title.y = element_text(margin = margin(r = 12)),
          axis.line = element_line(size = 1.0),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_y_continuous(limits = c(0, ylim_upper)) +
        scale_fill_manual(
          "LA Selection",
          values = c("Selected" = "#12436D", "Statistical Neighbours" = "#88A1B5")
        )
    )
  }



  # label if no cases for selected factor

  if (selected_geo_lvl == "Local authority") {
    selected_row <- filtered_data %>% filter(selected_la$geo_breakdown == Breakdown)
    selected_rate <- selected_row$`Rate Per 10000`
    if (length(selected_rate) > 0 && selected_rate < 1) {
      y_max <- ylim_upper
      x_len <- length(filtered_data$Breakdown)
      p <- p + annotate(y = y_max, x = x_len / 2, geom = "text", label = "Selected local authority not highlighted, as factor was not recorded or the volume of data is too low to be published.", color = "red")
    }
  }

  return(p)
}

statistical_neighbours_plot_uasc <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue, yaxis_title, ylim_upper) {
  # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
  ylim_upper <- (ceiling(ylim_upper / 10) * 10) + (ylim_upper * 0.05)

  selected_la <- dataset %>%
    filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown == selected_geo_breakdown) %>%
    select(geo_breakdown, old_la_code)

  selected_la$old_la_code <- as.numeric(selected_la$old_la_code)

  neighbours_list <- stats_neighbours %>%
    filter(stats_neighbours$LA.number == selected_la$old_la_code) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.list()

  colors <- c(
    "Unaccompanied asylum-seeking children (Selected)" = "#28A197",
    "Non-unaccompanied asylum-seeking children (Selected)" = "#12436D",
    "Unaccompanied asylum-seeking children (Not Selected)" = "#28A1977F",
    "Non-unaccompanied asylum-seeking children (Not Selected)" = "#12436D7F"
  )

  filtered_data <- dataset %>%
    filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, neighbours_list)) %>%
    filter(
      population_count == "Children starting to be looked after each year",
      characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")
    ) %>%
    select(geo_breakdown, `yvalue`, characteristic) %>%
    mutate(
      geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
      is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours"),
      characteristic_selected = ifelse(is_selected == "Selected", paste0(characteristic, " (Selected)"), paste0(characteristic, " (Not Selected)"))
    ) %>%
    rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
    rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

  ggplot(filtered_data, aes(
    x = Breakdown, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = factor(characteristic_selected,
      levels = c(
        "Unaccompanied asylum-seeking children (Selected)",
        "Non-unaccompanied asylum-seeking children (Selected)",
        "Unaccompanied asylum-seeking children (Not Selected)",
        "Non-unaccompanied asylum-seeking children (Not Selected)"
      )
    ),
    text = paste0(
      "Placement rate per 10,000: ", !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), "<br>",
      "UASC status: ", factor(characteristic, levels = c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")), "<br>",
      "Local authority: ", Breakdown, "<br>",
      "Selection: ", Selection, "<br>",
      "Time period: ", max(dataset$time_period)
    )
  )) +
    geom_bar(stat = "identity") +
    # geom_col(position = position_dodge()) +
    ylab(yaxis_title) +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_y_continuous(limits = c(0, ylim_upper)) +
    scale_fill_manual(
      "UASC Status",
      values = colors,
      labels = c(
        "Unaccompanied asylum-seeking children (Selected)",
        "Non-unaccompanied asylum-seeking children (Selected)",
        "Unaccompanied asylum-seeking children (Not Selected)",
        "Non-unaccompanied asylum-seeking children (Not Selected)"
      )
    )
}

statistical_neighbours_plot_ofsted <- function(dataset, selected_geo_breakdown) {
  # Find the old_la_code for the selected geo_breakdown
  selected_la_code <- dataset %>%
    filter(geo_breakdown == selected_geo_breakdown) %>%
    pull(old_la_code) %>%
    unique()

  # Get the list of statistical neighbours for the selected old_la_code
  neighbours_list <- stats_neighbours %>%
    filter(LA.number == selected_la_code) %>%
    select(starts_with("SN")) %>%
    unlist() %>%
    as.character()

  # Filter the main dataset for the selected geo_breakdown and its neighbours
  # and only include rows where Count equals 1
  filtered_data <- dataset %>%
    filter(
      geo_breakdown %in% c(selected_geo_breakdown, neighbours_list),
      Count == 1,
      geographic_level == "Local authority"
    ) %>%
    mutate(Rating = recode(Rating,
      "inadequate_count" = "Inadequate",
      "requires_improvement_count" = "Requires Improvement",
      "good_count" = "Good",
      "outstanding_count" = "Outstanding"
    )) %>%
    filter(geographic_level == "Local authority") %>%
    group_by(geo_breakdown) %>%
    mutate(latest_rating = max(time_period)) %>%
    ungroup() %>%
    select(geo_breakdown, Rating, latest_rating)

  # Ensure 'Rating' is treated as a discrete variable
  filtered_data$Rating <- factor(filtered_data$Rating, levels = c("Inadequate", "Requires Improvement", "Good", "Outstanding"))

  ggplot(filtered_data, aes(
    x = geo_breakdown, y = Rating, fill = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours"),
    text = paste0(
      "Rating: ", Rating, "<br>",
      "Local authority: ", geo_breakdown, "<br>",
      "Last rated: ", latest_rating
    )
  )) +
    geom_point(shape = 23, size = 4) +
    labs(x = "Geographic Breakdown", y = "Latest leadership rating", fill = "LA Selection") +
    scale_fill_manual(values = c("Selected" = "#12436D", "Statistical Neighbours" = "#88A1B5")) +
    scale_y_discrete(limits = c("Inadequate", "Requires Improvement", "Good", "Outstanding")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

stats_neighbours_table <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, selectedcolumn = NULL, yvalue = NULL) {
  selected_la <- dataset %>%
    filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown == selected_geo_breakdown) %>%
    select(geo_breakdown, old_la_code)

  selected_la$old_la_code <- as.numeric(selected_la$old_la_code)

  neighbours_list <- stats_neighbours %>%
    filter(stats_neighbours$LA.number == selected_la$old_la_code) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.list()
  if (is.null(selectedcolumn)) {
    data2 <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, neighbours_list)) %>%
      select(time_period, geo_breakdown, `yvalue`) %>%
      mutate(
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours")
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
  } else {
    data2 <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, neighbours_list)) %>%
      select(all_of(c("time_period", "geo_breakdown", selectedcolumn, yvalue))) %>%
      mutate(
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours")
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

stats_neighbours_table_uasc <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue) {
  selected_la <- dataset %>%
    filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown == selected_geo_breakdown) %>%
    select(geo_breakdown, old_la_code)

  selected_la$old_la_code <- as.numeric(selected_la$old_la_code)

  neighbours_list <- stats_neighbours %>%
    filter(stats_neighbours$LA.number == selected_la$old_la_code) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.list()
  data2 <- dataset %>%
    filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, neighbours_list)) %>%
    select(geo_breakdown, characteristic, `yvalue`) %>%
    mutate(
      is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours")
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
  selected_la <- dataset %>%
    filter(geographic_level == "Local authority", geo_breakdown == selected_geo_breakdown, Count == 1) %>%
    select(geo_breakdown, old_la_code)

  selected_la$old_la_code <- as.numeric(selected_la$old_la_code)

  neighbours_list <- stats_neighbours %>%
    filter(stats_neighbours$LA.number == selected_la$old_la_code) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.list()

  if (is.null(selectedcolumn)) {
    data2 <- dataset %>%
      filter(geographic_level == "Local authority", geo_breakdown %in% c(selected_geo_breakdown, neighbours_list), Count == 1) %>%
      select(latest_rating, geo_breakdown, `yvalue`) %>%
      mutate(
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours")
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
      filter(geographic_level == "Local authority", geo_breakdown %in% c(selected_geo_breakdown, neighbours_list)) %>%
      select(all_of(c("latest_rating", "geo_breakdown", selectedcolumn, yvalue))) %>%
      mutate(
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Statistical Neighbours")
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


# Ordering tables with suppression
cellfunc <- function(value) {
  if (value == -100) {
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
cellfunc_percent <- function(value) {
  if (value == -100) {
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
