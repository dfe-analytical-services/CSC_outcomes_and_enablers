# CSC charts

# Time series repeat function ----
# This is a repeat use function for all of the time series plots in this dashboard.

plotly_time_series_custom_scale <- function(dataset, level, breakdown, yvalue, yaxis_title, ylim_upper, add_rect = FALSE, decimal_percentage = FALSE) {
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
      text = if (decimal_percentage) {
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
        axis.line = element_line(linewidth = 1.0)
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
        str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
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
        axis.line = element_line(linewidth = 1.0)
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

# By Region bar chart Plot -----
by_region_bar_plot <- function(dataset, yvalue, yaxis_title, yupperlim, add_rect = FALSE, decimal_percentage = FALSE) {
  if (add_rect == FALSE) {
    reg_data <- dataset %>%
      filter(geographic_level == "Regional", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, `yvalue`) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`)))) %>% # Order by yvalue rate
      rename(`Breakdown` = `geo_breakdown`) %>%
      rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

    p <- ggplot(reg_data, aes(
      x = `Breakdown`, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = factor(time_period),
      text = if (decimal_percentage) {
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
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + # Wrap the labels
      theme(
        text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(linewidth = 1.0)
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
        str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_title(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
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
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + # Wrap the labels
        theme(
          text = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(r = 12)),
          axis.line = element_line(linewidth = 1.0)
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




# By LA bar chart Plot (legacy version) ----

by_la_bar_plot <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue, yaxis_title, yupperlim = NULL, add_rect = FALSE, decimal_percentage = FALSE) {
  # prepare the yaxis title so it wraps at 25 chars
  yaxis_title <- str_wrap(yaxis_title, width = 25)

  if (selected_geo_lvl == "Local authority") {
    if (add_rect == FALSE) {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "Not Selected")
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    } else {
      la_data <- dataset %>%
        filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`, score_label) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "Not Selected")
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
          is_selected = selected_geo_breakdown
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    } else {
      la_data <- dataset %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `yvalue`, score_label) %>%
        mutate(
          geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
          is_selected = selected_geo_breakdown
        ) %>%
        rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
        rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))
    }
  }

  if (add_rect == FALSE) {
    p <- ggplot(la_data, aes(
      x = Breakdown, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = if (decimal_percentage) {
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
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(linewidth = 1.0)
      ) +
      scale_fill_manual(
        "LA Selection",
        values = setNames(c("#12436D", "#88A1B5"), c(selected_geo_breakdown, "Not Selected"))
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
    # SDQ version
    p <- ggplot(la_data, aes(
      x = Breakdown, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = paste0(
        str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
        "SDQ score: ",
        "Local authority: ", Breakdown, "<br>",
        "Time period: ", time_period, "<br>",
        "Selection: ", Selection
      )
    )) +
      ylab(yaxis_title) +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(linewidth = 1.0)
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_fill_manual(
        "LA Selection",
        values = setNames(c("#12436D", "#88A1B5"), c(selected_geo_breakdown, "Not Selected"))
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
    p2 <- p1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # diagonal the labels
      scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) # Wrap the labels
  } else {
    p2 <- p1 +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) # no labels
  }
  return(p2)
}



# By LA bar chart Plot (Revised version) ----
# This revised version will replace the legacy version of the function which is widely in use.  This version supports the modular implementation
# of the LA Bar Chart, with distinction being the data filtering (the reactive dataset is pre-filtered to year and geographies)
# in order to feed both the chart and the table.  Hence less data manipulation is done here than the original function

by_la_bar_plot_revised <- function(dataset, selected_geo_lvl, selected_geo_breakdown, yvalue, yaxis_title, yupperlim = NULL, add_rect = FALSE, decimal_percentage = FALSE) {
  # prepare the finishing touches to the dataset which is already filtered as we require
  cols_to_keep <- c("time_period", "geo_breakdown", yvalue)
  if (add_rect == TRUE) cols_to_keep <- c(cols_to_keep, "score_label")

  plot_data <- copy(dataset[, .SD, .SDcols = cols_to_keep])
  plot_data <- plot_data %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))))

  if (selected_geo_lvl == "Local authority") {
    plot_data[, is_selected := ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "Not Selected")]
  } else if (selected_geo_lvl == "National") {
    plot_data[, is_selected := "Not Selected"]
  } else if (selected_geo_lvl == "Regional") {
    plot_data[, is_selected := selected_geo_breakdown]
  }

  # get the column names right for plotting
  plot_data <- plot_data %>%
    rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
    rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))

  # prepare the yaxis title so it wraps at 25 chars
  yaxis_title <- str_wrap(yaxis_title, width = 25)

  # now generate the plot

  if (add_rect == FALSE) {
    p <- ggplot(plot_data, aes(
      x = Breakdown, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = if (decimal_percentage) {
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
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(linewidth = 1.0)
      ) +
      scale_fill_manual(
        "LA Selection",
        values = setNames(c("#12436D", "#88A1B5"), c(selected_geo_breakdown, "Not Selected"))
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
    # SDQ version
    p <- ggplot(la_data, aes(
      x = Breakdown, y = !!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = paste0(
        str_to_sentence(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_sentence(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
        "SDQ score: ",
        "Local authority: ", Breakdown, "<br>",
        "Time period: ", time_period, "<br>",
        "Selection: ", Selection
      )
    )) +
      ylab(yaxis_title) +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(linewidth = 1.0)
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_fill_manual(
        "LA Selection",
        values = setNames(c("#12436D", "#88A1B5"), c(selected_geo_breakdown, "Not Selected"))
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
    p2 <- p1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # diagonal the labels
      scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) # Wrap the labels
  } else {
    p2 <- p1 +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) # no labels
  }

  return(p2)
}


# Statistical Neighbours Plot (Legacy version) ----
statistical_neighbours_plot <- function(dataset,
                                        selected_geo_breakdown = NULL,
                                        selected_geo_lvl = NULL,
                                        yvalue,
                                        yaxis_title,
                                        ylim_upper,
                                        add_rect = FALSE,
                                        decimal_percentage = FALSE) {
  # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
  ylim_upper <- (ceiling(ylim_upper / 10) * 10) + (ylim_upper * 0.05)

  # prepare the yaxis title so it wraps at 25 chars
  yaxis_title <- str_wrap(yaxis_title, width = 25)

  sn_names <- stats_neighbours %>%
    filter(stats_neighbours$LA.Name == selected_geo_breakdown) %>%
    select("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10") %>%
    as.character()

  if (add_rect == FALSE) {
    filtered_data <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, sn_names)) %>%
      select(geo_breakdown, `yvalue`) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "statistical neighbours")
      ) %>%
      rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

    ggplot(filtered_data, aes(
      x = Breakdown, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = if (decimal_percentage) {
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
      scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) + # Wrap the labels
      theme(
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(linewidth = 1.0),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(limits = c(0, ylim_upper)) +
      scale_fill_manual(
        "LA Selection",
        values = setNames(c("#12436D", "#88A1B5"), c(selected_geo_breakdown, "statistical neighbours"))
      )
  } else {
    filtered_data <- dataset %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), geo_breakdown %in% c(selected_geo_breakdown, sn_names)) %>%
      select(geo_breakdown, `yvalue`, score_label) %>%
      mutate(
        geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`))),
        is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "statistical neighbours")
      ) %>%
      rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
      rename_at(yvalue, ~ str_to_title(str_replace_all(., "_", " ")))

    max_xaxis <- 11 # ten neighbours and selected LA
    suppressWarnings(
      p <- ggplot(filtered_data, aes(
        x = Breakdown, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
        text = paste0(
          str_to_title(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_title(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
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
          axis.line = element_line(linewidth = 1.0),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) + # Wrap the labels
        scale_y_continuous(limits = c(0, ylim_upper)) +
        scale_fill_manual(
          "LA Selection",
          values = setNames(c("#12436D", "#88A1B5"), c(selected_geo_breakdown, "statistical neighbours"))
        )
    )
  }
}



# Statistical Neighbours Plot (Revised version) ----
statistical_neighbours_plot_revised <- function(dataset,
                                                selected_geo_lvl = NULL,
                                                selected_geo_breakdown = NULL,
                                                yvalue,
                                                yaxis_title,
                                                ylim_upper,
                                                add_rect = FALSE,
                                                decimal_percentage = FALSE) {
  # prepare the finishing touches to the dataset which is already filtered as we require
  cols_to_keep <- c("time_period", "geo_breakdown", yvalue)
  if (add_rect == TRUE) cols_to_keep <- c(cols_to_keep, "score_label")

  plot_data <- copy(dataset[, .SD, .SDcols = cols_to_keep])
  plot_data <- plot_data %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`)))) %>%
    mutate(is_selected = ifelse(geo_breakdown == selected_geo_breakdown, selected_geo_breakdown, "statistical neighbours")) %>%
    rename(`Breakdown` = `geo_breakdown`, `Selection` = `is_selected`) %>%
    rename_at(yvalue, ~ str_to_sentence(str_replace_all(., "_", " ")))

  # now generate the plot
  # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
  ylim_upper <- (ceiling(ylim_upper / 10) * 10) + (ylim_upper * 0.05)

  # prepare the y-axis title so it is wrapped at 25 characters
  yaxis_title <- str_wrap(yaxis_title, width = 25)

  if (add_rect == FALSE) {
    # default version of the plot (i.e. not with the SDQ thresholds plotted)
    ggplot(plot_data, aes(
      x = Breakdown, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
      text = if (decimal_percentage) {
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
      scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) + # Wrap the labels
      theme(
        text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.line = element_line(linewidth = 1.0),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(limits = c(0, ylim_upper)) +
      scale_fill_manual(
        "LA Selection",
        values = setNames(c("#12436D", "#88A1B5"), c(selected_geo_breakdown, "statistical neighbours"))
      )
  } else {
    # SDQ version of the plot
    max_xaxis <- 11 # ten neighbours and selected LA
    suppressWarnings(
      p <- ggplot(filtered_data, aes(
        x = Breakdown, y = !!sym(str_to_title(str_replace_all(yvalue, "_", " "))), fill = `Selection`,
        text = paste0(
          str_to_title(str_replace_all(yvalue, "_", " ")), ": ", format(!!sym(str_to_title(str_replace_all(yvalue, "_", " "))), nsmall = 1), "<br>",
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
        scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) + # Wrap the labels
        theme(
          text = element_text(size = 12),
          axis.title.y = element_text(margin = margin(r = 12)),
          axis.line = element_line(linewidth = 1.0),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_y_continuous(limits = c(0, ylim_upper)) +
        scale_fill_manual(
          "LA Selection",
          values = setNames(c("#12436D", "#88A1B5"), c(selected_geo_breakdown, "statistical neighbours"))
        )
    )
  }
}
