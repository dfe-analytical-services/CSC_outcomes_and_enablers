# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {
  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # The template uses bookmarking to store input choices in the url. You can
  # exclude specific inputs (for example extra info created for a datatable
  # or plotly chart) using the list below, but it will need updating to match
  # any entries in your own dashboard's bookmarking url that you don't want
  # including.
  setBookmarkExclude(c(
    "cookies", "link_to_app_content_tab",
    "tabBenchmark_rows_current", "tabBenchmark_rows_all",
    "tabBenchmark_columns_selected", "tabBenchmark_cell_clicked",
    "tabBenchmark_cells_selected", "tabBenchmark_search",
    "tabBenchmark_rows_selected", "tabBenchmark_row_last_clicked",
    "tabBenchmark_state",
    "plotly_relayout-A",
    "plotly_click-A", "plotly_hover-A", "plotly_afterplot-A",
    ".clientValue-default-plotlyCrosstalkOpts",
    "bookmark1", "bookmark2"
  ))

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  observe({
    if (input$navlistPanel == "dashboard") {
      change_window_title(
        session,
        paste0(
          site_title, " - ",
          input$selectPhase, ", ",
          input$selectArea
        )
      )
    } else {
      change_window_title(
        session,
        paste0(
          site_title, " - ",
          input$navlistPanel
        )
      )
    }
  })

  output$cookie_status <- dfeshiny::cookie_banner_server(
    "cookies",
    input_cookies = reactive(input$cookies),
    input_clear = reactive(input$cookie_consent_clear),
    parent_session = session,
    google_analytics_key = google_analytics_key
  )

  # output$choice_text_test <- renderText({
  #   c(paste0("you have selected",input$select_geography))
  # })

  # Time period dropdown also does not need to appear here - does not need to be reactive

  # Simple server stuff goes here ------------------------------------------------------------
  # reactiveRevBal <- reactive({
  #   dfRevBal %>% filter(
  #     area_name == input$selectArea | area_name == "England",
  #     school_phase == input$selectPhase
  #   )
  # })

  # Define server logic required to draw a histogram
  # output$lineRevBal <- renderPlotly({
  #   ggplotly(createAvgRevTimeSeries(reactiveRevBal(), input$selectArea)) %>%
  #     config(displayModeBar = F) %>%
  #     layout(legend = list(orientation = "h", x = 0, y = -0.2))
  # })
  #
  # reactiveBenchmark <- reactive({
  #   dfRevBal %>%
  #     filter(
  #       area_name %in% c(input$selectArea, input$selectBenchLAs),
  #       school_phase == input$selectPhase,
  #       year == max(year)
  #     )
  # })
  #
  # output$colBenchmark <- renderPlotly({
  #   ggplotly(
  #     plotAvgRevBenchmark(reactiveBenchmark()) %>%
  #       config(displayModeBar = F),
  #     height = 420
  #   )
  # })
  #
  # output$tabBenchmark <- renderDataTable({
  #   datatable(
  #     reactiveBenchmark() %>%
  #       select(
  #         Area = area_name,
  #         `Average Revenue Balance (£)` = average_revenue_balance,
  #         `Total Revenue Balance (£m)` = total_revenue_balance_million
  #       ),
  #     options = list(
  #       scrollX = TRUE,
  #       paging = FALSE
  #     )
  #   )
  # })

  # Dropdown Validation -----
  iv <- InputValidator$new()
  # outcome1
  iv$add_rule("select_geography_o1", sv_required())
  iv$add_rule("geographic_breakdown_o1", sv_required())
  # outcome2
  iv$add_rule("select_geography_o2", sv_required())
  iv$add_rule("geographic_breakdown_o2", sv_required())
  # outcome3
  iv$add_rule("select_geography_o3", sv_required())
  iv$add_rule("geographic_breakdown_o3", sv_required())
  # outcome4
  iv$add_rule("select_geography_o4", sv_required())
  iv$add_rule("geographic_breakdown_o4", sv_required())
  # enabler2
  iv$add_rule("select_geography_e2", sv_required())
  iv$add_rule("geographic_breakdown_e2", sv_required())
  # enabler3
  iv$add_rule("select_geography_e3", sv_required())
  iv$add_rule("geographic_breakdown_e3", sv_required())


  iv$enable()


  # CSC server logic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Enabler 3 ----
  # Geographic level does not need to be here as it does not need to change depending on other dropdowns

  # Geographic breakdown e3 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_e3
  }, {
    choices <- sort(unique(workforce_data[(workforce_data$geographic_level == input$select_geography_e3 & workforce_data$time_period == max(workforce_data$time_period)), "geo_breakdown"]), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_e3",
      selected = choices[1],
      choices = choices
    )
  })

  ## Confirmation sentence E3 -------
  # This function gets the selected region to put into the confirmation text below

  workforce_region_e3 <- reactive({
    location_data_workforce %>%
      filter(la_name == input$geographic_breakdown_e3) %>%
      pull(region_name) %>%
      as.character() # Convert to character
  })

  # First sentence for the dropdown choices
  output$enabler3_choice_text1 <- renderText({
    if (input$select_geography_e3 == "National") {
      paste0("You have selected ", tags$b(input$select_geography_e3), " level statistics on ", tags$b("England"), ".")
    } else if (input$select_geography_e3 == "Regional") {
      paste0("You have selected ", tags$b(input$select_geography_e3), " level statistics for ", tags$b(input$geographic_breakdown_e3), ".")
    } else if (input$select_geography_e3 == "Local authority") {
      paste0("You have selected ", tags$b(input$select_geography_e3), " level statistics for ", tags$b(input$geographic_breakdown_e3), ", in ", workforce_region_e3(), ".")
    }
  })

  output$enabler3_choice_text2 <- renderText({
    # Checking to see if they picked national average comparison
    if (!is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      paste0("You have also selected to compare with the ", tags$b("National Average."))
      # If they picked regional comparison
    } else if (is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      paste0("You have also selected to compare with the ", tags$b("Regional average."))
      # Picked both national and regional comparison
    } else if (!is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      paste0("You have also selected to compare with the ", tags$b("National average"), " and the ", tags$b("Regional average."))
    }
  })


  # Social worker turnover rate headline box
  ## Turnover rate plot and table -----
  output$s_w_headline_txt <- renderText({
    if (input$geographic_breakdown_e3 == "") {
      stat <- "NA"
    } else {
      stat <- format(workforce_data %>%
        filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(turnover_rate_fte), nsmall = 1)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_data$time_period), ")", "</p>"
    )
  })

  # Social worker turnover rate benchmarking plot
  output$plot_s_w_turnover <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter(geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name) | geographic_level == "National"))
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e3, input$geographic_breakdown_e3, "Turnover Rate Fte", "Turnover rate (FTE) %", 100) %>%
      config(displayModeBar = F)

    p <- p + ggtitle("Social worker turnover rate (FTE) %")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Social worker turnover rate benchmarking table alternative
  output$table_s_w_turnover <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Turnover Rate (FTE) %" = "Turnover Rate Fte")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Turnover Rate (FTE) %" = "Turnover Rate Fte")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name))) %>%
        select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Turnover Rate (FTE) %" = "Turnover Rate Fte")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Turnover rate (FTE) %" = "Turnover Rate Fte")
    }

    reactable(
      filtered_data,
      columns = list(
        `Turnover Rate (FTE) %` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### turnover rate by region plot----
  output$plot_turnover_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    p <- by_region_bar_plot(workforce_data, "Turnover Rate Fte", "Turnover Rate (FTE) %", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Social worker turnover rate (FTE) % by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # turnover rate by region table
  output$table_turnover_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- workforce_data %>%
      filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
      select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
      arrange(desc(`Turnover Rate Fte`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Turnover rate (FTE) %" = "Turnover Rate Fte")

    reactable(
      data,
      columns = list(
        `Turnover rate (FTE) %` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### Turnover Rate by LA plot ----
  output$plot_turnover_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    p <- by_la_bar_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Turnover Rate Fte", "Turnover Rate (FTE) %") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Social worker turnover rate (FTE) % by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Turnover Rate by LA table
  output$table_turnover_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    if (input$select_geography_e3 == "Regional") {
      if (input$geographic_breakdown_e3 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e3) %>%
          pull(la_name)
      }
      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
        arrange(desc(`Turnover Rate Fte`)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Turnover rate (FTE) %" = "Turnover Rate Fte")
    } else if (input$select_geography_e3 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(
          time_period, geo_breakdown,
          `Turnover Rate Fte`
        ) %>%
        arrange(desc(`Turnover Rate Fte`)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Turnover rate (FTE) %" = "Turnover Rate Fte")
    }

    reactable(
      data,
      columns = list(
        `Turnover rate (FTE) %` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })


  ## Agency Rate ----
  output$agency_rate_txt <- renderText({
    if (input$geographic_breakdown_e3 == "") {
      stat <- "NA"
    } else {
      stat <- format(workforce_data %>%
        filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(agency_rate_fte), nsmall = 1)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_data$time_period), ")", "</p>"
    )
  })

  ### Agency worker rate benchmarking plot ----
  output$plot_agency_worker <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter(geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name) | geographic_level == "National"))
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e3, input$geographic_breakdown_e3, "Agency Rate Fte", "Agency worker rate (FTE) %", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Agency worker rate (FTE) %")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Agency worker rate table alternative
  output$table_agency_worker <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(time_period, geo_breakdown, "Agency Rate Fte") %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Agency worker rate (FTE) %" = "Agency Rate Fte")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, "Agency Rate Fte") %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Agency worker rate (FTE) %" = "Agency Rate Fte")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name))) %>%
        select(time_period, geo_breakdown, "Agency Rate Fte") %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Agency worker rate (FTE) %" = "Agency Rate Fte")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, "Agency Rate Fte") %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Agency worker rate (FTE) %" = "Agency Rate Fte")
    }

    reactable(
      filtered_data,
      columns = list(
        `Agency worker rate (FTE) %` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### agency rate plot by region ----
  output$plot_agency_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    p <- by_region_bar_plot(workforce_data, "Agency Rate Fte", "Agency worker rate (FTE) %", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Agency worker rate (FTE) % by region")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # agency rate table by region
  output$table_agency_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    data <- workforce_data %>%
      filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
      select(time_period, geo_breakdown, `Agency Rate Fte`) %>%
      arrange(desc(`Agency Rate Fte`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Agency worker rate (FTE) %" = "Agency Rate Fte")

    reactable(
      data,
      columns = list(
        `Agency worker rate (FTE) %` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### agency rate by la plot -----
  output$plot_agency_rate_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    p <- by_la_bar_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Agency Rate Fte", "Agency worker rate (FTE) %") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Agency worker rate (FTE) % by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # agency rate by la table alternative
  output$table_agency_rate_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    if (input$select_geography_e3 == "Regional") {
      if (input$geographic_breakdown_e3 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e3) %>%
          pull(la_name)
      }

      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, "Agency Rate Fte") %>%
        arrange(desc(`Agency Rate Fte`)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Agency worker rate (FTE) %" = "Agency Rate Fte")
    } else if (input$select_geography_e3 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(time_period, geo_breakdown, "Agency Rate Fte") %>%
        arrange(desc(`Agency Rate Fte`)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Agency worker rate (FTE) %" = "Agency Rate Fte")
    }

    reactable(
      data,
      columns = list(
        `Agency worker rate (FTE) %` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ## Vacancy Rate -----
  # Vacancy rate headline box

  output$vacancy_rate_txt <- renderText({
    if (input$geographic_breakdown_e3 == "") {
      stat <- "NA"
    } else {
      stat <- format(workforce_data %>%
        filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(vacancy_rate_fte), nsmall = 1)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_data$time_period), ")", "</p>"
    )
  })

  ### Vacancy Rate benchmarking plot ----
  output$plot_vacancy_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter(geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name) | geographic_level == "National"))
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e3, input$geographic_breakdown_e3, "Vacancy Rate Fte", "Vacancy rate (FTE) %", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Vacancy rate (FTE) %")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })



  # Vacancy Rate benchmarking table alternative
  output$table_vacancy_rate <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(time_period, geo_breakdown, "Vacancy Rate Fte") %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Vacancy rate (FTE) %" = "Vacancy Rate Fte")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, "Vacancy Rate Fte") %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Vacancy rate (FTE) %" = "Vacancy Rate Fte")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name))) %>%
        select(time_period, geo_breakdown, "Vacancy Rate Fte") %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Vacancy rate (FTE) %" = "Vacancy Rate Fte")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, "Vacancy Rate Fte") %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Vacancy rate (FTE) %" = "Vacancy Rate Fte")
    }

    reactable(
      filtered_data,
      columns = list(
        `Vacancy rate (FTE) %` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### vacancy rate by la plot ----
  output$plot_vacancy_rate_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    p <- by_la_bar_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Vacancy Rate Fte", "Vacancy rate (FTE) %") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Vacancy rate (FTE) % by local authority")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # vacancy rate by la table alternative
  output$table_vacancy_rate_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    if (input$select_geography_e3 == "Regional") {
      if (input$geographic_breakdown_e3 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e3) %>%
          pull(la_name)
      }

      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, "Vacancy Rate Fte") %>%
        arrange(desc(vacancy_rate_fte)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Vacancy rate (FTE) %" = "Vacancy Rate Fte")
    } else if (input$select_geography_e3 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(time_period, geo_breakdown, `Vacancy Rate Fte`) %>%
        arrange(desc(`Vacancy Rate Fte`)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Vacancy rate (FTE) %" = "Vacancy Rate Fte")
    }

    reactable(
      data,
      columns = list(
        `Vacancy rate (FTE) %` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )

    # datatable(
    #   data,
    #   colnames = c("Time period", "Local authority", "Vacancy rate (FTE) %"),
    #   options = list(
    #     scrollx = FALSE,
    #     paging = TRUE
    #   )
    # )
  })


  ### vacancy rate plot by region ----
  output$plot_vacancy_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    p <- by_region_bar_plot(workforce_data, "Vacancy Rate Fte", "Vacancy rate (FTE) %", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Vacancy rate (FTE) % by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # vacancy rate table by region
  output$table_vacancy_reg <- renderReactable({
    data <- workforce_data %>%
      filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
      select(time_period, geo_breakdown, `Vacancy Rate Fte`) %>%
      arrange(desc(`Vacancy Rate Fte`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Vacancy rate (FTE) %" = "Vacancy Rate Fte")

    reactable(
      data,
      columns = list(
        `Vacancy rate (FTE) %` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ## Caseload ----
  # Caseload headline box

  output$caseload_txt <- renderText({
    if (input$geographic_breakdown_e3 == "") {
      stat <- "NA"
      paste0(stat, "%", "<br>")
    } else {
      previous_year_value <- workforce_data %>%
        filter(time_period == (max(workforce_data$time_period) - 1) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(caseload_fte)

      current_year_value <- workforce_data %>%
        filter(time_period == (max(workforce_data$time_period)) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(caseload_fte)

      if ((current_year_value < previous_year_value)) {
        context <- " down from "
      } else {
        context <- " up from "
      }
      stat <- format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e3) %>% select(caseload_fte), nsmall = 1)
      paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "in ", max(workforce_data$time_period), context, previous_year_value, " ", (max(workforce_data$time_period) - 1), "</p>")
    }
  })

  ### Caseload benchmarking plot ----
  output$caseload_plot <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter(geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name) | geographic_level == "National"))
    }

    # Set the max y-axis scale
    max_rate <- max(workforce_data$`Caseload Fte`, na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e3, input$geographic_breakdown_e3, "Caseload Fte", "Average caseload (FTE)", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average caseload (FTE)")

    ggplotly(p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })


  # caseload benchamrking table alternative
  output$table_caseload <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(time_period, geo_breakdown, `Caseload Fte`) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e3) && is.null(input$region_comparison_checkbox_e3)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e3 & geo_breakdown %in% input$geographic_breakdown_e3) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, `Caseload Fte`) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name))) %>%
        select(time_period, geo_breakdown, `Caseload Fte`) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e3) && !is.null(input$region_comparison_checkbox_e3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e3)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e3, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, `Caseload Fte`) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")
    }

    reactable(
      filtered_data,
      columns = list(
        `Average caseload (FTE)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### Caseload by region ----
  output$plot_caseload_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    p <- by_region_bar_plot(workforce_data, "Caseload Fte", "Average Caseload (FTE)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average caseload (FTE) by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Caseload by region table
  output$table_caseload_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- workforce_data %>%
      filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
      select(time_period, geo_breakdown, "Caseload Fte") %>%
      arrange(desc(`Caseload Fte`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")

    reactable(
      data,
      columns = list(
        `Average caseload (FTE)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### caseload by la -----
  output$plot_caseload_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    p <- by_la_bar_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Caseload Fte", "Average Caseload (FTE)") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average caseload (FTE) by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Caseload by LA table
  output$table_caseload_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    if (input$select_geography_e3 == "Regional") {
      if (input$geographic_breakdown_e3 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e3) %>%
          pull(la_name)
      }

      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, "Caseload Fte") %>%
        arrange(desc(`Caseload Fte`)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")
    } else if (input$select_geography_e3 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(time_period, geo_breakdown, "Caseload Fte") %>%
        arrange(desc(`Caseload Fte`)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")
    }
    reactable(
      data,
      columns = list(
        `Average caseload (FTE)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  # Ethnicity and Diversity Domain-----

  output$non_white_txt <- renderText({
    white_stat <- workforce_eth %>%
      filter(time_period == max(workforce_eth$time_period) &
        geo_breakdown %in% input$geographic_breakdown_e3 &
        role == "Total" &
        breakdown == "White") %>%
      select(inpost_headcount_percentage)

    if (input$geographic_breakdown_e3 == "") {
      non_white_stat <- "NA"
    } else {
      non_white_stat <- format(100 - as.numeric(white_stat), nsmall = 1)
    }
    paste0(non_white_stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_eth$time_period), ")", "</p>")
  })

  output$plot_ethnicity_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location."),
    )
    p <- plot_ethnicity_rate(input$geographic_breakdown_e3, input$select_geography_e3) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Social worker ethnicity %")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$plot_population_ethnicity_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    p <- plot_population_ethnicity_rate(input$geographic_breakdown_e3) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Social worker ethnicity % vs. general population ethnicity %")
    ggplotly(
      p,
      height = 420
      # This one does not need to have a customised tooltip
    )
  })

  output$table_ethnicity_rate <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- workforce_eth %>%
      filter(geo_breakdown %in% input$geographic_breakdown_e3, role == "Total", breakdown_topic == "Ethnicity major") %>%
      select(time_period, geo_breakdown, breakdown, inpost_headcount, inpost_headcount_percentage) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Ethnicity" = "breakdown", "Headcount" = "inpost_headcount", "Headcount (%)" = "inpost_headcount_percentage")

    reactable(
      data,
      columns = list(
        `Headcount` = colDef(cell = cellfunc),
        `Headcount (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })



  output$table_population_ethnicity_rate <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- combined_ethnicity_data %>%
      filter(geo_breakdown %in% input$geographic_breakdown_e3) %>%
      select(geo_breakdown, breakdown, inpost_headcount_percentage, Percentage) %>%
      rename("Location" = "geo_breakdown", "Ethnicity group" = "breakdown", "Workforce (%)" = "inpost_headcount_percentage", "Population (%)" = "Percentage")

    reactable(
      data,
      columns = list(
        `Workforce (%)` = colDef(cell = cellfunc),
        `Population (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  output$plot_seniority_eth <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    p <- plot_seniority_eth(input$geographic_breakdown_e3, input$select_geography_e3) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Social worker ethnicity by seniority level %")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  cols <- c("time_period", "geo_breakdown", "seniority", "breakdown", "inpost_headcount", "Percentage")

  output$table_seniority_eth <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- workforce_eth_seniority[, cols] %>%
      filter(geo_breakdown %in% input$geographic_breakdown_e3, seniority != "Total", time_period == max(workforce_eth_seniority$time_period)) %>%
      select(time_period, geo_breakdown, seniority, breakdown, inpost_headcount, Percentage) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Seniority level" = "seniority", "Ethnicity" = "breakdown", "Headcount" = "inpost_headcount", "Headcount (%)" = "Percentage")

    reactable(
      data,
      columns = list(
        `Headcount` = colDef(cell = cellfunc),
        `Headcount (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  # Enabler 2 -----
  # Geographic breakdown e2 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_e2
  }, {
    choices <- sort(unique(spending_data[spending_data$geographic_level == input$select_geography_e2, ][["geo_breakdown"]]), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_e2",
      selected = choices[1],
      choices = choices
    )
  })

  ## Confirmation sentence e2 -------
  # This function gets the selected region to put into the confirmation text below

  workforce_region_e2 <- reactive({
    location_data %>%
      filter(la_name == input$geographic_breakdown_e2) %>%
      pull(region_name) %>%
      as.character() # Convert to character
  })

  # First sentence for the dropdown choices
  output$enabler3_choice_text1 <- renderText({
    if (input$select_geography_e2 == "National") {
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics on ", tags$b("England"), ".")
    } else if (input$select_geography_e2 == "Regional") {
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics for ", tags$b(input$geographic_breakdown_e2), ".")
    } else if (input$select_geography_e2 == "Local authority") {
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics for ", tags$b(input$geographic_breakdown_e2), ", in ", workforce_region_e2(), ".")
    }
  })

  output$enabler3_choice_text2 <- renderText({
    # Checking to see if they picked national average comparison
    if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      paste0("You have also selected to compare with the ", tags$b("National Average."))
      # If they picked regional comparison
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      paste0("You have also selected to compare with the ", tags$b("Regional average."))
      # Picked both national and regional comparison
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      paste0("You have also selected to compare with the ", tags$b("National average"), " and the ", tags$b("Regional average."))
    }
  })

  ## Spending -----

  ##### Headline stats
  # Share of total spend on CS
  output$total_spending_txt <- renderText({
    if (input$geographic_breakdown_e2 == "") {
      stat <- "NA"
    } else {
      stat <- format(spending_data %>% filter(time_period == "2022/23" &
        geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(`CS Share`), nsmall = 2)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(spending_data$time_period), ")", "</p>")
  })

  # Average spend per child
  output$avg_spend_per_child <- renderText({
    if (input$geographic_breakdown_e2 == "") {
      stat <- "NA"
    } else {
      stat <- format(spending_per_capita %>% filter(time_period == max(spending_per_capita$time_period) &
        geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(`Cost per child`))
    }

    paste0("~ £", stat, " per child", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(spending_per_capita$time_period), ")", "</p>")
  })

  # Share of total spend on children's services minus CLA
  output$spend_minus_cla_txt <- renderText({
    if (input$geographic_breakdown_e2 == "") {
      stat <- "NA"
    } else {
      stat <- format(spending_data_no_cla %>% filter(time_period == "2022/23" &
        geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(`Excluding CLA Share`), nsmall = 2)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(spending_data_no_cla$time_period), ")", "</p>")
  })


  # Dynamic titles
  output$spending_header1 <- renderUI({
    h2(paste(input$spending_choice, " by region"))
  })
  output$spending_header2 <- renderUI({
    h2(paste(input$spending_choice, " by local authority"))
  })


  ## Regional plot for spending
  output$plot_spending_region <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
      need(input$spending_choice != "", "Select a spending level.")
    )
    # Need an if statement to look at the spending level choice this will determine the data in the chart
    if (input$spending_choice == "Share of total spend on children's services") {
      data <- spending_data %>%
        filter(geographic_level == "Regional", time_period == max(spending_data$time_period)) %>%
        select(time_period, geographic_level, geo_breakdown, cs_share) # %>%

      max_y_lim <- ceiling(max(data$cs_share) / 10) * 10
      p <- by_region_bar_plot(data, "cs_share", "Share spent on children's services (%)", max_y_lim) %>%
        config(displayModeBar = F)
      p <- p + ggtitle("Share of total spend on children's services (%) by region")
    } else {
      data <- spending_per_capita %>%
        filter(geographic_level == "Regional", time_period == max(spending_data$time_period)) %>%
        select(time_period, geographic_level, geo_breakdown, cost_per_capita) %>%
        rename("Average spend per child (£)" = "cost_per_capita")

      max_y_lim <- ceiling(max(data$`Average spend per child (£)`) / 50) * 50

      p <- by_region_bar_plot(data, "Average spend per child (£)", "Average spend per child (£)", max_y_lim) %>%
        config(displayModeBar = F)
      p <- p + ggtitle("Average spend per child (£) by region")
    }

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # region table alternative
  output$table_tot_spending_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
      need(input$spending_choice != "", "Select a spending level.")
    )
    if (input$spending_choice == "Share of total spend on children's services") {
      data <- spending_data %>%
        filter(geographic_level == "Regional", time_period == max(spending_data$time_period)) %>%
        select(time_period, geo_breakdown, cs_share) %>%
        arrange(desc(cs_share)) %>%
        rename("Time period" = "time_period", "Region" = "geo_breakdown", "Share of spending on Children's services (%)" = "cs_share")

      table <- reactable(
        data,
        columns = list(
          `Share of spending on Children's services (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
        ),
        defaultPageSize = 15,
        searchable = TRUE,
      )
    } else {
      data <- spending_per_capita %>%
        filter(geographic_level == "Regional", time_period == max(spending_data$time_period)) %>%
        select(time_period, geo_breakdown, cost_per_capita) %>%
        arrange(desc(cost_per_capita)) %>%
        rename("Time period" = "time_period", "Region" = "geo_breakdown", "Average spend per child (£)" = "cost_per_capita")

      table <- reactable(
        data,
        columns = list(
          `Average spend per child (£)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
        ),
        defaultPageSize = 15,
        searchable = TRUE,
      )
    }
  })

  # Local authority spending
  output$plot_spending_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
      need(input$spending_choice != "", "Select a spending level.")
    )
    # Need an if statement to look at the spending level choice this will determine the data in the chart
    if (input$spending_choice == "Share of total spend on children's services") {
      data <- spending_data %>%
        filter(geographic_level == "Local authority", time_period == max(spending_data$time_period)) %>%
        select(time_period, geographic_level, geo_breakdown, cs_share) # %>%

      max_y_lim <- ceiling(max(data$cs_share) / 10) * 10
      p <- by_la_bar_plot(dataset = data, selected_geo_breakdown = input$geographic_breakdown_e2, selected_geo_lvl = input$select_geography_e2, yvalue = "cs_share", yaxis_title = "Share spent on children's services (%)") %>%
        config(displayModeBar = F)
      p <- p + ggtitle("Share of total spend on children's services (%) by local authority") +
        scale_y_continuous(limits = c(0, max_y_lim))
    } else {
      data <- spending_per_capita %>%
        filter(geographic_level == "Local authority", time_period == max(spending_data$time_period)) %>%
        select(time_period, geographic_level, geo_breakdown, cost_per_capita) %>%
        rename("Spend per child (£)" = "cost_per_capita")

      max_y_lim <- ceiling(max(data$`Spend per child (£)`) / 50) * 50

      p <- by_la_bar_plot(dataset = data, selected_geo_breakdown = input$geographic_breakdown_e2, selected_geo_lvl = input$select_geography_e2, yvalue = "Spend per child (£)", yaxis_title = "Average spend per child (£)") %>%
        config(displayModeBar = F)
      p <- p + ggtitle("Average spend per child (£) by local authority") +
        scale_y_continuous(limits = c(0, max_y_lim))
    }
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$table_tot_spending_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
      need(input$spending_choice != "", "Select a spending level.")
    )
    if (input$spending_choice == "Share of total spend on children's services") {
      data <- spending_data %>%
        filter(geographic_level == "Local authority", time_period == max(spending_data$time_period)) %>%
        select(time_period, geo_breakdown, cs_share) %>%
        arrange(desc(cs_share)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Share of spending on Children's services (%)" = "cs_share")

      table <- reactable(
        data,
        columns = list(
          `Share of spending on Children's services (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
        ),
        defaultPageSize = 15,
        searchable = TRUE,
      )
    } else {
      data <- spending_per_capita %>%
        filter(geographic_level == "Local authority", time_period == max(spending_data$time_period)) %>%
        select(time_period, geo_breakdown, cost_per_capita) %>%
        arrange(desc(cost_per_capita)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Average spend per child (£)" = "cost_per_capita")

      table <- reactable(
        data,
        columns = list(
          `Average spend per child (£)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
        ),
        defaultPageSize = 15,
        searchable = TRUE,
      )
    }
  })

  ## Spending minus CLA -------
  ## Regional plot for spending
  output$plot_spend_excl_cla_region <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )

    data <- spending_data_no_cla %>%
      filter(geographic_level == "Regional", time_period == max(spending_data_no_cla$time_period)) %>%
      select(time_period, geographic_level, geo_breakdown, minus_cla_share) # %>%

    max_y_lim <- ceiling(max(data$minus_cla_share) / 10) * 10
    p <- by_region_bar_plot(data, "minus_cla_share", "Share spent on children's services\n excluding CLA (%)", max_y_lim) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Share of total spend on children's services minus CLA (%) by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # region table alternative
  output$table_spend_excl_cla_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    data <- spending_data_no_cla %>%
      filter(geographic_level == "Regional", time_period == max(spending_data_no_cla$time_period)) %>%
      select(time_period, geo_breakdown, minus_cla_share) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Share of spend on children's services minus CLA (%)" = "minus_cla_share")

    reactable(
      data,
      columns = list(
        `Share of spend on children's services minus CLA (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # SPending minus CLA by local authority plot and table
  output$plot_spend_excl_cla_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    # Need an if statement to look at the spending level choice this will determine the data in the chart
    data <- spending_data_no_cla %>%
      filter(geographic_level == "Local authority", time_period == max(spending_data_no_cla$time_period)) %>%
      select(time_period, geographic_level, geo_breakdown, minus_cla_share) %>%
      rename("Share minus CLA (%)" = "minus_cla_share")

    max_y_lim <- ceiling(max(data$`Share minus CLA (%)`) / 50) * 50

    p <- by_la_bar_plot(dataset = data, selected_geo_breakdown = input$geographic_breakdown_e2, selected_geo_lvl = input$select_geography_e2, yvalue = "Share minus CLA (%)", yaxis_title = "Share of total children's services\n spend minus CLA (%)") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Share of total children's services spend minus CLA (%) by local authority") +
      scale_y_continuous(limits = c(0, max_y_lim))
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$spend_excl_cla_la_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    data <- spending_data_no_cla %>%
      filter(geographic_level == "Local authority", time_period == max(spending_data_no_cla$time_period)) %>%
      select(time_period, geo_breakdown, minus_cla_share) %>%
      arrange(desc(minus_cla_share)) %>%
      rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Share minus CLA (%)" = "minus_cla_share")

    reactable(
      data,
      columns = list(
        `Share minus CLA (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  ## Ofsted leadership rating ----
  output$ofsted_la_headline <- renderText({
    data <- ofsted_leadership_data_long %>%
      filter(geo_breakdown == input$geographic_breakdown_e2) %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      ))

    if (input$geographic_breakdown_e2 == "" || nrow(data) == 0) {
      return("NA")
    } else {
      paste0(data$Rating[which.max(data$Count)], "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
    }
  })


  output$ofsted_outstanding_headline <- renderText({
    data <- ofsted_leadership_data_long %>%
      # filter(geo_breakdown == input$geographic_breakdown_e2) %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      ))

    if (input$geographic_breakdown_e2 == "") {
      paste0("NA")
    } else {
      if (input$geographic_breakdown_e2 %in% c("Inner London", "Outer London", "London")) {
        stat <- data %>%
          filter(geo_breakdown == "London")
        stat_final <- stat$Count[which(stat$Rating == "Outstanding")]
        paste0(stat_final)
      } else if (input$geographic_breakdown_e2 %in% c("North East", "Yorkshire and The Humber")) {
        stat <- data %>%
          filter(geo_breakdown == "North East, Yorkshire and the Humber")
        stat_final <- stat$Count[which(stat$Rating == "Outstanding")]
        paste0(stat_final)
      } else {
        stat <- data %>%
          filter(geo_breakdown == input$geographic_breakdown_e2)
        stat_final <- stat$Count[which(stat$Rating == "Outstanding")]
        paste0(stat_final)
      }
    }
  })

  output$ofsted_good_headline <- renderText({
    data <- ofsted_leadership_data_long %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      ))

    if (input$geographic_breakdown_e2 == "") {
      paste0("NA")
    } else {
      if (input$geographic_breakdown_e2 %in% c("Inner London", "Outer London", "London")) {
        stat <- data %>%
          filter(geo_breakdown == "London")
        stat_final <- stat$Count[which(stat$Rating == "Good")]
        paste0(stat_final)
      } else if (input$geographic_breakdown_e2 %in% c("North East", "Yorkshire and The Humber")) {
        stat <- data %>%
          filter(geo_breakdown == "North East, Yorkshire and the Humber")
        stat_final <- stat$Count[which(stat$Rating == "Good")]
        paste0(stat_final)
      } else {
        stat <- data %>%
          filter(geo_breakdown == input$geographic_breakdown_e2)
        stat_final <- stat$Count[which(stat$Rating == "Good")]
        paste0(stat_final)
      }
    }
  })

  output$ofsted_improvement_headline <- renderText({
    data <- ofsted_leadership_data_long %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      ))

    if (input$geographic_breakdown_e2 == "") {
      paste0("NA")
    } else {
      if (input$geographic_breakdown_e2 %in% c("Inner London", "Outer London", "London")) {
        stat <- data %>%
          filter(geo_breakdown == "London")
        stat_final <- stat$Count[which(stat$Rating == "Requires Improvement")]
        paste0(stat_final)
      } else if (input$geographic_breakdown_e2 %in% c("North East", "Yorkshire and The Humber")) {
        stat <- data %>%
          filter(geo_breakdown == "North East, Yorkshire and the Humber")
        stat_final <- stat$Count[which(stat$Rating == "Requires Improvement")]
        paste0(stat_final)
      } else {
        stat <- data %>%
          filter(geo_breakdown == input$geographic_breakdown_e2)
        stat_final <- stat$Count[which(stat$Rating == "Requires Improvement")]
        paste0(stat_final)
      }
    }
  })

  output$ofsted_inadequate_headline <- renderText({
    data <- ofsted_leadership_data_long %>%
      # filter(geo_breakdown == input$geographic_breakdown_e2) %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      ))

    if (input$geographic_breakdown_e2 == "") {
      paste0("NA")
    } else {
      if (input$geographic_breakdown_e2 %in% c("Inner London", "Outer London", "London")) {
        stat <- data %>%
          filter(geo_breakdown == "London")
        stat_final <- stat$Count[which(stat$Rating == "Inadequate")]
        paste0(stat_final)
      } else if (input$geographic_breakdown_e2 %in% c("North East", "Yorkshire and The Humber")) {
        stat <- data %>%
          filter(geo_breakdown == "North East, Yorkshire and the Humber")
        stat_final <- stat$Count[which(stat$Rating == "Inadequate")]
        paste0(stat_final)
      } else {
        stat <- data %>%
          filter(geo_breakdown == input$geographic_breakdown_e2)
        stat_final <- stat$Count[which(stat$Rating == "Inadequate")]
        paste0(stat_final)
      }
      # paste0(data$Count[which(data$Rating == "Inadequate")])
    }
  })

  output$ofsted_latest_ratings_tbl <- renderReactable({
    filtered_data <- ofsted_leadership_data %>%
      filter(is.na(region) == FALSE) %>%
      select(geo_breakdown, time_period) %>%
      arrange(`geo_breakdown`) %>%
      rename(`Latest Rating` = `time_period`, `Location` = `geo_breakdown`)

    reactable(
      filtered_data,
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  output$plot_ofsted <- plotly::renderPlotly({
    p <- plot_ofsted() %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Ofsted – The impact of leaders on social work practice with children and families nationally")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$plot_ofsted_reg <- plotly::renderPlotly({
    p <- plot_ofsted_reg() %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Ofsted – The impact of leaders on social work practice with children and families by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$ofsted_tbl <- renderReactable({
    filtered_data <- ofsted_leadership_data_long %>%
      filter(geographic_level == "National", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, Rating, Count) %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      )) %>%
      arrange(desc(`Count`)) %>%
      rename(`Latest Publication` = `time_period`, `Breakdown` = `geo_breakdown`, `Ofsted leadership rating` = `Rating`)

    reactable(
      filtered_data,
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  output$ofsted_reg_tbl <- renderReactable({
    filtered_data <- ofsted_leadership_data_long %>%
      filter(geographic_level == "Regional", time_period == max(time_period)) %>%
      select(time_period, geographic_level, geo_breakdown, Rating, Count) %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      )) %>%
      arrange(desc(`Count`)) %>%
      rename(`Latest Publication` = `time_period`, `Geographic Level` = `geographic_level`, `Breakdown` = `geo_breakdown`, `Ofsted leadership rating` = `Rating`)

    reactable(
      filtered_data,
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # Outcome 1 -----
  # Geographic breakdown o1 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_o1
  }, {
    choices <- sort(unique(cla_rates[(cla_rates$geographic_level == input$select_geography_o1 & cla_rates$time_period == 2023), "geo_breakdown"]), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_o1",
      selected = choices[1],
      choices = choices,
    )
  })
  # outcome 1 confirmation text

  region_for_la_o1 <- reactive({
    selected_la <- input$geographic_breakdown_o1
    location_data %>%
      filter(la_name == selected_la) %>%
      pull(region_name)
  })

  output$outcome1_choice_text1 <- renderText({
    if (input$select_geography_o1 == "National") {
      paste0("You have selected ", tags$b(input$select_geography_o1), " level statistics on ", tags$b("England"), ".")
    } else if (input$select_geography_o1 == "Regional") {
      paste0("You have selected ", tags$b(input$select_geography_o1), " level statistics for ", tags$b(input$geographic_breakdown_o1), ".")
    } else if (input$select_geography_o1 == "Local authority") {
      paste0("You have selected ", tags$b(input$select_geography_o1), " level statistics for ", tags$b(input$geographic_breakdown_o1), ", in ", region_for_la_o1(), ".")
    }
  })

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

  output$outcome1_choice_text2 <- renderText({
    # Checking to see if they picked national average comparison
    if (!is.null(input$national_comparison_checkbox) && is.null(input$region_comparison_checkbox)) {
      paste0("You have also selected to compare with the ", tags$b("National Average."))
      # If they picked regional comparison
    } else if (is.null(input$national_comparison_checkbox) && !is.null(input$region_comparison_checkbox)) {
      paste0("You have also selected to compare with the ", tags$b("Regional average."))
      # Picked both national and regional comparison
    } else if (!is.null(input$national_comparison_checkbox) && !is.null(input$region_comparison_checkbox)) {
      paste0("You have also selected to compare with the ", tags$b("National average"), " and the ", tags$b("Regional average."))
    }
  })

  output$outcome1_choice_social_care_group_text <- renderText({
    paste0("Percentage of overall absence, total of authorised and unauthorised absence, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_text_1 <- renderText({
    paste0("Percentage of persistent absentees, pupils with overall absence at 10% or more, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_text_2 <- renderText({
    paste0("Percentage of pupils achieving expected standard in reading, writing and maths combined for ", tags$b(input$attainment_extra_breakdown), ".")
  })

  output$outcome1_choice_social_care_group_text_3 <- renderText({
    paste0("Average achievement of pupils in up to 8 qualifications, including English language, English literature and maths, for ", tags$b(input$attainment_extra_breakdown), ".")
  })


  outcomes_time_period_max <- outcomes_absence %>%
    filter(time_period == max(outcomes_absence$time_period)) %>%
    filter(geographic_level == "National", school_type == "Total", social_care_group == "CINO at 31 March") %>%
    mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
    pull(time_period)



  output$outcome1_time_period_text <- renderText({
    paste0("The charts below represent data from ", outcomes_time_period_max, ", for ", input$wellbeing_extra_breakdown, " and ", input$wellbeing_school_breakdown, " school type.")
  })

  output$outcome1_time_period_text_2 <- renderText({
    paste0("The charts below represent data from ", outcomes_time_period_max, ", for ", input$wellbeing_extra_breakdown, " and ", input$wellbeing_school_breakdown, " school type.")
  })

  output$outcome1_time_period_text_3 <- renderText({
    paste0("The charts below represent data from ", outcomes_time_period_max, ", for ", input$attainment_extra_breakdown, ".")
  })

  output$outcome1_time_period_text_4 <- renderText({
    paste0("The charts below represent data from ", outcomes_time_period_max, ", for ", input$attainment_extra_breakdown, ".")
  })



  ## CLA rate headline ----
  output$cla_rate_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(cla_rates %>% filter(time_period == max(cla_rates$time_period) &
        geo_breakdown %in% input$geographic_breakdown_o1 &
        population_count == "Children starting to be looked after each year") %>% select(rate_per_10000), nsmall = 0)
    }
    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cla_rates$time_period), ")", "</p>")
  })

  # UASC rate headline
  output$uasc_rate_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(combined_cla_data %>% filter(time_period == max(combined_cla_data$time_period) &
        geo_breakdown %in% input$geographic_breakdown_o1 &
        population_count == "Children starting to be looked after each year" &
        characteristic == "Unaccompanied asylum-seeking children") %>% select(placement_per_10000), nsmall = 0)
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(combined_cla_data$time_period), ")", "</p>")
  })

  # CLA March rate headline
  output$cla_march_rate_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(cla_rates %>% filter(time_period == max(cla_rates$time_period) &
        geo_breakdown %in% input$geographic_breakdown_o1 &
        population_count == "Children looked after at 31 March each year") %>% select(rate_per_10000), nsmall = 0)
    }
    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cla_rates$time_period), ")", "</p>")
  })

  # CLA rate Plot
  output$plot_cla_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National"))
    }

    filtered_data <- filtered_data %>%
      filter(population_count == "Children starting to be looked after each year") %>%
      rename("Rate per 10,000" = "Rate Per 10000")

    # Set the max y-axis scale
    max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Rate per 10,000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000")


    ggplotly(p, height = 420, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })

  # CLA rate TABLE
  output$table_cla_rate <- renderReactable({ # renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`, population_count)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`, population_count)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`, population_count)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`, population_count)
    }

    data <- filtered_data %>%
      filter(population_count == "Children starting to be looked after each year") %>%
      select(time_period, geo_breakdown, number, `Rate Per 10000`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Number of children starting to be looked after` = `number`, `Rate of children starting to be looked after, per 10,000` = `Rate Per 10000`)

    reactable(
      data,
      columns = list(
        `Number of children starting to be looked after` = colDef(cell = cellfunc),
        `Rate of children starting to be looked after, per 10,000` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # CLA rate regional plot
  output$plot_cla_rate_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cla_rate_reg() %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # CLA rate regional table
  output$table_cla_rate_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- cla_rates %>%
      filter(geographic_level == "Regional", time_period == max(cla_rates$time_period), population_count == "Children starting to be looked after each year") %>%
      select(time_period, geo_breakdown, number, "Rate Per 10000") %>%
      arrange(desc(`Rate Per 10000`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Number of children starting to be looked after` = `number`, `Rate of children starting to be looked after, per 10,000` = `Rate Per 10000`)

    reactable(
      data,
      columns = list(
        `Number of children starting to be looked after` = colDef(cell = cellfunc),
        `Rate of children starting to be looked after, per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # CLA rate LA plot
  output$plot_cla_rate_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cla_rate_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 by local authority")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # CLA rate La table
  output$table_cla_rate_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }

      data <- cla_rates %>%
        filter(geo_breakdown %in% location, time_period == max(time_period), population_count == "Children starting to be looked after each year") %>%
        select(time_period, geo_breakdown, Number, `Rate Per 10000`) %>%
        arrange(desc(`Rate Per 10000`))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cla_rates %>%
        filter(geographic_level == "Local authority", time_period == max(cla_rates$time_period), population_count == "Children starting to be looked after each year") %>%
        select(
          time_period, geo_breakdown, Number, `Rate Per 10000`
        ) %>%
        arrange(desc(`Rate Per 10000`))
    }

    data2 <- data %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Number of children starting to be looked after` = `Number`, `Rate of children starting to be looked after, per 10,000` = `Rate Per 10000`)

    reactable(
      data2,
      columns = list(
        `Number of children starting to be looked after` = colDef(cell = cellfunc),
        `Rate of children starting to be looked after, per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## UASC ------
  # UASC chart
  output$plot_uasc <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_uasc(input$geographic_breakdown_o1, input$select_geography_o1) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 with UASC breakdown")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # UASC table
  output$table_uasc <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- combined_cla_data %>%
      filter(
        geo_breakdown %in% input$geographic_breakdown_o1,
        characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children"),
        population_count == "Children starting to be looked after each year"
      ) %>%
      select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
      arrange(desc(time_period)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Number of children starting to be looked after` = `placements_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)

    reactable(
      data,
      columns = list(
        `Number of children starting to be looked after` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # UASC chart by region
  output$plot_uasc_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_uasc_reg() %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 with UASC breakdown by region")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # UASC table by region
  output$table_uasc_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- combined_cla_data %>%
      filter(
        geographic_level == "Regional", characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children"),
        population_count == "Children starting to be looked after each year",
        time_period == max(time_period)
      ) %>%
      select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `UASC status` = `characteristic`, `Number of children starting to be looked after` = `placements_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)

    reactable(
      data,
      columns = list(
        `Number of children starting to be looked after` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # UASC plot by LA
  output$plot_uasc_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_uasc_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 with UASC breakdown by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # UASC table by LA
  output$table_uasc_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }

      data <- combined_cla_data %>%
        filter(
          geo_breakdown %in% location, time_period == max(combined_cla_data$time_period), characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children"),
          population_count == "Children starting to be looked after each year",
        ) %>%
        select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
        arrange(desc(`Placement Rate Per 10000`)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `UASC status` = `characteristic`, `Number of children starting to be looked after` = `placements_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- combined_cla_data %>%
        filter(
          geographic_level == "Local authority", time_period == max(combined_cla_data$time_period), characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children"),
          population_count == "Children starting to be looked after each year",
        ) %>%
        select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
        arrange(desc(`Placement Rate Per 10000`)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `UASC status` = `characteristic`, `Number of children starting to be looked after` = `placements_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)
    }

    reactable(
      data,
      columns = list(
        `Number of children starting to be looked after` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## CLA Rate chart for March ----
  output$plot_cla_rate_march <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National"))
    }

    filtered_data <- filtered_data %>%
      filter(population_count == "Children looked after at 31 March each year") %>%
      rename("Rate per 10,000" = "Rate Per 10000")

    # Set the max y-axis scale
    max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children looked after at 31 March each year"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Rate per 10,000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 on 31 March")


    ggplotly(p, height = 420, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })

  # CLA rate march TABLE
  output$table_cla_rate_march <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`, population_count)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`, population_count)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`, population_count)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`, population_count)
    }

    data <- filtered_data %>%
      filter(population_count == "Children looked after at 31 March each year") %>%
      select(time_period, geo_breakdown, number, `Rate Per 10000`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Number of children looked after on 31 March` = `number`, `Rate per 10,000 children` = `Rate Per 10000`)


    reactable(
      data,
      columns = list(
        `Number of children looked after on 31 March` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # CLA rate March regional plot
  output$plot_cla_march_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cla_march_reg() %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 on 31 March by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # CLA rate March regional table
  output$table_cla_march_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    data <- cla_rates %>%
      filter(geographic_level == "Regional", time_period == max(cla_rates$time_period), population_count == "Children looked after at 31 March each year") %>%
      select(time_period, geo_breakdown, number, `Rate Per 10000`) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Number of children looked after on 31 March` = `number`, `Rate per 10,000 children` = `Rate Per 10000`)

    reactable(
      data,
      columns = list(
        `Number of children looked after on 31 March` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # CLA rate March LA plot
  output$plot_cla_march_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cla_march_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 on 31 March by local authority")
    ggplotly(,
      height = 420,
      tooltip = "text"
    )
  })

  # CLA rate March La table
  output$table_cla_march_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }

      data <- cla_rates %>%
        filter(geo_breakdown %in% location, time_period == max(time_period), population_count == "Children looked after at 31 March each year") %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`) %>%
        arrange(desc(`Rate Per 10000`)) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Number of children looked after on 31 March` = `number`, `Rate per 10,000 children` = `Rate Per 10000`)
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cla_rates %>%
        filter(geographic_level == "Local authority", time_period == max(cla_rates$time_period), population_count == "Children looked after at 31 March each year") %>%
        select(
          time_period, geo_breakdown,
          number, `Rate Per 10000`
        ) %>%
        arrange(desc(`Rate Per 10000`)) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Number of children looked after on 31 March` = `number`, `Rate per 10,000 children` = `Rate Per 10000`)
    }

    reactable(
      data,
      columns = list(
        `Number of children looked after on 31 March` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## CIN rate headline ----
  output$cin_rate_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(cin_rates %>% filter(time_period == max(cin_rates$time_period) & geo_breakdown %in% input$geographic_breakdown_o1)
        %>% select(At31_episodes_rate), nsmall = 1)
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cin_rates$time_period), ")", "</p>")
  })

  # CIN rate plot
  output$plot_cin_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_rates %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) %>%
        rename("CIN rate per 10,000" = CIN_rate)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        rename("CIN rate per 10,000" = CIN_rate)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        rename("CIN rate per 10,000" = CIN_rate)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        rename("CIN rate per 10,000" = CIN_rate)
    }


    # Set the max y-axis scale
    max_rate <- max(cin_rates$CIN_rate, na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "CIN rate per 10,000", "CIN rate per 10,000 Children", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CIN rate per 10,000 children")

    ggplotly(p, height = 420, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })

  # CIN rate table
  output$table_cin_rate <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_rates %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, At31_episodes, CIN_rate) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CIN number at 31 March` = `At31_episodes`, `CIN rates per 10,000` = `CIN_rate`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, At31_episodes, CIN_rate) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CIN number at 31 March` = `At31_episodes`, `CIN rates per 10,000` = `CIN_rate`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, At31_episodes, CIN_rate) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CIN number at 31 March` = `At31_episodes`, `CIN rates per 10,000` = `CIN_rate`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, At31_episodes, CIN_rate) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CIN number at 31 March` = `At31_episodes`, `CIN rates per 10,000` = `CIN_rate`)
    }

    reactable(
      filtered_data,
      columns = list(
        `CIN number at 31 March` = colDef(cell = cellfunc),
        `CIN rates per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # cin rate plot by region
  output$plot_cin_rate_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cin_rate_reg() %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CIN rate per 10,000 children by region")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })


  # cin rate table by region
  output$table_cin_rates_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- cin_rates %>%
      filter(geographic_level == "Regional", time_period == max(cin_rates$time_period)) %>%
      select(
        time_period, geo_breakdown, At31_episodes, CIN_rate
      ) %>%
      arrange(desc(CIN_rate)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `CIN number at 31 March` = `At31_episodes`, `CIN rate per 10,000` = `CIN_rate`)

    reactable(
      data,
      columns = list(
        `CIN number at 31 March` = colDef(cell = cellfunc),
        `CIN rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # cin rate table by LA
  output$table_cin_rates_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }

      data <- cin_rates %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, At31_episodes, CIN_rate) %>%
        arrange(desc(CIN_rate)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `CIN number at 31 March` = `At31_episodes`, `CIN rates per 10,000` = `CIN_rate`)
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cin_rates %>%
        filter(geographic_level == "Local authority", time_period == max(cin_rates$time_period)) %>%
        select(
          time_period, geo_breakdown, At31_episodes, CIN_rate
        ) %>%
        arrange(desc(CIN_rate)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `CIN number at 31 March` = `At31_episodes`, `CIN rates per 10,000` = `CIN_rate`)
    }
    reactable(
      data,
      columns = list(
        `CIN number at 31 March` = colDef(cell = cellfunc),
        `CIN rates per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # cin rate chart by LA
  output$plot_cin_rates_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cin_rates_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CIN rate per 10,000 children by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  ## CIN referral headline -----
  output$cin_referral_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(cin_referrals %>% filter(time_period == max(cin_referrals$time_period) & geo_breakdown %in% input$geographic_breakdown_o1)
        %>% select(Re_referrals_percent), nsmall = 1)
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cin_referrals$time_period), ")", "</p>")
  })

  ## CIN referral plot
  output$plot_cin_referral <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_referrals %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) # %>%
      # rename("Re_referrals_percentage" = "Re-referrals (%)")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_referrals %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") # %>%
      # rename("Re_referrals_percentage" = "Re-referrals (%)")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) # %>%
      # rename("Re_referrals_percentage" = "Re-referrals (%)")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) # %>%
      # rename("Re_referrals_percentage" = "Re-referrals (%)")
    }
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Re-referrals (%)", "Re-referrals (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Re-referrals within 12 months %")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # CIN referral table
  output$table_cin_referral <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_referrals %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, `Re-referrals (%)`) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Referrals in the year` = `Referrals`, `Re-referrals within 12 months of a previous referral` = `Re_referrals`, `Re-referrals within 12 months (%)` = `Re-referrals (%)`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_referrals %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, `Re-referrals (%)`) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Referrals in the year` = `Referrals`, `Re-referrals within 12 months of a previous referral` = `Re_referrals`, `Re-referrals within 12 months (%)` = `Re-referrals (%)`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, `Re-referrals (%)`) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Referrals in the year` = `Referrals`, `Re-referrals within 12 months of a previous referral` = `Re_referrals`, `Re-referrals within 12 months (%)` = `Re-referrals (%)`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, `Re-referrals (%)`) %>%
        rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Referrals in the year` = `Referrals`, `Re-referrals within 12 months of a previous referral` = `Re_referrals`, `Re-referrals within 12 months (%)` = `Re-referrals (%)`)
    }

    reactable(
      filtered_data,
      columns = list(
        `Referrals in the year` = colDef(cell = cellfunc),
        `Re-referrals within 12 months of a previous referral` = colDef(cell = cellfunc),
        `Re-referrals within 12 months (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # cin referral table by region
  output$table_cin_referral_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- cin_referrals %>%
      filter(geographic_level == "Regional", time_period == max(cin_referrals$time_period)) %>%
      select(
        time_period, geo_breakdown, Referrals, Re_referrals, `Re-referrals (%)`
      ) %>%
      arrange(desc(`Re-referrals (%)`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Referrals in the year` = `Referrals`, `Re-referrals within 12 months of a previous referral` = `Re_referrals`, `Re-referrals within 12 months (%)` = `Re-referrals (%)`)

    reactable(
      data,
      columns = list(
        `Referrals in the year` = colDef(cell = cellfunc),
        `Re-referrals within 12 months of a previous referral` = colDef(cell = cellfunc),
        `Re-referrals within 12 months (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # cin referral table by LA
  output$table_cin_referral_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }

      data <- cin_referrals %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, `Re-referrals (%)`) %>%
        arrange(desc(`Re-referrals (%)`)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Referrals in the year` = `Referrals`, `Re-referrals within 12 months of a previous referral` = `Re_referrals`, `Re-referrals within 12 months (%)` = `Re-referrals (%)`)
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cin_referrals %>%
        filter(geographic_level == "Local authority", time_period == max(cin_referrals$time_period)) %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, `Re-referrals (%)`) %>%
        arrange(desc(`Re-referrals (%)`)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Referrals in the year` = `Referrals`, `Re-referrals within 12 months of a previous referral` = `Re_referrals`, `Re-referrals within 12 months (%)` = `Re-referrals (%)`)
    }

    reactable(
      data,
      columns = list(
        `Referrals in the year` = colDef(cell = cellfunc),
        `Re-referrals within 12 months of a previous referral` = colDef(cell = cellfunc),
        `Re-referrals within 12 months (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # cin referral plot by region
  output$plot_cin_referral_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cin_referral_reg() %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Re-referrals within 12 months % by region")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # cin referral chart by LA
  output$plot_cin_referral_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cin_referral_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Re-referrals within 12 months % by local authority")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Child wellbeing & development
  # overall absence headline ----

  # formatted time period
  formatted_time_period_wellbeing <- outcomes_absence %>%
    filter(time_period == max(time_period), geo_breakdown == "National", social_care_group == "CINO at 31 March", school_type == "Total") %>%
    mutate(time_period_new = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


  # CIN
  output$absence_CIN_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March", school_type == "Total")
        %>% select(pt_overall), nsmall = 1)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # CPPO
  output$absence_CPP_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March", school_type == "Total")
        %>% select(pt_overall), nsmall = 1)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # CLA
  output$absence_CLA_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March", school_type == "Total")
        %>% select(pt_overall), nsmall = 1)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })


  # overall absence timeseries chart
  output$absence_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) %>%
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Overall absence (%)", "Overall absence (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Overall absence rate %")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })


  # absence rate TABLE
  output$table_absence_rate <- renderReactable({
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `pt_overall`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `pt_overall`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `pt_overall`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `pt_overall`)
    }

    filtered_data2 <- filtered_data %>%
      filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `pt_overall`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Social care group` = `social_care_group`, `School Type` = `school_type`, `Total number of pupils` = `Total pupils`, `Overall absence (%)` = `pt_overall`)

    reactable(
      filtered_data2,
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Overall absence (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # Absence rate regional plot
  output$plot_absence_reg <- plotly::renderPlotly({
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    p <- by_region_bar_plot(data, "Overall absence (%)", "Overall absence (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Overall absence rate (%) by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Absence rate regional table
  output$table_absence_reg <- renderReactable({
    data <- outcomes_absence %>%
      filter(geographic_level == "Regional" & time_period == max(outcomes_absence$time_period) & school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Overall absence (%)`) %>%
      arrange(desc(`Overall absence (%)`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Social care group` = `social_care_group`, `School type` = `school_type`, `Total number of pupils` = `Total pupils`, `Overall absence (%)` = `Overall absence (%)`)

    reactable(
      data,
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Overall absence (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # absence by la
  output$plot_absence_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown)
    p <- by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Overall absence (%)", "Overall absence (%)") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Overall absence rate (%) by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Absence by LA table
  output$table_absence_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }

      data <- outcomes_absence %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Overall absence (%)`
        ) %>%
        arrange(desc(`Overall absence (%)`))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_absence %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Overall absence (%)`
        ) %>%
        arrange(desc(`Overall absence (%)`))
    }

    data2 <- data %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Social care group` = `social_care_group`, `School type` = `school_type`, `Total number of pupils` = `Total pupils`, `Overall absence (%)` = `Overall absence (%)`)

    reactable(
      data2,
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Overall absence (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # persistent absentees headline ----
  # CIN
  output$persistent_CIN_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March", school_type == "Total")
        %>% select(pt_pupils_pa_10_exact), nsmall = 1)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # CPPO
  output$persistent_CPP_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March", school_type == "Total")
        %>% select(pt_pupils_pa_10_exact), nsmall = 1)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # CLA
  output$persistent_CLA_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March", school_type == "Total")
        %>% select(pt_pupils_pa_10_exact), nsmall = 1)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })


  # persistent absence timeseries chart
  output$persistence_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) %>%
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
    }
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Persistent absentees (%)", "Persistent absentees (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Persistent absentees (%)")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })


  # persistent rate TABLE
  output$table_persistent_rate <- renderReactable({
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Persistent absentees (%)`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Persistent absentees (%)`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Persistent absentees (%)`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Persistent absentees (%)`)
    }

    data <- filtered_data %>%
      filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Persistent absentees (%)`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Social care group` = `social_care_group`, `School type` = `school_type`, `Total number of pupils` = `Total pupils`, `Persistent absentees (%)` = `Persistent absentees (%)`)

    reactable(
      data,
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Persistent absentees (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # Persistence absence regional plot
  output$plot_persistent_reg <- plotly::renderPlotly({
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    p <- by_region_bar_plot(data, "Persistent absentees (%)", "Persistent absentees (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Persistent absentees (%) by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Persistence Absence regional table
  output$table_persistent_reg <- renderReactable({
    data <- outcomes_absence %>%
      filter(
        geographic_level == "Regional", time_period == max(outcomes_absence$time_period),
        school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown
      ) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Persistent absentees (%)`) %>%
      arrange(desc(`Persistent absentees (%)`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Social care group` = `social_care_group`, `School type` = `school_type`, `Total number of pupils` = `Total pupils`, `Persistent absentees (%)` = `Persistent absentees (%)`)

    reactable(
      data,
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Persistent absentees (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })



  # persistent absence by la
  output$plot_persistent_absence_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown)
    p <- by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Persistent absentees (%)", "Persistent absentees (%)") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Persistent absentees (%) by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Persistent Absence by LA table
  output$table_persistent_absence_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }

      data <- outcomes_absence %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Persistent absentees (%)`
        ) %>%
        arrange(desc(`Persistent absentees (%)`))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_absence %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Persistent absentees (%)`
        ) %>%
        arrange(desc(`Persistent absentees (%)`))
    }

    data2 <- data %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Social care group` = `social_care_group`, `School type` = `school_type`, `Total number of pupils` = `Total pupils`, `Persistent absentees (%)` = `Persistent absentees (%)`)

    reactable(
      data2,
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Persistent absentees (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # Education attainment

  # KS2 headline ----

  # formatted time period
  formatted_time_period <- outcomes_ks2 %>%
    filter(time_period == max(time_period), geo_breakdown == "National", social_care_group == "CINO at 31 March") %>%
    mutate(time_period_new = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

  # CIN
  output$KS2_CIN_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_ks2 %>% filter(time_period == max(outcomes_ks2$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March")
        %>% select(pt_rwm_met_expected_standard), nsmall = 1)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CPPO
  output$KS2_CPP_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_ks2 %>% filter(time_period == max(outcomes_ks2$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March")
        %>% select(pt_rwm_met_expected_standard), nsmall = 1)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CLA
  output$KS2_CLA_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_ks2 %>% filter(time_period == max(outcomes_ks2$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March")
        %>% select(pt_rwm_met_expected_standard), nsmall = 1)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # KS4 headline ----
  # CIN
  output$KS4_CIN_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_ks4 %>% filter(time_period == max(outcomes_ks4$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March")
        %>% select(avg_att8), nsmall = 1)
    }
    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CPPO
  output$KS4_CPP_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_ks4 %>% filter(time_period == max(outcomes_ks4$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March")
        %>% select(avg_att8), nsmall = 1)
    }
    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CLA
  output$KS4_CLA_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(outcomes_ks4 %>% filter(time_period == max(outcomes_ks4$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March")
        %>% select(avg_att8), nsmall = 1)
    }
    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  ## KS2 attainment -----
  # KS2 % expected plot
  output$plot_ks2_expected <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks2 %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)
      #  mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks2 %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National")
      # mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks2 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)))
      # mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks2 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National"))
      #  mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
    }

    filtered_data <- filtered_data %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Expected standard reading writing maths (%)", "Expected standard combined (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage meeting combined expected standard (KS2)")


    ggplotly(p, height = 420, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, 100)))
  })


  # ks2 TABLE
  output$table_ks2_expected <- renderReactable({
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks2 %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, `Expected standard reading writing maths (%)`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks2 %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, `Expected standard reading writing maths (%)`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks2 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, `Expected standard reading writing maths (%)`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks2 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, `Expected standard reading writing maths (%)`)
    }

    data <- filtered_data %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, `Expected standard reading writing maths (%)`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Social care group` = `social_care_group`, `Total number of eligible pupils` = `t_rwm_eligible_pupils`, `Expected standard reading writing maths (%)` = `Expected standard reading writing maths (%)`)

    reactable(
      data,
      columns = list(
        `Total number of eligible pupils` = colDef(cell = cellfunc),
        `Expected standard reading writing maths (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # KS2 regional plot
  output$plot_ks2_reg <- plotly::renderPlotly({
    data <- outcomes_ks2 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    p <- by_region_bar_plot(data, "Expected standard reading writing maths (%)", "Expected standard combined (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage meeting combined expected standard (KS2) by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # KS2 regional table
  output$table_ks2_reg <- renderReactable({
    data <- outcomes_ks2 %>%
      filter(
        geographic_level == "Regional", time_period == max(outcomes_ks2$time_period),
        social_care_group %in% input$attainment_extra_breakdown
      ) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, `Expected standard reading writing maths (%)`) %>%
      arrange(desc(`Expected standard reading writing maths (%)`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Social care group` = `social_care_group`, `Total number of eligible pupils` = `t_rwm_eligible_pupils`, `Expected standard reading writing maths (%)` = `Expected standard reading writing maths (%)`)


    reactable(
      data,
      columns = list(
        `Total number of eligible pupils` = colDef(cell = cellfunc),
        `Expected standard reading writing maths (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # KS2 by la
  output$plot_KS2_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_ks2 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown)

    p <- by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Expected standard reading writing maths (%)", "Expected standard combined (%)") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage meeting combined expected standard (KS2) by local authority")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # KS2 by LA table
  output$table_KS2_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }

      data <- outcomes_ks2 %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(social_care_group %in% input$attainment_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group,
          t_rwm_eligible_pupils, `Expected standard reading writing maths (%)`
        ) %>%
        arrange(desc(`Expected standard reading writing maths (%)`))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_ks2 %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(social_care_group %in% input$attainment_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown,
          social_care_group, t_rwm_eligible_pupils, `Expected standard reading writing maths (%)`
        ) %>%
        arrange(desc(`Expected standard reading writing maths (%)`))
    }
    data2 <- data %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Social care group` = `social_care_group`, `Total number of eligible pupils` = `t_rwm_eligible_pupils`, `Expected standard reading writing maths (%)` = `Expected standard reading writing maths (%)`)

    reactable(
      data2,
      columns = list(
        `Total number of eligible pupils` = colDef(cell = cellfunc),
        `Expected standard reading writing maths (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## KS4 attainment -----
  # KS4 % expected plot
  output$plot_ks4 <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks4 %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)
      #  mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks4 %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National")
      # mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks4 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)))
      # mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks4 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National"))
      #  mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
    }

    filtered_data <- filtered_data %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_ks4$`Average Attainment 8`, na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 10) * 10


    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Average Attainment 8", "Average Attainment 8 score", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average attainment 8 score (KS4)")


    ggplotly(p, height = 420, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })


  # KS4 rate TABLE
  output$table_ks4 <- renderReactable({
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks4 %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, social_care_group, t_pupils, avg_att8)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks4 %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, social_care_group, t_pupils, avg_att8)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks4 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, social_care_group, t_pupils, avg_att8)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks4 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, social_care_group, t_pupils, avg_att8)
    }
    data <- filtered_data %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, t_pupils, avg_att8) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Social care group" = "social_care_group", "Total number of pupils" = "t_pupils", "Average attainment 8 score" = "avg_att8")

    reactable(
      data,
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Average attainment 8 score` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # KS4 regional plot
  output$plot_ks4_reg <- plotly::renderPlotly({
    data <- outcomes_ks4 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    p <- by_region_bar_plot(data, "Average Attainment 8", "Average Attainment 8", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average attainment 8 score (KS4) by region")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # KS4 regional table
  output$table_ks4_reg <- renderReactable({
    data <- outcomes_ks4 %>%
      filter(geographic_level == "Regional", time_period == max(outcomes_ks4$time_period), social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, `Total pupils`, `Average Attainment 8`) %>%
      arrange(desc(`Average Attainment 8`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Social care group` = `social_care_group`, `Total number of pupils` = `Total pupils`, `Average attainment 8 score` = `Average Attainment 8`)

    reactable(
      data,
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Average attainment 8 score` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # KS4 by la
  output$plot_KS4_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_ks4 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown)
    p <- by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Average Attainment 8", "Average Attainment 8 score") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average attainment 8 score (KS4) by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # KS4 by LA table
  output$table_KS4_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }

      data <- outcomes_ks4 %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(social_care_group %in% input$attainment_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(time_period, geo_breakdown, social_care_group, `Total pupils`, `Average Attainment 8`) %>%
        arrange(desc(`Average Attainment 8`))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_ks4 %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(social_care_group %in% input$attainment_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(time_period, geo_breakdown, social_care_group, `Total pupils`, `Average Attainment 8`) %>%
        arrange(desc(`Average Attainment 8`))
    }

    data2 <- data %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Social care group` = `social_care_group`, `Total number of pupils` = `Total pupils`, `Average attainment 8 score` = `Average Attainment 8`)

    reactable(
      data2,
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Average attainment 8 score` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })



  # Outcome 2 -----
  # Geographic breakdown o2 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_o2
  }, {
    choices <- sort(unique(ceased_cla_data[(ceased_cla_data$geographic_level == input$select_geography_o2 & ceased_cla_data$time_period == max(ceased_cla_data$time_period)), "geo_breakdown"]), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_o2",
      selected = choices[1],
      choices = choices,
    )
  })

  region_for_la_o2 <- reactive({
    selected_la <- input$geographic_breakdown_o2
    location_data %>%
      filter(la_name == selected_la) %>%
      pull(region_name)
  })

  output$outcome2_choice_text1 <- renderText({
    if (input$select_geography_o2 == "National") {
      paste0("You have selected ", tags$b(input$select_geography_o2), " level statistics on ", tags$b("England"), ".")
    } else if (input$select_geography_o2 == "Regional") {
      paste0("You have selected ", tags$b(input$select_geography_o2), " level statistics for ", tags$b(input$geographic_breakdown_o2), ".")
    } else if (input$select_geography_o2 == "Local authority") {
      paste0("You have selected ", tags$b(input$select_geography_o2), " level statistics for ", tags$b(input$geographic_breakdown_o2), ", in ", region_for_la_o2(), ".")
    }
  })

  output$outcome2_choice_text2 <- renderText({
    # Checking to see if they picked national average comparison
    if (!is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      paste0("You have also selected to compare with the ", tags$b("National Average."))
      # If they picked regional comparison
    } else if (is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      paste0("You have also selected to compare with the ", tags$b("Regional average."))
      # Picked both national and regional comparison
    } else if (!is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      paste0("You have also selected to compare with the ", tags$b("National average"), " and the ", tags$b("Regional average."))
    }
  })

  ## Headline stats -----
  output$SGO_headline_txt <- renderText({
    if (input$geographic_breakdown_o2 == "") {
      stat <- "NA"
    } else {
      stat <- ceased_cla_data %>%
        filter(time_period == max(ceased_cla_data$time_period) &
          geo_breakdown %in% input$geographic_breakdown_o2 &
          cla_group == "Reason episode ceased" &
          characteristic == "Special guardianship orders") %>%
        select(percentage)
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(ceased_cla_data$time_period), ")", "</p>")
  })


  # Headline stat2
  output$CAO_headline_txt <- renderText({
    if (input$geographic_breakdown_o2 == "") {
      stat <- "NA"
    } else {
      stat <- ceased_cla_data %>%
        filter(time_period == max(ceased_cla_data$time_period) &
          geo_breakdown %in% input$geographic_breakdown_o2 &
          cla_group == "Reason episode ceased" &
          characteristic == "Residence order or child arrangement order granted") %>%
        select(percentage)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(ceased_cla_data$time_period), ")", "</p>")
  })

  # SGO ----
  # time series and table
  output$SGO_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter(geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) %>%
        filter(characteristic == "Special guardianship orders")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) | geographic_level == "National") %>%
        filter(characteristic == "Special guardianship orders")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name))) %>%
        filter(characteristic == "Special guardianship orders")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name) | geographic_level == "National")) %>%
        filter(characteristic == "Special guardianship orders")
    }
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o2, input$geographic_breakdown_o2, "Ceased (%)", "Ceased due to SGO (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to SGO")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })


  output$table_sgo_ceased <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% input$geographic_breakdown_o2)) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) | geographic_level == "National") %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name))) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name) | geographic_level == "National")) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`)
    }
    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      data,
      columns = list(
        `Number ceased` = colDef(cell = cellfunc),
        `Total ceased` = colDef(cell = cellfunc),
        `Ceased (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })
  ##
  # SGO by region -----

  output$plot_sgo_ceased_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>% filter(characteristic == "Special guardianship orders")

    p <- by_region_bar_plot(data, "Ceased (%)", "Ceased due to SGO (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to SGO by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # SGO by region table
  output$table_sgo_ceased_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )

    data <- ceased_cla_data %>%
      filter(geographic_level == "Regional", time_period == max(ceased_cla_data$time_period)) %>%
      filter(characteristic == "Special guardianship orders") %>%
      select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
      arrange(desc(`Ceased (%)`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Reason ceased" = "characteristic", "Total ceased" = "Total_num")

    reactable(
      data,
      columns = list(
        `Number ceased` = colDef(cell = cellfunc),
        `Total ceased` = colDef(cell = cellfunc),
        `Ceased (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## By SGO by LA
  output$plot_SGO_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>% filter(characteristic == "Special guardianship orders")
    p <- by_la_bar_plot(data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased due to SGO (%)") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to SGO by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # Special Guardianship orders by LA table
  output$table_sgo_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    if (input$select_geography_o2 == "Regional") {
      if (input$geographic_breakdown_o2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o2) %>%
          pull(la_name)
      }

      data <- ceased_cla_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
        arrange(desc(`Ceased (%)`))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- ceased_cla_data %>%
        filter(geographic_level == "Local authority", time_period == max(ceased_cla_data$time_period)) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
        arrange(desc(`Ceased (%)`))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
      arrange(desc(`Ceased (%)`)) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      data2,
      columns = list(
        `Number ceased` = colDef(cell = cellfunc),
        `Total ceased` = colDef(cell = cellfunc),
        `Ceased (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15, # 11 for stats neighbours, 15 for others?
      searchable = TRUE,
    )
  })



  ## CAO ----
  # time series and table
  output$CAO_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter(geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) %>%
        filter(characteristic == "Residence order or child arrangement order granted")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) | geographic_level == "National") %>%
        filter(characteristic == "Residence order or child arrangement order granted")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name))) %>%
        filter(characteristic == "Residence order or child arrangement order granted")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name) | geographic_level == "National")) %>%
        filter(characteristic == "Residence order or child arrangement order granted")
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o2, input$geographic_breakdown_o2, "Ceased (%)", "Ceased due to CAO (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to CAO")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })


  output$table_cao_ceased <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% input$geographic_breakdown_o2)) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) | geographic_level == "National") %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name))) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name) | geographic_level == "National")) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      data,
      columns = list(
        `Number ceased` = colDef(cell = cellfunc),
        `Total ceased` = colDef(cell = cellfunc),
        `Ceased (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15, # 11 for stats neighbours, 15 for others?
      searchable = TRUE,
    )
  })

  # by region
  output$plot_cao_ceased_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>% filter(characteristic == "Residence order or child arrangement order granted")

    p <- by_region_bar_plot(data, "Ceased (%)", "Ceased due to CAO (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to CAO by region")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # ceased by region table
  output$table_cao_ceased_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>%
      filter(geographic_level == "Regional", time_period == max(ceased_cla_data$time_period)) %>%
      filter(characteristic == "Residence order or child arrangement order granted") %>%
      select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
      arrange(desc(`Ceased (%)`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      data,
      columns = list(
        `Number ceased` = colDef(cell = cellfunc),
        `Total ceased` = colDef(cell = cellfunc),
        `Ceased (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15, # 11 for stats neighbours, 15 for others?
      searchable = TRUE,
    )
  })

  # by la
  output$plot_cao_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>% filter(characteristic == "Residence order or child arrangement order granted")
    p <- by_la_bar_plot(data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased due to CAO (%)") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to CAO by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # CAO by LA table
  output$table_cao_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    if (input$select_geography_o2 == "Regional") {
      if (input$geographic_breakdown_o2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o2) %>%
          pull(la_name)
      }

      data <- ceased_cla_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
        arrange(desc(`Ceased (%)`)) %>%
        rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)
    } else if (input$select_geography_o2 %in% c("Local authority", "National")) {
      data <- ceased_cla_data %>%
        filter(geographic_level == "Local authority", time_period == max(ceased_cla_data$time_period)) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
        arrange(desc(`Ceased (%)`)) %>%
        rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)
    }

    reactable(
      data,
      columns = list(
        `Number ceased` = colDef(cell = cellfunc),
        `Total ceased` = colDef(cell = cellfunc),
        `Ceased (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15, # 11 for stats neighbours, 15 for others?
      searchable = TRUE,
    )
  })


  # Outcome 3 -----
  # Geographic breakdown o3 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_o3
  }, {
    choices <- sort(unique(cla_rates[(cla_rates$geographic_level == input$select_geography_o3 & cla_rates$time_period == max(cla_rates$time_period)), "geo_breakdown"]), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_o3",
      selected = choices[1],
      choices = choices,
    )
  })

  # outcome 3 confirmation text

  region_for_la_o3 <- reactive({
    selected_la <- input$geographic_breakdown_o3
    location_data %>%
      filter(la_name == selected_la) %>%
      pull(region_name)
  })

  output$outcome3_choice_text1 <- renderText({
    if (input$select_geography_o3 == "National") {
      paste0("You have selected ", tags$b(input$select_geography_o3), " level statistics on ", tags$b("England"), ".")
    } else if (input$select_geography_o3 == "Regional") {
      paste0("You have selected ", tags$b(input$select_geography_o3), " level statistics for ", tags$b(input$geographic_breakdown_o3), ".")
    } else if (input$select_geography_o3 == "Local authority") {
      paste0("You have selected ", tags$b(input$select_geography_o3), " level statistics for ", tags$b(input$geographic_breakdown_o3), ", in ", region_for_la_o3(), ".")
    }
  })

  output$outcome3_choice_text2 <- renderText({
    # Checking to see if they picked national average comparison
    if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      paste0("You have also selected to compare with the ", tags$b("National Average."))
      # If they picked regional comparison
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      paste0("You have also selected to compare with the ", tags$b("Regional average."))
      # Picked both national and regional comparison
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      paste0("You have also selected to compare with the ", tags$b("National average"), " and the ", tags$b("Regional average."))
    }
  })

  # Child protection plan repeated during year headline box
  output$cpp_in_year_txt <- renderText({
    if (input$geographic_breakdown_o3 == "") {
      stat <- "NA"
    } else {
      stat <- format(repeat_cpp %>%
        filter(time_period == max(repeat_cpp$time_period) & geo_breakdown %in% input$geographic_breakdown_o3) %>%
        select(CPP_subsequent_percent), nsmall = 1)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(repeat_cpp$time_period), ")", "</p>"
    )
  })

  #### Repeat CPP ----
  # time series and table
  output$repeat_cpp_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- repeat_cpp %>%
        filter(geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) %>%
        rename("Repeat CPP (%)" = "Repeat_CPP_percent")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- repeat_cpp %>%
        filter((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") %>%
        rename("Repeat CPP (%)" = "Repeat_CPP_percent")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name))) %>%
        rename("Repeat CPP (%)" = "Repeat_CPP_percent")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National")) %>%
        rename("Repeat CPP (%)" = "Repeat_CPP_percent")
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "Repeat CPP (%)", "Repeat CPP (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Repeat CPP (%)")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$table_repeat_cpp <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% input$geographic_breakdown_o3)) %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "CPP Starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent", "Repeat CPP (%)" = "CPP_subsequent_percent")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- repeat_cpp %>%
        filter((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "CPP Starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent", "Repeat CPP (%)" = "CPP_subsequent_percent")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name))) %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "CPP Starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent", "Repeat CPP (%)" = "CPP_subsequent_percent")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent) %>%
        rename("Time period" = "time_period", "Location" = "geo_breakdown", "CPP Starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent", "Repeat CPP (%)" = "CPP_subsequent_percent")
    }

    reactable(
      filtered_data,
      columns = list(
        `CPP Starts` = colDef(cell = cellfunc),
        `Repeat CPP` = colDef(cell = cellfunc),
        `Repeat CPP (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # by region
  output$plot_cpp_repeat_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- repeat_cpp %>%
      rename("Repeat CPP (%)" = "Repeat_CPP_percent")

    p <- by_region_bar_plot(data, "Repeat CPP (%)", "Repeat CPP (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Repeat CPP (%) by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # cpp by region table
  output$table_cpp_repeat_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- repeat_cpp %>%
      filter(geographic_level == "Regional", time_period == max(repeat_cpp$time_period)) %>%
      select(time_period, geo_breakdown, CPP_start, CPP_subsequent, Repeat_CPP_percent) %>%
      arrange(desc(Repeat_CPP_percent)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "CPP Starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent", "Repeat CPP (%)" = "Repeat_CPP_percent")

    reactable(
      data,
      columns = list(
        `CPP Starts` = colDef(cell = cellfunc),
        `Repeat CPP` = colDef(cell = cellfunc),
        `Repeat CPP (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by la
  output$plot_cpp_repeat_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- repeat_cpp %>%
      rename("Repeat CPP (%)" = "Repeat_CPP_percent")
    p <- by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Repeat CPP (%)", "Repeat CPP (%)") %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Repeat CPP (%) by local authority")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # CPP by LA table
  output$table_cpp_repeat_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    if (input$select_geography_o3 == "Regional") {
      if (input$geographic_breakdown_o3 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o3) %>%
          pull(la_name)
      }

      data <- repeat_cpp %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, Repeat_CPP_percent) %>%
        arrange(desc(Repeat_CPP_percent)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `CPP Starts` = `CPP_start`, `Repeat CPP` = `CPP_subsequent`, `Repeat CPP (%)` = `Repeat_CPP_percent`)
    } else if (input$select_geography_o3 %in% c("Local authority", "National")) {
      data <- repeat_cpp %>%
        filter(geographic_level == "Local authority", time_period == max(repeat_cpp$time_period)) %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, Repeat_CPP_percent) %>%
        arrange(desc(Repeat_CPP_percent)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `CPP Starts` = `CPP_start`, `Repeat CPP` = `CPP_subsequent`, `Repeat CPP (%)` = `Repeat_CPP_percent`)
    }

    reactable(
      data,
      columns = list(
        `CPP Starts` = colDef(cell = cellfunc),
        `Repeat CPP` = colDef(cell = cellfunc),
        `Repeat CPP (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  #### CPP 2+ years (No LA) ----
  # Child protection plan longer than two years headline box
  output$cpp_duration_txt <- renderText({
    if (input$geographic_breakdown_o3 == "") {
      stat <- "NA"
    } else if (input$select_geography_o3 == "Local authority") {
      stat <- "NA"
    } else {
      stat <- format(duration_cpp %>%
        filter(time_period == max(duration_cpp$time_period) & geo_breakdown %in% input$geographic_breakdown_o3) %>%
        select(X2_years_or_more_percent), nsmall = 1)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(duration_cpp$time_period), ")", "</p>"
    )
  })

  # time series and table
  output$duration_cpp_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$select_geography_o3 != "Local authority", "LA data not available due to large amount of suppression."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- duration_cpp %>%
        filter(geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- duration_cpp %>%
        filter((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- duration_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- duration_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National"))
    }
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "X2_years_or_more_percent", "CPP 2+ years (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percent of CPP longer than 2 years")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$table_duration_cpp <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$select_geography_o3 != "Local authority", "LA data not available due to large amount of suppression."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- duration_cpp %>%
        filter((geo_breakdown %in% input$geographic_breakdown_o3)) %>%
        select(time_period, geo_breakdown, X2_years_or_more, X2_years_or_more_percent)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- duration_cpp %>%
        filter((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, X2_years_or_more, X2_years_or_more_percent)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- duration_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name))) %>%
        select(time_period, geo_breakdown, X2_years_or_more, X2_years_or_more_percent)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- duration_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, X2_years_or_more, X2_years_or_more_percent)
    }

    data <- filtered_data %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "CPP 2+ Years" = "X2_years_or_more", "CPP 2+ Years (%)" = "X2_years_or_more_percent")

    reactable(
      data,
      columns = list(
        `CPP 2+ Years` = colDef(cell = cellfunc),
        `CPP 2+ Years (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by region
  output$plot_cpp_duration_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- duration_cpp

    p <- by_region_bar_plot(data, "X2_years_or_more_percent", "CPP 2+ years (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percent of CPP longer than 2 years by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # by region table
  output$table_cpp_duration_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- duration_cpp %>%
      filter(geographic_level == "Regional", time_period == max(duration_cpp$time_period)) %>%
      select(time_period, geo_breakdown, X2_years_or_more, X2_years_or_more_percent) %>%
      arrange(desc(X2_years_or_more_percent)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "CPP 2+ Years" = "X2_years_or_more", "CPP 2+ Years (%)" = "X2_years_or_more_percent")

    reactable(
      data,
      columns = list(
        `CPP 2+ Years` = colDef(cell = cellfunc),
        `CPP 2+ Years (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Hospital admissions -----
  output$hosp_admissions_txt <- renderText({
    if (input$geographic_breakdown_o3 == "") {
      stat <- "NA"
    } else {
      stat <- format(hospital_admissions %>%
        filter(time_period == max(hospital_admissions$time_period) &
          geo_breakdown %in% input$geographic_breakdown_o3) %>%
        select(rate_per_10000), nsmall = 1)
    }
    paste0(format(stat, nsmall = 1), "<br>", "<p style='font-size:16px; font-weight:500;'>", "per 10,000 (", max(hospital_admissions$time_period), ")", "</p>")
  })


  output$admissions_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
    )

    data <- hospital_admissions %>%
      filter(time_period == max(hospital_admissions$time_period), geographic_level == "Regional") %>%
      rename("Rate per 10,000" = `Value`)

    max_lim <- max(data$`Rate per 10,000`) + 50

    p <- by_region_bar_plot(data, "Rate per 10,000", "Rate per 10,000", max_lim) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Hospital admissions caused by unintentional and deliberate injuries to young people (0 to 14 years), by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$admissions_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )

    data <- hospital_admissions %>%
      filter(time_period == max(hospital_admissions$time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, Value) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Rate per 10,000` = `Value`)

    reactable(
      data,
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  output$admissions_la_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )

    data <- hospital_admissions %>%
      filter(time_period == max(hospital_admissions$time_period), geographic_level == "Local authority") %>%
      rename("Rate per 10,000" = `Value`)

    national_data <- hospital_admissions %>%
      filter(geographic_level == "National") %>%
      select(time_period, geo_breakdown, Value)

    max_y_lim <- max(data$`Rate per 10,000`) + 50

    p <- by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Rate per 10,000", "Rate per 10,000") +
      scale_y_continuous(limits = c(0, max_y_lim))
    #+ geom_abline(intercept = national_data$Value, slope = 0, aes(text = paste("National rate per 10,000: ", national_data$Value)))
    # geom_hline(aes(yintercept = national_data$Value, text = paste("National rate per 10,000:",national_data$Value), colour = "#F46A25"), show.legend = FALSE)
    p <- p + ggtitle("Hospital admissions caused by unintentional and deliberate injuries to young people (0 to 14 years), by region")

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    )
  })

  output$admissions_la_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )

    data <- hospital_admissions %>%
      filter(time_period == max(hospital_admissions$time_period), geographic_level == "Local authority") %>%
      select(time_period, geo_breakdown, Value) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Rate per 10,000` = `Value`) %>%
      arrange(desc(`Rate per 10,000`))


    reactable(
      data,
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  #### child abuse titles -----
  output$ca_header1 <- renderUI({
    h2(paste(input$assessment_factors_1, " cases"))
  })
  output$ca_header2 <- renderUI({
    h2(paste(input$assessment_factors_1, " cases, by region"))
  })
  output$ca_header3 <- renderUI({
    h2(paste(input$assessment_factors_1, " cases, by local authority"))
  })

  ### Child abuse/neglect ----
  output$child_abuse_all_af_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    p <- all_assessment_factors_plot(assessment_factors, af_child_abuse_extra_filter, selected_geo_breakdown = input$geographic_breakdown_o3) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Factors identified at the end of assessment in the year to 31 March 2023 that include child abuse or neglect")
    ggplotly(
      p,
      tooltip = "text",
      height = 420
    )
  })

  # table alternative for all factors plot
  output$child_abuse_all_af_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- assessment_factors %>%
      filter(geo_breakdown == input$geographic_breakdown_o3, assessment_factor %in% (af_child_abuse_extra_filter), time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Assessment factor" = "assessment_factor") %>%
      dplyr::arrange(desc(rate_per_10000)) %>%
      rename(`Rate per 10000` = `rate_per_10000`)

    reactable(
      data,
      columns = list(
        `Rate per 10000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # time series chart
  # the time series chart, by region and la charts will need to be filtered by the extra dropdown
  output$child_abuse_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )
    if (is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3 & assessment_factor %in% input$assessment_factors_1)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_1)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name)) & assessment_factor %in% input$assessment_factors_1)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_1)
    }

    max_y_lim <- max(filtered_data$rate_per_10000) + 100

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "rate_per_10000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste(input$assessment_factors_1, "cases (rate per 10,000)")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # child abuse ts table alternative
  output$ca_ts_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )
    if (is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3 & assessment_factor %in% input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name)) & assessment_factor %in% input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)


    reactable(
      data,
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by region chart
  output$child_abuse_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )

    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_1) %>%
      # rename("rate_per_10000" = "Rate per 10,000") %>%
      filter(time_period == max(time_period), geographic_level == "Regional")

    max_lim <- max(data$rate_per_10000) + 100

    p <- by_region_bar_plot(data, "rate_per_10000", "Rate per 10,000", max_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste(input$assessment_factors_1, "cases (rate per 10,000), by region")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$child_abuse_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )

    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_1, time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      arrange(desc(rate_per_10000)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)


    reactable(
      data,
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # stats neighbours further down in the stats neighbours section
  output$plot_child_abuse_by_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )
    data <- assessment_factors %>% filter(assessment_factor == input$assessment_factors_1, geographic_level == "Local authority", time_period == max(time_period))

    max_y_lim <- max(data$rate_per_10000) + 100

    p <- by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "rate_per_10000", "Rate per 10,000") +
      scale_y_continuous(limits = c(0, max_y_lim))
    title_factor <- paste(input$assessment_factors_1, "cases (rate per 10,000), by local authority")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    )
  })

  # by la table alt
  output$table_child_ab_neg_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )
    if (input$select_geography_o3 == "Regional") {
      if (input$geographic_breakdown_o3 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o3) %>%
          pull(la_name)
      }

      data <- assessment_factors %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(assessment_factor == input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
        arrange(desc(rate_per_10000))
    } else if (input$select_geography_o3 %in% c("Local authority", "National")) {
      data <- assessment_factors %>%
        filter(geographic_level == "Local authority", time_period == max(assessment_factors$time_period)) %>%
        filter(assessment_factor == input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
        arrange(desc(rate_per_10000))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      arrange(desc(rate_per_10000)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)

    reactable(
      data2,
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  #### EFH titles -------
  output$efh_header1 <- renderUI({
    h2(paste(input$assessment_factors_2, " cases"))
  })
  output$efh_header2 <- renderUI({
    h2(paste(input$assessment_factors_2, " cases, by region"))
  })
  output$efh_header3 <- renderUI({
    h2(paste(input$assessment_factors_2, " cases, by local authority"))
  })

  ### Harms outside the home ------
  output$extra_familial_all_af_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    p <- all_assessment_factors_plot(assessment_factors, extra_familial_harm_af, selected_geo_breakdown = input$geographic_breakdown_o3) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Factors identified at the end of assessment in the year to 31 March 2023 that include extra familial harm.")
    ggplotly(
      p,
      tooltip = "text",
      height = 420
    )
  })

  # table alternative for all factors plot
  output$extra_familial_all_af_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- assessment_factors %>%
      filter(geo_breakdown == input$geographic_breakdown_o3, assessment_factor %in% (extra_familial_harm_af), time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Assessment factor" = "assessment_factor", "Rate per 10,000" = "rate_per_10000") %>%
      dplyr::arrange(desc(`Rate per 10,000`))

    reactable(
      data,
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # time series chart
  # the time series chart, by region and la charts will need to be filtered by the extra dropdown
  output$efh_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )
    if (is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3 & assessment_factor %in% input$assessment_factors_2)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_2)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name)) & assessment_factor %in% input$assessment_factors_2)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_2)
    }

    max_y_lim <- max(filtered_data$rate_per_10000) + 10
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "rate_per_10000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste(input$assessment_factors_2, "cases (rate per 10,000)")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # child abuse ts table alternative
  output$efh_ts_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )
    if (is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3 & assessment_factor %in% input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name)) & assessment_factor %in% input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)


    reactable(
      data,
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by region chart
  output$efh_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )

    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_2) %>%
      filter(time_period == max(time_period), geographic_level == "Regional")

    max_lim <- max(data$rate_per_10000) + 10
    p <- by_region_bar_plot(data, "rate_per_10000", "Rate per 10,000", max_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste(input$assessment_factors_2, "cases (rate per 10,000), by region")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$efh_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )

    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_2, time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      arrange(desc(rate_per_10000)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)


    reactable(
      data,
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # stats neighbours further down in the stats neighbours section
  output$plot_efh_by_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )
    data <- assessment_factors %>% filter(assessment_factor == input$assessment_factors_2, geographic_level == "Local authority", time_period == max(time_period))

    max_y_lim <- max(data$rate_per_10000) + 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "rate_per_10000", "Rate per 10,000") +
      scale_y_continuous(limits = c(0, max_y_lim))
    title_factor <- paste(input$assessment_factors_2, "cases (rate per 10,000), by local authority")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    )
  })

  # by la table alt
  output$table_efh_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )
    if (input$select_geography_o3 == "Regional") {
      if (input$geographic_breakdown_o3 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o3) %>%
          pull(la_name)
      }

      data <- assessment_factors %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(assessment_factor == input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
        arrange(desc(rate_per_10000))
    } else if (input$select_geography_o3 %in% c("Local authority", "National")) {
      data <- assessment_factors %>%
        filter(geographic_level == "Local authority", time_period == max(assessment_factors$time_period)) %>%
        filter(assessment_factor == input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
        arrange(desc(rate_per_10000))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      arrange(desc(rate_per_10000)) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)

    reactable(
      data2,
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # Outcome 4 -----
  # Geographic breakdown o4 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_o4
  }, {
    choices <- sort(unique(placement_data[(placement_data$geographic_level == input$select_geography_o4 & placement_data$time_period == max(placement_data$time_period)), "geo_breakdown"]), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_o4",
      selected = choices[1],
      choices = choices,
    )
  })

  region_for_la_o4 <- reactive({
    selected_la <- input$geographic_breakdown_o4
    location_data %>%
      filter(la_name == selected_la) %>%
      pull(region_name)
  })

  output$outcome4_choice_text1 <- renderText({
    if (input$select_geography_o4 == "National") {
      paste0("You have selected ", tags$b(input$select_geography_o4), " level statistics on ", tags$b("England"), ".")
    } else if (input$select_geography_o4 == "Regional") {
      paste0("You have selected ", tags$b(input$select_geography_o4), " level statistics for ", tags$b(input$geographic_breakdown_o4), ".")
    } else if (input$select_geography_o4 == "Local authority") {
      paste0("You have selected ", tags$b(input$select_geography_o4), " level statistics for ", tags$b(input$geographic_breakdown_o4), ", in ", region_for_la_o4(), ".")
    }
  })

  output$outcome4_choice_text2 <- renderText({
    # Checking to see if they picked national average comparison
    if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      paste0("You have also selected to compare with the ", tags$b("National Average."))
      # If they picked regional comparison
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      paste0("You have also selected to compare with the ", tags$b("Regional average."))
      # Picked both national and regional comparison
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      paste0("You have also selected to compare with the ", tags$b("National average"), " and the ", tags$b("Regional average."))
    }
  })

  ### Headline boxes ----
  output$placement_changes_txt <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat <- format(placement_changes_data %>%
        filter(time_period == max(placement_changes_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
        filter(placement_stability == "With 3 or more placements during the year") %>%
        select(Percentage), nsmall = 0)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_changes_data$time_period), ")", "</p>"
    )
  })

  output$foster_placement_txt <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat <- format(placement_data %>%
        filter(time_period == max(placement_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
        filter(characteristic == "Foster placements") %>%
        select(percentage), nsmall = 0)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_data$time_period), ")", "</p>"
    )
  })

  output$secure_unit_placement_txt <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat <- format(placement_data %>%
        filter(time_period == max(placement_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
        filter(characteristic == "Secure units, children's homes and semi-independent living accommodation") %>%
        select(percentage), nsmall = 0)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_data$time_period), ")", "</p>"
    )
  })

  output$residential_placement_txt <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat <- format(placement_data %>%
        filter(time_period == max(placement_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
        filter(characteristic == "Other residential settings") %>%
        select(percentage), nsmall = 0)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_data$time_period), ")", "</p>"
    )
  })

  output$placement_distance_txt <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat <- format(placement_data %>%
        filter(time_period == max(placement_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
        filter(characteristic == "Placed more than 20 miles from home") %>%
        select(percentage), nsmall = 0)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_data$time_period), ")", "</p>"
    )
  })

  output$care_leavers_employment_txt1 <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat <- care_leavers_activity_data %>%
        filter(time_period == max(care_leavers_activity_data$time_period) &
          geo_breakdown %in% input$geographic_breakdown_o4 &
          age == "17 to 18 years" &
          activity == "Total in education, employment or training") %>%
        select(percentage)
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(care_leavers_activity_data$time_period), ")", "</p>")
  })

  output$care_leavers_employment_txt2 <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat <- care_leavers_activity_data %>%
        filter(time_period == max(care_leavers_activity_data$time_period) &
          geo_breakdown %in% input$geographic_breakdown_o4 &
          age == "19 to 21 years" &
          activity == "Total in education, employment or training") %>%
        select(percentage)
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(care_leavers_activity_data$time_period), ")", "</p>")
  })

  output$care_leavers_accommodation_txt1 <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat <- care_leavers_accommodation_data %>%
        filter(time_period == max(time_period) &
          geo_breakdown %in% input$geographic_breakdown_o4 &
          age == "17 to 18 years" &
          accommodation_suitability == "Accommodation considered suitable") %>%
        select(percentage)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(care_leavers_accommodation_data$time_period), ")", "</p>")
  })

  output$care_leavers_accommodation_txt2 <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat <- care_leavers_accommodation_data %>%
        filter(time_period == max(time_period) &
          geo_breakdown %in% input$geographic_breakdown_o4 &
          age == "19 to 21 years" &
          accommodation_suitability == "Accommodation considered suitable") %>%
        select(percentage)
    }
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(care_leavers_accommodation_data$time_period), ")", "</p>")
  })

  output$placement_order_match_txt <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else if (input$geographic_breakdown_o4 != "National") {
      stat <- "NA"
    } else {
      stat <- format(placement_order_match_data %>%
        filter(time_period == max(placement_order_match_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
        filter(age_start_poc == "Total") %>%
        select(months), nsmall = 0)
    }
    paste0(
      stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(All ages - ", max(placement_order_match_data$time_period), ")", "</p>"
    )
  })

  ### Placement type charts and tables ----
  # Time series chart
  output$placement_type_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & characteristic %in% input$placement_type_breakdown) %>%
        rename("Placements (%)" = "Percent")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & characteristic %in% input$placement_type_breakdown) %>%
        rename("Placements (%)" = "Percent")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & characteristic %in% input$placement_type_breakdown) %>%
        rename("Placements (%)" = "Percent")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & characteristic %in% input$placement_type_breakdown) %>%
        rename("Placements (%)" = "Percent")
    }
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Placements (%)", "Placements (%)", 100) %>%
      config(displayModeBar = F)
    title_placements <- paste("Children living in", input$placement_type_breakdown, "(%)")
    p <- p + ggtitle(title_placements)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_type_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & characteristic %in% input$placement_type_breakdown) %>%
        select(time_period, geo_breakdown, characteristic, Percent)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & characteristic %in% input$placement_type_breakdown) %>%
        select(time_period, geo_breakdown, characteristic, Percent)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & characteristic %in% input$placement_type_breakdown) %>%
        select(time_period, geo_breakdown, characteristic, Percent)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & characteristic %in% input$placement_type_breakdown) %>%
        select(time_period, geo_breakdown, characteristic, Percent)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Placement Type` = `characteristic`, `Placements (%)` = `Percent`)

    reactable(
      data,
      columns = list(
        `Placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by region chart and table
  output$placement_type_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )

    data <- placement_data %>%
      filter(characteristic == input$placement_type_breakdown) %>%
      filter(time_period == max(time_period), geographic_level == "Regional") %>%
      rename("Placements (%)" = "Percent")

    p <- by_region_bar_plot(data, "Placements (%)", "Placements (%)", 100) %>%
      config(displayModeBar = F)
    title_placements <- paste("Children living in", input$placement_type_breakdown, "(%) by region")
    p <- p + ggtitle(title_placements)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_type_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )

    data <- placement_data %>%
      filter(characteristic == input$placement_type_breakdown, time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, characteristic, Percent) %>%
      arrange(desc(Percent))

    data <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Placement Type` = `characteristic`, `Placements (%)` = `Percent`)
    reactable(
      data,
      columns = list(
        `Placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by LA chart and table
  output$placement_type_la_plot <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )
    data <- placement_data %>%
      filter(characteristic == input$placement_type_breakdown, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Placements (%)" = "Percent")


    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Placements (%)", "Placements (%)") +
      scale_y_continuous(limits = c(0, 100))
    title_placements <- paste("Children living in", input$placement_type_breakdown, "(%) by local authority")
    p <- p + ggtitle(title_placements)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_type_la_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )
    if (input$select_geography_o4 == "Regional") {
      if (input$geographic_breakdown_o4 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o4) %>%
          pull(la_name)
      }
      data <- placement_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(characteristic == input$placement_type_breakdown) %>%
        select(time_period, geo_breakdown, characteristic, Percent) %>%
        arrange(desc(Percent))
    } else if (input$select_geography_o4 %in% c("Local authority", "National")) {
      data <- placement_data %>%
        filter(geographic_level == "Local authority", time_period == max(placement_data$time_period)) %>%
        filter(characteristic == input$placement_type_breakdown) %>%
        select(time_period, geo_breakdown, characteristic, Percent) %>%
        arrange(desc(Percent))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, characteristic, Percent) %>%
      arrange(desc(Percent)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Placement type` = `characteristic`, `Placements (%)` = `Percent`)

    reactable(
      data2,
      columns = list(
        `Placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })



  ### Placement changes ----
  # Time series chart
  output$placement_changes_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_changes_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & placement_stability == "With 3 or more placements during the year") %>%
        rename("CLA with 3 or more placements (%)" = "Percent")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_changes_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") &
          placement_stability == "With 3 or more placements during the year") %>%
        rename("CLA with 3 or more placements (%)" = "Percent")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_changes_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & placement_stability == "With 3 or more placements during the year") %>%
        rename("CLA with 3 or more placements (%)" = "Percent")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_changes_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & placement_stability == "With 3 or more placements during the year") %>%
        rename("CLA with 3 or more placements (%)" = "Percent")
    }
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "CLA with 3 or more placements (%)", "CLA with 3 or more placements (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage of CLA with 3 or more placements during the year")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_changes_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_changes_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & placement_stability == "With 3 or more placements during the year") %>%
        select(time_period, geo_breakdown, Percent)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_changes_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & placement_stability == "With 3 or more placements during the year") %>%
        select(time_period, geo_breakdown, Percent)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_changes_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & placement_stability == "With 3 or more placements during the year") %>%
        select(time_period, geo_breakdown, Percent)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_changes_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & placement_stability == "With 3 or more placements during the year") %>%
        select(time_period, geo_breakdown, Percent)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CLA with 3 or more placements during the year(%)` = `Percent`)


    reactable(
      data,
      columns = list(
        `CLA with 3 or more placements during the year(%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by region chart and table
  output$placement_changes_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )

    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year") %>%
      filter(time_period == max(time_period), geographic_level == "Regional") %>%
      rename("CLA with 3 or more placements (%)" = "Percent")

    p <- by_region_bar_plot(data, "CLA with 3 or more placements (%)", "CLA with 3 or more placements (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage of CLA with 3 or more placements during the year by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_changes_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )

    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year", time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, Percent) %>%
      arrange(desc(Percent)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CLA with 3 or more placements (%)` = `Percent`)


    reactable(
      data,
      columns = list(
        `CLA with 3 or more placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by LA chart and table
  output$placement_changes_la_plot <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("CLA with 3 or more placements (%)" = "Percent")


    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "CLA with 3 or more placements (%)", "CLA with 3 or more placements (%)") +
      scale_y_continuous(limits = c(0, 100))
    p <- p + ggtitle("Percentage of CLA with 3 or more placements during the year by local authority")

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_changes_la_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    if (input$select_geography_o4 == "Regional") {
      if (input$geographic_breakdown_o4 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o4) %>%
          pull(la_name)
      }

      data <- placement_changes_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(placement_stability == "With 3 or more placements during the year") %>%
        select(time_period, geo_breakdown, Percent) %>%
        arrange(desc(Percent))
    } else if (input$select_geography_o4 %in% c("Local authority", "National")) {
      data <- placement_changes_data %>%
        filter(geographic_level == "Local authority", time_period == max(placement_data$time_period)) %>%
        filter(placement_stability == "With 3 or more placements during the year") %>%
        select(time_period, geo_breakdown, Percent) %>%
        arrange(desc(Percent))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, Percent) %>%
      arrange(desc(Percent)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CLA with 3 or more placements (%)` = `Percent`)

    reactable(
      data2,
      columns = list(
        `CLA with 3 or more placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## Placement Distance -------------
  # Time series chart
  output$placement_distance_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & characteristic == "Placed more than 20 miles from home") %>%
        rename("Placements more then 20 miles from home (%)" = "Percent")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & characteristic == "Placed more than 20 miles from home") %>%
        rename("Placements more then 20 miles from home (%)" = "Percent")

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & characteristic == "Placed more than 20 miles from home") %>%
        rename("Placements more then 20 miles from home (%)" = "Percent")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & characteristic == "Placed more than 20 miles from home") %>%
        rename("Placements more then 20 miles from home (%)" = "Percent")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & characteristic == "Placed more than 20 miles from home") %>%
        rename("Placements more then 20 miles from home (%)" = "Percent")
    }
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Placements more then 20 miles from home (%)", "Placements (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage of placements more than 20 miles from home")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })


  # timeseries table alternative
  output$placement_dist_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & characteristic == "Placed more than 20 miles from home") %>%
        select(time_period, geo_breakdown, characteristic, Percent)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & characteristic == "Placed more than 20 miles from home") %>%
        select(time_period, geo_breakdown, characteristic, Percent)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & characteristic == "Placed more than 20 miles from home") %>%
        select(time_period, geo_breakdown, characteristic, Percent)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & characteristic == "Placed more than 20 miles from home") %>%
        select(time_period, geo_breakdown, characteristic, Percent)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Placement Distance` = `characteristic`, `Placements (%)` = `Percent`)

    reactable(
      data,
      columns = list(
        `Placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by region chart and table
  output$placement_dist_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    data <- placement_data %>%
      filter(characteristic == "Placed more than 20 miles from home") %>%
      filter(time_period == max(time_period), geographic_level == "Regional") %>%
      rename("Placements more then 20 miles from home (%)" = "Percent")

    p <- by_region_bar_plot(data, "Placements more then 20 miles from home (%)", "Placements (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage of placements more than 20 miles from home by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_dist_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    data <- placement_data %>%
      filter(characteristic == "Placed more than 20 miles from home", time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, characteristic, Percent) %>%
      arrange(desc(Percent))

    data <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Placement Distance` = `characteristic`, `Placements (%)` = `Percent`)
    reactable(
      data,
      columns = list(
        `Placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by LA chart and table
  output$placement_dist_la_plot <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )
    data <- placement_data %>%
      filter(characteristic == "Placed more than 20 miles from home", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Placements more then 20 miles from home (%)" = "Percent")


    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Placements more then 20 miles from home (%)", "Placements (%)") +
      scale_y_continuous(limits = c(0, 100))
    p <- p + ggtitle("Percentage of placements more than 20 miles from home by local authority")

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_dist_la_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )
    if (input$select_geography_o4 == "Regional") {
      if (input$geographic_breakdown_o4 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o4) %>%
          pull(la_name)
      }
      data <- placement_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(characteristic == "Placed more than 20 miles from home") %>%
        select(time_period, geo_breakdown, characteristic, Percent) %>%
        arrange(desc(Percent))
    } else if (input$select_geography_o4 %in% c("Local authority", "National")) {
      data <- placement_data %>%
        filter(geographic_level == "Local authority", time_period == max(placement_data$time_period)) %>%
        filter(characteristic == "Placed more than 20 miles from home") %>%
        select(time_period, geo_breakdown, characteristic, Percent) %>%
        arrange(desc(Percent))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, characteristic, Percent) %>%
      arrange(desc(Percent)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Placement Distance` = `characteristic`, `Placements (%)` = `Percent`)

    reactable(
      data2,
      columns = list(
        `Placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## Placement order and match ----
  output$placement_order_match_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 == "National", "Regional and local authority level data are not available for this indicator."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$select_age_group_o4 != "", "Select an age.")
    )

    filtered_data <- placement_order_match_data %>%
      filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & age_start_poc %in% input$select_age_group_o4)

    max_months <- max(placement_order_match_data$months)


    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "months", "Number of Months", max_months) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average time between placement order and match for those children who are adopted")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # timeseries table alternative
  output$placement_order_match_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 == "National", "Regional and local authority level data are not available for this indicator."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$select_age_group_o4 != "", "Select an age.")
    )

    filtered_data <- placement_order_match_data %>%
      filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & age_start_poc %in% input$select_age_group_o4) %>%
      select(time_period, geographic_level, stage_of_adoption_process, age_start_poc, months)


    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Geographic Level` = `geographic_level`, `Stage of Adoption Process` = `stage_of_adoption_process`, `Age` = `age_start_poc`, `Months` = `months`)

    reactable(
      data,
      columns = list(
        `Months` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  ## Wellbeing SDQ score ----------------
  output$wellbeing_score_stat <- renderText({
    if (input$geographic_breakdown_o4 == "") {
      stat <- "NA"
    } else {
      stat_prev <- format(wellbeing_sdq_data %>%
        filter(time_period == (max(wellbeing_sdq_data$time_period) - 1) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
        filter(characteristic == "SDQ average score") %>%
        select(number), nsmall = 1)

      stat_current <- format(wellbeing_sdq_data %>%
        filter(time_period == max(wellbeing_sdq_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
        filter(characteristic == "SDQ average score") %>%
        select(number), nsmall = 1)
      if ((stat_current < stat_prev)) {
        context <- " down from "
      } else {
        context <- " up from "
      }
    }
    paste0(
      stat_current, "<br>", "<p style='font-size:16px; font-weight:500;'>", "in ", max(wellbeing_sdq_data$time_period), context, stat_prev, " in ", (max(wellbeing_sdq_data$time_period) - 1), "</p>"
    )
  })

  # time series plot and chart for SDQ score
  output$sdq_time_series_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- wellbeing_sdq_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & characteristic == "SDQ average score")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- wellbeing_sdq_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & characteristic == "SDQ average score")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- wellbeing_sdq_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & characteristic == "SDQ average score")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- wellbeing_sdq_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & characteristic == "SDQ average score")
    }

    final_data <- filtered_data %>%
      select(time_period, geographic_level, geo_breakdown, characteristic, number_num) %>%
      rename("Score" = "number_num")

    max_y_lim <- (max(final_data$Score) + 5)
    p <- plotly_time_series_custom_scale(final_data, input$select_geography_o4, input$geographic_breakdown_o4, "Score", "SDQ average score", max_y_lim) +
      geom_hline(linetype = "dashed", colour = "red", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
      geom_hline(linetype = "dot", colour = "blue", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) %>%
      config(displayModeBar = F)

    p <- p + ggtitle("Average SDQ score")

    suppressWarnings(
      plot <- ggplotly(
        plotly_time_series_custom_scale(final_data, input$select_geography_o4, input$geographic_breakdown_o4, "Score", "SDQ average score", max_y_lim, add_rect = TRUE) #+
        # geom_hline(linetype = "dashed", colour = "red", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
        # # geom_hline(linetype = "dot", colour = "blue", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17")))
        # geom_rect( colour = NA, fill = "green",alpha = 0.1, aes(xmin = 0, xmax = 6,ymin = 0, ymax = 14, text = paste("Normal SDQ score: 0-13")))+
        # geom_rect( colour = NA, fill = "orange",alpha = 0.1, aes(xmin = 0, xmax = 6,ymin = 14, ymax = 17, text = paste("Borderline SDQ score: 14-16")))+
        # geom_rect( colour = NA, fill = "red",alpha = 0.1, aes(xmin = 0, xmax = 6,ymin = 17, ymax = max_y_lim, text = paste("Cause for concern SDQ score: 17-40")))
        %>%
          config(displayModeBar = F),
        height = 420,
        tooltip = "text"
      ) %>%
        layout(hovermode = "x")
    )
  })

  output$sqd_ts_table <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- wellbeing_sdq_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & characteristic == "SDQ average score")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- wellbeing_sdq_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & characteristic == "SDQ average score")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- wellbeing_sdq_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & characteristic == "SDQ average score")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- wellbeing_sdq_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & characteristic == "SDQ average score")
    }

    final_data <- filtered_data %>%
      select(time_period, geo_breakdown, characteristic, number_num) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `SDQ characteristic` = `characteristic`, "Score" = "number_num")

    reactable(
      final_data,
      columns = list(
        `Score` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## SDQ by region chart and table
  # by region chart and table
  output$SDQ_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    data <- wellbeing_sdq_data %>%
      filter(characteristic == "SDQ average score") %>%
      filter(time_period == max(time_period), geographic_level == "Regional") %>%
      rename("Score" = "number_num")

    max_y_lim <- (max(data$Score) + 5)

    p <- by_region_bar_plot(data, "Score", "Average SDQ score", max_y_lim) +
      geom_hline(linetype = "dashed", colour = "red", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
      geom_hline(linetype = "dot", colour = "blue", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) %>%
      config(displayModeBar = F)

    p <- p + ggtitle("Average SDQ score by region")


    ggplotly(
      by_region_bar_plot(data, "Score", "Average SDQ score", max_y_lim, add_rect = TRUE) #+
      # geom_hline(linetype = "dashed", colour = "red", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
      # geom_hline(linetype = "dot", colour = "blue", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17")))
      %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      layout(hovermode = "x")
  })

  output$SDQ_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    data <- wellbeing_sdq_data %>%
      filter(characteristic == "SDQ average score", time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, characteristic, number_num) %>%
      arrange(desc(number_num))

    data2 <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `SDQ characteristic` = `characteristic`, `Score` = `number_num`)

    reactable(
      data2,
      columns = list(
        `Score` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # SDQ by la chart and table
  output$sdq_by_la_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    data <- wellbeing_sdq_data %>%
      filter(characteristic == "SDQ average score" & geographic_level == "Local authority" & time_period == max(time_period)) %>%
      rename(`Score` = `number_num`)

    max_y_lim <- (max(data$`Score`) + 5)

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Score", "Average SDQ score", yupperlim = max_y_lim, add_rect = TRUE) +
      scale_y_continuous(limits = c(0, max_y_lim)) #+
    # geom_hline(linetype = "dashed", colour = "red", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
    # geom_hline(linetype = "dot", colour = "blue", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17")))

    p <- p + ggtitle("Average SDQ score by local authority")

    ggplotly(
      p %>%
        config(displayModeBar = F),
      tooltip = "text",
      height = 420
    ) %>%
      layout(hovermode = "x")
  })

  output$sdq_by_la_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )
    if (input$select_geography_o4 == "Regional") {
      if (input$geographic_breakdown_o4 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o4) %>%
          pull(la_name)
      }

      data <- wellbeing_sdq_data %>%
        filter(geo_breakdown %in% location, time_period == max(wellbeing_sdq_data$time_period)) %>%
        filter(characteristic == "SDQ average score") %>%
        select(time_period, geo_breakdown, characteristic, number_num) %>%
        arrange(desc(number_num))
    } else if (input$select_geography_o4 %in% c("Local authority", "National")) {
      data <- wellbeing_sdq_data %>%
        filter(geographic_level == "Local authority", time_period == max(wellbeing_sdq_data$time_period)) %>%
        filter(characteristic == "SDQ average score") %>%
        select(time_period, geo_breakdown, characteristic, number_num) %>%
        arrange(desc(number_num))
    }

    data2 <- data %>%
      arrange(desc(number_num)) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `SDQ characteristic` = `characteristic`, `Score` = `number_num`)

    reactable(
      data2,
      columns = list(
        `Score` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })




  ## care leavers ---------
  ### care leavers employment----
  ### Dynamic header text

  output$care_leavers_header1 <- renderUI({
    h2(paste("Care leavers in employment, education and training (", input$leavers_age, ")"))
  })
  output$care_leavers_header2 <- renderUI({
    h2(paste("Care leavers in employment, education and training (", input$leavers_age, ") by region"))
  })
  output$care_leavers_header3 <- renderUI({
    h2(paste("Care leavers in employment, education and training (", input$leavers_age, ") by local authority"))
  })


  # Time series chart
  output$care_activity_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_activity_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & age %in% input$leavers_age & activity == "Total in education, employment or training") %>%
        rename("Care leavers in education, employment or training (%)" = "percent")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_activity_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & age %in% input$leavers_age & activity == "Total in education, employment or training") %>%
        rename("Care leavers in education, employment or training (%)" = "percent")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_activity_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & age %in% input$leavers_age & activity == "Total in education, employment or training") %>%
        rename("Care leavers in education, employment or training (%)" = "percent")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_activity_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & age %in% input$leavers_age & activity == "Total in education, employment or training") %>%
        rename("Care leavers in education, employment or training (%)" = "percent")
    }
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Care leavers in education, employment or training (%)", "Care leavers in education, employment or training (%)", 100) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in employment, education and training (", input$leavers_age, ")")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # timeseries table alternative
  output$cl_activity_ts_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an assessment factor.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_activity_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & age %in% input$leavers_age & activity == "Total in education, employment or training") %>%
        select(time_period, geo_breakdown, activity, age, percent)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_activity_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & age %in% input$leavers_age & activity == "Total in education, employment or training") %>%
        select(time_period, geo_breakdown, activity, age, percent)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_activity_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & age %in% input$leavers_age & activity == "Total in education, employment or training") %>%
        select(time_period, geo_breakdown, activity, age, percent)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_activity_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & age %in% input$leavers_age & activity == "Total in education, employment or training") %>%
        select(time_period, geo_breakdown, activity, age, percent)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Activity` = `activity`, `Age range` = `age`, `Care leavers in education, employment or training (%)` = `percent`)

    reactable(
      data,
      columns = list(
        `Care leavers in education, employment or training (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # care leavers activity by region
  output$cl_activity_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )

    data <- care_leavers_activity_data %>%
      filter(age == input$leavers_age & activity == "Total in education, employment or training" & time_period == max(time_period) & geographic_level == "Regional") %>%
      rename("Care leavers in education, employment or training (%)" = "percent")

    p <- by_region_bar_plot(data, "Care leavers in education, employment or training (%)", "Care leavers in education, employment or training (%)", 100) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in employment, education and training (", input$leavers_age, ") by region")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$cl_activity_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )

    data <- care_leavers_activity_data %>%
      filter(age == input$leavers_age & time_period == max(time_period) & geographic_level == "Regional" & activity == "Total in education, employment or training") %>%
      select(time_period, geo_breakdown, activity, age, percentage) %>%
      arrange(desc(percentage))

    data <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Activity` = `activity`, `Age range` = `age`, `Care leavers in education, employment or training (%)` = `percentage`)

    reactable(
      data,
      columns = list(
        `Care leavers in education, employment or training (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # by la chart and table (stats neighbours is further down in stats neighbours area)
  output$plot_cl_activity_by_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )
    data <- care_leavers_activity_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & activity == "Total in education, employment or training") %>%
      rename("Care leavers in education, employment or training (%)" = "percent")

    # max_y_lim <- max(data$Number) + 500

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Care leavers in education, employment or training (%)", "Care leavers in education, employment or training (%)") +
      scale_y_continuous(limits = c(0, 100))
    age_title <- paste("Care leavers in employment, education and training (", input$leavers_age, ") by local authority")
    p <- p + ggtitle(age_title)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    )
  })

  # by la table alt
  output$table_cl_activity_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )
    if (input$select_geography_o4 == "Regional") {
      if (input$geographic_breakdown_o4 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o4) %>%
          pull(la_name)
      }

      data <- care_leavers_activity_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(age == input$leavers_age & activity == "Total in education, employment or training") %>%
        select(time_period, geo_breakdown, activity, age, percent) %>%
        arrange(desc(percent))
    } else if (input$select_geography_o4 %in% c("Local authority", "National")) {
      data <- care_leavers_activity_data %>%
        filter(geographic_level == "Local authority" & time_period == max(care_leavers_activity_data$time_period) & activity == "Total in education, employment or training") %>%
        filter(age == input$leavers_age) %>%
        select(time_period, geo_breakdown, activity, age, percent) %>%
        arrange(desc(percent))
    }

    data2 <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Activity` = `activity`, `Age range` = `age`, `Care leavers in education, employment or training (%)` = `percent`)

    reactable(
      data2,
      columns = list(
        `Care leavers in education, employment or training (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### care leavers accommodation----
  #  Dynamic headers
  output$care_leavers_header4 <- renderUI({
    h2(paste("Care leavers in suitable accommodation (", input$leavers_age, ")"))
  })
  output$care_leavers_header5 <- renderUI({
    h2(paste("Care leavers in suitable accommodation (", input$leavers_age, ") by region"))
  })
  output$care_leavers_header6 <- renderUI({
    h2(paste("Care leavers in suitable accommodation (", input$leavers_age, ") by local authority"))
  })

  # Headline stat


  # Time series chart
  output$care_accommodation_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_accommodation_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        rename("Care leavers in suitable accommodation (%)" = "percent")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_accommodation_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        rename("Care leavers in suitable accommodation (%)" = "percent")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_accommodation_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        rename("Care leavers in suitable accommodation (%)" = "percent")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_accommodation_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        rename("Care leavers in suitable accommodation (%)" = "percent")
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Care leavers in suitable accommodation (%)", "Care leavers in suitable accommodation (%)", 100) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in suitable accommodation (", input$leavers_age, ")")
    p <- p + ggtitle(age_title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # timeseries table alternative
  output$cl_accommodation_ts_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_accommodation_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        select(time_period, geo_breakdown, accommodation_suitability, age, percent) %>%
        rename("Care leavers in suitable accommodation (%)" = "percent")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_accommodation_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        select(time_period, geo_breakdown, accommodation_suitability, age, percent) %>%
        rename("Care leavers in suitable accommodation (%)" = "percent")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_accommodation_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        select(time_period, geo_breakdown, accommodation_suitability, age, percent) %>%
        rename("Care leavers in suitable accommodation (%)" = "percent")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_accommodation_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        select(time_period, geo_breakdown, accommodation_suitability, age, percent) %>%
        rename("Care leavers in suitable accommodation (%)" = "percent")
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Accommodation suitability` = `accommodation_suitability`, `Age range` = `age`)

    reactable(
      data,
      columns = list(
        `Care leavers in suitable accommodation (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # care leavers accommodation by region
  output$cl_accommodation_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )

    data <- care_leavers_accommodation_data %>%
      filter(age == input$leavers_age & accommodation_suitability == "Accommodation considered suitable" & time_period == max(time_period) & geographic_level == "Regional") %>%
      rename("Care leavers in suitable accommodation (%)" = "percent")
    p <- by_region_bar_plot(data, "Care leavers in suitable accommodation (%)", "Care leavers in suitable accommodation (%)", 100) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in suitable accommodation (", input$leavers_age, ") by region")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 430,
      tooltip = "text"
    )
  })

  output$cl_accommodation_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )

    data <- care_leavers_accommodation_data %>%
      filter(age == input$leavers_age & time_period == max(time_period) & geographic_level == "Regional" & accommodation_suitability == "Accommodation considered suitable") %>%
      select(time_period, geo_breakdown, accommodation_suitability, age, percentage) %>%
      arrange(desc(percentage)) %>%
      rename("Care leavers in suitable accommodation (%)" = "percentage")

    data <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Accommodation suitability` = `accommodation_suitability`, `Age range` = `age`)

    reactable(
      data,
      columns = list(
        `Care leavers in suitable accommodation (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by la chart and table (stats neighbours is further down in stats neighbours area)
  output$plot_cl_accommodation_by_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )
    data <- care_leavers_accommodation_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & accommodation_suitability == "Accommodation considered suitable") %>%
      rename("Care leavers in suitable accommodation (%)" = "percent")

    # max_y_lim <- max(data$Number) + 500

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Care leavers in suitable accommodation (%)", "Care leavers in suitable accommodation (%)") +
      scale_y_continuous(limits = c(0, 100))
    age_title <- paste("Care leavers in suitable accommodation (", input$leavers_age, ") by local authority")
    p <- p + ggtitle(age_title)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    )
  })

  # by la table alt
  output$table_cl_accommodation_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )
    if (input$select_geography_o4 == "Regional") {
      if (input$geographic_breakdown_o4 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o4) %>%
          pull(la_name)
      }

      data <- care_leavers_accommodation_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        filter(age == input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        select(time_period, geo_breakdown, accommodation_suitability, age, percent) %>%
        arrange(desc(percent))
    } else if (input$select_geography_o4 %in% c("Local authority", "National")) {
      data <- care_leavers_accommodation_data %>%
        filter(geographic_level == "Local authority" & time_period == max(care_leavers_accommodation_data$time_period) & accommodation_suitability == "Accommodation considered suitable") %>%
        filter(age == input$leavers_age) %>%
        select(time_period, geo_breakdown, accommodation_suitability, age, percent) %>%
        arrange(desc(percent))
    }

    data2 <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Accommodation suitability` = `accommodation_suitability`, `Age range` = `age`, `Care leavers in suitable accommodation (%)` = `percent`)

    reactable(
      data2,
      columns = list(
        `Care leavers in suitable accommodation (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # ALL statistical neighbours -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Outcome 1 ------
  ### CLA ------
  output$SN_cla <- renderUI({
    if (input$cla_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cla_rate_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_cla_rate_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_cla_rate_la")
          )
        ),
        details(
          inputId = "cla_rate_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
              tags$li("Only the first occasion on which a child started to be looked after in the LA during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
              tags$li("Figures exclude children looked after under a series of short-term placements."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("cla_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cla",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_cla_tbl")
          )
        ),
        details(
          inputId = "sn_cla_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # cla stats neighbours chart and table here
  output$cla_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    # Set the max y-axis scale
    max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    filtered_data <- cla_rates %>%
      filter(population_count == "Children starting to be looked after each year") %>%
      rename("Rate per 10,000" = "Rate Per 10000")

    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, "Rate per 10,000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # cla stats neighbour tables
  output$SN_cla_tbl <- renderReactable({
    filtered_data <- cla_rates %>% filter(population_count == "Children starting to be looked after each year")

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = "number", yvalue = "rate_per_10000"),
      columns = list(
        `Rate Per 10000` = colDef(name = "Rate per 10,000", cell = cellfunc, defaultSortOrder = "desc"), `number` = colDef(name = "Number of children starting to be looked after")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })


  ### UASC -------
  output$SN_uasc <- renderUI({
    if (input$uasc_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_uasc_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_uasc_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_uasc_la")
          )
        ),
        details(
          inputId = "cla_UASC_rate_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Rates are calculated using published number of children starting to be looked after each year, who are UASC and non-UASC, which have been rounded to the nearest 10 at national and regional level (unrounded for local authority figures)."),
              tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
              tags$li("Only the first occasion on which a child started to be looked after in the LA during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
              tags$li("Following the introduction of the National Transfer Scheme (NTS) in 2016, there has been an agreement between local authorities to transfer UASC to ensure a more equitable distribution of UASC across all local authorities. This means that some UASC will be counted more than once in the national and regional CLA starting figures if they started to be looked after within more than 1 local
                                  authority during the year. In 2019 we estimate that nationally, the number of UASC starts was overestimated by 9%, this increased to 15% in 2023 following the mandation of the NTS in February 2022."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("UASC_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_uasc",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_uasc_tbl")
          )
        ),
        details(
          inputId = "sn_usac_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # UASC stats neighbours chart and table here
  output$UASC_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    # Set the max y-axis scale
    max_rate <- max(
      combined_cla_data$`Placement Rate Per 10000`[combined_cla_data$population_count == "Children starting to be looked after each year" &
        combined_cla_data$characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")],
      na.rm = TRUE
    )

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- statistical_neighbours_plot_uasc(combined_cla_data, input$geographic_breakdown_o1, input$select_geography_o1, "Placement Rate Per 10000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CAL rate per 10,000 with UASC breakdown by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # cla UASC stats neighbour tables
  output$SN_uasc_tbl <- renderReactable({
    filtered_data <- combined_cla_data %>%
      filter(population_count == "Children starting to be looked after each year", characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")) # %>%
    #   rename("Placement rate per 10000" = "placement_per_10000")

    reactable(
      stats_neighbours_table_uasc(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, yvalue = "Placement Rate Per 10000"),
      columns = list(
        `Placement Rate Per 10000` = colDef(name = "Rate per 10,000", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### CLA march -------
  output$SN_cla_march <- renderUI({
    if (input$cla_march_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cla_march_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_cla_march_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_cla_march_la")
          )
        ),
        details(
          inputId = "cla_rate_march_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("cla_march_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cla_march",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_cla_march_tbl")
          )
        ),
        details(
          inputId = "sn_cla_march_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # cla march stats neighbours chart and table here
  output$cla_march_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    # Set the max y-axis scale
    max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children looked after at 31 March each year"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    filtered_data <- cla_rates %>%
      filter(population_count == "Children looked after at 31 March each year") %>%
      rename("Rate per 10,000" = "Rate Per 10000")

    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, "Rate per 10,000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CLA rate per 10,000 on 31 March by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # # cla March stats neighbour tables
  output$SN_cla_march_tbl <- renderReactable({
    filtered_data <- cla_rates %>% filter(population_count == "Children looked after at 31 March each year")

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = "number", yvalue = "rate_per_10000"),
      columns = list(
        `Rate Per 10000` = colDef(name = "Rate per 10,000", cell = cellfunc, defaultSortOrder = "desc"), `number` = colDef(name = "Number of children looked after on 31 March")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### CIN -------
  output$SN_cin <- renderUI({
    if (input$cin_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cin_rates_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_cin_rates_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_cin_rates_la")
          )
        ),
        details(
          inputId = "CIN_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Rate of children as at 31 March 2023 assessed as needing help and protection as a result of risks to their devlopment or health."),
              tags$li("Rates per 10,000 children are calculated based on ONS", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2021", "mid-year population estimates."), "for children aged 0 to 17 years. The rates for 2022 and 2023 are based on 2021 population estimates which in turn are based on 2021 Census data."),
              tags$li("The rates for 2023 have been calculated based on 2021 population estimates as 2022 estimates were not available at the time of publication. Therefore, some caution is needed when interpreting the 2023 rates, either in isolation or in comparison with other years. The 2023 rates will be revised as part of the next 2024 publication."),
              tags$li("Revised population estimates for 2012 to 2020 based on 2021 Census data, to calculate revised 2013 to 2021 rates, were not available at the time of publication. Therefore, some caution is needed when interpreting these rates, either in isolation or in comparison with other years. The 2013 to 2021 rates will be revised as part of the next 2024 publication."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("cin_SN_plot"),
        # p("This is under development."),
        br(),
        details(
          inputId = "tbl_sn_cin",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_cin_tbl")
            # p("This is under development.")
          )
        ),
        details(
          inputId = "sn_cin_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # cin stats neighbours chart and table here
  output$cin_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    # Set the max y-axis scale
    max_rate <- max(cin_rates$CIN_rate, na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    data <- cin_rates %>%
      rename("CIN rate per 10,000" = "CIN_rate")

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "CIN rate per 10,000", "CIN rate per 10,000", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CIN rate per 10,000 by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # # cin stats neighbours tables
  output$SN_cin_tbl <- renderReactable({
    # filtered_data <- cla_rates %>% filter(population_count == "Children looked after at 31 March each year")

    # renaming column
    data <- cin_rates %>% rename("CIN_rate_per_10000" = "At31_episodes_rate")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = "At31_episodes", yvalue = "CIN_rate_per_10000"),
      columns = list(
        `Cin Rate Per 10000` = colDef(name = "CIN rate per 10,000", cell = cellfunc, defaultSortOrder = "desc"), `At31_episodes` = colDef(name = "CIN number at 31 March")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### Repeat referrals ----------------------
  output$SN_cin_referral <- renderUI({
    if (input$cin_referral_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cin_referral_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_cin_referral_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_cin_referral_la")
          )
        ),
        details(
          inputId = "CIN_referral_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("If a child has more than one referral in a reporting year, then each referral is counted."),
              tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology section for more information."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("cin_referral_SN_plot"),
        # p("This is under development."),
        br(),
        details(
          inputId = "tbl_sn_cin_referral",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_cin_referral_tbl")
          )
        ),
        details(
          inputId = "sn_cin_referral_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # cin referral stats neighbours chart and table here
  output$cin_referral_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    # data <- cin_referrals %>%
    #  rename("Re_referrals_percentage" = "Re-referrals (%)")
    p <- statistical_neighbours_plot(cin_referrals, input$geographic_breakdown_o1, input$select_geography_o1, "Re-referrals (%)", "Re-referrals (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Re-referrals (%) by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # # cin stats neighbours tables
  output$SN_cin_referral_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(cin_referrals, input$geographic_breakdown_o1, input$select_geography_o1, yvalue = "Re-referrals (%)"),
      columns = list(
        `Re-Referrals (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### school attendance -------
  output$SN_absence <- renderUI({
    if (input$absence_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_absence_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_absence_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_absence_la")
          )
        ),
        details(
          inputId = "Attendance_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li(
                "Overall absence is the aggregated total of all authorised and unauthorised absences. Authorised absence is absence with permission from a teacher or other authorised school representative - including absences where a satisfactory explanation has been provided. For example, through illness.
                                Unauthorised absence is absence without permission from the school. This includes all unexplained or unjustified absences and arrivals after registration has closed. For further information see ",
                a(href = "https://explore-education-statistics.service.gov.uk/methodology/pupil-absence-in-schools-in-england#section3-1", "3.1 Overall absence methodology."),
              ),
              tags$li(
                "No absence data relating to the full 2019/20 academic year is available due to COVID-19.
                                  Due to the disruption during the 2020/21 and 2021/22 academic years, caution should be taken when comparing data to previous years. For more detailed information on this see ",
                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England."),
              ),
              tags$li("CINO refers to Children In Need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
              tags$li("CPPO refers to children on a Child Protection Plan, excluding children looked after."),
              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("absence_SN_plot"),
        # p("This is under development."),
        br(),
        details(
          inputId = "tbl_sn_absence",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_absence_tbl")
            # p("This is under development.")
          )
        ),
        details(
          inputId = "sn_absence_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # Absence SN plot
  output$absence_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    data <- outcomes_absence %>% filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown)
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Overall absence (%)", "Overall absence (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Overall absence rate (%) by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })
  # Absence SN table
  output$SN_absence_tbl <- renderReactable({
    filtered_data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      rename(`OA%` = `Overall absence (%)`, `Overall absence (%)` = `pt_overall`) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("social_care_group", "school_type", "Total pupils"), yvalue = "Overall absence (%)"),
      columns = list(
        `social_care_group` = colDef(name = "Social care group"),
        `school_type` = colDef(name = "School type"),
        `Total pupils` = colDef(name = "Total number of pupils"),
        `Overall Absence (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### persistent absence ------------
  output$SN_persistent_abs <- renderUI({
    if (input$persis_abs_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_persistent_absence_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_persistent_absence_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_persistent_absence_la")
          )
        ),
        details(
          inputId = "Persistent_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li(
                "Persistent absence is when a pupil enrolment’s overall absence equates to 10% or more of their possible sessions. For further information see ",
                a(href = "https://explore-education-statistics.service.gov.uk/methodology/pupil-absence-in-schools-in-england#section3-2", "3.2 Overall absence methodology."),
              ),
              tags$li(
                "No absence data relating to the full 2019/20 academic year is available due to COVID-19.
                                  Due to the disruption during the 2020/21 and 2021/22 academic years, caution should be taken when comparing data to previous years. For more detailed information on this see ",
                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England."),
              ),
              tags$li("CINO refers to Children In Need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
              tags$li("CPPO refers to children on a Child Protection Plan, excluding children looked after."),
              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("persistent_absence_SN_plot"),
        # p("This is under development."),
        br(),
        details(
          inputId = "tbl_sn_persistent_abs",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_persistent_absence_tbl")
            # p("This is under development.")
          )
        ),
        details(
          inputId = "sn_persistent_abs_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # persistent absence stats neighbours chart
  output$persistent_absence_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    data <- outcomes_absence %>% filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown)
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Persistent absentees (%)", "Persistent absentees (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Persistent absentees (%) by statistical neighbours")

    ggplotly(,
      height = 420,
      tooltip = "text"
    )
  })

  # Persistent Absence SN table
  output$SN_persistent_absence_tbl <- renderReactable({
    filtered_data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      rename(`PA%` = `Persistent absentees (%)`, `Persistent absentees (%)` = `pt_pupils_pa_10_exact`) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("social_care_group", "school_type", "Total pupils"), yvalue = "Persistent absentees (%)"),
      columns = list(
        `social_care_group` = colDef(name = "Social care group"), `school_type` = colDef(name = "School type"), `Total pupils` = colDef(name = "Total number of pupils"), `Persistent Absentees (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### KS2 attainment -------
  output$SN_ks2_attainment <- renderUI({
    if (input$ks2_attainment_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_KS2_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_KS2_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_KS2_la")
          )
        ),
        details(
          inputId = "ks2_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("No attainment data related to 2019/20 and 2020/21 academic year is available due to COVID-19."),
              tags$li(
                "Writing teacher assessment and reading, writing and maths (combined) measures from 2018 onwards are not directly comparable to previous years due to changes in the writing teacher assessment frameworks. For more detailed information on this see ",
                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment", "Key stage 2 attainment."),
              ),
              tags$li("CINO refers to Children In Need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
              tags$li("CPPO refers to children on a Child Protection Plan, excluding children looked after."),
              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("ks2_attain_SN_plot"),
        # p("This is under development."),
        br(),
        details(
          inputId = "tbl_sn_ks2",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_ks2_attain_tbl")
            # p("This is under development.")
          )
        ),
        details(
          inputId = "sn_ks2_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # ks2 attainment stats neighbours chart
  output$ks2_attain_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    data <- outcomes_ks2 %>% filter(social_care_group %in% input$attainment_extra_breakdown)
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Expected standard reading writing maths (%)", "Expected standard combined (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage meeting combined expected standard (KS2) by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # KS2 attainment SN table
  output$SN_ks2_attain_tbl <- renderReactable({
    data <- outcomes_ks2 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      select(-c("Expected standard reading writing maths (%)"))
    data <- data %>%
      rename("Expected standard reading writing maths (%)" = "pt_rwm_met_expected_standard") %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("social_care_group", "t_rwm_eligible_pupils"), yvalue = "Expected standard reading writing maths (%)"),
      columns = list(
        `Expected Standard Reading Writing Maths (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc"), `t_rwm_eligible_pupils` = colDef(name = "Total number of eligibile pupils"), `social_care_group` = colDef(name = "Social care group")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })


  ### KS4 attainment ------
  output$SN_ks4_attainment <- renderUI({
    if (input$ks4_attainment_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_KS4_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_KS4_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_KS4_la")
          )
        ),
        details(
          inputId = "ks4_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li(
                "Due to the impact of the COVID-19 pandemic, the summer examination series was cancelled in both 2020 and 2021, and alternative processes set up to award grades.
                                 The method to award grades was different in 2021 to that in 2020. The changes to the way GCSE grades were awarded in these two years means 2019/20 and 2020/21 pupil attainment data should not be
                                 directly compared to pupil attainment data from previous or later years for the purposes of measuring year on year changes in pupil performance. For more detailed information on this see ",
                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance", "Key stage 4 performance."),
              ),
              tags$li("In 2022/23 there was a return to pre-pandemic standards for GCSEs, with protection built into the grading process to recognise the disruption that students have faced. Therfore The more meaningful comparison is with 2019, the last year that summer exams were taken before the pandemic, as 2023 saw a return to pre-pandemic grading, with some protections.
                                  In 2022 outcomes broadly reflected a mid-point between 2019 and 2021, to take account of the impact of the pandemic and in line with Ofqual’s approach to grading in 2022. It is expected that performance in 2023 will generally be lower than in 2022. Users need to exercise extreme caution when considering comparisons over time, as they may not reflect changes in pupil performance alone."),
              tags$li("CINO refers to Children In Need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
              tags$li("CPPO refers to children on a Child Protection Plan, excluding children looked after."),
              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("ks4_attain_SN_plot"),
        # p("This is under development."),
        br(),
        details(
          inputId = "tbl_sn_ks4",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_ks4_attain_tbl")
            # p("This is under development.")
          )
        ),
        details(
          inputId = "sn_ks4_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # ks4 attainment stats neighbours chart
  output$ks4_attain_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    data <- outcomes_ks4 %>% filter(social_care_group %in% input$attainment_extra_breakdown)
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Average Attainment 8", "Average Attainment 8 score", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average attainment 8 score (KS4) by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  # KS4 attainment SN table
  output$SN_ks4_attain_tbl <- renderReactable({
    data <- outcomes_ks4 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      arrange(desc(`Average Attainment 8`)) %>%
      rename(`Social care group` = `social_care_group`, `Total number of pupils` = `Total pupils`, `Average attainment 8 score` = `Average Attainment 8`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("Social care group", "Total number of pupils"), yvalue = "Average attainment 8 score"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Average Attainment 8 Score` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## Outcome 2 ------
  ### SGO ------------
  output$SN_sgo <- renderUI({
    if (input$sgo_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_SGO_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_sgo_ceased_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_sgo_la")
          )
        ),
        details(
          inputId = "sgo_la_info",
          label = "Additional information:",
          help_text = (
            p(
              tags$li("Only one reason for children ceased to be looked after during the year shown. See ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after publication "), "for full list of reasons."),
              tags$li("Percentages rounded to the nearest whole number."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$li("Figures exclude children looked after under a series of short-term placements."),
              tags$li("Only the last occasion on which a child ceased to be looked after in the year has been counted."),
              tags$br(),
              "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance."),
              tags$br(),
              "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
          ))
        )
      )
    } else {
      validate(
        need(input$select_geography_o2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("sgo_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_sgo",
          label = "View chart as a table",
          help_text = (
            # dataTableOutput("SN_sgo_tbl")
            reactableOutput("SN_sgo_tbl")
          )
        ),
        details(
          inputId = "sn_sgo_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })
  # SGO SN plot and table alternative
  output$sgo_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    filtered_data <- ceased_cla_data %>% filter(characteristic == "Special guardianship orders")
    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased due to SGO (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to SGO by statistical neighbours")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$SN_sgo_tbl <- renderReactable({
    filtered_data <- ceased_cla_data %>%
      filter(characteristic == "Special guardianship orders") %>%
      rename(`Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, selectedcolumn = c("Reason ceased", "Number ceased", "Total ceased"), yvalue = "Ceased (%)"),
      columns = list(
        `Number ceased` = colDef(cell = cellfunc),
        `Total ceased` = colDef(cell = cellfunc),
        `Ceased (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### CAO --------------------
  output$SN_cao <- renderUI({
    if (input$cao_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cao_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_cao_ceased_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_cao_la")
          )
        ),
        details(
          inputId = "cao_la_info",
          label = "Additional information:",
          help_text = (
            p(
              tags$li("Only one reason for children ceased to be looked after during the year shown. See ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after publication "), "for full list of reasons."),
              tags$li("Percentages rounded to the nearest whole number."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$li("Figures exclude children looked after under a series of short-term placements."),
              tags$li("Only the last occasion on which a child ceased to be looked after in the year has been counted."),
              tags$br(),
              "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance."),
              tags$br(),
              "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
          ))
        )
      )
    } else {
      validate(
        need(input$select_geography_o2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("cao_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cao",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_cao_tbl")
          )
        ),
        details(
          inputId = "sn_cao_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })
  # CAO SN plot and table alternative
  output$cao_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    filtered_data <- ceased_cla_data %>% filter(characteristic == "Residence order or child arrangement order granted")
    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased due to CAO (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to CAO by statistical neighbours")
    ggplotly(,
      height = 420,
      tooltip = "text"
    )
  })

  output$SN_cao_tbl <- renderReactable({
    filtered_data <- ceased_cla_data %>%
      filter(characteristic == "Residence order or child arrangement order granted") %>%
      rename(`Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, selectedcolumn = c("Reason ceased", "Number ceased", "Total ceased"), yvalue = "Ceased (%)"),
      columns = list(
        `Number ceased` = colDef(cell = cellfunc),
        `Total ceased` = colDef(cell = cellfunc),
        `Ceased (%)` = colDef(name = "Reason ceased (%)", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ## Outcome 3 -----
  ### Repeat CPP -----
  # output all LA chart or stats neighbour chart for CPP repeat
  output$SN_CPP <- renderUI({
    if (input$CPP_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cpp_repeat_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_repeat_cpp_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_cpp_repeat_la")
          )
        ),
        details(
          inputId = "cpp_in_year_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The metric shown in the graph refers to the percentage of children who have been entered into a CPP during the year, where this plan was at least their second."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/2023/data-guidance", "Children in need data guidance."),
                tags$br(),
                "For more information about child protection plans, please refer to", a(href = "https://assets.publishing.service.gov.uk/media/65cb4349a7ded0000c79e4e1/Working_together_to_safeguard_children_2023_-_statutory_guidance.pdf", "Working together to safeguard children - statutory guidance.")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("cpp_repeat_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cpp",
          label = "View chart as a table",
          help_text = (
            # dataTableOutput("SN_sgo_tbl")
            reactableOutput("SN_cpp_repeat_tbl")
          )
        ),
        details(
          inputId = "sn_cpp_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # Repeat CPP SN plot and table alternative
  output$cpp_repeat_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    filtered_data <- repeat_cpp %>%
      rename("Repeat CPP (%)" = "Repeat_CPP_percent")
    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o3, input$select_geography_o3, "Repeat CPP (%)", "Repeat CPP (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Repeat CPP (%) by statistical neighbours")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })


  output$SN_cpp_repeat_tbl <- renderReactable({
    data <- repeat_cpp %>%
      rename("Repeat CPP (%)" = "Repeat_CPP_percent", "CPP starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, selectedcolumn = c("CPP starts", "Repeat CPP"), yvalue = "Repeat CPP (%)"),
      columns = list(
        `CPP starts` = colDef(name = "CPP Starts", cell = cellfunc),
        `Repeat CPP` = colDef(name = "Repeat CPP", cell = cellfunc),
        `Repeat Cpp (%)` = colDef(name = "Repeat CPP (%)", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### NO CPP for 2+ years by LA ----

  ### Hospital admissions -----
  output$SN_hosp_admissions <- renderUI({
    if (input$hosp_admission_toggle == "All local authorities") {
      tagList(
        plotlyOutput("admissions_la_plot"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_hosp_admission_la",
          label = "View chart as table",
          help_text = (
            reactableOutput("admissions_la_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "admissions_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("All sub national counts are rounded to the nearest 5. Rates are calculated using unrounded counts."),
              tags$li("For time points from 2012, all sub national counts are rounded to the nearest 5, and counts of 1 to 7 are suppressed. Rates and confidence intervals are calculated using unrounded counts."),
              tags$li("Until all data publications for Outcome 3 indicators are updated with local authority changes, Cumberland and Westmorland and Furness have been combined into Cumbria for hospital admissions data."),
              tags$li("Values relating to City of London and Isles of Scilly have been combined with Hackney and Cornwall."),
              tags$br(),
              p(
                "For more information on the data, please refer to the", a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/502/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0", "Public health data explorer."),
                tags$br(),
                "For more information on the definitions and methodology, please refer to the", a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/6/gid/1938133230/pat/159/par/K02000001/ati/15/are/E92000001/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1", "Indicator definitions and supporting information page.")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )

      tagList(
        plotlyOutput("hosp_admissions_SN_plot"),
        # p("stats neighbours plot here"),
        br(),
        details(
          inputId = "tbl_sn_hosp_ad",
          label = "View chart as a table",
          help_text = (
            reactableOutput("hosp_admissions_SN_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "sn_hosp_ad_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  output$hosp_admissions_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    data <- hospital_admissions %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename(`Rate per 10,000` = `Value`)

    max_y_lim <- max(data$`Rate per 10,000`) + 50

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Rate per 10,000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Hospital admissions caused by unintentional and deliberate injuries to young people (0 to 14 years), by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$hosp_admissions_SN_tbl <- renderReactable({
    data <- hospital_admissions %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename(`Rate per 10,000` = `Value`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, yvalue = "Rate per 10,000"),
      columns = list(
        `Rate Per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })




  ### Child abuse/Neglect ------
  output$SN_child_ab_neg <- renderUI({
    if (input$child_abuse_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_child_abuse_by_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_child_ab_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_child_ab_neg_la")
          )
        ),
        details(
          inputId = "child_abuse_add_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Figures exclude the category ‘no factors identified’."),
              tags$li("An episode of need may have more than one factor recorded."),
              tags$li("Information on child on child and adult on child physical and sexual abuse was collected and reported on for the third time in 2023. Previously physical abuse and sexual abuse was collected and reported on (irrespective of whether it was child on child or adult on child) and some local authorities have provided information on the old basis only, or a mixture of the old and new basis, since 2021. The old physical and sexual abuse categories have therefore been included to provide a more complete account of this category of assessment."),
              tags$li("The ‘Domestic violence’ factor was renamed as ‘Domestic abuse’ in the 2022 release. This is a change to the description of the factor and is not a change to the information collected for this factor."),
              tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology for more information."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("abuse_neg_SN_plot"),
        # p("stats neighbours plot here"),
        br(),
        details(
          inputId = "tbl_sn_ch_ab_neg",
          label = "View chart as a table",
          help_text = (
            # dataTableOutput("SN_turnover_tbl")
            reactableOutput("abuse_neg_SN_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "sn_abuse_neg_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # child abuse/neglect SN plot and table alternative
  output$abuse_neg_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_1, geographic_level == "Local authority", time_period == max(time_period))

    max_y_lim <- max(data$rate_per_10000) + 100
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "rate_per_10000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste(input$assessment_factors_1, "by statistical neighbours")
    p <- p + ggtitle(title_factor, "cases (rate per 10,000), by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$abuse_neg_SN_tbl <- renderReactable({
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_1, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Rate per 10,000" = "rate_per_10000", "Assessment factor" = `assessment_factor`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, selectedcolumn = c("Assessment factor"), yvalue = "Rate per 10,000"),
      columns = list(
        `Rate Per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Extra Familial harm ------
  output$SN_extra_familial_harm <- renderUI({
    if (input$extra_familial_harm_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_efh_by_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_extra_fam_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_efh_la")
            # p("table here")
          )
        ),
        details(
          inputId = "efh_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Figures exclude the category ‘no factors identified’."),
              tags$li("An episode of need may have more than one factor recorded."),
              tags$li("Information on child on child and adult on child physical and sexual abuse was collected and reported on for the third time in 2023. Previously physical abuse and sexual abuse was collected and reported on (irrespective of whether it was child on child or adult on child) and some local authorities have provided information on the old basis only, or a mixture of the old and new basis, since 2021. The old physical and sexual abuse categories have therefore been included to provide a more complete account of this category of assessment."),
              tags$li("The ‘Domestic violence’ factor was renamed as ‘Domestic abuse’ in the 2022 release. This is a change to the description of the factor and is not a change to the information collected for this factor."),
              tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology for more information."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("efh_SN_plot"),
        # p("stats neighbours plot here"),
        br(),
        details(
          inputId = "tbl_sn_efh",
          label = "View chart as a table",
          help_text = (
            # dataTableOutput("SN_turnover_tbl")
            reactableOutput("efh_SN_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "sn_efh_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # child abuse/neglect SN plot and table alternative
  output$efh_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_2, geographic_level == "Local authority", time_period == max(time_period))

    max_y_lim <- max(data$rate_per_10000) + 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "rate_per_10000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste(input$assessment_factors_2, "cases (rate per 10,000), by statistical neighbours")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$efh_SN_tbl <- renderReactable({
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_2, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Rate per 10,000" = "rate_per_10000", "Assessment factor" = "assessment_factor")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, selectedcolumn = "Assessment factor", yvalue = "Rate per 10,000"),
      columns = list(
        `Rate Per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  ## Outcome 4 ----------------
  ### Placement type --------------------
  output$SN_placement_type <- renderUI({
    if (input$placement_type_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("placement_type_la_plot"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_placement_type_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("placement_type_la_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "placement_type_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Numbers have been rounded to the nearest 10. Percentages rounded to the nearest whole number. Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to check for the equivalent table in earlier releases of this publication. Figures exclude children looked after under a series of short-term placements."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after in England data guidance."),
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("placement_type_SN_plot"),
        # p("stats neighbours plot here"),
        br(),
        details(
          inputId = "tbl_sn_placement_type",
          label = "View chart as a table",
          help_text = (
            # dataTableOutput("SN_turnover_tbl")
            reactableOutput("placement_type_SN_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "sn_placement_type_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  output$placement_type_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )
    data <- placement_data %>%
      filter(characteristic == input$placement_type_breakdown, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Placements (%)" = "Percent")
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Placements (%)", "Placements (%)", 100) %>%
      config(displayModeBar = F)
    placements_title <- paste("Children living in", input$placement_type_breakdown, "(%) by statistical neighbours")
    p <- p + ggtitle(placements_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_type_SN_tbl <- renderReactable({
    data <- placement_data %>%
      filter(characteristic == input$placement_type_breakdown, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Placements (%)" = "Percent", "Placement Type" = "characteristic")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, selectedcolumn = "Placement Type", yvalue = "Placements (%)"),
      columns = list(
        `Placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Placement changes ------
  output$SN_placement_changes <- renderUI({
    if (input$placement_changes_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("placement_changes_la_plot"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_placement_changes_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("placement_changes_la_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "placement_changes_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Numbers have been rounded to the nearest 10. Percentages rounded to the nearest whole number. Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to check for the equivalent table in earlier releases of this publication. Figures exclude children looked after under a series of short-term placements."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after in England data guidance."),
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("placement_changes_SN_plot"),
        # p("stats neighbours plot here"),
        br(),
        details(
          inputId = "tbl_sn_placement_changes",
          label = "View chart as a table",
          help_text = (
            # dataTableOutput("SN_turnover_tbl")
            reactableOutput("placement_changes_SN_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "sn_placement_changes_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  output$placement_changes_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )
    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year", geographic_level == "Local authority", time_period == max(time_period))

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Percent", "Percentage", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage of CLA with 3 or more placements during the year by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_changes_SN_tbl <- renderReactable({
    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename(`Percentage2` = `Percentage`, `Percentage` = `Percent`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Percentage"),
      columns = list(
        `Percentage` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Placement Distance ------------------
  output$SN_placement_distance <- renderUI({
    if (input$placement_dist_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("placement_dist_la_plot"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_placement_changes_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("placement_dist_la_tbl")
          )
        ),
        details(
          inputId = "placement_dist_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Percentages have been rounded to the nearest whole number. Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. Figures exclude children looked after under a series of short-term placements."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after in England data guidance."),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("placement_dist_SN_plot"),
        # p("stats neighbours plot here"),
        br(),
        details(
          inputId = "tbl_sn_placement_changes",
          label = "View chart as a table",
          help_text = (
            reactableOutput("placement_dist_SN_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "sn_placement_changes_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  output$placement_dist_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
    )
    data <- placement_data %>%
      filter(characteristic == "Placed more than 20 miles from home", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Placements more then 20 miles from home (%)" = "Percent")

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Placements more then 20 miles from home (%)", "Placements (%)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage of placements more then 20 miles from home by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$placement_dist_SN_tbl <- renderReactable({
    data <- placement_data %>%
      filter(characteristic == "Placed more than 20 miles from home", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename(`Placements (%)` = `Percent`, `Placement Distance` = `characteristic`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, selectedcolumn = "Placement Distance", yvalue = "Placements (%)"),
      columns = list(
        `Placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  ### Wellbeing - SDQ Score -------------
  output$SN_wellbeing_SDQ <- renderUI({
    if (input$sdq_score_toggle == "All local authorities") {
      tagList(
        plotlyOutput("sdq_by_la_plot"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_sdq_score_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("sdq_by_la_tbl")
          )
        ),
        details(
          inputId = "sdq_score_la_info",
          label = "Additional information:",
          help_text = (p(
            tags$li("Average SDQ scores have been rounded to the nearest one decimal place."),
            tags$li("An SDQ score is required of all children aged 4-16 on the date of last assessment. Date of assessment is not collected so data in this table is restricted to children aged 5 to 16 years."),
            tags$li("A higher score indicates more emotional difficulties. 0-13 is considered normal, 14-16 is borderline cause for concern and 17-40 is cause for concern."),
            tags$br(),
            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance."),
            tags$br(),
            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
          ))
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("SN_sdq_plot"),
        # p("stats neighours chart here"),
        br(),
        details(
          inputId = "tbl_sn_sdq_score",
          label = "View chart as a table",
          help_text = (
            # p("table")
            reactableOutput("SN_sdq_table")
          )
        ),
        details(
          inputId = "sn_sdq_score_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  output$SN_sdq_plot <- renderPlotly({
    validate(
      need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    data <- wellbeing_sdq_data %>%
      filter(characteristic == "SDQ average score", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Score" = "number_num")

    max_y_lim <- (max(data$Score) + 5)
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Score", "Average SDQ score", max_y_lim) +
      geom_hline(linetype = "dashed", colour = "red", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
      geom_hline(linetype = "dot", colour = "blue", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average SDQ score by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$SN_sdq_table <- renderReactable({
    data <- wellbeing_sdq_data %>%
      filter(characteristic == "SDQ average score" & geographic_level == "Local authority" & time_period == max(time_period)) %>%
      rename("SDQ characteristic" = characteristic, "Score" = "number_num")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, selectedcolumn = "SDQ characteristic", yvalue = "Score"),
      columns = list(
        `Score` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  ### Care leavers activity -------------
  output$SN_care_leavers_activity <- renderUI({
    if (input$cl_activity_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cl_activity_by_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_cl_activity_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_cl_activity_la")
            # p("table here")
          )
        ),
        details(
          inputId = "activity_la_info",
          label = "Additional information:",
          help_text = (
            p(
              tags$li("Numbers have been rounded to the nearest 10. Percentages rounded to the nearest whole number.
                                  Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication.
                                  However, users looking for a longer time series may wish to check for the equivalent table in earlier releases of this publication.
                                  Figures exclude young people who were looked after under an agreed series of short term placements, those who have died since leaving care,
                                  those who have returned home to parents or someone with parental responsibility for a continuous period of at
                                  least 6 months and those whose care was transferred to another local authority.
                                  Figures for the number of care leavers who have died each year can be found in the methodology document."),
              tags$li("'Local authority not in touch' excludes young people where activity information is known, as a third party provided it even though the local authority is not directly in touch with the young person."),
              tags$li("In touch, activity and accommodation information for 17-21 year old care leavers relates to contact around their birthday."),
              tags$li("Figures for 2023 exclude Barnsley who were unable to provide data in time for publication."),
              tags$br(),
              "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance."),
              tags$br(),
              "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
          ))
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("cl_activity_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cl_act",
          label = "View chart as a table",
          help_text = (
            reactableOutput("cl_activity_SN_tbl")
          )
        ),
        details(
          inputId = "sn_cl_activity_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # care leavers activity SN plot and table alternative
  output$cl_activity_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$leavers_age != "", "Select an age range.")
    )
    data <- care_leavers_activity_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & activity == "Total in education, employment or training") %>%
      rename("Care leavers in education, employment or training (%)" = "percent")

    # max_y_lim <- max(data$Number) + 500
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Care leavers in education, employment or training (%)", "Care leavers in education, employment or training (%)", 100) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in employment, education and training (", input$leavers_age, ") by statistical neighbours")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$cl_activity_SN_tbl <- renderReactable({
    data <- care_leavers_activity_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & activity == "Total in education, employment or training") %>%
      rename("Percent" = "percent")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Percent"),
      columns = list(
        `Percent` = colDef(name = "Care leavers in education, employment or training (%)", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Care leavers accommodation ----------------------
  output$SN_care_leavers_accommodation <- renderUI({
    if (input$cl_accommodation_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cl_accommodation_by_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_cl_accommodation_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_cl_accommodation_la")
            # p("table here")
          )
        ),
        details(
          inputId = "cl_la_accomm_info",
          label = "Additional information:",
          help_text = (
            p(
              tags$li("Numbers have been rounded to the nearest 10. Percentages rounded to the nearest whole number.
                                    Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication.
                                    However, users looking for a longer time series may wish to check for the equivalent table in earlier releases of this publication.
                                    Figures exclude young people who were looked after under an agreed series of short term placements, those who have died since leaving care,
                                    those who have returned home to parents or someone with parental responsibility for a continuous period of at least 6 months and those whose care was transferred to another local authority.
                                    Figures for the number of care leavers who have died each year can be found in the methodology document."),
              tags$li("Accommodation suitable/not suitable figures also exclude young people who have gone abroad, been deported or their residence is not know as in these cases the suitability of the accommodation will be unknown.
                                  This means the total of care leavers in this table will be slightly lower than the total in the care leaver accommodation table. Regulation 9(2) of the Care Leavers Regulations defines what is meant by 'Suitable accommodation'.
                                  'No information' includes young people whose accommodation is not known because either the local authority is not in touch, or the young person has refused contact or no longer requires services."),
              tags$li("In touch, activity and accommodation information for 17-21 year old care leavers relates to contact around their birthday."),
              tags$li("Figures for 2023 exclude Barnsley who were unable to provide data in time for publication."),
              tags$br(),
              "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance."),
              tags$br(),
              "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("cl_accommodation_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cl_accomm",
          label = "View chart as a table",
          help_text = (
            reactableOutput("cl_acccomm_SN_tbl")
            # p("table here")
          )
        ),
        details(
          inputId = "sn_cl_accommodation_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  # child abuse/neglect SN plot and table alternative
  output$cl_accommodation_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$leavers_age != "", "Select an age range.")
    )
    data <- care_leavers_accommodation_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & accommodation_suitability == "Accommodation considered suitable") %>%
      rename("Care leavers in suitable accommodation (%)" = "percent")

    # max_y_lim <- max(data$Number) + 500
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Care leavers in suitable accommodation (%)", "Care leavers in suitable accommodation (%)", 100) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in suitable accommodation (", input$leavers_age, ") by statistical neighbours")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$cl_acccomm_SN_tbl <- renderReactable({
    data <- care_leavers_accommodation_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & accommodation_suitability == "Accommodation considered suitable") %>%
      rename("Care leavers in suitable accommodation (%)" = "percent")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Care leavers in suitable accommodation (%)"),
      columns = list(
        `Care Leavers In Suitable Accommodation (%)` = colDef(name = "Care leavers in suitable accommodation (%)", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  ## Enabler 3 ------
  ### Turnover rate -----
  ## Button logic for turnover by LA
  output$SN_turnover <- renderUI({
    if (input$turnover_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_turnover_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_turnover_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_turnover_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("turnover_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_turnover",
          label = "View chart as a table",
          help_text = (
            # dataTableOutput("SN_turnover_tbl")
            reactableOutput("SN_turnover_tbl")
          )
        ),
        details(
          inputId = "sn_turnover_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })
  # turnover SN plot and table alternative
  output$turnover_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    p <- statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Turnover Rate Fte", "Turnover Rate %", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Social worker turnover (FTE) % by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$SN_turnover_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, yvalue = "Turnover Rate Fte"),
      columns = list(
        `Turnover Rate Fte` = colDef(name = "Turnover rate (FTE) %", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Agency Rate ------
  output$SN_agency <- renderUI({
    if (input$agency_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_agency_rate_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_agency_rate_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_agency_rate_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("agency_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_agency",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_agency_tbl")
          )
        ),
        details(
          inputId = "sn_agency_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })
  # Agency rate SN plot and table alternative
  output$agency_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    p <- statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Agency Rate Fte", "Agency worker rate (FTE) %", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Agency worker rate (FTE) % by statistical neighbours")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$SN_agency_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, yvalue = "Agency Rate Fte"),
      columns = list(
        `Agency Rate Fte` = colDef(name = "Agency worker rate (FTE) %", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Vacancy rate --------------------------------------------------------------

  output$SN_vacancy <- renderUI({
    if (input$vacancy_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_vacancy_rate_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_vacancy_rate_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_vacancy_rate_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("vacancy_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_vacancy",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_vacancy_tbl")
          )
        ),
        details(
          inputId = "sn_vacancy_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })
  # turnover SN plot and table alternative
  output$vacancy_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    p <- statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Vacancy Rate Fte", "Vacancy rate (FTE) %", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Vacancy rate (FTE) % by statistical neighbours")
    ggplotly(,
      height = 420,
      tooltip = "text"
    )
  })

  output$SN_vacancy_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, yvalue = "Vacancy Rate Fte"),
      columns = list(
        `Vacancy Rate Fte` = colDef(name = "Vacancy rate (FTE) %", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Caseload ---------------------------------
  output$SN_caseload <- renderUI({
    if (input$caseload_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_caseload_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_caseload_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_caseload_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("caseload_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_caseload",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_caseload_tbl")
          )
        ),
        details(
          inputId = "sn_caseload_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })
  # turnover SN plot and table alternative
  output$caseload_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    p <- statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Caseload Fte", "Average Caseload (FTE)", 100) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average caseload (FTE) by statistical neighbours")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$SN_caseload_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, yvalue = "Caseload Fte"),
      columns = list(
        `Caseload Fte` = colDef(name = "Average caseload (FTE)", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })



  # Enabler 2 ----
  ### Ofsted leadership rating --------
  output$ofsted_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    p <- statistical_neighbours_plot_ofsted(ofsted_leadership_data_long, input$geographic_breakdown_e2) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Ofsted – The impact of leaders on social work practice with children and families with statistical neighbours")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$ofsted_SN_tbl <- renderReactable({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this table, you must select \"Local authority\" level and select a local authority.")
    )
    data <- ofsted_leadership_data_long %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      )) %>%
      group_by(geo_breakdown) %>%
      mutate(latest_rating = max(time_period)) %>%
      ungroup()
    reactable(
      stats_neighbours_table_ofsted(data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Rating"),
      defaultPageSize = 11,
      searchable = TRUE,
    )
  })

  ### Spending total ----
  output$SN_total_spending <- renderUI({
    if (input$spending1_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_spending_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_tot_spend_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("table_tot_spending_la")
          )
        ),
        details(
          inputId = "tot_spend_la_information",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Share of spend is calculated by taking total children’s services expenditure divided by total children’s services expenditure"),
              tags$li("Average per child spend is calculated based on", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates"), "for children aged 0 to 17 years and total children’s services expenditure."),
              tags$li("Average per child spend has been rounded to the nearest whole number."),
              tags$li("Spending data is based on the RO3 and RSX data files from the", a(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2022-to-2023-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2022 to 2023 individual local authority data – outturn")),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://www.gov.uk/government/publications/general-fund-revenue-account-outturn/general-fund-revenue-account-outturn-general-guidance-notes", "General fund revenue account outturn: general guidance notes."),
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("total_spending_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_total_spending",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_tot_spend_tbl")
            # p("table")
          )
        ),
        details(
          inputId = "sn_tot_spend_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })

  output$total_spending_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    # Need an if statement to look at the spending level choice this will determine the data in the chart
    if (input$spending_choice == "Share of total spend on children's services") {
      data <- spending_data

      max_y_lim <- ceiling(max(data$cs_share) / 10) * 10
      p <- statistical_neighbours_plot(data, input$geographic_breakdown_e2, input$select_geography_e2, "cs_share", "Share spent on children's services (%)", max_y_lim) %>%
        config(displayModeBar = F)
      p <- p + ggtitle("Share of total spend on children's services (%) by statistical neighbours")
    } else {
      data <- spending_per_capita %>%
        rename("Spend per child (£)" = "cost_per_capita")

      max_y_lim <- ceiling(max(data$`Spend per child (£)`) / 50) * 50

      p <- statistical_neighbours_plot(data, input$geographic_breakdown_e2, input$select_geography_e2, "Spend per child (£)", "Average spend per child (£)", max_y_lim) %>%
        config(displayModeBar = F)
      p <- p + ggtitle("Average spend per child (£) by statistical neighbours")
    }


    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$SN_tot_spend_tbl <- renderReactable({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    # Need an if statement to look at the spending level choice this will determine the data in the chart
    if (input$spending_choice == "Share of total spend on children's services") {
      data <- spending_data %>%
        rename("Children's services share (%)" = "cs_share")

      table <- stats_neighbours_table(data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Children's services share (%)")

      reactable(
        table,
        columns = list(
          `Children's Services Share (%)` = colDef(name = "Children's Services share (%)", cell = cellfunc, defaultSortOrder = "desc")
        ),
        defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
        searchable = TRUE,
      )
    } else {
      data <- spending_per_capita %>%
        rename("Average spend per child (£)" = "cost_per_capita")

      table <- stats_neighbours_table(data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Average spend per child (£)")

      reactable(
        table,
        columns = list(
          `Average Spend Per Child (£)` = colDef(name = "Average spend per child (£)", cell = cellfunc, defaultSortOrder = "desc")
        ),
        defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
        searchable = TRUE,
      )
    }
  })


  ### Spending Excluding CLA -----
  output$SN_spending_minus_cla <- renderUI({
    if (input$spending2_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_spend_excl_cla_la"),
        br(),
        p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
        br(),
        details(
          inputId = "tbl_tot_no_cla_spend_la",
          label = "View chart as a table",
          help_text = (
            reactableOutput("spend_excl_cla_la_tbl")
          )
        ),
        details(
          inputId = "no_cla_spend_information",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Share of spend is calculated by taking total children’s services expenditure minus total CLA expenditure, divided by total children’s services expenditure"),
              tags$li("Spending data is based on the RO3 and RSX data files from the", a(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2022-to-2023-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2022 to 2023 individual local authority data – outturn")),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://www.gov.uk/government/publications/general-fund-revenue-account-outturn/general-fund-revenue-account-outturn-general-guidance-notes", "General fund revenue account outturn: general guidance notes."),
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("spend_excl_cla_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_spending",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_spend_no_cla_tbl")
            # p("table")
          )
        ),
        details(
          inputId = "sn_spend_excl_cla_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
              tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
              br(),
              p(
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication."),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.")
              ),
            )
          )
        )
      )
    }
  })


  output$spend_excl_cla_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    data <- spending_data_no_cla
    max_y_lim <- ceiling(max(data$minus_cla_share) / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_e2, input$select_geography_e2, "minus_cla_share", "Share spent on children's services\n minus CLA (%)", max_y_lim) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Share of total spend on children's services minus CLA (%) by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    )
  })

  output$SN_spend_no_cla_tbl <- renderReactable({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )

    data <- spending_data_no_cla %>%
      rename("Share of spend minus CLA (%)" = "minus_cla_share")

    table <- stats_neighbours_table(data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Share of spend minus CLA (%)")

    reactable(
      table,
      columns = list(
        `Share Of Spend Minus Cla (%)` = colDef(name = "Share of spend minus CLA (%)", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  # Don't touch the code below -----------------------

  observeEvent(input$go, {
    toggle(id = "div_a", anim = T)
  })


  observeEvent(input$link_to_app_content_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  })

  # Download the underlying data button
  output$download_data <- downloadHandler(
    filename = "shiny_template_underlying_data.csv",
    content = function(file) {
      write.csv(dfRevBal, file)
    }
  )

  # Add input IDs here that are within the relevant drop down boxes to create dynamic text
  output$dropdown_label <- renderText({
    paste0("Current selections: ", input$selectPhase, ", ", input$selectArea)
  })


  # # Reactive value for last selected tab that isn't user guide
  # backTo <- reactive({
  #   if (input$navlistPanel != "user_guide") {
  #     return(input$navlistPanel)
  #   }
  # })
  #
  # # Observe return button click
  # observeEvent(input$go_back, {
  #   updateTabsetPanel(session, "navlistPanel", selected = "backTo")
  # })

  observeEvent(input$tutorial, {
    updateTabsetPanel(session, "navlistPanel", selected = "user_guide")
  })

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
