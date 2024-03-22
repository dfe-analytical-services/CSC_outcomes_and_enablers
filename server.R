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
  reactiveRevBal <- reactive({
    dfRevBal %>% filter(
      area_name == input$selectArea | area_name == "England",
      school_phase == input$selectPhase
    )
  })

  # Define server logic required to draw a histogram
  output$lineRevBal <- renderPlotly({
    ggplotly(createAvgRevTimeSeries(reactiveRevBal(), input$selectArea)) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  reactiveBenchmark <- reactive({
    dfRevBal %>%
      filter(
        area_name %in% c(input$selectArea, input$selectBenchLAs),
        school_phase == input$selectPhase,
        year == max(year)
      )
  })

  output$colBenchmark <- renderPlotly({
    ggplotly(
      plotAvgRevBenchmark(reactiveBenchmark()) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$tabBenchmark <- renderDataTable({
    datatable(
      reactiveBenchmark() %>%
        select(
          Area = area_name,
          `Average Revenue Balance (£)` = average_revenue_balance,
          `Total Revenue Balance (£m)` = total_revenue_balance_million
        ),
      options = list(
        scrollX = TRUE,
        paging = FALSE
      )
    )
  })
  # Dropdown Validation -----
  iv <- InputValidator$new()
  # outcome1
  iv$add_rule("select_geography_o1", sv_required())
  iv$add_rule("geographic_breakdown_o1", sv_required())
  # outcome2
  iv$add_rule("select_geography_o2", sv_required())
  iv$add_rule("geographic_breakdown_o2", sv_required())
  # enabler2
  iv$add_rule("select_geography_e2", sv_required())
  iv$add_rule("geographic_breakdown_e2", sv_required())


  iv$enable()


  # CSC server logic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Enabler 2 ----
  # Geographic level does not need to be here as it does not need to change depending on other dropdowns

  # Geographic breakdown e2 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_e2
  }, {
    choices <- sort(unique(workforce_data[(workforce_data$geographic_level == input$select_geography_e2 & workforce_data$time_period == max(workforce_data$time_period)), "geo_breakdown"]), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_e2",
      selected = choices[1],
      choices = choices
    )
  })

  ## Confirmation sentence E2 -------
  # This function gets the selected region to put into the confirmation text below

  workforce_region <- reactive({
    location_data_workforce %>%
      filter(la_name == input$geographic_breakdown_e2) %>%
      pull(region_name) %>%
      as.character() # Convert to character
  })

  # First sentence for the dropdown choices
  output$enabler2_choice_text1 <- renderText({
    if (input$select_geography_e2 == "National") {
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics on ", tags$b("England"), ".")
    } else if (input$select_geography_e2 == "Regional") {
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics for ", tags$b(input$geographic_breakdown_e2), ".")
    } else if (input$select_geography_e2 == "Local authority") {
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics for ", tags$b(input$geographic_breakdown_e2), ", in ", workforce_region(), ".")
    }
  })

  output$enabler2_choice_text2 <- renderText({
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


  # Social worker turnover rate headline box
  ## Turnover rate plot and table -----
  output$s_w_headline_txt <- renderText({
    if (input$geographic_breakdown_e2 == "") {
      stat <- "NA"
    } else {
      stat <- format(workforce_data %>%
        filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(turnover_rate_fte), nsmall = 1)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_data$time_period), ")", "</p>"
    )
  })

  # Social worker turnover rate benchmarking plot
  output$plot_s_w_turnover <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter(geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National"))
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2, "turnover_rate_fte", "Turnover rate (FTE) %", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Social worker turnover rate benchmarking table alternative
  output$table_s_w_turnover <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(time_period, geo_breakdown, turnover_rate_fte)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, turnover_rate_fte)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown, turnover_rate_fte)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, turnover_rate_fte)
    }
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "Turnover rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ### turnover rate by region plot----
  output$plot_turnover_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      # plot_turnover_reg() %>%
      by_region_bar_plot(workforce_data, "turnover_rate_fte", "Turnover Rate (FTE) %") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # turnover rate by region table
  output$table_turnover_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    datatable(
      workforce_data %>% filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        turnover_rate_fte
      ) %>%
        arrange(desc(turnover_rate_fte)),
      colnames = c("Time period", "Geographical breakdown", "Turnover rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ### Turnover Rate by LA plot ----
  output$plot_turnover_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      by_la_bar_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "turnover_rate_fte", "Turnover Rate (FTE) %") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Turnover Rate by LA table
  output$table_turnover_la <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    if (input$select_geography_e2 == "Regional") {
      if (input$geographic_breakdown_e2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e2) %>%
          pull(la_name)
      }

      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, turnover_rate_fte) %>%
        arrange(desc(turnover_rate_fte))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(
          time_period, geo_breakdown,
          turnover_rate_fte
        ) %>%
        arrange(desc(turnover_rate_fte))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "Turnover rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })


  ## Agency Rate ----
  output$agency_rate_txt <- renderText({
    if (input$geographic_breakdown_e2 == "") {
      stat <- "NA"
    } else {
      stat <- format(workforce_data %>%
        filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(agency_rate_fte), nsmall = 1)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_data$time_period), ")", "</p>"
    )
  })

  ### Agency worker rate benchmarking plot ----
  output$plot_agency_worker <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter(geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National"))
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2, "agency_rate_fte", "Agency worker rate (FTE) %", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # output$table_agency_worker <- renderDataTable({
  #   datatable(
  #     workforce_data %>% filter(geo_breakdown %in% input$geographic_breakdown) %>% select(
  #       time_period, geo_breakdown,
  #       agency_rate_fte
  #     ),
  #     colnames = c("Time Period", "Geographical Breakdown", "Agency Worker Rate (FTE) %"),
  #     options = list(
  #       scrollx = FALSE,
  #       paging = TRUE
  #     )
  #   )
  # })

  # Agency worker rate table alternative
  output$table_agency_worker <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(time_period, geo_breakdown, agency_rate_fte)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, agency_rate_fte)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown, agency_rate_fte)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, agency_rate_fte)
    }
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "Agency worker rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ### agency rate plot by region ----
  output$plot_agency_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      by_region_bar_plot(workforce_data, "agency_rate_fte", "Agency worker rate (FTE) %") %>%
        # plot_agency_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # agency rate table by region
  output$table_agency_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    datatable(
      workforce_data %>% filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        agency_rate_fte
      ) %>%
        arrange(desc(agency_rate_fte)),
      colnames = c("Time period", "Geographical breakdown", "Agency worker rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ### agency rate by la plot -----
  output$plot_agency_rate_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      by_la_bar_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "agency_rate_fte", "Agency worker rate (FTE) %") %>%
        # plot_agency_rate_la(input$geographic_breakdown_e2, input$select_geography_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # agency rate by la table alternative
  output$table_agency_rate_la <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    if (input$select_geography_e2 == "Regional") {
      if (input$geographic_breakdown_e2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e2) %>%
          pull(la_name)
      }

      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, agency_rate_fte) %>%
        arrange(desc(agency_rate_fte))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(
          time_period, geo_breakdown,
          agency_rate_fte
        ) %>%
        arrange(desc(agency_rate_fte))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "Agency worker rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ## Vacancy Rate -----
  # Vacancy rate headline box

  output$vacancy_rate_txt <- renderText({
    if (input$geographic_breakdown_e2 == "") {
      stat <- "NA"
    } else {
      stat <- format(workforce_data %>%
        filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(vacancy_rate_fte), nsmall = 1)
    }
    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_data$time_period), ")", "</p>"
    )
  })

  ### Vacancy Rate benchmarking plot ----
  output$plot_vacancy_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter(geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National"))
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2, "vacancy_rate_fte", "Vacancy rate (FTE) %", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })



  # Vacancy Rate benchmarking table alternative
  output$table_vacancy_rate <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(time_period, geo_breakdown, vacancy_rate_fte)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, vacancy_rate_fte)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown, vacancy_rate_fte)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, vacancy_rate_fte)
    }
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "Vacancy rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ### vacancy rate by la plot ----
  output$plot_vacancy_rate_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      by_la_bar_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "vacancy_rate_fte", "Vacancy rate (FTE) %") %>%
        # plot_vacancy_rate_la(input$geographic_breakdown_e2, input$select_geography_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # vacancy rate by la table alternative
  output$table_vacancy_rate_la <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    if (input$select_geography_e2 == "Regional") {
      if (input$geographic_breakdown_e2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e2) %>%
          pull(la_name)
      }

      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, vacancy_rate_fte) %>%
        arrange(desc(vacancy_rate_fte))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(
          time_period, geo_breakdown,
          vacancy_rate_fte
        ) %>%
        arrange(desc(vacancy_rate_fte))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "Vacancy rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })


  ### vacancy rate plot by region ----
  output$plot_vacancy_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      by_region_bar_plot(workforce_data, "vacancy_rate_fte", "Vacancy rate (FTE) %") %>%
        # plot_vacancy_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # vacancy rate table by region
  output$table_vacancy_reg <- renderDataTable({
    datatable(
      workforce_data %>% filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        vacancy_rate_fte
      ) %>%
        arrange(desc(vacancy_rate_fte)),
      colnames = c("Time period", "Geographical breakdown", "Vacancy rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ## Caseload ----
  # Caseload headline box

  output$caseload_txt <- renderText({
    if (input$geographic_breakdown_e2 == "") {
      stat <- "NA"
      paste0(stat, "%", "<br>")
    } else {
      previous_year_value <- workforce_data %>%
        filter(time_period == (max(workforce_data$time_period) - 1) & geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(caseload_fte)

      current_year_value <- workforce_data %>%
        filter(time_period == (max(workforce_data$time_period)) & geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(caseload_fte)

      if ((current_year_value < previous_year_value)) {
        context <- " down from "
      } else {
        context <- " up from "
      }
      stat <- format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e2) %>% select(caseload_fte), nsmall = 1)
      paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "in ", max(workforce_data$time_period), context, previous_year_value, (max(workforce_data$time_period) - 1), "</p>")
    }
  })

  ### Caseload benchmarking plot ----
  output$caseload_plot <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter(geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National"))
    }

    # Set the max y-axis scale
    max_rate <- max(workforce_data$caseload_fte, na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2, "caseload_fte", "Average caseload (FTE)", max_rate) %>%
      config(displayModeBar = F)


    ggplotly(p, height = 420) %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })


  # caseload benchamrking table alternative
  output$table_caseload <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(time_period, geo_breakdown, caseload_fte)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, caseload_fte)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown, caseload_fte)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, caseload_fte)
    }
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "Average caseload (FTE)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ### Caseload by region ----
  output$plot_caseload_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      by_region_bar_plot(workforce_data, "caseload_fte", "Average Caseload (FTE)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Caseload by region table
  output$table_caseload_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    datatable(
      workforce_data %>% filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        caseload_fte
      ) %>%
        arrange(desc(caseload_fte)),
      colnames = c("Time period", "Geographical breakdown", "Average caseload (FTE)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ### caseload by la -----
  output$plot_caseload_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      by_la_bar_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "caseload_fte", "Average Caseload (FTE)") %>%
        # plot_caseload_la(input$geographic_breakdown_e2, input$select_geography_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Caseload by LA table
  output$table_caseload_la <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    if (input$select_geography_e2 == "Regional") {
      if (input$geographic_breakdown_e2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e2) %>%
          pull(la_name)
      }

      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, caseload_fte) %>%
        arrange(desc(caseload_fte))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(
          time_period, geo_breakdown,
          caseload_fte
        ) %>%
        arrange(desc(caseload_fte))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "Average caseload (FTE)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # Ethnicity and Diversity Domain-----

  output$non_white_txt <- renderText({
    white_stat <- workforce_eth %>%
      filter(time_period == max(workforce_eth$time_period) &
        geo_breakdown %in% input$geographic_breakdown_e2 &
        role == "Total" &
        breakdown == "White") %>%
      select(inpost_headcount_percentage)

    if (input$geographic_breakdown_e2 == "") {
      non_white_stat <- "NA"
    } else {
      non_white_stat <- format(100 - as.numeric(white_stat), nsmall = 1)
    }
    paste0(non_white_stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_eth$time_period), ")", "</p>")
  })

  output$plot_ethnicity_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      plot_ethnicity_rate(input$geographic_breakdown_e2, input$select_geography_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$plot_population_ethnicity_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      plot_population_ethnicity_rate(input$geographic_breakdown_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$table_ethnicity_rate <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    datatable(
      workforce_eth %>%
        filter(
          geo_breakdown %in% input$geographic_breakdown_e2,
          role == "Total", breakdown_topic == "Ethnicity major"
        ) %>%
        select(time_period, geo_breakdown, breakdown, inpost_headcount, inpost_headcount_percentage),
      colnames = c("Time period", "Geographical breakdown", "Ethnicity", "Headcount", "Headcount (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })



  output$table_population_ethnicity_rate <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    datatable(
      combined_ethnicity_data %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(geo_breakdown, breakdown, inpost_headcount_percentage, Percentage),
      colnames = c(
        "Geographical breakdown",
        "Ethnicity group", "Workforce (%)", "Population (%)"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  output$plot_seniority_eth <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    ggplotly(
      plot_seniority_eth(input$geographic_breakdown_e2, input$select_geography_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  cols <- c("time_period", "geo_breakdown", "seniority", "breakdown", "inpost_headcount", "Percentage")

  output$table_seniority_eth <- renderDataTable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    datatable(
      workforce_eth_seniority[, cols] %>%
        filter(geo_breakdown %in% input$geographic_breakdown_e2, seniority != "Total", time_period == max(workforce_eth_seniority$time_period)) %>%
        select(time_period, geo_breakdown, seniority, breakdown, inpost_headcount, Percentage),
      colnames = c("Time period", "Geographical breakdown", "Seniority level", "Ethnicity", "Headcount", "Headcount (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE,
        target = "column"
      )
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
    paste0("Percentage of overall absence for ", tags$b(input$wellbeing_extra_breakdown), ".")
  })

  output$outcome1_choice_social_care_group_text_1 <- renderText({
    paste0("Percentage of persistent absentees for ", tags$b(input$wellbeing_extra_breakdown), ".")
  })

  output$outcome1_choice_social_care_group_text_2 <- renderText({
    paste0("Percentage of pupils achieving expected standard in reading, writing and maths combined for ", tags$b(input$attainment_extra_breakdown), ".")
  })

  output$outcome1_choice_social_care_group_text_3 <- renderText({
    paste0("Average achievement of pupils in up to 8 qualifications, including English language, English literature and maths, for ", tags$b(input$attainment_extra_breakdown), ".")
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
      filter(population_count == "Children starting to be looked after each year")

    # Set the max y-axis scale
    max_rate <- max(cla_rates$rate_per_10000[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "rate_per_10000", "Rate of children starting in care, per 10,000", max_rate) %>%
      config(displayModeBar = F)


    ggplotly(p, height = 420) %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })

  # CLA rate TABLE
  output$table_cla_rate <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, rate_per_10000, population_count)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, rate_per_10000, population_count)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, rate_per_10000, population_count)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, rate_per_10000, population_count)
    }

    datatable(
      filtered_data %>%
        filter(population_count == "Children starting to be looked after each year") %>%
        select(time_period, geo_breakdown, rate_per_10000),
      colnames = c("Time period", "Geographical breakdown", "Rate of children starting in care, per 10,000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # CLA rate regional plot
  output$plot_cla_rate_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_cla_rate_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # CLA rate regional table
  output$table_cla_rate_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    datatable(
      cla_rates %>% filter(geographic_level == "Regional", time_period == max(cla_rates$time_period), population_count == "Children starting to be looked after each year") %>% select(
        time_period, geo_breakdown,
        rate_per_10000
      ) %>%
        arrange(desc(rate_per_10000)),
      colnames = c("Time period", "Geographical breakdown", "Rate of children starting in care, per 10,000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # CLA rate LA plot
  output$plot_cla_rate_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_cla_rate_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # CLA rate La table
  output$table_cla_rate_la <- renderDataTable({
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
        select(time_period, geo_breakdown, rate_per_10000) %>%
        arrange(desc(rate_per_10000))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cla_rates %>%
        filter(geographic_level == "Local authority", time_period == max(cla_rates$time_period), population_count == "Children starting to be looked after each year") %>%
        select(
          time_period, geo_breakdown,
          rate_per_10000
        ) %>%
        arrange(desc(rate_per_10000))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "Rate of children starting in care, per 10,000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })



  ## CIN rate headline ----
  output$cin_rate_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(cin_rates %>% filter(time_period == max(cin_rates$time_period) & geo_breakdown %in% input$geographic_breakdown_o1)
        %>% select(CIN_rate), nsmall = 1)
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cin_rates$time_period), ")", "</p>")
  })

  # cin rate plot by region
  output$plot_cin_rate_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_cin_rate_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })


  # cin rate table by region
  output$table_cin_rates_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    datatable(
      cin_rates %>% filter(geographic_level == "Regional", time_period == max(cin_rates$time_period)) %>% select(
        time_period, geo_breakdown,
        CIN_rate
      ) %>%
        arrange(desc(CIN_rate)),
      colnames = c("Time period", "Geographical breakdown", "CIN rate per 10,000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })



  # cin rate table by LA
  output$table_cin_rates_la <- renderDataTable({
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
        select(time_period, geo_breakdown, CIN_rate) %>%
        arrange(desc(CIN_rate))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cin_rates %>%
        filter(geographic_level == "Local authority", time_period == max(cin_rates$time_period)) %>%
        select(
          time_period, geo_breakdown,
          CIN_rate
        ) %>%
        arrange(desc(CIN_rate))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "CIN rates per 10,000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # cin rate chart by LA
  output$plot_cin_rates_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_cin_rates_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # CIN referral headline
  output$cin_referral_headline_txt <- renderText({
    if (input$geographic_breakdown_o1 == "") {
      stat <- "NA"
    } else {
      stat <- format(cin_referrals %>% filter(time_period == max(cin_referrals$time_period) & geo_breakdown %in% input$geographic_breakdown_o1)
        %>% select(Re_referrals_percent), nsmall = 1)
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cin_referrals$time_period), ")", "</p>")
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
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National"))
    }


    # Set the max y-axis scale
    max_rate <- max(cin_rates$CIN_rate, na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "CIN_rate", "CIN rate per 10,000 Children", max_rate) %>%
      config(displayModeBar = F)


    ggplotly(p, height = 420) %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })

  # CIN rate table
  output$table_cin_rate <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_rates %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, CIN_number, CIN_rate)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, CIN_number, CIN_rate)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, CIN_number, CIN_rate)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, CIN_number, CIN_rate)
    }

    datatable(
      filtered_data %>%
        select(time_period, geo_breakdown, CIN_number, CIN_rate),
      colnames = c("Time period", "Geographical breakdown", "CIN at 31 March", "CIN rate per 10,000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
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
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_referrals %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National"))
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Re_referrals_percent", "Re-referrals (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # CIN referral table
  output$table_cin_referral <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_referrals %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, Re_referrals_percent)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_referrals %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, Re_referrals_percent)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, Re_referrals_percent)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, Re_referrals_percent)
    }

    datatable(
      filtered_data %>%
        select(time_period, geo_breakdown, Referrals, Re_referrals, Re_referrals_percent),
      colnames = c("Time period", "Geographical breakdown", "Referrals in the year", "Re-referrals within 12 months of a previous referral", "Re-referrals (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # cin referral table by region
  output$table_cin_referral_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    datatable(
      cin_referrals %>% filter(geographic_level == "Regional", time_period == max(cin_referrals$time_period)) %>% select(
        time_period, geo_breakdown,
        Referrals, Re_referrals, Re_referrals_percent
      ) %>%
        arrange(desc(Re_referrals_percent)),
      colnames = c(
        "Time period", "Geographical breakdown", "Referrals in the year",
        "Re-referrals within 12 months of a previous referral", "Re-referrals within 12 months (%)"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # cin referral table by LA
  output$table_cin_referral_la <- renderDataTable({
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
        select(
          time_period, geo_breakdown,
          Referrals, Re_referrals, Re_referrals_percent
        ) %>%
        arrange(desc(Re_referrals_percent))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cin_referrals %>%
        filter(geographic_level == "Local authority", time_period == max(cin_referrals$time_period)) %>%
        select(
          time_period, geo_breakdown,
          Referrals, Re_referrals, Re_referrals_percent
        ) %>%
        arrange(desc(Re_referrals_percent))
    }

    datatable(
      data,
      colnames = c(
        "Time period", "Geographical breakdown", "Referrals in the year",
        "Re-referrals within 12 months of a previous referral", "Re-referrals within 12 months (%)"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })


  # cin referral plot by region
  output$plot_cin_referral_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_cin_referral_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # cin referral chart by LA
  output$plot_cin_referral_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_cin_referral_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$plot_uasc <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_uasc(input$geographic_breakdown_o1, input$select_geography_o1) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$table_uasc <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    datatable(
      combined_cla_data %>%
        filter(
          geo_breakdown %in% input$geographic_breakdown_o1,
          characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children"),
          population_count == "Children starting to be looked after each year"
        ) %>%
        select(time_period, geo_breakdown, placement_per_10000, characteristic) %>%
        arrange(desc(time_period)),
      colnames = c("Time period", "Geographical breakdown", "Rate per 10,000 children", "UASC status"),
      options = list(
        scrollx = FALSE,
        paging = TRUE,
        target = "column"
      )
    )
  })

  output$plot_uasc_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_uasc_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$table_uasc_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    datatable(
      combined_cla_data %>% filter(
        geographic_level == "Regional", characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children"),
        population_count == "Children starting to be looked after each year",
        time_period == max(time_period)
      ) %>%
        select(time_period, geo_breakdown, placement_per_10000, characteristic),
      colnames = c("Time period", "Geographical breakdown", "Rate per 10,000 children", "UASC status"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  output$plot_uasc_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_uasc_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$table_uasc_la <- renderDataTable({
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
        select(time_period, geo_breakdown, placement_per_10000, characteristic) %>%
        arrange(desc(placement_per_10000))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- combined_cla_data %>%
        filter(
          geographic_level == "Local authority", time_period == max(combined_cla_data$time_period), characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children"),
          population_count == "Children starting to be looked after each year",
        ) %>%
        select(time_period, geo_breakdown, placement_per_10000, characteristic) %>%
        arrange(desc(placement_per_10000))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "Rate per 10,000 children", "UASC status"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # CLA Rate chart for March
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
      filter(population_count == "Children looked after at 31 March each year")

    # Set the max y-axis scale
    max_rate <- max(cla_rates$rate_per_10000[cla_rates$population_count == "Children looked after at 31 March each year"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "rate_per_10000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)


    ggplotly(p, height = 420) %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })

  # CLA rate march TABLE
  output$table_cla_rate_march <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, rate_per_10000, population_count)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cla_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, rate_per_10000, population_count)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, rate_per_10000, population_count)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, rate_per_10000, population_count)
    }

    datatable(
      filtered_data %>%
        filter(population_count == "Children looked after at 31 March each year") %>%
        select(time_period, geo_breakdown, rate_per_10000),
      colnames = c("Time period", "Geographical breakdown", "Rate per 10,000 children"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # CLA rate March regional plot
  output$plot_cla_march_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_cla_march_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # CLA rate March regional table
  output$table_cla_march_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    datatable(
      cla_rates %>% filter(geographic_level == "Regional", time_period == max(cla_rates$time_period), population_count == "Children looked after at 31 March each year") %>% select(
        time_period, geo_breakdown,
        rate_per_10000
      ) %>%
        arrange(desc(rate_per_10000)),
      colnames = c("Time period", "Geographical breakdown", "Rate per 10,000 children"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # CLA rate March LA plot
  output$plot_cla_march_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    ggplotly(
      plot_cla_march_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # CLA rate March La table
  output$table_cla_march_la <- renderDataTable({
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
        select(time_period, geo_breakdown, rate_per_10000) %>%
        arrange(desc(rate_per_10000))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cla_rates %>%
        filter(geographic_level == "Local authority", time_period == max(cla_rates$time_period), population_count == "Children looked after at 31 March each year") %>%
        select(
          time_period, geo_breakdown,
          rate_per_10000
        ) %>%
        arrange(desc(rate_per_10000))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "Rate per 10,000 children"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
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
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March", school_type == "Total")
      %>% select(`Overall absence (%)`), nsmall = 1)
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # CPPO
  output$absence_CPP_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March", school_type == "Total")
      %>% select(`Overall absence (%)`), nsmall = 1)
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # CLA
  output$absence_CLA_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March", school_type == "Total")
      %>% select(`Overall absence (%)`), nsmall = 1)
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # persistent absentees headline ----
  # CIN
  output$persistent_CIN_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March", school_type == "Total")
      %>% select(`Persistent absentees (%)`), nsmall = 1)
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # CPPO
  output$persistent_CPP_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March", school_type == "Total")
      %>% select(`Persistent absentees (%)`), nsmall = 1)
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # CLA
  output$persistent_CLA_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March", school_type == "Total")
      %>% select(`Persistent absentees (%)`), nsmall = 1)
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })


  # overall absence timeseries chart
  output$absence_time_series <- plotly::renderPlotly({
    validate(
      need(!is.null(input$select_geography_o1), "Select a geography level."),
      need(!is.null(input$geographic_breakdown_o1), "Select a breakdown.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Overall absence (%)", "Overall absence (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })


  # absence rate TABLE
  output$table_absence_rate <- renderDataTable({
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_overall`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_overall`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_overall`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_overall`)
    }

    datatable(
      filtered_data %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_overall`),
      colnames = c("Time period", "Geographical breakdown", "Social care group", "School type", "Total number of pupils", "Overall absence (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # Absence rate regional plot
  output$plot_absence_reg <- plotly::renderPlotly({
    data <- outcomes_absence %>%
      filter(school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    ggplotly(
      by_region_bar_plot(data, "Overall absence (%)", "Overall absence (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Absence rate regional table
  output$table_absence_reg <- renderDataTable({
    datatable(
      outcomes_absence %>% filter(
        geographic_level == "Regional", time_period == max(outcomes_absence$time_period),
        school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown
      ) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
        %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type,
          t_pupils, pt_overall
        ) %>%
        arrange(desc(`pt_overall`)),
      colnames = c(
        "Time period", "Geographical breakdown", "Social care group",
        "School type", "Total number of pupils", "Overall absence (%)"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })


  # absence by la
  output$plot_absence_la <- plotly::renderPlotly({
    data <- outcomes_absence %>%
      filter(school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown)
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Overall absence (%)", "Overall absence (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Absence by LA table
  output$table_absence_la <- renderDataTable({
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
        filter(school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type,
          t_pupils, pt_overall
        ) %>%
        arrange(desc(pt_overall))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_absence %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown,
          social_care_group, school_type, t_pupils, pt_overall
        ) %>%
        arrange(desc(pt_overall))
    }

    datatable(
      data,
      colnames = c(
        "Time period", "Geographical breakdown", "Social Care Group", "School type",
        "Total number of pupils", "Overall absence (%)"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })




  # persistent absence timeseries chart
  output$persistence_time_series <- plotly::renderPlotly({
    validate(
      need(!is.null(input$select_geography_o1), "Select a geography level."),
      need(!is.null(input$geographic_breakdown_o1), "Select a breakdown.")
    )
    # not both
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Persistent absentees (%)", "Persistent absentees (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })


  # persistent rate TABLE
  output$table_persistent_rate <- renderDataTable({
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_pupils_pa_10_exact`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_absence %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_pupils_pa_10_exact`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_pupils_pa_10_exact`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_absence %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_pupils_pa_10_exact`)
    }

    datatable(
      filtered_data %>%
        filter(school_type == "Total" & social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(time_period, geo_breakdown, social_care_group, school_type, t_pupils, `pt_pupils_pa_10_exact`),
      colnames = c("Time period", "Geographical breakdown", "Social care group", "School type", "Total number of pupils", "Persistence absentees (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })


  # Persistence absence regional plot
  output$plot_persistent_reg <- plotly::renderPlotly({
    data <- outcomes_absence %>%
      filter(school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    ggplotly(
      by_region_bar_plot(data, "Persistent absentees (%)", "Persistent absentees (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Persistence Absence regional table
  output$table_persistent_reg <- renderDataTable({
    datatable(
      outcomes_absence %>% filter(
        geographic_level == "Regional", time_period == max(outcomes_absence$time_period),
        school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown
      ) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
        %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type,
          t_pupils, pt_pupils_pa_10_exact
        ) %>%
        arrange(desc(`pt_pupils_pa_10_exact`)),
      colnames = c(
        "Time period", "Geographical breakdown", "Social care group",
        "School type", "Total number of pupils", "Persistent absentees (%)"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })



  # persistent absence by la
  output$plot_persistent_absence_la <- plotly::renderPlotly({
    data <- outcomes_absence %>%
      filter(school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown)
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Persistent absentees (%)", "Persistent absentees (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Persistent Absence by LA table
  output$table_persistent_absence_la <- renderDataTable({
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
        filter(school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type,
          t_pupils, pt_pupils_pa_10_exact
        ) %>%
        arrange(desc(pt_pupils_pa_10_exact))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_absence %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(school_type == "Total", social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown,
          social_care_group, school_type, t_pupils, pt_pupils_pa_10_exact
        ) %>%
        arrange(desc(pt_pupils_pa_10_exact))
    }

    datatable(
      data,
      colnames = c(
        "Time period", "Geographical breakdown", "Social Care Group", "School type",
        "Total number of pupils", "Persistent absentees (%)"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
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
    stat <- format(outcomes_ks2 %>% filter(time_period == max(outcomes_ks2$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March")
      %>% select(`Expected standard reading writing maths (%)`), nsmall = 1)
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CPPO
  output$KS2_CPP_headline_txt <- renderText({
    stat <- format(outcomes_ks2 %>% filter(time_period == max(outcomes_ks2$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March")
      %>% select(`Expected standard reading writing maths (%)`), nsmall = 1)
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CLA
  output$KS2_CLA_headline_txt <- renderText({
    stat <- format(outcomes_ks2 %>% filter(time_period == max(outcomes_ks2$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March")
      %>% select(`Expected standard reading writing maths (%)`), nsmall = 1)
    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # KS4 headline ----
  # CIN
  output$KS4_CIN_headline_txt <- renderText({
    stat <- format(outcomes_ks4 %>% filter(time_period == max(outcomes_ks4$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March")
      %>% select(`Average Attainment 8`), nsmall = 1)
    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CPPO
  output$KS4_CPP_headline_txt <- renderText({
    stat <- format(outcomes_ks4 %>% filter(time_period == max(outcomes_ks4$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March")
      %>% select(`Average Attainment 8`), nsmall = 1)
    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CLA
  output$KS4_CLA_headline_txt <- renderText({
    stat <- format(outcomes_ks4 %>% filter(time_period == max(outcomes_ks4$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March")
      %>% select(`Average Attainment 8`), nsmall = 1)
    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })


  # KS2 % expected plot
  output$plot_ks2_expected <- plotly::renderPlotly({
    validate(
      need(!is.null(input$select_geography_o1), "Select a geography level."),
      need(!is.null(input$geographic_breakdown_o1), "Select a breakdown.")
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


    ggplotly(p, height = 420) %>%
      layout(yaxis = list(range = c(0, 100)))
  })


  # ks2 TABLE
  output$table_ks2_expected <- renderDataTable({
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks2 %>%
        filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, pt_rwm_met_expected_standard)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- outcomes_ks2 %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, pt_rwm_met_expected_standard)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks2 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, pt_rwm_met_expected_standard)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- outcomes_ks2 %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, pt_rwm_met_expected_standard)
    }

    datatable(
      filtered_data %>%
        filter(social_care_group %in% input$attainment_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, pt_rwm_met_expected_standard),
      colnames = c("Time period", "Geographical breakdown", "Social care group", "Total number of eligible pupils", "Expected standard reading writing maths (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })


  # KS2 regional plot
  output$plot_ks2_reg <- plotly::renderPlotly({
    data <- outcomes_ks2 %>%
      filter(social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    ggplotly(
      by_region_bar_plot(data, "Expected standard reading writing maths (%)", "Expected standard combined (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # KS2 regional table
  output$table_ks2_reg <- renderDataTable({
    datatable(
      outcomes_ks2 %>% filter(
        geographic_level == "Regional", time_period == max(outcomes_ks2$time_period),
        social_care_group %in% input$wellbeing_extra_breakdown
      ) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
        %>%
        select(
          time_period, geo_breakdown, social_care_group,
          t_rwm_eligible_pupils, pt_rwm_met_expected_standard
        ) %>%
        arrange(desc(`pt_rwm_met_expected_standard`)),
      colnames = c(
        "Time period", "Geographical breakdown", "Social care group",
        "Total number of eligible pupils", "Expected standard reading writing maths (%)"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })


  # KS2 by la
  output$plot_KS2_la <- plotly::renderPlotly({
    data <- outcomes_ks2 %>%
      filter(social_care_group %in% input$wellbeing_extra_breakdown)
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Expected standard reading writing maths (%)", "Expected standard combined (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # KS2 by LA table
  output$table_KS2_la <- renderDataTable({
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
        filter(social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group,
          t_rwm_eligible_pupils, pt_rwm_met_expected_standard
        ) %>%
        arrange(desc(pt_rwm_met_expected_standard))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_ks2 %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown,
          social_care_group, t_rwm_eligible_pupils, pt_rwm_met_expected_standard
        ) %>%
        arrange(desc(pt_rwm_met_expected_standard))
    }

    datatable(
      data,
      colnames = c(
        "Time period", "Geographical breakdown", "Social Care Group",
        "Total number of eligible pupils", "Expected standard reading writing maths (%)"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })


  # KS4 % expected plot
  output$plot_ks4 <- plotly::renderPlotly({
    validate(
      need(!is.null(input$select_geography_o1), "Select a geography level."),
      need(!is.null(input$geographic_breakdown_o1), "Select a breakdown.")
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


    ggplotly(p, height = 420) %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })


  # persistent rate TABLE
  output$table_ks4 <- renderDataTable({
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

    datatable(
      filtered_data %>%
        filter(social_care_group %in% input$attainment_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(time_period, geo_breakdown, social_care_group, t_pupils, avg_att8),
      colnames = c("Time period", "Geographical breakdown", "Social care group", "Total number of pupils", "Average attainment 8 score"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # KS4 regional plot
  output$plot_ks4_reg <- plotly::renderPlotly({
    data <- outcomes_ks4 %>%
      filter(social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    ggplotly(
      by_region_bar_plot(data, "Average Attainment 8", "Average Attainment 8") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # KS4 regional table
  output$table_ks4_reg <- renderDataTable({
    datatable(
      outcomes_ks4 %>% filter(
        geographic_level == "Regional", time_period == max(outcomes_ks4$time_period),
        social_care_group %in% input$wellbeing_extra_breakdown
      ) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
        %>%
        select(
          time_period, geo_breakdown, social_care_group,
          t_pupils, avg_att8
        ) %>%
        arrange(desc(`avg_att8`)),
      colnames = c(
        "Time period", "Geographical breakdown", "Social care group",
        "Total number of pupils", "Average Attainment 8 score"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # KS4 by la
  output$plot_KS4_la <- plotly::renderPlotly({
    data <- outcomes_ks4 %>%
      filter(social_care_group %in% input$wellbeing_extra_breakdown)
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Average Attainment 8", "Average Attainment 8 score") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # KS4 by LA table
  output$table_KS4_la <- renderDataTable({
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
        filter(social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group,
          t_pupils, avg_att8
        ) %>%
        arrange(desc(avg_att8))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_ks4 %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown,
          social_care_group, t_pupils, avg_att8
        ) %>%
        arrange(desc(avg_att8))
    }

    datatable(
      data,
      colnames = c(
        "Time period", "Geographical breakdown", "Social Care Group",
        "Total number of pupils", "Average attainment 8 score"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })



  # Outcome 2 -----
  # Geographic breakdown o1 (list of either LA names or Region names)
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

  # Headline stat1 -----
  # output$SGO_headline_txt <- renderText({
  #   numerator <- ceased_cla_data %>% filter(time_period == max(ceased_cla_data$time_period)
  #                                           & geo_breakdown %in% input$geographic_breakdown_o2
  #                                           & cla_group == "Reason episode ceased"
  #                                           & characteristic == "Special guardianship orders") %>% select(number)
  #
  #   denominator <- ceased_cla_data %>% filter(time_period == max(ceased_cla_data$time_period)
  #                                             & geo_breakdown %in% input$geographic_breakdown_o2
  #                                             & cla_group == "Reason episode ceased"
  #                                             & characteristic == "Total") %>% select(number)
  #   percent <- (numerator/denominator)*100
  #
  #   stat <- round(percent, digits = 1)
  #   paste0(stat,"%","<br>","<p style='font-size:16px; font-weight:500;'>","(",max(ceased_cla_data$time_period),")", "</p>")
  # })
  #

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
        select(`Ceased (%)`)
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
        select(`Ceased (%)`)
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

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o2, input$geographic_breakdown_o2, "Ceased (%)", "Ceased (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })


  output$table_sgo_ceased <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% input$geographic_breakdown_o2)) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, perc)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) | geographic_level == "National") %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, perc)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name))) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, perc)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name) | geographic_level == "National")) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, perc)
    }
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "Characteristic", "Number", "Total", "Ceased (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
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

    ggplotly(
      by_region_bar_plot(data, "Ceased (%)", "Ceased (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # SGO by region table
  output$table_sgo_ceased_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    datatable(
      ceased_cla_data %>% filter(geographic_level == "Regional", time_period == max(ceased_cla_data$time_period)) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, perc) %>%
        arrange(desc(perc)),
      colnames = c("Time period", "Geographical breakdown", "Characteristic", "Ceased (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  ## By SGO by LA
  output$plot_SGO_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>% filter(characteristic == "Special guardianship orders")
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Special Guardianship orders by LA table
  output$table_sgo_la <- renderDataTable({
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
        select(time_period, geo_breakdown, characteristic, perc) %>%
        arrange(desc(perc))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- ceased_cla_data %>%
        filter(geographic_level == "Local authority", time_period == max(ceased_cla_data$time_period)) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, perc) %>%
        arrange(desc(perc))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "Characteristic", "Ceased (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
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

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o2, input$geographic_breakdown_o2, "Ceased (%)", "Ceased (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })


  output$table_cao_ceased <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% input$geographic_breakdown_o2)) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, perc)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) | geographic_level == "National") %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, perc)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name))) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, perc)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name) | geographic_level == "National")) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, perc)
    }
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "Characteristic", "Number", "Total", "Ceased (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # by region
  output$plot_cao_ceased_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>% filter(characteristic == "Residence order or child arrangement order granted")

    ggplotly(
      by_region_bar_plot(data, "Ceased (%)", "Ceased (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # turnover rate by region table
  output$table_cao_ceased_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    datatable(
      ceased_cla_data %>% filter(geographic_level == "Regional", time_period == max(ceased_cla_data$time_period)) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, perc) %>%
        arrange(desc(perc)),
      colnames = c("Time period", "Geographical breakdown", "Characteristic", "Ceased (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # by la
  output$plot_cao_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>% filter(characteristic == "Residence order or child arrangement order granted")
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # CAO by LA table
  output$table_cao_la <- renderDataTable({
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
        select(time_period, geo_breakdown, characteristic, perc) %>%
        arrange(desc(perc))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- ceased_cla_data %>%
        filter(geographic_level == "Local authority", time_period == max(ceased_cla_data$time_period)) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, perc) %>%
        arrange(desc(perc))
    }

    datatable(
      data,
      colnames = c("Time period", "Geographical breakdown", "Characteristic", "Ceased (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # ALL statistical neighbours -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Outcome 1 ------

  ## Enabler 2 ------
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
            dataTableOutput("table_turnover_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("turnover_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_turnover",
          label = "View chart as a table",
          help_text = (
            dataTableOutput("SN_turnover_tbl")
          )
        ),
        details(
          inputId = "sn_turnover_info",
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
          )
        )
      )
    }
  })
  # turnover SN plot and table alternative
  output$turnover_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    ggplotly(
      statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "turnover_rate_fte", "Turnover Rate %", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$SN_turnover_tbl <- renderDataTable({
    datatable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "turnover_rate_fte"),
      colnames = c("Geographical breakdown", "Turnover rate (FTE) %", "LA Selection"),
      options = list(
        scrollx = FALSE,
        paging = FALSE
      )
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
            dataTableOutput("table_agency_rate_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("agency_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_agency",
          label = "View chart as a table",
          help_text = (
            dataTableOutput("SN_agency_tbl")
          )
        ),
        details(
          inputId = "sn_agency_info",
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
          )
        )
      )
    }
  })
  # turnover SN plot and table alternative
  output$agency_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    ggplotly(
      statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "agency_rate_fte", "Agency worker rate (FTE) %", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$SN_agency_tbl <- renderDataTable({
    datatable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "agency_rate_fte"),
      colnames = c("Geographical breakdown", "Agency worker rate (FTE) %", "LA Selection"),
      options = list(
        scrollx = FALSE,
        paging = FALSE
      )
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
            dataTableOutput("table_vacancy_rate_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("vacancy_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_vacancy",
          label = "View chart as a table",
          help_text = (
            dataTableOutput("SN_vacancy_tbl")
          )
        ),
        details(
          inputId = "sn_vacancy_info",
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
          )
        )
      )
    }
  })
  # turnover SN plot and table alternative
  output$vacancy_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    ggplotly(
      statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "vacancy_rate_fte", "Vacancy rate (FTE) %", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$SN_vacancy_tbl <- renderDataTable({
    datatable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "vacancy_rate_fte"),
      colnames = c("Geographical breakdown", "vacancy rate (FTE) %", "LA Selection"),
      options = list(
        scrollx = FALSE,
        paging = FALSE
      )
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
