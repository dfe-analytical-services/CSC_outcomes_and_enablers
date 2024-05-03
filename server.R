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
  # outcome3
  iv$add_rule("select_geography_o3", sv_required())
  iv$add_rule("geographic_breakdown_o3", sv_required())
  # outcome4
  iv$add_rule("select_geography_o4", sv_required())
  iv$add_rule("geographic_breakdown_o4", sv_required())
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
      plotly_time_series_custom_scale(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2, "Turnover Rate Fte", "Turnover rate (FTE) %", 100) %>%
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
        select(time_period, geo_breakdown, `Turnover Rate Fte`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, `Turnover Rate Fte`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown, `Turnover Rate Fte`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, `Turnover Rate Fte`)
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
      by_region_bar_plot(workforce_data, "Turnover Rate Fte", "Turnover Rate (FTE) %", 100) %>%
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
      workforce_data %>% filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
        select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
        arrange(desc(`Turnover Rate Fte`)),
      colnames = c("Time period", "Region", "Turnover rate (FTE) %"),
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
      by_la_bar_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "Turnover Rate Fte", "Turnover Rate (FTE) %") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Turnover Rate by LA table
  output$table_turnover_la <- renderDataTable({ # renderReactable({
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
        select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
        arrange(desc(`Turnover Rate Fte`))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(
          time_period, geo_breakdown,
          `Turnover Rate Fte`
        ) %>%
        arrange(desc(`Turnover Rate Fte`))
    }

    # data2 <- data %>%
    #   select(time_period, geo_breakdown, turnover_rate_fte) %>%
    #   mutate(turnover_rate_fte = case_when(
    #     turnover_rate_fte == "z" ~ -400,
    #     turnover_rate_fte == "c" ~ -100,
    #     turnover_rate_fte == "k" ~ -200,
    #     turnover_rate_fte == "x" ~ -300,
    #     TRUE ~ as.numeric(turnover_rate_fte)
    #   )) %>%
    #   arrange(desc(turnover_rate_fte)) %>%
    #   rename(`Time period` = `time_period`, `Geographical breakdown` = `geo_breakdown`, `Turnover Rate (FTE) %` = `turnover_rate_fte`)
    #
    # reactable(
    #   data2,
    #   columns = list(
    #     `Turnover Rate (FTE) %` = colDef(cell = cellfunc, defaultSortOrder = "desc")
    #   ),
    #   defaultPageSize = 15,
    #   searchable = TRUE,
    # )

    datatable(
      data,
      colnames = c("Time period", "Local authority", "Turnover rate (FTE) %"),
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
      plotly_time_series_custom_scale(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2, "Agency Rate Fte", "Agency worker rate (FTE) %", 100) %>%
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
        select(time_period, geo_breakdown, "Agency Rate Fte")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, "Agency Rate Fte")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown, "Agency Rate Fte")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, "Agency Rate Fte")
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
      by_region_bar_plot(workforce_data, "Agency Rate Fte", "Agency worker rate (FTE) %", 100) %>%
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
        `Agency Rate Fte`
      ) %>%
        arrange(desc(`Agency Rate Fte`)),
      colnames = c("Time period", "Region", "Agency worker rate (FTE) %"),
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
      by_la_bar_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "Agency Rate Fte", "Agency worker rate (FTE) %") %>%
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
          "Agency Rate Fte"
        ) %>%
        arrange(desc(`Agency Rate Fte`))
    }

    datatable(
      data,
      colnames = c("Time period", "Local authority", "Agency worker rate (FTE) %"),
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
      plotly_time_series_custom_scale(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2, "Vacancy Rate Fte", "Vacancy rate (FTE) %", 100) %>%
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
        select(time_period, geo_breakdown, "Vacancy Rate Fte")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, "Vacancy Rate Fte")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown, "Vacancy Rate Fte")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, "Vacancy Rate Fte")
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
      by_la_bar_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "Vacancy Rate Fte", "Vacancy rate (FTE) %") %>%
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
        select(time_period, geo_breakdown, "Vacancy Rate Fte") %>%
        arrange(desc(vacancy_rate_fte))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(
          time_period, geo_breakdown,
          `Vacancy Rate Fte`
        ) %>%
        arrange(desc(`Vacancy Rate Fte`))
    }

    datatable(
      data,
      colnames = c("Time period", "Local authority", "Vacancy rate (FTE) %"),
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
      by_region_bar_plot(workforce_data, "Vacancy Rate Fte", "Vacancy rate (FTE) %", 100) %>%
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
        `Vacancy Rate Fte`
      ) %>%
        arrange(desc(`Vacancy Rate Fte`)),
      colnames = c("Time period", "Region", "Vacancy rate (FTE) %"),
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
      paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "in ", max(workforce_data$time_period), context, previous_year_value, " ", (max(workforce_data$time_period) - 1), "</p>")
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
    max_rate <- max(workforce_data$`Caseload Fte`, na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2, "Caseload Fte", "Average caseload (FTE)", max_rate) %>%
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
        select(time_period, geo_breakdown, `Caseload Fte`)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      filtered_data <- workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, `Caseload Fte`)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown, `Caseload Fte`)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)

      filtered_data <- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, `Caseload Fte`)
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
      by_region_bar_plot(workforce_data, "Caseload Fte", "Average Caseload (FTE)", 100) %>%
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
        "Caseload Fte"
      ) %>%
        arrange(desc(`Caseload Fte`)),
      colnames = c("Time period", "Region", "Average caseload (FTE)"),
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
      by_la_bar_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "Caseload Fte", "Average Caseload (FTE)") %>%
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
        select(time_period, geo_breakdown, "Caseload Fte") %>%
        arrange(desc(`Caseload Fte`))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>%
        filter(geographic_level == "Local authority", time_period == max(workforce_data$time_period)) %>%
        select(
          time_period, geo_breakdown,
          "Caseload Fte"
        ) %>%
        arrange(desc(`Caseload Fte`))
    }

    datatable(
      data,
      colnames = c("Time period", "Local authority", "Average caseload (FTE)"),
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
      need(input$geographic_breakdown_e2 != "", "Select a location."),
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
      filter(population_count == "Children starting to be looked after each year")

    # Set the max y-axis scale
    max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Rate Per 10000", "Rate per 10,000 children", max_rate) %>%
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

    datatable(
      filtered_data %>%
        filter(population_count == "Children starting to be looked after each year") %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`),
      colnames = c("Time period", "Geographical breakdown", "Number of children starting to be looked after", "Rate of children starting to be looked after, per 10,000"),
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
        number, "Rate Per 10000"
      ) %>%
        arrange(desc(`Rate Per 10000`)),
      colnames = c("Time period", "Region", "Number of children starting to be looked after", "Rate of children starting to be looked after, per 10,000"),
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
        select(time_period, geo_breakdown, number, `Rate Per 10000`) %>%
        arrange(desc(`Rate Per 10000`))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cla_rates %>%
        filter(geographic_level == "Local authority", time_period == max(cla_rates$time_period), population_count == "Children starting to be looked after each year") %>%
        select(
          time_period, geo_breakdown,
          number, `Rate Per 10000`
        ) %>%
        arrange(desc(`Rate Per 10000`))
    }

    datatable(
      data,
      colnames = c("Time period", "Local authority", "Number of children starting to be looked after", "Rate of children starting to be looked after, per 10,000"),
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
        %>% select(At31_episodes_rate), nsmall = 1)
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
        At31_episodes, At31_episodes_rate
      ) %>%
        arrange(desc(At31_episodes_rate)),
      colnames = c("Time period", "Region", "CIN number at 31 March", "CIN rate per 10,000"),
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
        select(time_period, geo_breakdown, At31_episodes, At31_episodes_rate) %>%
        arrange(desc(At31_episodes_rate))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cin_rates %>%
        filter(geographic_level == "Local authority", time_period == max(cin_rates$time_period)) %>%
        select(
          time_period, geo_breakdown,
          At31_episodes, At31_episodes_rate
        ) %>%
        arrange(desc(At31_episodes_rate))
    }

    datatable(
      data,
      colnames = c("Time period", "Local authority", "CIN number at 31 March", "CIN rates per 10,000"),
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
        select(time_period, geo_breakdown, At31_episodes, At31_episodes_rate)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      filtered_data <- cin_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, At31_episodes, At31_episodes_rate)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown, At31_episodes, At31_episodes_rate)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)

      filtered_data <- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, At31_episodes, At31_episodes_rate)
    }

    datatable(
      filtered_data %>%
        select(time_period, geo_breakdown, At31_episodes, At31_episodes_rate),
      colnames = c("Time period", "Geographical breakdown", "CIN number at 31 March", "CIN rate per 10,000"),
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
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Re_referrals_percentage", "Re-referrals (%)", 100) %>%
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
        "Time period", "Region", "Referrals in the year",
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
        "Time period", "Local authority", "Referrals in the year",
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
  ## UASC ------
  # UASC chart
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

  # UASC table
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
        select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
        arrange(desc(time_period)),
      colnames = c("Time period", "Geographical breakdown", "UASC status", "Number of children starting to be looked after", "Rate per 10,000 children"),
      options = list(
        scrollx = FALSE,
        paging = TRUE,
        target = "column"
      )
    )
  })

  # UASC chart by region
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

  # UASC table by region
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
        select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`),
      colnames = c("Time period", "Region", "UASC status", "Number of children starting to be looked", "Rate per 10,000 children"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # UASC plot by LA
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

  # UASC table by LA
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
        select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
        arrange(desc(`Placement Rate Per 10000`))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- combined_cla_data %>%
        filter(
          geographic_level == "Local authority", time_period == max(combined_cla_data$time_period), characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children"),
          population_count == "Children starting to be looked after each year",
        ) %>%
        select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
        arrange(desc(`Placement Rate Per 10000`))
    }

    datatable(
      data,
      colnames = c("Time period", "Local authority", "UASC status", "Number of children starting to be looked after", "Rate per 10,000 children"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
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
      filter(population_count == "Children looked after at 31 March each year")

    # Set the max y-axis scale
    max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children looked after at 31 March each year"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Rate Per 10000", "Rate per 10,000 children", max_rate) %>%
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

    datatable(
      filtered_data %>%
        filter(population_count == "Children looked after at 31 March each year") %>%
        select(time_period, geo_breakdown, number, `Rate Per 10000`),
      colnames = c("Time period", "Geographical breakdown", "Number of children looked after on 31 March", "Rate per 10,000 children"),
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
        number, `Rate Per 10000`
      ) %>%
        arrange(desc(`Rate Per 10000`)),
      colnames = c("Time period", "Region", "Number of children looked after on 31 March", "Rate per 10,000 children"),
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
        select(time_period, geo_breakdown, number, `Rate Per 10000`) %>%
        arrange(desc(`Rate Per 10000`))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cla_rates %>%
        filter(geographic_level == "Local authority", time_period == max(cla_rates$time_period), population_count == "Children looked after at 31 March each year") %>%
        select(
          time_period, geo_breakdown,
          number, `Rate Per 10000`
        ) %>%
        arrange(desc(`Rate Per 10000`))
    }

    datatable(
      data,
      colnames = c("Time period", "Local authority", "Number of children looked after on 31 March", "Rate per 10,000 children"),
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
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
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
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    ggplotly(
      by_region_bar_plot(data, "Overall absence (%)", "Overall absence (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Absence rate regional table
  output$table_absence_reg <- renderDataTable({
    datatable(
      outcomes_absence %>% filter(
        geographic_level == "Regional", time_period == max(outcomes_absence$time_period),
        school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown
      ) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
        %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type,
          t_pupils, pt_overall
        ) %>%
        arrange(desc(`pt_overall`)),
      colnames = c(
        "Time period", "Region", "Social care group",
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
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown)
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Overall absence (%)", "Overall absence (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Absence by LA table
  output$table_absence_la <- renderDataTable({
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
          time_period, geo_breakdown, social_care_group, school_type,
          t_pupils, pt_overall
        ) %>%
        arrange(desc(pt_overall))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_absence %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
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
        "Time period", "Local authority", "Social Care Group", "School type",
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
        filter(school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
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
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    ggplotly(
      by_region_bar_plot(data, "Persistent absentees (%)", "Persistent absentees (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Persistence Absence regional table
  output$table_persistent_reg <- renderDataTable({
    datatable(
      outcomes_absence %>% filter(
        geographic_level == "Regional", time_period == max(outcomes_absence$time_period),
        school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown
      ) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
        %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type,
          t_pupils, pt_pupils_pa_10_exact
        ) %>%
        arrange(desc(`pt_pupils_pa_10_exact`)),
      colnames = c(
        "Time period", "Region", "Social care group",
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
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown)
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Persistent absentees (%)", "Persistent absentees (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Persistent Absence by LA table
  output$table_persistent_absence_la <- renderDataTable({
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
          time_period, geo_breakdown, social_care_group, school_type,
          t_pupils, pt_pupils_pa_10_exact
        ) %>%
        arrange(desc(pt_pupils_pa_10_exact))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_absence %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
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
        "Time period", "Local authority", "Social Care Group", "School type",
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
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    ggplotly(
      by_region_bar_plot(data, "Expected standard reading writing maths (%)", "Expected standard combined (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # KS2 regional table
  output$table_ks2_reg <- renderDataTable({
    datatable(
      outcomes_ks2 %>% filter(
        geographic_level == "Regional", time_period == max(outcomes_ks2$time_period),
        social_care_group %in% input$attainment_extra_breakdown
      ) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
        %>%
        select(
          time_period, geo_breakdown, social_care_group,
          t_rwm_eligible_pupils, pt_rwm_met_expected_standard
        ) %>%
        arrange(desc(`pt_rwm_met_expected_standard`)),
      colnames = c(
        "Time period", "Region", "Social care group",
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
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_ks2 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown)
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Expected standard reading writing maths (%)", "Expected standard combined (%)") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # KS2 by LA table
  output$table_KS2_la <- renderDataTable({
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
          t_rwm_eligible_pupils, pt_rwm_met_expected_standard
        ) %>%
        arrange(desc(pt_rwm_met_expected_standard))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_ks2 %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(social_care_group %in% input$attainment_extra_breakdown) %>%
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
        "Time period", "Local authority", "Social Care Group",
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


    ggplotly(p, height = 420) %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })


  # KS4 rate TABLE
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
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    ggplotly(
      by_region_bar_plot(data, "Average Attainment 8", "Average Attainment 8", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # KS4 regional table
  output$table_ks4_reg <- renderDataTable({
    datatable(
      outcomes_ks4 %>% filter(
        geographic_level == "Regional", time_period == max(outcomes_ks4$time_period),
        social_care_group %in% input$attainment_extra_breakdown
      ) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
        %>%
        select(
          time_period, geo_breakdown, social_care_group,
          t_pupils, avg_att8
        ) %>%
        arrange(desc(`avg_att8`)),
      colnames = c(
        "Time period", "Region", "Social care group",
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
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_ks4 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown)
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Average Attainment 8", "Average Attainment 8 score") %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # KS4 by LA table
  output$table_KS4_la <- renderDataTable({
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
        select(
          time_period, geo_breakdown, social_care_group,
          t_pupils, avg_att8
        ) %>%
        arrange(desc(avg_att8))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_ks4 %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(social_care_group %in% input$attainment_extra_breakdown) %>%
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
        "Time period", "Local authority", "Social Care Group",
        "Total number of pupils", "Average attainment 8 score"
      ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
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

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o2, input$geographic_breakdown_o2, "Ceased (%)", "Reason ceased (%)", 100) %>%
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
        select(time_period, geo_breakdown, characteristic, number, Total, percentage)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) | geographic_level == "National") %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, percentage)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name))) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, percentage)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name) | geographic_level == "National")) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, percentage)
    }
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "Reason ceased", "Number", "Total ceased", "Reason ceased (%)"),
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
      by_region_bar_plot(data, "Ceased (%)", "Reason ceased (%)", 100) %>%
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
        select(time_period, geo_breakdown, characteristic, percentage) %>%
        arrange(desc(percentage)),
      colnames = c("Time period", "Region", "Reason ceased", "Reason ceased (%)"),
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
      by_la_bar_plot(data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Reason ceased (%)") %>%
        config(displayModeBar = F),
      height = 420
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
        select(time_period, geo_breakdown, characteristic, percentage) %>%
        arrange(desc(percentage))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- ceased_cla_data %>%
        filter(geographic_level == "Local authority", time_period == max(ceased_cla_data$time_period)) %>%
        filter(characteristic == "Special guardianship orders") %>%
        select(time_period, geo_breakdown, characteristic, percentage) %>%
        arrange(desc(percentage))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, characteristic, percentage) %>%
      mutate(percentage = case_when(
        percentage == "z" ~ -400,
        percentage == "c" ~ -100,
        percentage == "k" ~ -200,
        percentage == "x" ~ -300,
        TRUE ~ as.numeric(percentage)
      )) %>%
      arrange(desc(percentage)) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Reason ceased (%)` = `percentage`)

    reactable(
      data2,
      columns = list(
        `Reason ceased (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o2, input$geographic_breakdown_o2, "Ceased (%)", "Reason ceased (%)", 100) %>%
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
        select(time_period, geo_breakdown, characteristic, number, Total, percentage)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o2) && is.null(input$region_comparison_checkbox_o2)) {
      filtered_data <- ceased_cla_data %>%
        filter((geographic_level %in% input$select_geography_o2 & geo_breakdown %in% input$geographic_breakdown_o2) | geographic_level == "National") %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, percentage)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name))) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, percentage)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o2) && !is.null(input$region_comparison_checkbox_o2)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o2)

      filtered_data <- ceased_cla_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o2, location$region_name) | geographic_level == "National")) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, number, Total, percentage)
    }
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "Reason ceased", "Number", "Total ceased", "Reason ceased (%)"),
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
      by_region_bar_plot(data, "Ceased (%)", "Reason ceased (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # ceased by region table
  output$table_cao_ceased_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    datatable(
      ceased_cla_data %>% filter(geographic_level == "Regional", time_period == max(ceased_cla_data$time_period)) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, percentage) %>%
        arrange(desc(percentage)),
      colnames = c("Time period", "Region", "Reason ceased", "Reason ceased (%)"),
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
      by_la_bar_plot(data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Reason ceased (%)") %>%
        config(displayModeBar = F),
      height = 420
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
        select(time_period, geo_breakdown, characteristic, `Ceased (%)`) %>%
        arrange(desc(`Ceased (%)`))
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- ceased_cla_data %>%
        filter(geographic_level == "Local authority", time_period == max(ceased_cla_data$time_period)) %>%
        filter(characteristic == "Residence order or child arrangement order granted") %>%
        select(time_period, geo_breakdown, characteristic, `Ceased (%)`) %>%
        arrange(desc(`Ceased (%)`))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, characteristic, `Ceased (%)`) %>%
      # mutate(perc = case_when(
      #   perc == "z" ~ -400,
      #   perc == "c" ~ -100,
      #   perc == "k" ~ -200,
      #   perc == "x" ~ -300,
      #   TRUE ~ as.numeric(perc)
      # )) %>%
      arrange(desc(`Ceased (%)`)) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Reason ceased (%)` = `Ceased (%)`)

    reactable(
      data2,
      columns = list(
        `Reason ceased (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15, # 11 for stats neighbours, 15 for others?
      searchable = TRUE,
    )

    # datatable(
    #   data,
    #   colnames = c("Time period", "Geographical breakdown", "Characteristic", "Ceased (%)"),
    #   options = list(
    #     scrollx = FALSE,
    #     paging = TRUE
    #   )
    # )
  })


  # Outcome 3 -----
  # Geographic breakdown o3 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_o3
  }, {
    choices <- sort(unique(cla_rates[(cla_rates$geographic_level == input$select_geography_o3 & cla_rates$time_period == 2023), "geo_breakdown"]), decreasing = FALSE)

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
        filter(geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- repeat_cpp %>%
        filter((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name)))

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National"))
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "Repeat_CPP_percent", "Repeat CPP (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$table_repeat_cpp <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    # neither checkboxes
    if (is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% input$geographic_breakdown_o3)) %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- repeat_cpp %>%
        filter((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name))) %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- repeat_cpp %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National")) %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent)
    }
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "CPP Starts", "Repeat CPP", "Repeat CPP (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })


  # by region
  output$plot_cpp_repeat_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- repeat_cpp

    ggplotly(
      by_region_bar_plot(data, "Repeat_CPP_percent", "Repeat CPP (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # cpp by region table
  output$table_cpp_repeat_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    datatable(
      repeat_cpp %>% filter(geographic_level == "Regional", time_period == max(repeat_cpp$time_period)) %>%
        select(time_period, geo_breakdown, CPP_start, CPP_subsequent, Repeat_CPP_percent) %>%
        arrange(desc(Repeat_CPP_percent)),
      colnames = c("Time period", "Region", "CPP Starts", "Repeat CPP", "Repeat CPP (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # by la
  output$plot_cpp_repeat_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- repeat_cpp
    ggplotly(
      by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Repeat_CPP_percent", "Repeat CPP (%)") %>%
        config(displayModeBar = F),
      height = 420
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
        select(time_period, geo_breakdown, `Repeat_CPP_percent`) %>%
        arrange(desc(`Repeat_CPP_percent`))
    } else if (input$select_geography_o3 %in% c("Local authority", "National")) {
      data <- repeat_cpp %>%
        filter(geographic_level == "Local authority", time_period == max(ceased_cla_data$time_period)) %>%
        select(time_period, geo_breakdown, `Repeat_CPP_percent`) %>%
        arrange(desc(`Repeat_CPP_percent`))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, `Repeat_CPP_percent`) %>%
      # mutate(perc = case_when(
      #   perc == "z" ~ -400,
      #   perc == "c" ~ -100,
      #   perc == "k" ~ -200,
      #   perc == "x" ~ -300,
      #   TRUE ~ as.numeric(perc)
      # )) %>%
      arrange(desc(`Repeat_CPP_percent`)) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Repeat CPP (%)` = `Repeat_CPP_percent`)

    reactable(
      data2,
      columns = list(
        `Repeat CPP (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15, # 11 for stats neighbours, 15 for others?
      searchable = TRUE,
    )
  })

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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
          )
        )
      )
    }
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

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "X2_years_or_more_percent", "CPP 2+ years (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$table_duration_cpp <- renderDataTable({
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
    datatable(
      filtered_data,
      colnames = c("Time period", "Geographical breakdown", "CPP 2+ Years", "CPP 2+ Year (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # by region
  output$plot_cpp_duration_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- duration_cpp

    ggplotly(
      by_region_bar_plot(data, "X2_years_or_more_percent", "CPP 2+ years (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # by region table
  output$table_cpp_duration_reg <- renderDataTable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    datatable(
      duration_cpp %>% filter(geographic_level == "Regional", time_period == max(duration_cpp$time_period)) %>%
        select(time_period, geo_breakdown, X2_years_or_more, X2_years_or_more_percent) %>%
        arrange(desc(X2_years_or_more_percent)),
      colnames = c("Time period", "Geographical breakdown", "CPP 2+ Years", "CPP 2+ Year (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
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
    ggplotly(
      all_assessment_factors_plot(assessment_factors, af_child_abuse_extra_filter, selected_geo_breakdown = input$geographic_breakdown_o3) %>%
        config(displayModeBar = F),
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
      select(time_period, geo_breakdown, assessment_factor, Number) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Assessment factor" = "assessment_factor") %>%
      dplyr::arrange(desc(Number))

    reactable(
      data,
      columns = list(
        `Number` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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

    max_y_lim <- max(filtered_data$Number) + 100

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "Number", "Number", max_y_lim) %>%
        config(displayModeBar = F),
      height = 420
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
        select(time_period, geo_breakdown, assessment_factor, Number)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, Number)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name)) & assessment_factor %in% input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, Number)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, Number)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Number of cases` = `Number`)


    reactable(
      data,
      columns = list(
        `Number of cases` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
      rename("Number_of_cases" = "Number") %>%
      filter(time_period == max(time_period), geographic_level == "Regional")

    max_lim <- max(data$Number_of_cases) + 500

    ggplotly(
      by_region_bar_plot(data, "Number_of_cases", "Number of cases", max_lim) %>%
        config(displayModeBar = F),
      height = 420
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
      select(time_period, geo_breakdown, assessment_factor, Number) %>%
      arrange(desc(Number))

    datatable(
      data,
      colnames = c("Time period", "Location", "Assessment factor", "Number of cases"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
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

    max_y_lim <- max(data$Number) + 500

    p <- by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Number", "Number of cases") +
      scale_y_continuous(limits = c(0, max_y_lim))

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420
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
        select(time_period, geo_breakdown, assessment_factor, Number) %>%
        arrange(desc(Number))
    } else if (input$select_geography_o3 %in% c("Local authority", "National")) {
      data <- assessment_factors %>%
        filter(geographic_level == "Local authority", time_period == max(assessment_factors$time_period)) %>%
        filter(assessment_factor == input$assessment_factors_1) %>%
        select(time_period, geo_breakdown, assessment_factor, Number) %>%
        arrange(desc(Number))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, assessment_factor, Number) %>%
      mutate(Number = case_when(
        Number == "z" ~ -400,
        Number == "c" ~ -100,
        Number == "k" ~ -200,
        Number == "x" ~ -300,
        TRUE ~ as.numeric(Number)
      )) %>%
      arrange(desc(Number)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Number of cases` = `Number`)

    reactable(
      data2,
      columns = list(
        `Number of cases` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
    ggplotly(
      all_assessment_factors_plot(assessment_factors, extra_familial_harm_af, selected_geo_breakdown = input$geographic_breakdown_o3) %>%
        config(displayModeBar = F),
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
      select(time_period, geo_breakdown, assessment_factor, Number) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Assessment factor" = "assessment_factor") %>%
      dplyr::arrange(desc(Number))

    reactable(
      data,
      columns = list(
        `Number` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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

    max_y_lim <- max(filtered_data$Number) + 500

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "Number", "Number", max_y_lim) %>%
        config(displayModeBar = F),
      height = 420
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
        select(time_period, geo_breakdown, assessment_factor, Number)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o3) && is.null(input$region_comparison_checkbox_o3)) {
      filtered_data <- assessment_factors %>%
        filter(((geographic_level %in% input$select_geography_o3 & geo_breakdown %in% input$geographic_breakdown_o3) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, Number)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name)) & assessment_factor %in% input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, Number)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o3) && !is.null(input$region_comparison_checkbox_o3)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o3)

      filtered_data <- assessment_factors %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o3, location$region_name) | geographic_level == "National") & assessment_factor %in% input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, Number)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Number of cases` = `Number`)


    reactable(
      data,
      columns = list(
        `Number of cases` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
      rename("Number_of_cases" = "Number") %>%
      filter(time_period == max(time_period), geographic_level == "Regional")

    max_lim <- max(data$Number_of_cases) + 500

    ggplotly(
      by_region_bar_plot(data, "Number_of_cases", "Number of cases", max_lim) %>%
        config(displayModeBar = F),
      height = 420
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
      select(time_period, geo_breakdown, assessment_factor, Number) %>%
      arrange(desc(Number))

    datatable(
      data,
      colnames = c("Time period", "Location", "Assessment factor", "Number of cases"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
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

    max_y_lim <- max(data$Number) + 500

    p <- by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Number", "Number of cases") +
      scale_y_continuous(limits = c(0, max_y_lim))

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420
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
        select(time_period, geo_breakdown, assessment_factor, Number) %>%
        arrange(desc(Number))
    } else if (input$select_geography_o3 %in% c("Local authority", "National")) {
      data <- assessment_factors %>%
        filter(geographic_level == "Local authority", time_period == max(assessment_factors$time_period)) %>%
        filter(assessment_factor == input$assessment_factors_2) %>%
        select(time_period, geo_breakdown, assessment_factor, Number) %>%
        arrange(desc(Number))
    }

    data2 <- data %>%
      select(time_period, geo_breakdown, assessment_factor, Number) %>%
      mutate(Number = case_when(
        Number == "z" ~ -400,
        Number == "c" ~ -100,
        Number == "k" ~ -200,
        Number == "x" ~ -300,
        TRUE ~ as.numeric(Number)
      )) %>%
      arrange(desc(Number)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Number of cases` = `Number`)

    reactable(
      data2,
      columns = list(
        `Number of cases` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & characteristic %in% input$placement_type_breakdown)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & characteristic %in% input$placement_type_breakdown)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & characteristic %in% input$placement_type_breakdown)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & characteristic %in% input$placement_type_breakdown)
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Percent", "Placements (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
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
      filter(time_period == max(time_period), geographic_level == "Regional")

    ggplotly(
      by_region_bar_plot(data, "Percent", "Placements (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
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
    data <- placement_data %>% filter(characteristic == input$placement_type_breakdown, geographic_level == "Local authority", time_period == max(time_period))


    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Percent", "Placements (%)") +
      scale_y_continuous(limits = c(0, 100))

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420
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
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & placement_stability == "With 3 or more placements during the year")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- placement_changes_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") &
          placement_stability == "With 3 or more placements during the year")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_changes_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & placement_stability == "With 3 or more placements during the year")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- placement_changes_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & placement_stability == "With 3 or more placements during the year")
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Percent", "Percentage", 100) %>%
        config(displayModeBar = F),
      height = 420
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
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Percentage` = `Percent`)


    reactable(
      data,
      columns = list(
        `Percentage` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
      filter(time_period == max(time_period), geographic_level == "Regional")

    ggplotly(
      by_region_bar_plot(data, "Percent", "Percentage", 100) %>%
        config(displayModeBar = F),
      height = 420
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
      arrange(desc(Percent))

    data <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Percentage` = `Percent`)


    reactable(
      data,
      columns = list(
        `Percentage` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
    data <- placement_changes_data %>% filter(placement_stability == "With 3 or more placements during the year", geographic_level == "Local authority", time_period == max(time_period))


    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Percent", "Percentage") +
      scale_y_continuous(limits = c(0, 100))

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420
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
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Percentage` = `Percent`)

    reactable(
      data2,
      columns = list(
        `Percentage` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## Placement Distance -------------
  # output$placement_distance_txt <- renderText({
  #   if (input$geographic_breakdown_o4 == "") {
  #     stat <- "NA"
  #   } else {
  #     stat <- format(placement_data %>%
  #                      filter(time_period == max(placement_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
  #                      filter(characteristic == "Placed more than 20 miles from home") %>%
  #                      select(percentage), nsmall = 0)
  #   }
  #   paste0(
  #     stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_data$time_period), ")", "</p>"
  #   )
  # })


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

  # Headline stat
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

  # Time series chart
  output$care_activity_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_activity_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & age %in% input$leavers_age & activity == "Total in education, employment or training")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_activity_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & age %in% input$leavers_age & activity == "Total in education, employment or training")

      filtered_data <- care_leavers_activity_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & age %in% input$leavers_age & activity == "Total in education, employment or training")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_activity_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & age %in% input$leavers_age & activity == "Total in education, employment or training")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_activity_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & age %in% input$leavers_age & activity == "Total in education, employment or training")
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "percent", "Percent", 100) %>%
        config(displayModeBar = F),
      height = 420
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
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Activity` = `activity`, `Age range` = `age`, `Percent` = `percent`)

    reactable(
      data,
      columns = list(
        `Percent` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
      filter(age == input$leavers_age & activity == "Total in education, employment or training" & time_period == max(time_period) & geographic_level == "Regional")

    ggplotly(
      by_region_bar_plot(data, "percent", "Percent", 100) %>%
        config(displayModeBar = F),
      height = 420
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
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Activity` = `activity`, `Age range` = `age`, `Percent` = `percentage`)

    reactable(
      data,
      columns = list(
        `Percent` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
    data <- care_leavers_activity_data %>% filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & activity == "Total in education, employment or training")

    # max_y_lim <- max(data$Number) + 500

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "percent", "Percent") +
      scale_y_continuous(limits = c(0, 100))

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420
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
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Activity` = `activity`, `Age range` = `age`, `Percent` = `percent`)

    reactable(
      data2,
      columns = list(
        `Percent` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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

  # Time series chart
  output$care_accommodation_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )
    if (is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_accommodation_data %>%
        filter(geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4 & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable")

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_accommodation_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable")

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_accommodation_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable")

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_accommodation_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable")
    }

    ggplotly(
      plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "percent", "Percent", 100) %>%
        config(displayModeBar = F),
      height = 420
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
        select(time_period, geo_breakdown, accommodation_suitability, age, percent)

      # national only
    } else if (!is.null(input$national_comparison_checkbox_o4) && is.null(input$region_comparison_checkbox_o4)) {
      filtered_data <- care_leavers_accommodation_data %>%
        filter(((geographic_level %in% input$select_geography_o4 & geo_breakdown %in% input$geographic_breakdown_o4) | geographic_level == "National") & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        select(time_period, geo_breakdown, accommodation_suitability, age, percent)

      # regional only
    } else if (is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_accommodation_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name)) & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        select(time_period, geo_breakdown, accommodation_suitability, age, percent)

      # both selected
    } else if (!is.null(input$national_comparison_checkbox_o4) && !is.null(input$region_comparison_checkbox_o4)) {
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o4)

      filtered_data <- care_leavers_accommodation_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o4, location$region_name) | geographic_level == "National") & age %in% input$leavers_age & accommodation_suitability == "Accommodation considered suitable") %>%
        select(time_period, geo_breakdown, accommodation_suitability, age, percent)
    }

    data <- filtered_data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Accommodation suitability` = `accommodation_suitability`, `Age range` = `age`, `Percent` = `percent`)

    reactable(
      data,
      columns = list(
        `Percent` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
      filter(age == input$leavers_age & accommodation_suitability == "Accommodation considered suitable" & time_period == max(time_period) & geographic_level == "Regional")

    ggplotly(
      by_region_bar_plot(data, "percent", "Percent", 100) %>%
        config(displayModeBar = F),
      height = 420
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
      arrange(desc(percentage))

    data <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Accommodation suitability` = `accommodation_suitability`, `Age range` = `age`, `Percent` = `percentage`)

    reactable(
      data,
      columns = list(
        `Percent` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
    data <- care_leavers_accommodation_data %>% filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & accommodation_suitability == "Accommodation considered suitable")

    # max_y_lim <- max(data$Number) + 500

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "percent", "Percent") +
      scale_y_continuous(limits = c(0, 100))

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420
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
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Accommodation suitability` = `accommodation_suitability`, `Age range` = `age`, `Percent` = `percent`)

    reactable(
      data2,
      columns = list(
        `Percent` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
            dataTableOutput("table_cla_rate_la")
          )
        ),
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    filtered_data <- cla_rates %>% filter(population_count == "Children starting to be looked after each year")

    ggplotly(
      statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, "Rate Per 10000", "Rate per 10,000 children", max_rate) %>%
        config(displayModeBar = F),
      height = 420
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
            dataTableOutput("table_uasc_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("UASC_SN_plot"),
        # p("This is under development."),
        br(),
        details(
          inputId = "tbl_sn_uasc",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_uasc_tbl")
            # p("This is under development.")
          )
        ),
        details(
          inputId = "sn_usac_info",
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    ggplotly(
      statistical_neighbours_plot_uasc(combined_cla_data, input$geographic_breakdown_o1, input$select_geography_o1, "Placement Rate Per 10000", "Rate per 10,000 children", max_rate) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # cla UASC stats neighbour tables
  output$SN_uasc_tbl <- renderReactable({
    filtered_data <- combined_cla_data %>%
      filter(population_count == "Children starting to be looked after each year", characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")) # %>%
    # rename("Placement rate per 10000" = "placement_per_10000")

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
            dataTableOutput("table_cla_march_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
      )
      tagList(
        plotlyOutput("cla_march_SN_plot"),
        # p("This is under development."),
        br(),
        details(
          inputId = "tbl_sn_cla_march",
          label = "View chart as a table",
          help_text = (
            reactableOutput("SN_cla_march_tbl")
            # p("This is under development.")
          )
        ),
        details(
          inputId = "sn_cla_march_info",
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    filtered_data <- cla_rates %>% filter(population_count == "Children looked after at 31 March each year")

    ggplotly(
      statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, "Rate Per 10000", "Rate per 10,000 children", max_rate) %>%
        config(displayModeBar = F),
      height = 420
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
            dataTableOutput("table_cin_rates_la")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    ggplotly(
      statistical_neighbours_plot(cin_rates, input$geographic_breakdown_o1, input$select_geography_o1, "CIN_rate", "CIN rates per 10,000", max_rate) %>%
        config(displayModeBar = F),
      height = 420
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
            dataTableOutput("table_cin_referral_la")
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
            # p("This is under development.")
          )
        ),
        details(
          inputId = "sn_cin_referral_info",
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    ggplotly(
      statistical_neighbours_plot(cin_referrals, input$geographic_breakdown_o1, input$select_geography_o1, "Re_referrals_percentage", "Re-referrals (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
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
            dataTableOutput("table_absence_la")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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
    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Overall absence (%)", "Overall absence (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  # Absence SN table
  output$SN_absence_tbl <- renderReactable({
    filtered_data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      rename(`OA%` = `Overall absence (%)`, `Overall absence (%)` = `pt_overall`) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("social_care_group", "school_type", "t_pupils"), yvalue = "Overall absence (%)"),
      columns = list(
        `social_care_group` = colDef(name = "Social care group"), `school_type` = colDef(name = "School type"), `t_pupils` = colDef(name = "Total number of pupils"), `Overall Absence (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
            dataTableOutput("table_persistent_absence_la")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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
    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Persistent absentees (%)", "Persistent absentees (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # Persistent Absence SN table
  output$SN_persistent_absence_tbl <- renderReactable({
    filtered_data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      rename(`PA%` = `Persistent absentees (%)`, `Persistent absentees (%)` = `pt_pupils_pa_10_exact`) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("social_care_group", "school_type", "t_pupils"), yvalue = "Persistent absentees (%)"),
      columns = list(
        `social_care_group` = colDef(name = "Social care group"), `school_type` = colDef(name = "School type"), `t_pupils` = colDef(name = "Total number of pupils"), `Persistent Absentees (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
            dataTableOutput("table_KS2_la")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Expected standard reading writing maths (%)", "Expected standard combined (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
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
            dataTableOutput("table_KS4_la")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Average Attainment 8", "Average Attainment 8 score", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # KS4 attainment SN table
  output$SN_ks4_attain_tbl <- renderReactable({
    data <- outcomes_ks4 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      rename(`AA8` = `Average Attainment 8`, `Average Attainment 8` = `avg_att8`) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("social_care_group", "t_pupils"), yvalue = "Average Attainment 8"),
      columns = list(
        `Average Attainment 8` = colDef(cell = cellfunc, defaultSortOrder = "desc"), `t_pupils` = colDef(name = "Total number of pupils"), `social_care_group` = colDef(name = "Social care group")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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
    ggplotly(
      statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Reason ceased (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # output$SN_sgo_tbl <- renderDataTable({
  #   filtered_data <- ceased_cla_data %>% filter(characteristic == "Special guardianship orders")
  #
  #   datatable(
  #     stats_neighbours_table(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, "percentage"),
  #     colnames = c("Geographical breakdown", "Ceased (%)", "LA Selection"),
  #     options = list(
  #       scrollx = FALSE,
  #       paging = FALSE
  #     )
  #   )
  # })

  output$SN_sgo_tbl <- renderReactable({
    filtered_data <- ceased_cla_data %>% filter(characteristic == "Special guardianship orders")

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, yvalue = "percentage"),
      columns = list(
        Percentage = colDef(name = "Reason ceased (%)", cell = cellfunc, defaultSortOrder = "desc")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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
    ggplotly(
      statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Reason ceased (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # output$SN_cao_tbl <- renderDataTable({
  #   filtered_data <- ceased_cla_data %>% filter(characteristic == "Residence order or child arrangement order granted")
  #   datatable(
  #     stats_neighbours_table(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, "percentage"),
  #     colnames = c("Geographical breakdown", "Ceased (%)", "LA Selection"),
  #     options = list(
  #       scrollx = FALSE,
  #       paging = FALSE
  #     )
  #   )
  # })

  output$SN_cao_tbl <- renderReactable({
    filtered_data <- ceased_cla_data %>% filter(characteristic == "Residence order or child arrangement order granted")

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, yvalue = "Ceased (%)"),
      columns = list(
        `Ceased (%)` = colDef(name = "Reason ceased (%)", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ## Outcome 3 -----
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
            # p("table here")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    max_y_lim <- max(data$Number) + 500

    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Number", "Number of cases", max_y_lim) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$abuse_neg_SN_tbl <- renderReactable({
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_1, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Number of cases" = "value")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, yvalue = "Number of cases"),
      columns = list(
        `Number Of Cases` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    max_y_lim <- max(data$Number) + 500

    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Number", "Number of cases", max_y_lim) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$efh_SN_tbl <- renderReactable({
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_2, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Number of cases" = "value")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, yvalue = "Number of cases"),
      columns = list(
        `Number Of Cases` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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
      filter(characteristic == input$placement_type_breakdown, geographic_level == "Local authority", time_period == max(time_period))

    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Percent", "Placements (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$placement_type_SN_tbl <- renderReactable({
    data <- placement_data %>%
      filter(characteristic == input$placement_type_breakdown, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Placements (%)" = "Percent")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Placements (%)"),
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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

    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Percent", "Percentage", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$placement_changes_SN_tbl <- renderReactable({
    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename(`Percentage2` = `Percentage`, `Percentage` = `Percent`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Percent"),
      columns = list(
        `Percentage` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Placement Distance ------------------




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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & activity == "Total in education, employment or training")

    # max_y_lim <- max(data$Number) + 500

    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "percent", "Percent", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$cl_activity_SN_tbl <- renderReactable({
    data <- care_leavers_activity_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & activity == "Total in education, employment or training") %>%
      rename("Percent" = "percent")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Percent"),
      columns = list(
        `Percent` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
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
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & accommodation_suitability == "Accommodation considered suitable")

    # max_y_lim <- max(data$Number) + 500

    ggplotly(
      statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "percent", "Percent", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$cl_acccomm_SN_tbl <- renderReactable({
    data <- care_leavers_accommodation_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & accommodation_suitability == "Accommodation considered suitable") %>%
      rename("Percent" = "percent")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Percent"),
      columns = list(
        `Percent` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


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
            # dataTableOutput("SN_turnover_tbl")
            reactableOutput("SN_turnover_tbl")
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
      statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "Turnover Rate Fte", "Turnover Rate %", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$SN_turnover_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Turnover Rate Fte"),
      columns = list(
        `Turnover Rate Fte` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
            reactableOutput("SN_agency_tbl")
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
      statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "Agency Rate Fte", "Agency worker rate (FTE) %", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$SN_agency_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Agency Rate Fte"),
      columns = list(
        `Agency Rate Fte` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
            reactableOutput("SN_vacancy_tbl")
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
      statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "Vacancy Rate Fte", "Vacancy rate (FTE) %", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$SN_vacancy_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Vacancy Rate Fte"),
      columns = list(
        `Vacancy Rate Fte` = colDef(cell = cellfunc, defaultSortOrder = "desc")
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
            dataTableOutput("table_caseload_la")
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
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
          label = "Additional information",
          help_text = (
            p("Additional information about stats neighbours file.")
          )
        )
      )
    }
  })
  # turnover SN plot and table alternative
  output$caseload_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    ggplotly(
      statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, "Caseload Fte", "Average Caseload (FTE)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$SN_caseload_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Caseload Fte"),
      columns = list(
        `Caseload Fte` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # Repeat CPP SN plot and table alternative
  output$cpp_repeat_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority.")
    )
    filtered_data <- repeat_cpp
    ggplotly(
      statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o3, input$select_geography_o3, "Repeat_CPP_percent", "Repeat CPP (%)", 100) %>%
        config(displayModeBar = F),
      height = 420
    )
  })


  output$SN_cpp_repeat_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(repeat_cpp, input$geographic_breakdown_o3, input$select_geography_o3, yvalue = "Repeat_CPP_percent"),
      columns = list(
        `Repeat Cpp Percent` = colDef(name = "Repeat CPP (%)", cell = cellfunc, defaultSortOrder = "desc")
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
