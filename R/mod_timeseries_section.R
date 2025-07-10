# The timeseries_section module consists of a server and a ui.

timeseries_section_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("timeseries_plot")),
    br(),
    details(
      inputId = paste0("tbl_", id),
      label = "View chart as a table",
      help_text = (
        HTML(paste0(
          csvDownloadButton(ns("timeseries_table"), filename = paste0("tbl_", id, ".csv")),
          reactableOutput(ns("timeseries_table"))
        ))
      )
    )
  )
}



calculate_max_rate <- function(dataset, column_name) {
  max_rate <- max(dataset[[column_name]], na.rm = TRUE)
  max_rate <- ceiling(max_rate / 20) * 20
}

timeseries_section_server <- function(id, rv, dataset, chart_title = "My Chart", yvalue, yaxis_title, max_rate, rt_columns, rt_col_defs, decimal_percentage) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
      req(rv$select_geographic_level)
      filter_time_series_data(
        dataset_in = dataset,
        select_geographic_level = rv$select_geographic_level,
        select_geo_breakdown = rv$select_geo_breakdown,
        check_compare_national = rv$check_compare_national,
        check_compare_regional = rv$check_compare_regional,
        check_compare_sn = rv$check_compare_sn
      )
    })
    # print(head(filtered_data))
    # rv_hosp_admissions$filtered_data <- filtered_data


    output$timeseries_plot <- renderPlotly({
      req(filtered_data())
      p <- plotly_time_series_custom_scale(
        dataset = filtered_data(),
        level = rv$select_geography,
        breakdown = rv$select_geo_breakdown,
        yvalue = yvalue, # "Repeat CPP (%)",
        yaxis_title = yaxis_title, # "Repeat CPP (%)",
        ylim_upper = max_rate,
        decimal_percentage = TRUE
      ) %>%
        config(displayModeBar = F)

      p <- p + ggtitle(chart_title)

      ggplotly(
        p,
        height = 420,
        tooltip = "text"
      ) %>%
        layout(yaxis = list(tickmode = "auto")) %>%
        config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
    })

    output$timeseries_table <- renderReactable({
      req(filtered_data())
      filtered_data() %>%
        select(any_of(as.character(rt_columns))) %>%
        setnames(names(rt_columns)) %>%
        reactable(
          defaultColDef = colDef(align = "center"),
          columns = rt_col_defs,
          defaultPageSize = 15, # 11 for stats neighbours, 15 for others?
          searchable = TRUE,
        )
    })

    # prepare a chart and then render it




    # prepare a table and render it
  })
}








# output$repeat_cpp_time_series <- plotly::renderPlotly({
#   shiny::validate(
#     need(input$select_geography_o3 != "", "Select a geography level."),
#     need(input$geographic_breakdown_o3 != "", "Select a location.")
#   )
#
#   # filter the dataset based on the context and user selections
#   filtered_data <- filter_time_series_data(
#     dataset_in = repeat_cpp,
#     select_geographic_level = input$select_geography_o3,
#     select_geo_breakdown = input$geographic_breakdown_o3,
#     check_compare_national = input$national_comparison_checkbox_o3,
#     check_compare_regional = input$region_comparison_checkbox_o3,
#     check_compare_sn = input$sn_comparison_checkbox_o3,
#     dimensional_filters = list()
#   ) %>%
#     rename("Repeat CPP (%)" = "Repeat_CPP_percent")
#
#   max_rate <- max(repeat_cpp$`Repeat_CPP_percent`, na.rm = TRUE)
#   max_rate <- ceiling(max_rate / 20) * 20
#
#   p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "Repeat CPP (%)", "Repeat CPP (%)", max_rate, decimal_percentage = TRUE) %>%
#     config(displayModeBar = F)
#   p <- p + ggtitle("Repeat CPP (%)")
#
#   ggplotly(
#     p,
#     height = 420,
#     tooltip = "text"
#   ) %>%
#     layout(yaxis = list(tickmode = "auto")) %>%
#     config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
# })
#
# ### Repeat CPP time series table ----
# output$table_repeat_cpp <- renderReactable({
#   shiny::validate(
#     need(input$select_geography_o3 != "", "Select a geography level."),
#     need(input$geographic_breakdown_o3 != "", "Select a location.")
#   )
#
#   # filter the dataset based on the context and user selections
#   filtered_data <- filter_time_series_data(
#     dataset_in = repeat_cpp,
#     select_geographic_level = input$select_geography_o3,
#     select_geo_breakdown = input$geographic_breakdown_o3,
#     check_compare_national = input$national_comparison_checkbox_o3,
#     check_compare_regional = input$region_comparison_checkbox_o3,
#     check_compare_sn = input$sn_comparison_checkbox_o3,
#     dimensional_filters = list()
#   ) %>%
#     select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent) %>%
#     rename("Time period" = "time_period", "Location" = "geo_breakdown", "CPP Starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent", "Repeat CPP (%)" = "CPP_subsequent_percent")
#
#   reactable(
#     filtered_data,
#     defaultColDef = colDef(align = "center"),
#     columns = list(
#       `CPP Starts` = colDef(cell = cellfunc),
#       `Repeat CPP` = colDef(cell = cellfunc),
#       `Repeat CPP (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
#     ),
#     defaultPageSize = 15,
#     searchable = TRUE,
#   )
# })
