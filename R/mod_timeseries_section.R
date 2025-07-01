timeseries_section_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$p(paste("Hello", id)),
    textOutput(ns("selected_geo_breakdown"))
  )
}

timeseries_section_server <- function(id, dataset, selected_geography, selected_geo_breakdown) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(selected_geography))
    stopifnot(is.reactive(selected_geo_breakdown))
    # select_geography <- reactive(selected_values[[paste0("select_geography_", page_indicator)]])
    # select_geo_breakdown <- reactive(selected_values[[paste0("geo_breakdown_", page_indicator)]])
    # shiny::validate(
    #   need(selected_geography() != "", "Select a geography level."),
    #   need(selected_geo_breakdown() != "", "Select a location.")
    # )
    # need to filter a timeseries here
    # filtered_data <- filter_time_series_data(dataset_in = dataset, select_geographic_level = )
    observeEvent(selected_geo_breakdown(), {
      output$selected_geo_breakdown <- renderText({
        browser()
        req(selected_geo_breakdown())
        selected_geo_breakdown()
      })
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
