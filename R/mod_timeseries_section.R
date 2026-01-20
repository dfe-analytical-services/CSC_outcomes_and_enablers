# The timeseries_section module consists of a server and a ui along with several supporting functions which relate solely to the module


# This component contains a timeseries plot, a reactable of the same data and a download button, wrapped in a details section
# Here is the simple layout of components rendered by the server function of this module
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
    ),
    details(
      inputId = paste0(id, "_ts_info"),
      label = "Additional information:",
      help_text = get_additional_info(id)
    )
  )
}


# THis is the server part of the module which returns 2 outputs: the plot and the table
timeseries_section_server <- function(id,
                                      rv_geo_filters,
                                      rv_dimensional_filters,
                                      dataset,
                                      chart_title = "",
                                      yvalue,
                                      yaxis_title,
                                      max_rate,
                                      rt_columns,
                                      rt_col_defs,
                                      decimal_percentage) {
  # the module server contains all of the backend logic for this module
  moduleServer(id, function(input, output, session) {
    # we start with a data reactive which is filtering the dataset for chosen geographies (and additional dimensions tbc)
    filtered_data <- reactive({
      req(rv_geo_filters$select_geographic_level)
      filter_time_series_data(
        dataset_in = dataset,
        select_geographic_level = rv_geo_filters$select_geographic_level,
        select_geo_breakdown = rv_geo_filters$select_geo_breakdown,
        check_compare_national = rv_geo_filters$check_compare_national,
        check_compare_regional = rv_geo_filters$check_compare_regional,
        check_compare_sn = rv_geo_filters$check_compare_sn,
        dimensional_filters = rv_dimensional_filters$dimensional_filters
      )
    })

    max_yvalue <- calculate_max_rate(dataset, column_name = yvalue)

    # prepare a chart and then render it
    output$timeseries_plot <- renderPlotly({
      req(filtered_data())
      p <- plotly_time_series_custom_scale(
        dataset = filtered_data(),
        level = rv_geo_filters$select_geography,
        breakdown = rv_dimensional_filters$select_geo_breakdown,
        yvalue = yvalue,
        yaxis_title = yaxis_title,
        ylim_upper = max_yvalue,
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

    # prepare a table and render it
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
  })
}

# function to calculate the max rate for the y-axis
calculate_max_rate <- function(dataset, column_name, ceiling_adjustment = 20) {
  max_rate <- max(dataset[[column_name]], na.rm = TRUE)
  max_rate <- ceiling(max_rate / ceiling_adjustment) * ceiling_adjustment
}
