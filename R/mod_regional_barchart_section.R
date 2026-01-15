###  DRAFT .....


# The regional barchart module consists of a server and a ui along with several supporting functions which relate solely to the module


# This component contains a barchart plot, a reactable of the same data and a download button, wrapped in a details section
# Here is the simple layout of components rendered by the server function of this module
regional_barchart_section_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
    br(),
    plotlyOutput(ns("regional_barchart_plot")),
    br(),
    details(
      inputId = paste0("tbl_", id),
      label = "View chart as a table",
      help_text = (
        HTML(paste0(
          csvDownloadButton(ns("regional_barchart_table"), filename = paste0("tbl_", id, ".csv")),
          reactableOutput(ns("regional_barchart_table"))
        ))
      )
    )
  )
}


# THis is the server part of the module which returns 2 outputs: the plot and the table
regional_barchart_section_server <- function(id,
                                             rv_geo_filters,
                                             rv_dimensional_filters,
                                             dataset,
                                             chart_title = "",
                                             yvalue,
                                             yaxis_title,
                                             max_rate = NULL,
                                             rt_columns,
                                             rt_col_defs,
                                             decimal_percentage) {
  moduleServer(id, function(input, output, session) {
    # we start with a data reactive which is filtering the dataset for chosen geographies (and additional dimensions tbc)
    filtered_data <- reactive({
      # apply any dimensional filters for this dataset (e.g. characteristic, placement type, assessment factor)
      if (length(rv_dimensional_filters$dimensional_filters) > 0) {
        dataset <- dataset[eval(AndEQUAL(rv_dimensional_filters$dimensional_filters))]
      }
      dataset[geographic_level == "Regional" & time_period == max(dataset$time_period)]
    })

    # prepare a chart and then render it
    output$regional_barchart_plot <- plotly::renderPlotly({
      req(filtered_data())
      max_rate <- max(filtered_data()[[yvalue]], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 10) * 10

      p <- by_region_bar_plot(
        dataset = filtered_data(),
        yvalue = yvalue,
        yaxis_title = yaxis_title,
        yupperlim = max_rate,
        decimal_percentage = TRUE
      ) %>%
        config(displayModeBar = F)

      # we need to construct the chart title

      # title <- paste0("Social worker turnover rate (FTE) % by region (", max(p$data$time_period), ")")
      p <- p + ggtitle(chart_title)

      ggplotly(
        p,
        height = 420,
        tooltip = "text"
      ) %>%
        config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
    })

    # prepare a table and render it
    output$regional_barchart_table <- renderReactable({
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
calculate_max_rate <- function(dataset, column_name) {
  max_rate <- max(dataset[[column_name]], na.rm = TRUE)
  max_rate <- ceiling(max_rate / 20) * 20
}


#
#
# output$plot_turnover_reg <- plotly::renderPlotly({
#
# })
#
# # turnover rate by region table
# output$table_turnover_reg <- renderReactable({
#   shiny::validate(
#     need(input$select_geography_e3 != "", "Select a geography level."),
#     #   need(input$geographic_breakdown_e3 != "", "Select a location.")
#   )
#   data <- workforce_data %>%
#     filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
#     select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
#     arrange(desc(`Turnover Rate Fte`)) %>%
#     rename("Time period" = "time_period", "Region" = "geo_breakdown", "Turnover rate (FTE) %" = "Turnover Rate Fte")
#
#   reactable(
#     data,
#     defaultColDef = colDef(align = "center"),
#     columns = list(
#       `Turnover rate (FTE) %` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
#     ),
#     defaultPageSize = 10,
#     searchable = TRUE,
#   )
# })
