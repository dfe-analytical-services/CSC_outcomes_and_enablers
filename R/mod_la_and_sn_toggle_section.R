la_and_sn_toggle_section_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(
      ns("la_sn_toggle_button"),
      label = NULL,
      choices = c("All local authorities", "10 statistical neighbours"),
      selected = "All local authorities",
      justified = TRUE
    ),
    uiOutput(ns("la_sn_toggle_output")),
  )
}


la_and_sn_toggle_section_server <- function(id,
                                            rv_geo_filters,
                                            rv_dimensional_filters,
                                            dataset,
                                            chart_title,
                                            yvalue,
                                            yaxis_title,
                                            max_rate,
                                            rt_columns,
                                            rt_col_defs,
                                            decimal_percentage,
                                            selectedcolumn = NULL) {
  # this is he definition of the module server which contains reactive datasets and render functions
  moduleServer(id, function(input, output, session) {
    # this is constant for the dataset driving the specific instance of the module
    max_time_period <- max(dataset$time_period, na.rm = TRUE)

    max_yvalue <- calculate_max_rate(dataset, column_name = yvalue)

    # we start with the data reactives which are filtering the datasets for chosen geographies (and additional dimensions tbc)
    filtered_data_la <- reactive({
      req(isolate(rv_geo_filters$select_geographic_level))
      req(rv_geo_filters$select_geo_breakdown)

      filter_la_toggle_dataset(
        dataset_in = dataset,
        select_geographic_level = rv_geo_filters$select_geographic_level,
        select_geo_breakdown = rv_geo_filters$select_geo_breakdown,
        select_time_period = max_time_period,
        dimensional_filters = rv_dimensional_filters$dimensional_filters
      ) %>%
        arrange(desc(!!sym(yvalue)), geo_breakdown)
    })


    filtered_data_sn <- reactive({
      # sn_names <- stat_neighbours_for_la(selected_geo_breakdown)
      filter_sn_toggle_dataset(
        dataset_in = dataset,
        select_geographic_level = rv_geo_filters$select_geographic_level,
        select_geo_breakdown = rv_geo_filters$select_geo_breakdown,
        select_time_period = max_time_period,
        dimensional_filters = rv_dimensional_filters$dimensional_filters
      ) %>%
        arrange(desc(!!sym(yvalue)), geo_breakdown)
    })


    ## Rendering out the UI portion
    output$la_sn_toggle_output <- renderUI({
      ns <- session$ns
      if (input$la_sn_toggle_button == "All local authorities") { # LA Chart and table
        tagList(
          plotlyOutput(ns("plot_la_toggle")),
          br(),
          p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
          br(),
          details(
            inputId = paste0("tbl_la_", id),
            label = "View chart as a table",
            help_text = (
              HTML(paste0(
                csvDownloadButton(ns("table_la_toggle"), filename = paste0("tbl_la_", id, ".csv")),
                reactableOutput(ns("table_la_toggle"))
              ))
            )
          ),
          details(
            inputId = paste0(id, "_la_info"),
            label = "Additional information:",
            help_text = get_additional_info(id)
          )
        )
      } else { # UI for the stats neighbours plot and table
        validate(
          need(rv_geo_filters$select_geographic_level == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
          need(rv_geo_filters$select_geo_breakdown != "", "Select a location."),
        )
        tagList(
          plotlyOutput(ns("plot_sn_toggle")),
          br(),
          details(
            inputId = "tbl_sn_toggle",
            label = "View chart as a table",
            help_text = (
              HTML(paste0(
                csvDownloadButton(ns("table_sn_toggle"), filename = paste0("Stat_Neighbours_Plot_", rv_geo_filters$select_geo_breakdown, ".csv")),
                reactableOutput(ns("table_sn_toggle"))
              ))
            )
          ),
          details(
            inputId = paste0(id, "_sn_info"),
            label = "Additional information:",
            help_text = (
              tags$ul(
                tags$li("The ‘Children’s services statistical neighbour benchmarking tool’ was used to select each local authority’s ’10 closest statistical neighbours’ (local authorities with similar characteristics)."),
                tags$li("The 10 closest local authorities are based on a weighted “distance” calculation across a range of local socio-economic/ characteristic/ demographic variables – which are deemed to have strong relationships with the Children’s Services policy indicators (the types of measures in this dashboard)."),
                br(),
                p(
                  "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                  tags$br(),
                  "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
                )
              )
            )
          )
        )
      }
    })


    ### LA plot toggle
    output$plot_la_toggle <- plotly::renderPlotly({
      req(filtered_data_la(), nrow(filtered_data_la()) > 0)

      shiny::validate(
        need(rv_geo_filters$select_geographic_level != "", "Select a geography level."),
        need(rv_geo_filters$select_geo_breakdown != "", "Select a location.")
      )

      p <- by_la_bar_plot_revised(filtered_data_la(), rv_geo_filters$select_geographic_level, rv_geo_filters$select_geo_breakdown, yvalue, yaxis_title, max_yvalue, decimal_percentage = decimal_percentage) %>%
        config(displayModeBar = F)

      # we need to construct the chart title
      chart_title <- paste0(chart_title, " (", max(p$data$time_period), ")")
      p <- p + ggtitle(chart_title)

      ggplotly(
        p,
        height = 420,
        tooltip = "text"
      ) %>%
        config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
    })


    # LA table rendering
    output$table_la_toggle <- renderReactable({
      req(filtered_data_la())
      shiny::validate(
        need(rv_geo_filters$select_geographic_level != "", "Select a geography level."),
        need(rv_geo_filters$select_geo_breakdown != "", "Select a location.")
      )
      # build the dataset for the table
      filtered_data_la() %>%
        select(any_of(as.character(rt_columns))) %>%
        setnames(names(rt_columns)) %>%
        reactable(
          defaultColDef = colDef(align = "center"),
          columns = rt_col_defs,
          defaultPageSize = 10, # 11 for stats neighbours, 15 for others?
          searchable = TRUE,
        )
    })

    # caseload SN plot and table alternative
    output$plot_sn_toggle <- plotly::renderPlotly({
      req(filtered_data_sn(), nrow(filtered_data_sn()) > 0)
      validate(
        need(rv_geo_filters$select_geographic_level == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(rv_geo_filters$select_geo_breakdown != "", "Select a location."),
      )

      # max_rate <- max(filtered_data_sn()[[yvalue]], na.rm = TRUE)
      # max_rate <- ceiling(max_rate / 10) * 10

      p <- statistical_neighbours_plot_revised(
        dataset = filtered_data_sn(),
        selected_geo_lvl = rv_geo_filters$select_geographic_level,
        selected_geo_breakdown = rv_geo_filters$select_geo_breakdown,
        yvalue = yvalue,
        yaxis_title = yaxis_title,
        ylim_upper = max_yvalue,
        decimal_percentage = decimal_percentage
      ) %>%
        config(displayModeBar = F)
      # p <- p + ggtitle("Average caseload (FTE) by statistical neighbours")
      title <- paste0(chart_title, " (", max(filtered_data_sn()$time_period), ")")
      p <- p + ggtitle(title)

      ggplotly(
        p,
        height = 420,
        tooltip = "text"
      ) %>%
        config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
    })



    output$table_sn_toggle <- renderReactable({
      req(filtered_data_sn(), nrow(filtered_data_sn()) > 0)

      shiny::validate(
        need(rv_geo_filters$select_geographic_level != "", "Select a geography level."),
        need(rv_geo_filters$select_geo_breakdown != "", "Select a location.")
      )

      output_data <- stats_neighbours_table(filtered_data_sn(), selected_geo_breakdown = rv_geo_filters$select_geo_breakdown, selected_geo_lvl = rv_geo_filters$select_geographic_level, yvalue = yvalue)

      reactable(
        output_data,
        defaultColDef = colDef(align = "center"),
        columns = rt_col_defs,
        defaultPageSize = 15,
        searchable = TRUE,
      )
    })
  })
}
