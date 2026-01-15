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
  moduleServer(id, function(input, output, session) {
    # we start with the data reactives which are filtering the datasets for chosen geographies (and additional dimensions tbc)
    filtered_data_la <- reactive({
      req(isolate(rv_geo_filters$select_geographic_level))
      req(rv_geo_filters$select_geo_breakdown)

      if (length(rv_dimensional_filters$dimensional_filters) > 0) {
        dataset <- dataset[eval(AndEQUAL(rv_dimensional_filters$dimensional_filters))]
      }

      # need logic for the 3 selections National, Regional, LA here
      if (rv_geo_filters$select_geographic_level %in% c("National", "Local authority")) {
        dataset[geographic_level == "Local authority" & time_period == max(dataset$time_period)]
      } else if (rv_geo_filters$select_geographic_level == "Regional") {
        # Check if the selected region is London
        if (rv_geo_filters$select_geo_breakdown == "London") {
          # Include both Inner London and Outer London
          # get the location data
          location <- location_data %>%
            filter(region_name %in% c("Inner London", "Outer London")) %>%
            pull(la_name)
        } else {
          # Get the la_name values within the selected region_name
          location <- location_data %>%
            filter(region_name == rv_geo_filters$select_geo_breakdown) %>%
            pull(la_name)
        }
        dataset[geographic_level == "Local authority" & geo_breakdown %in% location & time_period == max(dataset$time_period)]
      }
    })


    filtered_data_sn <- reactive({
      # sn_names <- stat_neighbours_for_la(selected_geo_breakdown)
      filter_sn_toggle_dataset(
        dataset_in = dataset,
        select_geographic_level = rv_geo_filters$select_geographic_level,
        select_geo_breakdown = rv_geo_filters$select_geo_breakdown,
        select_time_period = max(dataset$time_period),
        dimensional_filters = rv_dimensional_filters$dimensional_filters
      )
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
            inputId = "la_info",
            label = "Additional information:",
            help_text = ( # this needs to be moved from here into either the server function or a metadata list somewhere
              tags$ul(
                tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
                tags$li("Average caseload at 30 September per year is calculated as the total number of cases held by FTE social workers, including agency workers, in post divided by the number of FTE social workers, including agency workers, in post that held one or more cases."),
                tags$br(),
                p(
                  "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                  tags$br(),
                  "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                )
              )
            )
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
            inputId = "sn_caseload_info",
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

      max_rate <- max(filtered_data_la()[[yvalue]], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 10) * 10

      # max_rate <- max(workforce_data$`Caseload Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      #                                                 workforce_data$geographic_level == "Local authority"], na.rm = TRUE)
      # max_rate <- ceiling(max_rate / 10) * 10

      p <- by_la_bar_plot_revised(filtered_data_la(), rv_geo_filters$select_geographic_level, rv_geo_filters$select_geo_breakdown, yvalue, yaxis_title, max_rate, decimal_percentage = TRUE) %>%
        config(displayModeBar = F)

      # p <- p + ggtitle("Average caseload (FTE) by local authority")
      # title <- paste0("Average caseload (FTE) by local authority (", max(p$data$time_period), ")")
      p <- p + ggtitle(chart_title)

      ggplotly(
        p,
        height = 420,
        tooltip = "text"
      ) %>%
        config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
    })

    # LA table
    output$table_la_toggle <- renderReactable({
      req(filtered_data_la())
      shiny::validate(
        need(rv_geo_filters$select_geographic_level != "", "Select a geography level."),
        need(rv_geo_filters$select_geo_breakdown != "", "Select a location.")
      )
      # build the dataset for the table

      filtered_data_la() %>%
        mutate(geo_breakdown = reorder(geo_breakdown, -(!!sym(`yvalue`)))) %>%
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

      max_rate <- max(filtered_data_sn()[[yvalue]], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 10) * 10

      p <- statistical_neighbours_plot_revised(
        dataset = filtered_data_sn(),
        selected_geo_lvl = rv_geo_filters$select_geographic_level,
        selected_geo_breakdown = rv_geo_filters$select_geo_breakdown,
        yvalue = yvalue,
        yaxis_title = yaxis_title,
        ylim_upper = max_rate,
        decimal_percentage = TRUE
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
