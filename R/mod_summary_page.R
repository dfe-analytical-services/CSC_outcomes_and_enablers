sp_accordion_cols_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(ns("accordion_col_headers"))
  )
}


sp_accordion_cols_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    data_in <- reactive(rv$summary_data_filtered)
    output$accordion_col_headers <- renderReactable({
      req(data_in())
      reactable(
        data = transform_summary_data(data_in(), rv$select_geographic_level, headers_only = TRUE),
        class = "hidden-table-body",
        # Prevent hidden column headers from being tabbed/focused
        sortable = FALSE,
        defaultColDef = colDef(
          minWidth = 200,
          align = "center",
          vAlign = "center",
          headerStyle = list(background = "#f3f2f1")
        ),
        columns = list(
          metric_text = colDef(name = "Indicator", minWidth = 500, align = "left") # 50% width, 200px minimum
        )
      )
    })
  })
}

sp_domain_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("domain_text")),
    reactableOutput(ns("domain_table"))
  )
}

sp_domain_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # note that we need to guard against 2 domains starting with the same text string e.g. 'Child wellbeing' and 'Child wellbeing and development'
    # if (id == "Child wellbeing") id <- "Child wellbeing ("
    data_in <- reactive(rv$summary_data_filtered)
    output$domain_text <- renderText({
      req(data_in())
      data_in()[str_starts(heading_text, paste(id, "\\("))]$heading_text[1]
    })
    output$domain_table <- renderReactable({
      req(data_in())
      req(rv$select_geographic_level)

      reactable(
        data = transform_summary_data(data_in()[str_starts(heading_text, paste(id, "\\("))], rv$select_geographic_level),
        class = "hidden-column-headers",
        # Prevent hidden column headers from being tabbed/focused
        sortable = FALSE,
        defaultPageSize = 20,
        defaultColDef = colDef(minWidth = 200, align = "center"),
        columns = list(
          metric_text = colDef(minWidth = 500, align = "left") # 50% width, 200px minimum
        )
      )
    })
  })
}


# supporting functions for summary page: Filter and transform ----

filter_summary_data <- function(data_in, select_geographic_level, select_geo_breakdown) {
  # take the summary dataset and filter it based on geographic selections
  # if national display national only
  if (select_geographic_level == "National") {
    filtered_summary_data <- data_in[geographic_level == "National"]
  } else if (select_geographic_level == "Regional") {
    # if regional display the regions selected and National
    filtered_summary_data <- data_in[geographic_level == "National" | (geographic_level == "Regional" & geo_breakdown == select_geo_breakdown)]
  } else if (select_geographic_level == "Local authority") {
    # if LA selected then display the LA, it's SN, it's region and national
    # get the region from the LA and apply additional filtering
    region_name <- location_data %>%
      filter(la_name %in% select_geo_breakdown) %>%
      pull(region_name)
    filtered_summary_data <- data_in[geographic_level == "National" |
      (geographic_level == "Regional" & geo_breakdown == region_name) |
      (geographic_level == "Local authority" & geo_breakdown == select_geo_breakdown) |
      (geographic_level == "Statistical neighbours (median)" & geo_breakdown_sn == select_geo_breakdown)]
  }
  filtered_summary_data
}
## dat_sp <- filter_summary_data(summary_data, "Local authority", "Merton")


# summary_data <- collect_summary_data_all()
# writexl::write_xlsx(summary_data, "~/CSC shiny dashboard/summary_data_for_QA.xlsx")
# filter_summary_data(summary_data, select_geographic_level = "Local authority", select_geo_breakdown = "Sutton")

transform_summary_data <- function(filtered_summary_data, select_geographic_level = NULL, headers_only = FALSE) {
  transformed_data <- dcast(
    filtered_summary_data,
    sort_order + metric_text ~ geographic_level,
    value.var = "value",
    fun.aggregate = function(x) x[1]
  )

  # TODO:  when we do the download we need to remove the empty columns

  # ensure columns are in the correct order

  if (select_geographic_level == "National") {
    transformed_data[, c("Regional", "Local authority", "Statistical neighbours (median)") := ""]
    setcolorder(transformed_data, c("metric_text", "National", "Regional", "Local authority", "Statistical neighbours (median)"), skip_absent = TRUE)
  } else if (select_geographic_level == "Regional") {
    if (!("Regional" %in% names(transformed_data))) transformed_data[, c("Regional") := ""]
    transformed_data[, c("Local authority", "Statistical neighbours (median)") := ""]
    setcolorder(transformed_data, c("metric_text", "Regional", "National", "Local authority", "Statistical neighbours (median)"), skip_absent = TRUE)
  } else if (select_geographic_level == "Local authority") {
    if (!("Regional" %in% names(transformed_data))) transformed_data[, c("Regional") := ""]
    if (!("Statistical neighbours (median)" %in% names(transformed_data))) transformed_data[, c("Statistical neighbours (median)") := ""]
    if (!("Local authority" %in% names(transformed_data))) transformed_data[, c("Local authority") := ""]
    setcolorder(transformed_data, c("metric_text", "Local authority", "Statistical neighbours (median)", "Regional", "National"), skip_absent = TRUE)
  }

  # get the column names correct with region_name and la_name
  geographic_levels <- unique(filtered_summary_data$geographic_level)
  if ("Regional" %in% geographic_levels) setnames(transformed_data, old = "Regional", new = filtered_summary_data[geographic_level == "Regional"]$geo_breakdown[1])
  if ("Local authority" %in% geographic_levels) setnames(transformed_data, old = "Local authority", new = filtered_summary_data[geographic_level == "Local authority"]$geo_breakdown[1])
  if ("Statistical neighbours (median)" %in% geographic_levels) setnames(transformed_data, old = "Statistical neighbours (median)", new = "Statistical neighbours")

  transformed_data[, sort_order := NULL]

  if (headers_only) {
    dt_temp <- data.table(matrix(ncol = ncol(transformed_data), nrow = 0))
    table_col_names <- names(transformed_data)
    setnames(dt_temp, new = table_col_names)
    dt_temp <- rbindlist(list(dt_temp, as.list(table_col_names)))
    return(dt_temp)
  }
  # transformed_data <- transformed_data[order(sort_order)]
  return(transformed_data)
}


download_summary_data <- function(summary_data_filtered, select_geographic_level) {
  # take the data and prepare it (crosstab, column order/naming etc)
  download_data <- transform_summary_data(summary_data_filtered, select_geographic_level)
  # remove unrequired columns
  if (select_geographic_level != "Local authority") download_data[, (c("Local authority", "Statistical neighbours (median)")) := NULL]
  if (select_geographic_level == "National") download_data[, Regional := NULL]
  # add in the accordion text and heading text
  extra_columns <- unique(summary_data_filtered[, .(tab_name, sort_order, metric_text, heading_text, accordion_text)])
  download_data <- merge(download_data, extra_columns, by = "metric_text")
  setcolorder(download_data, c("tab_name", "accordion_text", "heading_text"))
  downoad_data <- download_data[order(-tab_name, sort_order)]
  # download_data[, sort_order := NULL]

  return(download_data)
}
