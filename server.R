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
  #
  #   observe({
  #     # Trigger this observer every time an input changes
  #     reactiveValuesToList(input)
  #     session$doBookmark()
  #   })
  #
  #   onBookmarked(function(url) {
  #     updateQueryString(url)
  #   })

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

  output$cookies_status <- dfeshiny::cookies_banner_server(
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key
  )

  dfeshiny::cookies_panel_server(
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )

  # Download handler
  csvDownloadHandler <- function(id, filename) {
    session$sendCustomMessage("downloadDataWithTransformation", list(id = id, filename = filename))
  }

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

  # Summary Page ----
  # Geographic breakdown o1 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_sp
  }, {
    choices <- sort(unique(cla_rates[(cla_rates$geographic_level == input$select_geography_sp & cla_rates$time_period == 2023)]$geo_breakdown), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_sp",
      selected = choices[1],
      choices = choices,
    )
  })
  ## summary page confirmation text ----
  region_for_la_sp <- reactive({
    selected_la <- input$geographic_breakdown_sp
    location_data %>%
      filter(la_name == selected_la) %>%
      pull(region_name)
  })

  output$summary_page_choice_text1 <- renderText({
    generate_choice_text1(input$select_geography_sp, input$geographic_breakdown_sp, region_for_la_sp())
  })

  output$summary_page_choice_text2 <- renderText({
    generate_choice_text2(input$national_comparison_checkbox_sp, input$region_comparison_checkbox_sp, input$sn_comparison_checkbox_sp)
  })


  rv_summary_page <- reactiveValues(summary_data_filtered = NULL, select_geographic_level = NULL)

  observeEvent(req(input$geographic_breakdown_sp, input$select_geography_sp), {
    filtered_data <- filter_summary_data(
      data_in = copy(summary_data),
      select_geographic_level = input$select_geography_sp,
      select_geo_breakdown = input$geographic_breakdown_sp
    )

    rv_summary_page$summary_data_filtered <- filtered_data
    rv_summary_page$select_geographic_level <- input$select_geography_sp
  })

  output$summary_page_download <- downloadHandler(
    filename = function() {
      paste("summary_page_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      dt_out <- download_summary_data(rv_summary_page$summary_data_filtered, rv_summary_page$select_geographic_level)[order(-tab_name, sort_order)]
      setnames(dt_out, 1:4, c("Outcome/Enabler", "Category", "Domain", "Metric"))
      setcolorder(dt_out, "sort_order")
      write.csv(dt_out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # Outcome 1 domains on summary page ----
  sp_accordion_cols_server(id = "outcome1", rv_summary_page)
  sp_domain_server(id = "Access to support and getting help", rv_summary_page)
  sp_domain_server(id = "Family stability", rv_summary_page)
  sp_domain_server(id = "Child wellbeing and development", rv_summary_page)
  sp_domain_server(id = "Educational attainment", rv_summary_page)

  # Outcome 2 domain on summary page
  sp_accordion_cols_server(id = "outcome2", rv_summary_page)
  sp_domain_server(id = "Families engaging and receiving support from their family network", rv_summary_page)

  # outcome 3 domains on summary page
  sp_accordion_cols_server(id = "outcome3", rv_summary_page)
  sp_domain_server(id = "Child safety - general", rv_summary_page)
  sp_domain_server(id = "Child abuse / neglect", rv_summary_page)
  sp_domain_server(id = "Harms outside the home", rv_summary_page)

  # outcome 4 domains on summary page
  sp_accordion_cols_server(id = "outcome4", rv_summary_page)
  sp_domain_server(id = "Stability and quality of where a child lives", rv_summary_page)
  sp_domain_server(id = "Child wellbeing", rv_summary_page)
  sp_domain_server(id = "Quality of life for care experienced people", rv_summary_page)

  # Enabler 2 domains on summary page ----
  sp_accordion_cols_server(id = "enabler1", rv_summary_page)
  sp_domain_server(id = "Spending", rv_summary_page)
  sp_domain_server(id = "Culture focused on outcomes from children and families and continually improving services", rv_summary_page)

  # Enabler 3 domain on summary page
  sp_accordion_cols_server(id = "enabler2", rv_summary_page)
  sp_domain_server(id = "Workforce stability", rv_summary_page)
  sp_domain_server(id = "Quality of support for children and families", rv_summary_page)
  sp_domain_server(id = "Social worker ethnicity", rv_summary_page)



  # Outcome 1 -----
  # Geographic breakdown o1 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_o1
  }, {
    choices <- sort(unique(cla_rates[(cla_rates$geographic_level == input$select_geography_o1 & cla_rates$time_period == 2023)]$geo_breakdown), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_o1",
      selected = choices[1],
      choices = choices,
    )
  })
  ## outcome 1 confirmation text ----
  region_for_la_o1 <- reactive({
    selected_la <- input$geographic_breakdown_o1
    location_data %>%
      filter(la_name == selected_la) %>%
      pull(region_name)
  })

  output$outcome1_choice_text1 <- renderText({
    generate_choice_text1(input$select_geography_o1, input$geographic_breakdown_o1, region_for_la_o1())
  })

  output$outcome1_choice_text2 <- renderText({
    generate_choice_text2(input$national_comparison_checkbox_o1, input$region_comparison_checkbox_o1, input$sn_comparison_checkbox_o1)
  })

  observeEvent(input$select_geography_o1, {
    if (input$select_geography_o1 == "Regional") {
      updateCheckboxInput(session, "Yes_national_o1", value = FALSE)
      updateCheckboxInput(session, "Yes_region_o1", value = FALSE)
    } else if (input$select_geography_o1 == "National") {
      updateCheckboxInput(session, "Yes_national_o1", value = FALSE)
      updateCheckboxInput(session, "Yes_region_o1", value = FALSE)
    }
  })

  output$outcome1_choice_social_care_group_text <- renderText({
    paste0("Percentage of overall absence, total of authorised and unauthorised absence, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_by_region_text <- renderText({
    paste0("Percentage of overall absence, total of authorised and unauthorised absence by region, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_by_la_text <- renderText({
    paste0("Percentage of overall absence, total of authorised and unauthorised absence by local authority, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_text_1 <- renderText({
    paste0("Percentage of persistent absentees, pupils with overall absence at 10% or more, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_by_region_text_1 <- renderText({
    paste0("Percentage of persistent absentees, pupils with overall absence at 10% or more by region, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_by_la_text_1 <- renderText({
    paste0("Percentage of persistent absentees, pupils with overall absence at 10% or more by local authority, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_text_severe <- renderText({
    paste0("Percentage of severe absentees, pupils with overall absence at 50% or more, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_by_region_text_severe <- renderText({
    paste0("Percentage of severe absentees, pupils with overall absence at 50% or more by region, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_by_la_text_severe <- renderText({
    paste0("Percentage of severe absentees, pupils with overall absence at 50% or more by local authority, for ", tags$b(input$wellbeing_extra_breakdown), " and ", tags$b(input$wellbeing_school_breakdown), " school type.")
  })

  output$outcome1_choice_social_care_group_text_2 <- renderText({
    paste0("Percentage of pupils achieving expected standard in reading, writing and maths combined for ", tags$b(input$attainment_extra_breakdown), ".")
  })

  output$outcome1_choice_social_care_group_by_region_text_2 <- renderText({
    paste0("Percentage of pupils achieving expected standard in reading, writing and maths combined by region for ", tags$b(input$attainment_extra_breakdown), ".")
  })

  output$outcome1_choice_social_care_group_by_la_text_2 <- renderText({
    paste0("Percentage of pupils achieving expected standard in reading, writing and maths combined by local authority for ", tags$b(input$attainment_extra_breakdown), ".")
  })

  output$outcome1_choice_social_care_group_text_3 <- renderText({
    paste0("Average achievement of pupils in up to 8 qualifications, including English language, English literature and maths, for ", tags$b(input$attainment_extra_breakdown), ".")
  })

  output$outcome1_choice_social_care_group_by_region_text_3 <- renderText({
    paste0("Average achievement of pupils in up to 8 qualifications, including English language, English literature and maths by region, for ", tags$b(input$attainment_extra_breakdown), ".")
  })

  output$outcome1_choice_social_care_group_by_la_text_3 <- renderText({
    paste0("Average achievement of pupils in up to 8 qualifications, including English language, English literature and maths by region by local authority, for ", tags$b(input$attainment_extra_breakdown), ".")
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

  output$outcome1_time_period_text_severe <- renderText({
    # if statement  to only report tables for primary
    charts_or_tables <- if (input$wellbeing_school_breakdown != "State-funded primary") "charts" else "tables"
    paste0("The ", charts_or_tables, " below represent data from ", outcomes_time_period_max, ", for ", input$wellbeing_extra_breakdown, " and ", input$wellbeing_school_breakdown, " school type.")
  })


  output$outcome1_time_period_text_3 <- renderText({
    paste0("The charts below represent data from ", outcomes_time_period_max, ", for ", input$attainment_extra_breakdown, ".")
  })

  output$outcome1_time_period_text_4 <- renderText({
    paste0("The charts below represent data from ", outcomes_time_period_max, ", for ", input$attainment_extra_breakdown, ".")
  })

  output$outcome4_choice_placement_type_text <- renderText({
    paste0("Percentage of children living in ", tags$b(input$placement_type_breakdown))
  })

  output$outcome4_choice_placement_type_by_region_text <- renderText({
    paste0("Percentage of children living in ", tags$b(input$placement_type_breakdown), " by region")
  })

  output$outcome4_choice_placement_type_by_la_text <- renderText({
    paste0("Percentage of children living in ", tags$b(input$placement_type_breakdown), " by local authority")
  })


  ## Headline stats ---------
  ## CLA rate headline
  output$cla_rate_headline_txt <- renderText({
    stat <- format(cla_rates %>% filter(time_period == max(cla_rates$time_period) &
      geo_breakdown %in% input$geographic_breakdown_o1 &
      population_count == "Children starting to be looked after each year") %>% select(rate_per_10000), nsmall = 0)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cla_rates$time_period), ")", "</p>")
  })

  # UASC rate headline
  output$uasc_rate_headline_txt <- renderText({
    stat <- format(combined_cla_data %>% filter(time_period == max(combined_cla_data$time_period) &
      geo_breakdown %in% input$geographic_breakdown_o1 &
      population_count == "Children starting to be looked after each year" &
      characteristic == "UASC") %>% select(placement_per_10000), nsmall = 0)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(combined_cla_data$time_period), ")", "</p>")
  })

  # UASC 31 March rate headline
  output$uasc_31_march_rate_headline_txt <- renderText({
    stat <- format(combined_cla_31_march_data %>% filter(time_period == max(combined_cla_31_march_data$time_period) &
      geo_breakdown %in% input$geographic_breakdown_o1 &
      population_count == "Children looked after at 31 March each year" &
      characteristic == "UASC") %>% select(placement_per_10000), nsmall = 0)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(combined_cla_data$time_period), ")", "</p>")
  })

  # CLA March rate headline
  output$cla_march_rate_headline_txt <- renderText({
    stat <- format(cla_rates %>% filter(time_period == max(cla_rates$time_period) &
      geo_breakdown %in% input$geographic_breakdown_o1 &
      population_count == "Children looked after at 31 March each year") %>% select(rate_per_10000), nsmall = 0)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cla_rates$time_period), ")", "</p>")
  })
  ## CIN rate headline
  output$cin_rate_headline_txt <- renderText({
    stat <- format(cin_rates %>% filter(time_period == max(cin_rates$time_period) & geo_breakdown %in% input$geographic_breakdown_o1)
      %>% select(At31_episodes_rate), nsmall = 0)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cin_rates$time_period), ")", "</p>")
  })

  ## CIN referral headline
  output$cin_referral_headline_txt <- renderText({
    stat <- format(cin_referrals %>% filter(time_period == max(cin_referrals$time_period) & geo_breakdown %in% input$geographic_breakdown_o1)
      %>% select(Re_referrals_percent), nsmall = 1)
    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(cin_referrals$time_period), ")", "</p>")
  })

  # formatted time period for the persistent absence headline stat boxes
  formatted_time_period_wellbeing <- outcomes_absence %>%
    filter(time_period == max(time_period), geo_breakdown == "National", social_care_group == "CINO at 31 March", school_type == "Total") %>%
    mutate(time_period_new = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

  # absence rates for CIN
  output$absence_CIN_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March", school_type == "Total")
      %>% select(pt_overall), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # absence rates for CPPO
  output$absence_CPP_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March", school_type == "Total")
      %>% select(pt_overall), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # absence rates for CLA
  output$absence_CLA_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March", school_type == "Total")
      %>% select(pt_overall), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # persistent absentees headline stats
  # Persistent absence for CIN
  output$persistent_CIN_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March", school_type == "Total")
      %>% select(pt_pupils_pa_10_exact), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # Persistent absence for CPPO
  output$persistent_CPP_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March", school_type == "Total")
      %>% select(pt_pupils_pa_10_exact), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # Persistent absence for CLA
  output$persistent_CLA_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March", school_type == "Total")
      %>% select(pt_pupils_pa_10_exact), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # severe absentees headline stats
  # Severe absence for CIN
  output$severe_CIN_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March", school_type == "Total")
      %>% select(pt_pupils_pa_50_exact), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # Severe absence for CPPO
  output$severe_CPP_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March", school_type == "Total")
      %>% select(pt_pupils_pa_50_exact), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  # Severe absence for CLA
  output$severe_CLA_headline_txt <- renderText({
    stat <- format(outcomes_absence %>% filter(time_period == max(outcomes_absence$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March", school_type == "Total")
      %>% select(pt_pupils_pa_50_exact), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period_wellbeing$time_period_new, ")", "</p>")
  })

  ## CLA rates ----------------
  # CLA rate Plot
  output$plot_cla_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = cla_rates,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("population_count" = "Children starting to be looked after each year")
    )

    filtered_data <- filtered_data %>%
      filter(population_count == "Children starting to be looked after each year") %>%
      rename("Rate per 10,000" = "Rate Per 10000")

    if (input$geographic_breakdown_o1 == "City of London") {
      # Set the max y-axis scale with City of London
      max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 50) * 50
    } else {
      # Set the max y-axis scale without City of London
      max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year" & cla_rates$geo_breakdown != "City of London"], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 20) * 20
    }


    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Rate per 10,000", "Rate per 10,000 children", max_rate) %>%
      config(
        displayModeBar = F
      )
    p <- p + ggtitle("CLA rate per 10,000")


    ggplotly(p, height = 420, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, max_rate), tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # CLA rate TABLE
  output$table_cla_rate <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = cla_rates,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("population_count" = "Children starting to be looked after each year")
    )
    # select the right columns and give them user-friendly names
    filtered_data <- filtered_data %>%
      select(time_period, geo_breakdown, number, `Rate Per 10000`) %>%
      rename(
        `Time period` = `time_period`,
        `Location` = `geo_breakdown`,
        `Number of children starting to be looked after` = `number`,
        `Rate of children starting to be looked after, per 10,000` = `Rate Per 10000`
      )

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cla_rate_reg() %>%
      config(displayModeBar = F)
    title <- paste0("CLA rate per 10,000 by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # CLA rate regional table
  output$table_cla_rate_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- cla_rates %>%
      filter(geographic_level == "Regional", time_period == max(cla_rates$time_period), population_count == "Children starting to be looked after each year") %>%
      select(time_period, geo_breakdown, number, "Rate Per 10000") %>%
      arrange(desc(`Rate Per 10000`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Number of children starting to be looked after` = `number`, `Rate of children starting to be looked after, per 10,000` = `Rate Per 10000`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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
    title <- paste0("CLA rate per 10,000 by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
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
    p <- p + ggtitle("CLA rate per 10,000 with Unaccompanied asylum-seeking children breakdown")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
        characteristic %in% c("UASC", "Non-UASC"),
        population_count == "Children starting to be looked after each year"
      ) %>%
      mutate(characteristic = case_when(
        characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
        characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
        TRUE ~ as.character(characteristic)
      )) %>%
      select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
      arrange(desc(time_period)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Number of children starting to be looked after` = `placements_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_uasc_reg() %>%
      config(displayModeBar = F)
    title <- paste0("CLA rate per 10,000 with Unaccompanied asylum-seeking children breakdown by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # UASC table by region
  output$table_uasc_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- combined_cla_data %>%
      filter(
        geographic_level == "Regional", characteristic %in% c("UASC", "Non-UASC"),
        population_count == "Children starting to be looked after each year",
        time_period == max(time_period)
      ) %>%
      mutate(characteristic = case_when(
        characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
        characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
        TRUE ~ as.character(characteristic)
      )) %>%
      select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `UASC status` = `characteristic`, `Number of children starting to be looked after` = `placements_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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
    title <- paste0("CLA rate per 10,000 with Unaccompanied asylum-seeking children breakdown by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
          geo_breakdown %in% location, time_period == max(combined_cla_data$time_period), characteristic %in% c("UASC", "Non-UASC"),
          population_count == "Children starting to be looked after each year",
        ) %>%
        mutate(characteristic = case_when(
          characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
          characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
          TRUE ~ as.character(characteristic)
        )) %>%
        select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
        arrange(desc(`Placement Rate Per 10000`)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `UASC status` = `characteristic`, `Number of children starting to be looked after` = `placements_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- combined_cla_data %>%
        filter(
          geographic_level == "Local authority", time_period == max(combined_cla_data$time_period), characteristic %in% c("UASC", "Non-UASC"),
          population_count == "Children starting to be looked after each year",
        ) %>%
        mutate(characteristic = case_when(
          characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
          characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
          TRUE ~ as.character(characteristic)
        )) %>%
        select(time_period, geo_breakdown, characteristic, placements_number, `Placement Rate Per 10000`) %>%
        arrange(desc(`Placement Rate Per 10000`)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `UASC status` = `characteristic`, `Number of children starting to be looked after` = `placements_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)
    }

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Number of children starting to be looked after` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # UASC 31 March chart
  output$plot_uasc_31_march <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_uasc_31_march(input$geographic_breakdown_o1, input$select_geography_o1) %>%
      config(displayModeBar = F)

    p <- p + ggtitle("CLA rate on 31 March per 10,000 with Unaccompanied asylum-seeking children breakdown")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # UASC 31 March table
  output$table_uasc_31_march <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- combined_cla_31_march_data %>%
      filter(
        geo_breakdown %in% input$geographic_breakdown_o1,
        characteristic %in% c("UASC", "Non-UASC"),
        population_count == "Children looked after at 31 March each year"
      ) %>%
      mutate(characteristic = case_when(
        characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
        characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
        TRUE ~ as.character(characteristic)
      )) %>%
      select(time_period, geo_breakdown, characteristic, cla_31_march_number, `Placement Rate Per 10000`) %>%
      arrange(desc(time_period)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Number of children looked after on the 31st March` = `cla_31_march_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Number of children looked after on the 31st March` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # UASC 31 March chart by region
  output$plot_uasc_31_march_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_uasc_31_march_reg() %>%
      config(displayModeBar = F)
    title <- paste0("CLA rate on 31st March per 10,000 with Unaccompanied asylum-seeking children breakdown by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # UASC 31 March table by region
  output$table_uasc_31_march_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- combined_cla_31_march_data %>%
      filter(
        geographic_level == "Regional", characteristic %in% c("UASC", "Non-UASC"),
        population_count == "Children looked after at 31 March each year",
        time_period == max(time_period)
      ) %>%
      mutate(characteristic = case_when(
        characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
        characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
        TRUE ~ as.character(characteristic)
      )) %>%
      select(time_period, geo_breakdown, characteristic, cla_31_march_number, `Placement Rate Per 10000`) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `UASC status` = `characteristic`, `Number of children looked after on 31st March` = `cla_31_march_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Number of children looked after on the 31st March` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # UASC 31 March plot by LA
  output$plot_uasc_31_march_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_uasc_31_march_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
      config(displayModeBar = F)
    title <- paste0("CLA on 31st March rate per 10,000 with Unaccompanied asylum-seeking children breakdown by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # UASC 31 March table by LA
  output$table_uasc_31_march_la <- renderReactable({
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

      data <- combined_cla_31_march_data %>%
        filter(
          geo_breakdown %in% location, time_period == max(combined_cla_31_march_data$time_period), characteristic %in% c("UASC", "Non-UASC"),
          population_count == "Children looked after at 31 March each year",
        ) %>%
        mutate(characteristic = case_when(
          characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
          characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
          TRUE ~ as.character(characteristic)
        )) %>%
        select(time_period, geo_breakdown, characteristic, cla_31_march_number, `Placement Rate Per 10000`) %>%
        arrange(desc(`Placement Rate Per 10000`)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `UASC status` = `characteristic`, `Number of children looked after on the 31st March` = `cla_31_march_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- combined_cla_31_march_data %>%
        filter(
          geographic_level == "Local authority", time_period == max(combined_cla_31_march_data$time_period), characteristic %in% c("UASC", "Non-UASC"),
          population_count == "Children looked after at 31 March each year",
        ) %>%
        mutate(characteristic = case_when(
          characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
          characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
          TRUE ~ as.character(characteristic)
        )) %>%
        select(time_period, geo_breakdown, characteristic, cla_31_march_number, `Placement Rate Per 10000`) %>%
        arrange(desc(`Placement Rate Per 10000`)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `UASC status` = `characteristic`, `Number of children looked after on the 31st March` = `cla_31_march_number`, `Rate per 10,000 children` = `Placement Rate Per 10000`)
    }

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Number of children looked after on the 31st March` = colDef(cell = cellfunc),
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

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = cla_rates,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("population_count" = "Children looked after at 31 March each year")
    )

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
      layout(yaxis = list(range = c(0, max_rate), tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # CLA rate march TABLE
  output$table_cla_rate_march <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = cla_rates,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("population_count" = "Children looked after at 31 March each year")
    )

    filtered_data <- filtered_data %>%
      select(time_period, geo_breakdown, number, `Rate Per 10000`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Number of children looked after on 31 March` = `number`, `Rate per 10,000 children` = `Rate Per 10000`)


    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cla_march_reg() %>%
      config(displayModeBar = F)
    title <- paste0("CLA rate per 10,000 on 31 March by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # CLA rate March regional table
  output$table_cla_march_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    data <- cla_rates %>%
      filter(geographic_level == "Regional", time_period == max(cla_rates$time_period), population_count == "Children looked after at 31 March each year") %>%
      select(time_period, geo_breakdown, number, `Rate Per 10000`) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Number of children looked after on 31 March` = `number`, `Rate per 10,000 children` = `Rate Per 10000`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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
    title <- paste0("CLA rate per 10,000 on 31 March by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Number of children looked after on 31 March` = colDef(cell = cellfunc),
        `Rate per 10,000 children` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## CIN rate plot -----
  output$plot_cin_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = cin_rates,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1
    ) %>% rename("CIN rate per 10,000" = CIN_rate)


    if (input$geographic_breakdown_o1 == "City of London") {
      # Set the max y-axis scale with City of London
      max_rate <- max(cin_rates$CIN_rate, na.rm = TRUE)
      max_rate <- ceiling(max_rate / 50) * 50
    } else {
      # Set the max y-axis scale without City of London
      max_rate <- max(cin_rates$CIN_rate[cin_rates$geo_breakdown != "City of London"], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 20) * 20
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "CIN rate per 10,000", "CIN rate per 10,000 Children", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("CIN rate per 10,000 children")

    ggplotly(p, height = 420, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, max_rate), tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # CIN rate table
  output$table_cin_rate <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = cin_rates,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1
    ) %>%
      select(time_period, geo_breakdown, At31_episodes, CIN_rate) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CIN number at 31 March` = `At31_episodes`, `CIN rates per 10,000` = `CIN_rate`)

    # now output the table itself
    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cin_rate_reg() %>%
      config(displayModeBar = F)
    title <- paste0("CIN rate per 10,000 children by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })


  # cin rate table by region
  output$table_cin_rates_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
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
      defaultColDef = colDef(align = "center"),
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
      defaultColDef = colDef(align = "center"),
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
    title <- paste0("CIN rate per 10,000 children by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })


  ## CIN referrals --------------------------
  ### CIN referral plot ----
  output$plot_cin_referral <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = cin_referrals,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1
    )

    # Set the max y-axis scale
    max_rate <- max(filtered_data$`Re-referrals (%)`, na.rm = TRUE)

    # Round the max_rate to the nearest 20
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Re-referrals (%)", "Re-referrals (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Re-referrals within 12 months %")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### CIN referral table ----
  output$table_cin_referral <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = cin_referrals,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1
    ) %>%
      select(time_period, geo_breakdown, Referrals, Re_referrals, `Re-referrals (%)`) %>%
      rename(
        `Time period` = `time_period`, `Location` = `geo_breakdown`, `Referrals in the year` = `Referrals`,
        `Re-referrals within 12 months of a previous referral` = `Re_referrals`, `Re-referrals within 12 months (%)` = `Re-referrals (%)`
      )

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Referrals in the year` = colDef(cell = cellfunc),
        `Re-referrals within 12 months of a previous referral` = colDef(cell = cellfunc),
        `Re-referrals within 12 months (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # cin referral table by region
  output$table_cin_referral_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Referrals in the year` = colDef(cell = cellfunc),
        `Re-referrals within 12 months of a previous referral` = colDef(cell = cellfunc),
        `Re-referrals within 12 months (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Referrals in the year` = colDef(cell = cellfunc),
        `Re-referrals within 12 months of a previous referral` = colDef(cell = cellfunc),
        `Re-referrals within 12 months (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  # cin referral plot by region
  output$plot_cin_referral_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cin_referral_reg() %>%
      config(displayModeBar = F)
    title <- paste0("Re-referrals within 12 months % by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # cin referral chart by LA
  output$plot_cin_referral_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- plot_cin_referral_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
      config(displayModeBar = F)
    title <- paste0("Re-referrals within 12 months % by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ## Child wellbeing & development - School absence and attainment ----
  ### Overall Absence timeseries chart ----
  # TODO: do wee need additional required parameters here for school type and social care group?
  output$absence_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = outcomes_absence,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("social_care_group" = input$wellbeing_extra_breakdown, "school_type" = input$wellbeing_school_breakdown)
    )

    filtered_data <- filtered_data %>% mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    # Set the max y-axis scale
    max_rate <- max(outcomes_absence$`Overall absence (%)`[!outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)

    # Round the max_rate to the nearest 20
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Overall absence (%)", "Overall absence (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Overall absence rate (%)")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })


  ### Absence rate table ----
  output$table_absence_rate <- renderReactable({
    validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = outcomes_absence,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("social_care_group" = input$wellbeing_extra_breakdown, "school_type" = input$wellbeing_school_breakdown)
    )

    # TODO: data cleansing shouldn't be here
    # filtered_data[is.na(`Overall absence (%)`), `Overall absence (%)` := "x"]
    filtered_data <- filtered_data %>%
      select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `pt_overall`) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Social care group` = `social_care_group`, `School Type` = `school_type`, `Total number of pupils` = `Total pupils`, `Overall absence (%)` = `pt_overall`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Overall absence (%)` = colDef(cell = cellfunc_decimal_percent)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Absence rate regional plot ----
  output$plot_absence_reg <- plotly::renderPlotly({
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_absence$`Overall absence (%)`[outcomes_absence$time_period == max(outcomes_absence$time_period) &
      outcomes_absence$geographic_level == "Regional" &
      !outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Overall absence (%)", "Overall absence (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Overall absence rate (%) by region ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  ### Absence rate regional table ----
  output$table_absence_reg <- renderReactable({
    data <- outcomes_absence %>%
      filter(geographic_level == "Regional" & time_period == max(outcomes_absence$time_period) & school_type %in% input$wellbeing_school_breakdown & social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Overall absence (%)`) %>%
      arrange(desc(`Overall absence (%)`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Social care group` = `social_care_group`, `School type` = `school_type`, `Total number of pupils` = `Total pupils`, `Overall absence (%)` = `Overall absence (%)`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Overall absence (%)` = colDef(cell = cellfunc_decimal_percent)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  ### Absence by LA plot ----
  output$plot_absence_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_absence$`Overall absence (%)`[outcomes_absence$time_period == max(outcomes_absence$time_period) &
      outcomes_absence$geographic_level == "Local authority" &
      !outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Overall absence (%)", "Overall absence (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Overall absence rate (%) by local authority ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### Absence by LA table ----
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Overall absence (%)` = colDef(cell = cellfunc_decimal_percent)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## Persistent absence ------------

  ### Persistent absence timeseries chart ----
  output$persistence_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = outcomes_absence,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("social_care_group" = input$wellbeing_extra_breakdown, "school_type" = input$wellbeing_school_breakdown)
    )

    filtered_data <- filtered_data %>% mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))
    # Set the max y-axis scale
    max_rate <- max(outcomes_absence$`Persistent absentees (%)`[!outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)

    # Round the max_rate to the nearest 20
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Persistent absentees (%)", "Persistent absentees (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Persistent absentees (%)")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })


  ### Persistent absence rate TABLE ----
  output$table_persistent_rate <- renderReactable({
    validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = outcomes_absence,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("social_care_group" = input$wellbeing_extra_breakdown, "school_type" = input$wellbeing_school_breakdown)
    )

    filtered_data <- filtered_data %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Persistent absentees (%)`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Social care group` = `social_care_group`, `School type` = `school_type`, `Total number of pupils` = `Total pupils`, `Persistent absentees (%)` = `Persistent absentees (%)`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Persistent absentees (%)` = colDef(cell = cellfunc_decimal_percent)
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

    max_rate <- max(outcomes_absence$`Persistent absentees (%)`[outcomes_absence$time_period == max(outcomes_absence$time_period) &
      outcomes_absence$geographic_level == "Regional" &
      !outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Persistent absentees (%)", "Persistent absentees (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Persistent absentees (%) by region ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Persistent absentees (%)` = colDef(cell = cellfunc_decimal_percent)
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
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_absence$`Persistent absentees (%)`[outcomes_absence$time_period == max(outcomes_absence$time_period) &
      outcomes_absence$geographic_level == "Local authority" &
      !outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Persistent absentees (%)", "Persistent absentees (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Persistent absentees (%) by local authority ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Persistent absentees (%)` = colDef(cell = cellfunc_decimal_percent)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## Severe absence ----
  ### Severe absence timeseries chart + table : module

  # reactive values and a way to update them
  rv_severe_absence <- reactiveValues(
    select_geographic_level = NULL, select_geo_breakdown = NULL,
    check_compare_national = NULL, check_compare_regional = NULL, check_compare_sn = NULL,
    dimensional_filters = list()
  )

  observeEvent(ignoreInit = TRUE, list(
    input$select_geography_o1, input$geographic_breakdown_o1,
    input$national_comparison_checkbox_o1, input$region_comparison_checkbox_o1, input$sn_comparison_checkbox_o1,
    input$wellbeing_extra_breakdown, input$wellbeing_school_breakdown
  ), {
    req(input$select_geography_o1, input$geographic_breakdown_o3)
    rv_severe_absence$select_geographic_level <- input$select_geography_o1
    rv_severe_absence$select_geo_breakdown <- input$geographic_breakdown_o1
    rv_severe_absence$check_compare_national <- input$national_comparison_checkbox_o1
    rv_severe_absence$check_compare_regional <- input$region_comparison_checkbox_o1
    rv_severe_absence$check_compare_sn <- input$sn_comparison_checkbox_o1
    rv_severe_absence$dimensional_filters <- list("social_care_group" = input$wellbeing_extra_breakdown, "school_type" = input$wellbeing_school_breakdown)
  }) # bindEvent(list(input$geographic_breakdown_o3,input$select_geography_o3))


  timeseries_section_server("severe_absence",
    rv = rv_severe_absence,
    dataset = copy(outcomes_absence),
    # dimensional_filters = list("social_care_group" = input$wellbeing_extra_breakdown, "school_type" = input$wellbeing_school_breakdown),
    chart_title = "Severe absentees (%)",
    yvalue = "Severe absentees (%)",
    yaxis_title = "Severe absentees (%)",
    max_rate = calculate_max_rate(outcomes_absence, "Severe absentees (%)"),
    rt_columns = list("Time period" = "time_period", "Location" = "geo_breakdown", "Social care group" = "social_care_group", "School type" = "school_type", "Total number of pupils" = "Total pupils", "Severe absentees (%)" = "Severe absentees (%)"),
    rt_col_defs = list(
      "Total number of pupils" = colDef(cell = cellfunc),
      "Severe absentees (%)" = colDef(cell = cellfunc_decimal_percent)
    ),
    decimal_percentage = TRUE
  )

  ### Severe absence other charts and tables
  # Severe absence regional plot
  output$plot_severe_reg <- plotly::renderPlotly({
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_absence$`Severe absentees (%)`[outcomes_absence$time_period == max(outcomes_absence$time_period) &
      outcomes_absence$geographic_level == "Regional" &
      !outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Severe absentees (%)", "Severe absentees (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Severe absentees (%) by region ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # Severe Absence regional table
  output$table_severe_reg <- renderReactable({
    data <- outcomes_absence %>%
      filter(
        geographic_level == "Regional", time_period == max(outcomes_absence$time_period),
        school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown
      ) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Severe absentees (%)`) %>%
      arrange(desc(`Severe absentees (%)`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Social care group` = `social_care_group`, `School type` = `school_type`, `Total number of pupils` = `Total pupils`, `Severe absentees (%)` = `Severe absentees (%)`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Severe absentees (%)` = colDef(cell = cellfunc_decimal_percent)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })



  # Severe absence by la
  output$plot_severe_absence_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_absence$`Severe absentees (%)`[outcomes_absence$time_period == max(outcomes_absence$time_period) &
      outcomes_absence$geographic_level == "Local authority" &
      !outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Severe absentees (%)", "Severe absentees (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Severe absentees (%) by local authority ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # Severe Absence by LA table
  output$table_severe_absence_la <- renderReactable({
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
          time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Severe absentees (%)`
        ) %>%
        arrange(desc(`Severe absentees (%)`))
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- outcomes_absence %>%
        filter(geographic_level == "Local authority", time_period == max(outcomes_absence$time_period)) %>%
        filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
        mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
        select(
          time_period, geo_breakdown, social_care_group, school_type, `Total pupils`, `Severe absentees (%)`
        ) %>%
        arrange(desc(`Severe absentees (%)`))
    }

    data2 <- data %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `Social care group` = `social_care_group`, `School type` = `school_type`, `Total number of pupils` = `Total pupils`, `Severe absentees (%)` = `Severe absentees (%)`)
    reactable(
      data2,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Severe absentees (%)` = colDef(cell = cellfunc_decimal_percent)
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
    stat <- format(outcomes_ks2 %>% filter(time_period == max(outcomes_ks2$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March")
      %>% select(pt_rwm_met_expected_standard), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CPPO
  output$KS2_CPP_headline_txt <- renderText({
    stat <- format(outcomes_ks2 %>% filter(time_period == max(outcomes_ks2$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March")
      %>% select(pt_rwm_met_expected_standard), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CLA
  output$KS2_CLA_headline_txt <- renderText({
    stat <- format(outcomes_ks2 %>% filter(time_period == max(outcomes_ks2$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March")
      %>% select(pt_rwm_met_expected_standard), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # KS4 headline ----
  # CIN
  output$KS4_CIN_headline_txt <- renderText({
    stat <- format(outcomes_ks4 %>% filter(time_period == max(outcomes_ks4$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CINO at 31 March")
      %>% select(avg_att8), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CPPO
  output$KS4_CPP_headline_txt <- renderText({
    stat <- format(outcomes_ks4 %>% filter(time_period == max(outcomes_ks4$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CPPO at 31 March")
      %>% select(avg_att8), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  # CLA
  output$KS4_CLA_headline_txt <- renderText({
    stat <- format(outcomes_ks4 %>% filter(time_period == max(outcomes_ks4$time_period), geo_breakdown %in% input$geographic_breakdown_o1, social_care_group == "CLA 12 months at 31 March")
      %>% select(avg_att8), nsmall = 1)

    if (input$geographic_breakdown_o1 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", formatted_time_period$time_period_new, ")", "</p>")
  })

  ## KS2 attainment -----
  ### KS2 % expected plot ----
  output$plot_ks2_expected <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = outcomes_ks2,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("social_care_group" = input$attainment_extra_breakdown)
    )

    filtered_data <- filtered_data %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    # Set the max y-axis scale
    max_rate <- max(outcomes_ks2$`Expected standard reading writing maths (%)`, na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Expected standard reading writing maths (%)", "Expected standard combined (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage meeting combined expected standard (KS2)")

    ggplotly(p, height = 420, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, max_rate), tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })


  ### ks2 time series TABLE ----
  output$table_ks2_expected <- renderReactable({
    validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = outcomes_ks2,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("social_care_group" = input$attainment_extra_breakdown)
    )

    filtered_data <- filtered_data %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, t_rwm_eligible_pupils, `Expected standard reading writing maths (%)`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Social care group` = `social_care_group`, `Total number of eligible pupils` = `t_rwm_eligible_pupils`, `Expected standard reading writing maths (%)` = `Expected standard reading writing maths (%)`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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

    max_rate <- max(outcomes_ks2$`Expected standard reading writing maths (%)`[outcomes_ks2$time_period == max(outcomes_ks2$time_period) &
      outcomes_ks2$geographic_level == "Regional"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Expected standard reading writing maths (%)", "Expected standard combined (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    title <- paste0("Percentage meeting combined expected standard (KS2) by region ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
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
      defaultColDef = colDef(align = "center"),
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
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_ks2$`Expected standard reading writing maths (%)`[outcomes_ks2$time_period == max(outcomes_ks2$time_period) &
      outcomes_ks2$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Expected standard reading writing maths (%)", "Expected standard combined (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    title <- paste0("Percentage meeting combined expected standard (KS2) by local authority ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of eligible pupils` = colDef(cell = cellfunc),
        `Expected standard reading writing maths (%)` = colDef(cell = cellfunc)
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## KS4 attainment -----
  ### KS4 % expected plot ----
  output$plot_ks4 <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = outcomes_ks4,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("social_care_group" = input$attainment_extra_breakdown)
    )

    filtered_data <- filtered_data %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_ks4$`Average Attainment 8`, na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 20) * 20


    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1, "Average Attainment 8", "Average Attainment 8 score", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average attainment 8 score (KS4)")


    ggplotly(p, height = 420, tooltip = "text") %>%
      layout(yaxis = list(range = c(0, max_rate), tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })


  # KS4 rate TABLE
  output$table_ks4 <- renderReactable({
    validate(
      need(input$select_geography_o1 != "", "Select a geography level."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = outcomes_ks4,
      select_geographic_level = input$select_geography_o1,
      select_geo_breakdown = input$geographic_breakdown_o1,
      check_compare_national = input$national_comparison_checkbox_o1,
      check_compare_regional = input$region_comparison_checkbox_o1,
      check_compare_sn = input$sn_comparison_checkbox_o1,
      dimensional_filters = list("social_care_group" = input$attainment_extra_breakdown)
    )
    filtered_data <- filtered_data %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period)))) %>%
      select(time_period, geo_breakdown, social_care_group, t_pupils, avg_att8) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Social care group" = "social_care_group", "Total number of pupils" = "t_pupils", "Average attainment 8 score" = "avg_att8")

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Average attainment 8 score` = colDef(cell = cellfunc_decimal_percent)
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

    max_rate <- max(outcomes_ks4$`Average Attainment 8`[outcomes_ks4$time_period == max(outcomes_ks4$time_period) &
      outcomes_ks4$geographic_level == "Regional"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Average Attainment 8", "Average Attainment 8", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Average attainment 8 score (KS4) by region ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Average attainment 8 score` = colDef(cell = cellfunc_decimal_percent)
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
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_ks4$`Average Attainment 8`[outcomes_ks4$time_period == max(outcomes_ks4$time_period) &
      outcomes_ks4$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Average Attainment 8", "Average Attainment 8 score", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Average attainment 8 score (KS4) by local authority ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Average attainment 8 score` = colDef(cell = cellfunc_decimal_percent)
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
    choices <- sort(unique(ceased_cla_data[(ceased_cla_data$geographic_level == input$select_geography_o2 & ceased_cla_data$time_period == max(ceased_cla_data$time_period))]$geo_breakdown), decreasing = FALSE)
    #    choices <- sort(unique(ceased_cla_data[(ceased_cla_data$geographic_level == input$select_geography_o1 & ceased_cla_data$time_period == 2023)]$geo_breakdown), decreasing = FALSE)

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
    generate_choice_text1(input$select_geography_o2, input$geographic_breakdown_o2, region_for_la_o2())
  })

  output$outcome2_choice_text2 <- renderText({
    generate_choice_text2(input$national_comparison_checkbox_o2, input$region_comparison_checkbox_o2, input$sn_comparison_checkbox_o2)
  })

  observeEvent(input$select_geography_o2, {
    if (input$select_geography_o2 == "Regional") {
      updateCheckboxInput(session, "Yes_national_o2", value = FALSE)
      updateCheckboxInput(session, "Yes_region_o2", value = FALSE)
    } else if (input$select_geography_o2 == "National") {
      updateCheckboxInput(session, "Yes_national_o2", value = FALSE)
      updateCheckboxInput(session, "Yes_region_o2", value = FALSE)
    }
  })

  ## Headline stats -----
  output$SGO_headline_txt <- renderText({
    stat <- ceased_cla_data %>%
      filter(time_period == max(ceased_cla_data$time_period) &
        geo_breakdown %in% input$geographic_breakdown_o2 &
        cla_group == "Reason episode ceased" &
        characteristic == "Special guardianship orders") %>%
      select(percentage)

    if (input$geographic_breakdown_o2 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(ceased_cla_data$time_period), ")", "</p>")
  })


  # Headline stat2
  output$CAO_headline_txt <- renderText({
    stat <- ceased_cla_data %>%
      filter(time_period == max(ceased_cla_data$time_period) &
        geo_breakdown %in% input$geographic_breakdown_o2 &
        cla_group == "Reason episode ceased" &
        characteristic == "Residence order or child arrangement order granted") %>%
      select(percentage)

    if (input$geographic_breakdown_o2 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(ceased_cla_data$time_period), ")", "</p>")
  })

  ## SGO ----
  ### SGO time series chart ----
  output$SGO_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = ceased_cla_data,
      select_geographic_level = input$select_geography_o2,
      select_geo_breakdown = input$geographic_breakdown_o2,
      check_compare_national = input$national_comparison_checkbox_o2,
      check_compare_regional = input$region_comparison_checkbox_o2,
      check_compare_sn = input$sn_comparison_checkbox_o2,
      dimensional_filters = list("characteristic" = "Special guardianship orders")
    )

    # Set the max y-axis scale
    max_rate <- max(ceased_cla_data$`Ceased (%)`[ceased_cla_data$characteristic == "Special guardianship orders"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o2, input$geographic_breakdown_o2, "Ceased (%)", "Ceased due to SGO (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to SGO")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### SGO time series table ----
  output$table_sgo_ceased <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = ceased_cla_data,
      select_geographic_level = input$select_geography_o2,
      select_geo_breakdown = input$geographic_breakdown_o2,
      check_compare_national = input$national_comparison_checkbox_o2,
      check_compare_regional = input$region_comparison_checkbox_o2,
      check_compare_sn = input$sn_comparison_checkbox_o2,
      dimensional_filters = list("characteristic" = "Special guardianship orders")
    ) %>%
      select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
  # SGO by region

  output$plot_sgo_ceased_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>% filter(characteristic == "Special guardianship orders")

    max_rate <- max(ceased_cla_data$`Ceased (%)`[ceased_cla_data$time_period == max(ceased_cla_data$time_period) &
      ceased_cla_data$geographic_level == "Regional" &
      ceased_cla_data$characteristic == "Special guardianship orders"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Ceased (%)", "Ceased due to SGO (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage ceased CLA due to SGO by region")
    title <- paste0("Percentage ceased CLA due to SGO by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # SGO by region table
  output$table_sgo_ceased_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o2 != "", "Select a location.")
    )

    data <- ceased_cla_data %>%
      filter(geographic_level == "Regional", time_period == max(ceased_cla_data$time_period)) %>%
      filter(characteristic == "Special guardianship orders") %>%
      select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
      arrange(desc(`Ceased (%)`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Reason ceased" = "characteristic", "Total ceased" = "Total_num")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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

    max_rate <- max(ceased_cla_data$`Ceased (%)`[ceased_cla_data$time_period == max(ceased_cla_data$time_period) &
      ceased_cla_data$geographic_level == "Local authority" &
      ceased_cla_data$characteristic == "Special guardianship orders"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased due to SGO (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage ceased CLA due to SGO by local authority")
    title <- paste0("Percentage ceased CLA due to SGO by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
    } else if (input$select_geography_o2 %in% c("Local authority", "National")) {
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
      defaultColDef = colDef(align = "center"),
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
  ### CAO time series plot ----
  output$CAO_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = ceased_cla_data,
      select_geographic_level = input$select_geography_o2,
      select_geo_breakdown = input$geographic_breakdown_o2,
      check_compare_national = input$national_comparison_checkbox_o2,
      check_compare_regional = input$region_comparison_checkbox_o2,
      check_compare_sn = input$sn_comparison_checkbox_o2,
      dimensional_filters = list("characteristic" = "Residence order or child arrangement order granted")
    )

    # Set the max y-axis scale
    max_rate <- max(ceased_cla_data$`Ceased (%)`[ceased_cla_data$characteristic == "Residence order or child arrangement order granted"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o2, input$geographic_breakdown_o2, "Ceased (%)", "Ceased due to CAO (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage ceased CLA due to CAO")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### CAO time series table ----
  output$table_cao_ceased <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      need(input$geographic_breakdown_o2 != "", "Select a location.")
    )

    filtered_data <- filter_time_series_data(
      dataset_in = ceased_cla_data,
      select_geographic_level = input$select_geography_o2,
      select_geo_breakdown = input$geographic_breakdown_o2,
      check_compare_national = input$national_comparison_checkbox_o2,
      check_compare_regional = input$region_comparison_checkbox_o2,
      check_compare_sn = input$sn_comparison_checkbox_o2,
      dimensional_filters = list("characteristic" = "Residence order or child arrangement order granted")
    ) %>%
      select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      # need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>% filter(characteristic == "Residence order or child arrangement order granted")

    max_rate <- max(ceased_cla_data$`Ceased (%)`[ceased_cla_data$time_period == max(ceased_cla_data$time_period) &
      ceased_cla_data$geographic_level == "Regional" &
      ceased_cla_data$characteristic == "Residence order or child arrangement order granted"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Ceased (%)", "Ceased due to CAO (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage ceased CLA due to CAO by region")
    title <- paste0("Percentage ceased CLA due to CAO by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # ceased by region table
  output$table_cao_ceased_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o2 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o2 != "", "Select a location.")
    )
    data <- ceased_cla_data %>%
      filter(geographic_level == "Regional", time_period == max(ceased_cla_data$time_period)) %>%
      filter(characteristic == "Residence order or child arrangement order granted") %>%
      select(time_period, geo_breakdown, characteristic, `Number ceased`, Total_num, `Ceased (%)`) %>%
      arrange(desc(`Ceased (%)`)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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

    max_rate <- max(ceased_cla_data$`Ceased (%)`[ceased_cla_data$time_period == max(ceased_cla_data$time_period) &
      ceased_cla_data$geographic_level == "Local authority" &
      ceased_cla_data$characteristic == "Residence order or child arrangement order granted"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased due to CAO (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage ceased CLA due to CAO by local authority")
    title <- paste0("Percentage ceased CLA due to CAO by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
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
    choices <- sort(unique(cla_rates[(cla_rates$geographic_level == input$select_geography_o3 & cla_rates$time_period == max(cla_rates$time_period))]$geo_breakdown), decreasing = FALSE)

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
    generate_choice_text1(input$select_geography_o3, input$geographic_breakdown_o3, region_for_la_o3())
  })

  output$outcome3_choice_text2 <- renderText({
    generate_choice_text2(input$national_comparison_checkbox_o3, input$region_comparison_checkbox_o3, input$sn_comparison_checkbox_o3)
  })

  observeEvent(input$select_geography_o3, {
    if (input$select_geography_o3 == "Regional") {
      updateCheckboxInput(session, "Yes_national_o3", value = FALSE)
      updateCheckboxInput(session, "Yes_region_o3", value = FALSE)
    } else if (input$select_geography_o3 == "National") {
      updateCheckboxInput(session, "Yes_national_o3", value = FALSE)
      updateCheckboxInput(session, "Yes_region_o3", value = FALSE)
    }
  })

  # Child protection plan repeated during year headline box
  output$cpp_in_year_txt <- renderText({
    stat <- format(repeat_cpp %>%
      filter(time_period == max(repeat_cpp$time_period) & geo_breakdown %in% input$geographic_breakdown_o3) %>%
      select(CPP_subsequent_percent), nsmall = 1)

    if (input$geographic_breakdown_o3 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(repeat_cpp$time_period), ")", "</p>"
    )
  })

  ## Repeat CPP ----
  ### Repeat CPP time series plot ----
  output$repeat_cpp_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = repeat_cpp,
      select_geographic_level = input$select_geography_o3,
      select_geo_breakdown = input$geographic_breakdown_o3,
      check_compare_national = input$national_comparison_checkbox_o3,
      check_compare_regional = input$region_comparison_checkbox_o3,
      check_compare_sn = input$sn_comparison_checkbox_o3,
      dimensional_filters = list()
    ) %>%
      rename("Repeat CPP (%)" = "Repeat_CPP_percent")

    max_rate <- max(repeat_cpp$`Repeat_CPP_percent`, na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "Repeat CPP (%)", "Repeat CPP (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Repeat CPP (%)")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### Repeat CPP time series table ----
  output$table_repeat_cpp <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = repeat_cpp,
      select_geographic_level = input$select_geography_o3,
      select_geo_breakdown = input$geographic_breakdown_o3,
      check_compare_national = input$national_comparison_checkbox_o3,
      check_compare_regional = input$region_comparison_checkbox_o3,
      check_compare_sn = input$sn_comparison_checkbox_o3,
      dimensional_filters = list()
    ) %>%
      select(time_period, geo_breakdown, CPP_start, CPP_subsequent, CPP_subsequent_percent) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "CPP Starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent", "Repeat CPP (%)" = "CPP_subsequent_percent")

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      # need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- repeat_cpp %>%
      rename("Repeat CPP (%)" = "Repeat_CPP_percent")

    max_rate <- max(repeat_cpp$`Repeat_CPP_percent`[repeat_cpp$time_period == max(repeat_cpp$time_period) &
      repeat_cpp$geographic_level == "Regional"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Repeat CPP (%)", "Repeat CPP (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Repeat CPP (%) by region")
    title <- paste0("Repeat CPP (%) by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # cpp by region table
  output$table_cpp_repeat_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- repeat_cpp %>%
      filter(geographic_level == "Regional", time_period == max(repeat_cpp$time_period)) %>%
      select(time_period, geo_breakdown, CPP_start, CPP_subsequent, Repeat_CPP_percent) %>%
      arrange(desc(Repeat_CPP_percent)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "CPP Starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent", "Repeat CPP (%)" = "Repeat_CPP_percent")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `CPP Starts` = colDef(cell = cellfunc),
        `Repeat CPP` = colDef(cell = cellfunc),
        `Repeat CPP (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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

    max_rate <- max(repeat_cpp$`Repeat_CPP_percent`[repeat_cpp$time_period == max(repeat_cpp$time_period) &
      repeat_cpp$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Repeat CPP (%)", "Repeat CPP (%)", yupperlim = max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Repeat CPP (%) by local authority")
    title <- paste0("Repeat CPP (%) by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `CPP Starts` = colDef(cell = cellfunc),
        `Repeat CPP` = colDef(cell = cellfunc),
        `Repeat CPP (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  ## CPP 2+ years ----
  # Child protection plan longer than two years headline box
  output$cpp_duration_txt <- renderText({
    stat <- format(duration_cpp %>%
      filter(time_period == max(duration_cpp$time_period) & geo_breakdown %in% input$geographic_breakdown_o3) %>%
      select(X2_years_or_more_percent), nsmall = 1)

    if (input$geographic_breakdown_o3 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(duration_cpp$time_period), ")", "</p>"
    )
  })

  ### CPP 2+ time series chart ----
  output$duration_cpp_time_series <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      # need(input$select_geography_o3 != "Local authority", "LA data not available due to large amount of suppression. Please select 'Omitted Data Reasons' for more information"),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )


    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = duration_cpp,
      select_geographic_level = input$select_geography_o3,
      select_geo_breakdown = input$geographic_breakdown_o3,
      check_compare_national = input$national_comparison_checkbox_o3,
      check_compare_regional = input$region_comparison_checkbox_o3,
      check_compare_sn = input$sn_comparison_checkbox_o3,
      dimensional_filters = list()
    )

    # Set the max y-axis scale
    max_rate <- max(duration_cpp$`CPP_2_years_or_more_percent`, na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "CPP_2_years_or_more_percent", "CPP 2+ years (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percent of CPP longer than 2 years")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### CPP 2+ time series table ----
  output$table_duration_cpp <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = duration_cpp,
      select_geographic_level = input$select_geography_o3,
      select_geo_breakdown = input$geographic_breakdown_o3,
      check_compare_national = input$national_comparison_checkbox_o3,
      check_compare_regional = input$region_comparison_checkbox_o3,
      check_compare_sn = input$sn_comparison_checkbox_o3,
      dimensional_filters = list()
    )

    filtered_data <- filtered_data %>%
      select(time_period, geo_breakdown, X2_years_or_more, X2_years_or_more_percent) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "CPP 2+ Years" = "X2_years_or_more", "CPP 2+ Years (%)" = "X2_years_or_more_percent")

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `CPP 2+ Years` = colDef(cell = cellfunc),
        `CPP 2+ Years (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by region
  output$plot_cpp_duration_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- duration_cpp

    max_rate <- max(duration_cpp$`CPP_2_years_or_more_percent`[duration_cpp$time_period == max(duration_cpp$time_period) &
      duration_cpp$geographic_level == "Regional"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "CPP_2_years_or_more_percent", "CPP 2+ years (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percent of CPP longer than 2 years by region")
    title <- paste0("Percent of CPP longer than 2 years by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # by region table
  output$table_cpp_duration_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- duration_cpp %>%
      filter(geographic_level == "Regional", time_period == max(duration_cpp$time_period)) %>%
      select(time_period, geo_breakdown, X2_years_or_more, X2_years_or_more_percent) %>%
      arrange(desc(X2_years_or_more_percent)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "CPP 2+ Years" = "X2_years_or_more", "CPP 2+ Years (%)" = "X2_years_or_more_percent")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `CPP 2+ Years` = colDef(cell = cellfunc),
        `CPP 2+ Years (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # by la
  output$plot_cpp_duration_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    data <- duration_cpp %>%
      rename("CPP 2+ years (%)" = "CPP_2_years_or_more_percent")

    max_rate <- max(duration_cpp$`CPP_2_years_or_more_percent`[duration_cpp$time_period == max(duration_cpp$time_period) &
      duration_cpp$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "CPP 2+ years (%)", "CPP 2+ years (%)", yupperlim = max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Repeat CPP (%) by local authority")
    title <- paste0("CPP 2+ years (%) by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # CPP by LA table
  output$table_cpp_duration_la <- renderReactable({
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

      data <- duration_cpp %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, X2_years_or_more, CPP_2_years_or_more_percent) %>%
        arrange(desc(CPP_2_years_or_more_percent)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `CPP 2+ years` = X2_years_or_more, `CPP 2+ years (%)` = CPP_2_years_or_more_percent)
    } else if (input$select_geography_o3 %in% c("Local authority", "National")) {
      data <- duration_cpp %>%
        filter(geographic_level == "Local authority", time_period == max(duration_cpp$time_period)) %>%
        select(time_period, geo_breakdown, X2_years_or_more, CPP_2_years_or_more_percent) %>%
        arrange(desc(CPP_2_years_or_more_percent)) %>%
        rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `CPP 2+ years` = X2_years_or_more, `CPP 2+ years (%)` = CPP_2_years_or_more_percent)
    }

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `CPP 2+ years` = colDef(cell = cellfunc),
        `CPP 2+ years (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### Hospital admissions -----
  # hospital admission is a recently added chart/table and uses a reactiveValues() and moduleServer set up.

  rv_hosp_admissions <- reactiveValues(select_geographic_level = NULL, select_geo_breakdown = NULL, check_compare_national = NULL, check_compare_regional = NULL, check_compare_sn = NULL)

  observeEvent(ignoreInit = TRUE, list(
    input$select_geography_o3, input$geographic_breakdown_o3, input$national_comparison_checkbox_o3, input$region_comparison_checkbox_o3, input$sn_comparison_checkbox_o3
  ), {
    req(input$select_geography_o3, input$geographic_breakdown_o3)
    rv_hosp_admissions$select_geographic_level <- input$select_geography_o3
    rv_hosp_admissions$select_geo_breakdown <- input$geographic_breakdown_o3
    rv_hosp_admissions$check_compare_national <- input$national_comparison_checkbox_o3
    rv_hosp_admissions$check_compare_regional <- input$region_comparison_checkbox_o3
    rv_hosp_admissions$check_compare_sn <- input$sn_comparison_checkbox_o3
  }) # bindEvent(list(input$geographic_breakdown_o3,input$select_geography_o3))

  timeseries_section_server("hospital_admissions",
    rv = rv_hosp_admissions,
    dataset = copy(hospital_admissions),
    chart_title = "Hospital admissions rate per 10,000 children",
    yvalue = "Value",
    yaxis_title = "Rate per 10k",
    max_rate = calculate_max_rate(hospital_admissions, "Value"),
    rt_columns = list("Time period" = "time_period", "Location" = "geo_breakdown", "Rate per 10k" = "Value"),
    rt_col_defs = list(
      "Rate per 10k" = colDef(cell = cellfunc)
    ),
    decimal_percentage = TRUE
  )

  output$hosp_admissions_txt <- renderText({
    stat <- format(hospital_admissions %>%
      filter(time_period == max(hospital_admissions$time_period) &
        geo_breakdown %in% input$geographic_breakdown_o3) %>%
      select(rate_per_10000), nsmall = 0)

    if (input$geographic_breakdown_o3 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }
    paste0(format(stat, nsmall = 0), "<br>", "<p style='font-size:16px; font-weight:500;'>", "per 10,000 (", max(hospital_admissions$time_period), ")", "</p>")
  })


  output$admissions_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o3 != "", "Select a location."),
    )

    data <- hospital_admissions %>%
      filter(time_period == max(hospital_admissions$time_period), geographic_level == "Regional") %>%
      rename("Rate per 10,000" = `Value`)

    max_lim <- max(data$`Rate per 10,000`) + 10

    p <- by_region_bar_plot(data, "Rate per 10,000", "Rate per 10,000", max_lim) %>%
      config(displayModeBar = F)
    title <- paste0("Hospital admissions caused by unintentional and deliberate injuries to young people (0 to 14 years), by\nregion (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  output$admissions_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o3 != "", "Select a location.")
    )

    data <- hospital_admissions %>%
      filter(time_period == max(hospital_admissions$time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, Value) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Rate per 10,000` = `Value`)

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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

    max_y_lim <- max(data$`Rate per 10,000`) + 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Rate per 10,000", "Rate per 10,000") +
      scale_y_continuous(limits = c(0, max_y_lim))
    title <- paste0("Hospital admissions caused by unintentional and deliberate injuries to young people (0 to 14 years), by\nlocal authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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

    if (input$select_geography_o3 == "Regional") {
      # Check if the selected region is London
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
      data <- data %>%
        filter(`Local authority` %in% location)
    }
    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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
    p <- p + ggtitle("Factors identified at the end of assessment in the year to 31 March 2024 related to child abuse or neglect")
    ggplotly(
      p,
      tooltip = "text",
      height = 420
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Rate per 10000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## Assessment Factors ----
  ### Assessment Factors time series chart, by region and la charts will need to be filtered by the extra dropdown ----
  output$child_abuse_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = assessment_factors,
      select_geographic_level = input$select_geography_o3,
      select_geo_breakdown = input$geographic_breakdown_o3,
      check_compare_national = input$national_comparison_checkbox_o3,
      check_compare_regional = input$region_comparison_checkbox_o3,
      check_compare_sn = input$sn_comparison_checkbox_o3,
      dimensional_filters = list("assessment_factor" = input$assessment_factors_1)
    )

    max_y_lim <- max(filtered_data$rate_per_10000) + 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "rate_per_10000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste(input$assessment_factors_1, "cases (rate per 10,000)")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### Assessment Factors child abuse ts table ----
  output$ca_ts_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = assessment_factors,
      select_geographic_level = input$select_geography_o3,
      select_geo_breakdown = input$geographic_breakdown_o3,
      check_compare_national = input$national_comparison_checkbox_o3,
      check_compare_regional = input$region_comparison_checkbox_o3,
      check_compare_sn = input$sn_comparison_checkbox_o3,
      dimensional_filters = list("assessment_factor" = input$assessment_factors_1)
    )

    filtered_data <- filtered_data %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)


    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      # need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )

    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_1) %>%
      filter(time_period == max(time_period), geographic_level == "Regional")

    max_lim <- max(data$rate_per_10000) + 20

    p <- by_region_bar_plot(data, "rate_per_10000", "Rate per 10,000", max_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste0(input$assessment_factors_1, " cases (rate per 10,000), by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  output$child_abuse_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$assessment_factors_1 != "", "Select an assessment factor.")
    )

    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_1, time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      arrange(desc(rate_per_10000)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)


    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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

    max_y_lim <- max(data$rate_per_10000) + 20

    p <- factors_by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "rate_per_10000", "Rate per 10,000") +
      scale_y_continuous(limits = c(0, max_y_lim))
    title_factor <- paste0(input$assessment_factors_1, " cases (rate per 10,000), by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## EFH (Harms outside the home) -------
  ### EFH titles ----
  output$efh_header1 <- renderUI({
    h2(paste(input$assessment_factors_2, " cases"))
  })
  output$efh_header2 <- renderUI({
    h2(paste(input$assessment_factors_2, " cases, by region"))
  })
  output$efh_header3 <- renderUI({
    h2(paste(input$assessment_factors_2, " cases, by local authority"))
  })


  ### Harms outside the home bar plot------
  output$extra_familial_all_af_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    p <- all_assessment_factors_plot(assessment_factors, extra_familial_harm_af, selected_geo_breakdown = input$geographic_breakdown_o3) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Factors identified at the end of assessment in the year to 31 March 2024 related to specific types of harms\n outside the home")
    ggplotly(
      p,
      tooltip = "text",
      height = 420
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Rate per 10,000` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ### EF time series chart ----
  # the time series chart, by region and la charts will need to be filtered by the extra dropdown
  output$efh_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = assessment_factors,
      select_geographic_level = input$select_geography_o3,
      select_geo_breakdown = input$geographic_breakdown_o3,
      check_compare_national = input$national_comparison_checkbox_o3,
      check_compare_regional = input$region_comparison_checkbox_o3,
      check_compare_sn = input$sn_comparison_checkbox_o3,
      dimensional_filters = list("assessment_factor" = input$assessment_factors_2)
    )

    max_y_lim <- max(filtered_data$rate_per_10000) + 20
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o3, input$geographic_breakdown_o3, "rate_per_10000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste(input$assessment_factors_2, "cases (rate per 10,000)")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### EF child abuse ts table alternative ----
  output$efh_ts_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = assessment_factors,
      select_geographic_level = input$select_geography_o3,
      select_geo_breakdown = input$geographic_breakdown_o3,
      check_compare_national = input$national_comparison_checkbox_o3,
      check_compare_regional = input$region_comparison_checkbox_o3,
      check_compare_sn = input$sn_comparison_checkbox_o3,
      dimensional_filters = list("assessment_factor" = input$assessment_factors_2)
    ) %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )

    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_2) %>%
      filter(time_period == max(time_period), geographic_level == "Regional")

    max_lim <- max(data$rate_per_10000) + 10
    p <- by_region_bar_plot(data, "rate_per_10000", "Rate per 10,000", max_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste0(input$assessment_factors_2, " cases (rate per 10,000), by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  output$efh_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o3 != "", "Select a geography level."),
      need(input$assessment_factors_2 != "", "Select an assessment factor.")
    )

    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_2, time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, assessment_factor, rate_per_10000) %>%
      arrange(desc(rate_per_10000)) %>%
      rename(`Time period` = `time_period`, `Region` = `geo_breakdown`, `Assessment factor` = `assessment_factor`, `Rate per 10,000` = `rate_per_10000`)


    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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

    p <- factors_by_la_bar_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "rate_per_10000", "Rate per 10,000") +
      scale_y_continuous(limits = c(0, max_y_lim))
    title_factor <- paste0(input$assessment_factors_2, " cases (rate per 10,000), by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
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
    choices <- sort(unique(placement_data[geographic_level == input$select_geography_o4 & time_period == max(placement_data$time_period)]$geo_breakdown), decreasing = FALSE)

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
    generate_choice_text1(input$select_geography_o4, input$geographic_breakdown_o4, region_for_la_o4())
  })

  output$outcome4_choice_text2 <- renderText({
    generate_choice_text2(input$national_comparison_checkbox_o4, input$region_comparison_checkbox_o4, input$sn_comparison_checkbox_o4)
  })

  observeEvent(input$select_geography_o4, {
    if (input$select_geography_o4 == "Regional") {
      updateCheckboxInput(session, "Yes_national_o4", value = FALSE)
      updateCheckboxInput(session, "Yes_region_o4", value = FALSE)
    } else if (input$select_geography_o4 == "National") {
      updateCheckboxInput(session, "Yes_national_o4", value = FALSE)
      updateCheckboxInput(session, "Yes_region_o4", value = FALSE)
    }
  })

  ### Headline boxes ----
  output$placement_changes_txt <- renderText({
    stat <- format(placement_changes_data %>%
      filter(time_period == max(placement_changes_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
      filter(placement_stability == "With 3 or more placements during the year") %>%
      select(Percentage), nsmall = 1)

    if (input$geographic_breakdown_o4 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_changes_data$time_period), ")", "</p>"
    )
  })

  output$foster_placement_txt <- renderText({
    stat <- format(placement_data %>%
      filter(time_period == max(placement_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
      filter(characteristic == "Foster placements") %>%
      select(percentage), nsmall = 0)

    if (input$geographic_breakdown_o4 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_data$time_period), ")", "</p>"
    )
  })

  output$secure_home_placement_txt <- renderText({
    stat <- format(placement_data %>%
      filter(time_period == max(placement_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
      filter(characteristic == "Secure homes and children's homes") %>%
      select(percentage), nsmall = 0)

    if (input$geographic_breakdown_o4 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_data$time_period), ")", "</p>"
    )
  })

  output$residential_placement_txt <- renderText({
    stat <- format(placement_data %>%
      filter(time_period == max(placement_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
      filter(characteristic == "Independent and semi-independent living arrangements/supported accommodation") %>%
      select(percentage), nsmall = 0)

    if (input$geographic_breakdown_o4 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_data$time_period), ")", "</p>"
    )
  })

  output$placement_distance_txt <- renderText({
    stat <- format(placement_data %>%
      filter(time_period == max(placement_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
      filter(characteristic == "Placed more than 20 miles from home") %>%
      select(percentage), nsmall = 0)

    if (input$geographic_breakdown_o4 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(placement_data$time_period), ")", "</p>"
    )
  })

  output$care_leavers_employment_txt1 <- renderText({
    stat <- care_leavers_activity_data %>%
      filter(time_period == max(care_leavers_activity_data$time_period) &
        geo_breakdown %in% input$geographic_breakdown_o4 &
        age == "17 to 18 years" &
        activity == "Total in education, employment or training") %>%
      select(percentage)

    if (input$geographic_breakdown_o4 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(care_leavers_activity_data$time_period), ")", "</p>")
  })

  output$care_leavers_employment_txt2 <- renderText({
    stat <- care_leavers_activity_data %>%
      filter(time_period == max(care_leavers_activity_data$time_period) &
        geo_breakdown %in% input$geographic_breakdown_o4 &
        age == "19 to 21 years" &
        activity == "Total in education, employment or training") %>%
      select(percentage)

    if (input$geographic_breakdown_o4 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(care_leavers_activity_data$time_period), ")", "</p>")
  })

  output$care_leavers_accommodation_txt1 <- renderText({
    stat <- care_leavers_accommodation_data %>%
      filter(time_period == max(time_period) &
        geo_breakdown %in% input$geographic_breakdown_o4 &
        age == "17 to 18 years" &
        accommodation_suitability == "Accommodation considered suitable") %>%
      select(percentage)

    if (input$geographic_breakdown_o4 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(care_leavers_accommodation_data$time_period), ")", "</p>")
  })

  output$care_leavers_accommodation_txt2 <- renderText({
    stat <- care_leavers_accommodation_data %>%
      filter(time_period == max(time_period) &
        geo_breakdown %in% input$geographic_breakdown_o4 &
        age == "19 to 21 years" &
        accommodation_suitability == "Accommodation considered suitable") %>%
      select(percentage)

    if (input$geographic_breakdown_o4 == "" || nrow(stat) == 0) {
      stat <- "NA"
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
        filter(age_start_poc == "Total (all ages)") %>%
        select(months), nsmall = 0)
    }
    paste0(
      stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(All ages - ", max(placement_order_match_data$time_period), ")", "</p>"
    )
  })

  ## Placement type charts and tables ----
  #### Placement type Time series chart ----
  output$placement_type_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = placement_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("characteristic" = input$placement_type_breakdown)
    ) %>%
      rename("Placements (%)" = "Percent")

    # Set the max y-axis scale
    max_rate <- max(placement_data$`Percent`[placement_data$characteristic %in% c(unique(placement_type_filter))], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Placements (%)", "Placements (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Children living in selected placement type (%)")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### Placement type Time series table ----
  output$placement_type_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )
    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = placement_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("characteristic" = input$placement_type_breakdown)
    ) %>%
      select(time_period, geo_breakdown, characteristic, Percent) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Placement Type` = `characteristic`, `Placements (%)` = `Percent`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      need(input$placement_type_breakdown != "", "Select a placement type.")
    )

    data <- placement_data %>%
      filter(characteristic == input$placement_type_breakdown) %>%
      filter(time_period == max(time_period), geographic_level == "Regional") %>%
      rename("Placements (%)" = "Percent")

    max_rate <- max(placement_data$`Percent`[placement_data$time_period == max(placement_data$time_period) &
      placement_data$geographic_level == "Regional" &
      placement_data$characteristic %in% c(unique(placement_type_filter))], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Placements (%)", "Placements (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    title_placements <- paste("Children living in selected placement type (%) by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title_placements)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  output$placement_type_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o4 != "", "Select a location."),
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
      defaultColDef = colDef(align = "center"),
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

    max_rate <- max(placement_data$`Percent`[placement_data$time_period == max(placement_data$time_period) &
      placement_data$geographic_level == "Local authority" &
      placement_data$characteristic %in% c(unique(placement_type_filter))], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Placements (%)", "Placements (%)", max_rate, decimal_percentage = FALSE) +
      scale_y_continuous(limits = c(0, 100))
    title_placements <- paste0("Children living in selected placement type (%) by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title_placements)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })



  ## Placement changes ----
  #### Placement changes Time series chart ----
  output$placement_changes_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = placement_changes_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("placement_stability" = "With 3 or more placements during the year")
    ) %>%
      rename("CLA with 3 or more placements (%)" = "Percent")

    # Set the max y-axis scale
    max_rate <- max(placement_changes_data$`Percent`[placement_changes_data$placement_stability == "With 3 or more placements during the year"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "CLA with 3 or more placements (%)", "CLA with 3 or more placements (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage of CLA with 3 or more placements during the year")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  #### Placement changes Time series table ----
  output$placement_changes_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = placement_changes_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("placement_stability" = "With 3 or more placements during the year")
    ) %>%
      select(time_period, geo_breakdown, Percent) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CLA with 3 or more placements during the year(%)` = `Percent`)


    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      # need(input$geographic_breakdown_o4 != "", "Select a location."),
    )

    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year") %>%
      filter(time_period == max(time_period), geographic_level == "Regional") %>%
      rename("CLA with 3 or more placements (%)" = "Percent")

    max_rate <- max(placement_changes_data$`Percent`[placement_changes_data$time_period == max(placement_changes_data$time_period) &
      placement_changes_data$geographic_level == "Regional" &
      placement_changes_data$placement_stability == "With 3 or more placements during the year"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "CLA with 3 or more placements (%)", "CLA with 3 or more placements (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage of CLA with 3 or more placements during the year by region")
    title <- paste0("Percentage of CLA with 3 or more placements during the year by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  output$placement_changes_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o4 != "", "Select a location."),
    )

    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year", time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, Percent) %>%
      arrange(desc(Percent)) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `CLA with 3 or more placements (%)` = `Percent`)


    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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


    max_rate <- max(placement_changes_data$`Percent`[placement_changes_data$time_period == max(placement_changes_data$time_period) &
      placement_changes_data$geographic_level == "Local authority" &
      placement_changes_data$placement_stability == "With 3 or more placements during the year"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10


    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "CLA with 3 or more placements (%)", "CLA with 3 or more placements (%)", max_rate, decimal_percentage = FALSE)
    # p <- p + ggtitle("Percentage of CLA with 3 or more placements during the year by local authority")
    title <- paste0("Percentage of CLA with 3 or more placements during the year by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `CLA with 3 or more placements (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  ## Placement Distance -------------
  #### Placement distance Time series chart ----
  output$placement_distance_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = placement_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("characteristic" = "Placed more than 20 miles from home")
    ) %>%
      rename("Placements more then 20 miles from home (%)" = "Percent")

    # Set the max y-axis scale
    max_rate <- max(placement_data$`Percent`[placement_data$characteristic == "Placed more than 20 miles from home"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Placements more then 20 miles from home (%)", "Placements (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Percentage of placements more than 20 miles from home")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })


  #### Placement distance timeseries table ----
  output$placement_dist_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = placement_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("characteristic" = "Placed more than 20 miles from home")
    ) %>%
      select(time_period, geo_breakdown, characteristic, Percent) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Placement Distance` = `characteristic`, `Placements (%)` = `Percent`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      #  need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    data <- placement_data %>%
      filter(characteristic == "Placed more than 20 miles from home") %>%
      filter(time_period == max(time_period), geographic_level == "Regional") %>%
      rename("Placements more then 20 miles from home (%)" = "Percent")

    max_rate <- max(placement_data$`Percent`[placement_data$time_period == max(placement_data$time_period) &
      placement_data$geographic_level == "Regional" &
      placement_data$characteristic == "Placed more than 20 miles from home"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Placements more then 20 miles from home (%)", "Placements (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage of placements more than 20 miles from home by region")
    title <- paste0("Percentage of placements more than 20 miles from home by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  output$placement_dist_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    data <- placement_data %>%
      filter(characteristic == "Placed more than 20 miles from home", time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, characteristic, Percent) %>%
      arrange(desc(Percent))

    data <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Placement Distance` = `characteristic`, `Placements (%)` = `Percent`)
    reactable(
      data,
      defaultColDef = colDef(align = "center"),
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

    max_rate <- max(placement_data$`Percent`[placement_data$time_period == max(placement_data$time_period) &
      placement_data$geographic_level == "Local authority" &
      placement_data$characteristic == "Placed more than 20 miles from home"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Placements more then 20 miles from home (%)", "Placements (%)", max_rate, decimal_percentage = FALSE)
    # p <- p + ggtitle("Percentage of placements more than 20 miles from home by local authority")
    title <- paste0("Percentage of placements more than 20 miles from home by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
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
    title <- paste0("Average time between placement order and match for those children who are adopted", " (", input$select_age_group_o4, ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Months` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })


  ## Wellbeing SDQ score ----------------
  output$wellbeing_score_stat <- renderText({
    stat_prev <- format(wellbeing_sdq_data %>%
      filter(time_period == (max(wellbeing_sdq_data$time_period) - 1) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
      filter(characteristic == "SDQ average score") %>%
      select(number), nsmall = 1)

    stat_current <- format(wellbeing_sdq_data %>%
      filter(time_period == max(wellbeing_sdq_data$time_period) & geo_breakdown %in% input$geographic_breakdown_o4) %>%
      filter(characteristic == "SDQ average score") %>%
      select(number), nsmall = 1)

    if (input$geographic_breakdown_o4 == "" || nrow(stat_current) == 0) {
      stat_current <- "NA"
    }

    if (nrow(stat_prev) == 0) {
      stat_prev <- "NA"
    }

    if (is.na(as.numeric(stat_prev))) {
      context <- " compared to "
    } else if (is.na(as.numeric(stat_current))) {
      context <- " compared to "
    } else if ((as.numeric(stat_current) < as.numeric(stat_prev))) {
      context <- " down from "
    } else if ((as.numeric(stat_current) > as.numeric(stat_prev))) {
      context <- " up from "
    } else if ((as.numeric(stat_current) == as.numeric(stat_prev))) {
      context <- " no change from "
    } else {
      context <- " compared to "
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
      select(time_period, geographic_level, geo_breakdown, characteristic, score_label, number_num) %>%
      rename("Average score" = "number_num")

    max_y_lim <- (max(final_data$`Average score`) + 5)

    p <- plotly_time_series_custom_scale(final_data, input$select_geography_o4, input$geographic_breakdown_o4, "Average score", "Average SDQ score", max_y_lim, add_rect = TRUE) %>%
      config(displayModeBar = F)

    p <- p + ggtitle("Average SDQ score")

    plot <- ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(hovermode = "x") %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      select(time_period, geo_breakdown, characteristic, number_num, score_label) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `SDQ characteristic` = `characteristic`, `SDQ score` = `score_label`, "Average score" = "number_num")

    reactable(
      final_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Average score` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
      # need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    data <- wellbeing_sdq_data %>%
      filter(characteristic == "SDQ average score") %>%
      filter(time_period == max(time_period), geographic_level == "Regional") %>%
      rename("Average score" = "number_num")

    max_y_lim <- (max(data$`Average score`) + 5)

    suppressWarnings(
      p <- by_region_bar_plot(data, "Average score", "Average SDQ score", max_y_lim) +
        geom_hline(linetype = "dashed", colour = "red", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
        geom_hline(linetype = "dot", colour = "blue", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17"))) %>%
        config(displayModeBar = F)
    )

    p <- by_region_bar_plot(data, "Average score", "Average SDQ score", max_y_lim, add_rect = TRUE) #+
    # p <- p + ggtitle("Average SDQ score by region")
    title <- paste0("Average SDQ score by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      # geom_hline(linetype = "dashed", colour = "red", aes(yintercept = 14, text = paste("Borderline", "<br>", "Score: 14"))) +
      # geom_hline(linetype = "dot", colour = "blue", aes(yintercept = 17, text = paste("Cause for concern", "<br>", "Score: 17")))
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      layout(hovermode = "x") %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })


  output$SDQ_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      # need(input$geographic_breakdown_o4 != "", "Select a location.")
    )

    data <- wellbeing_sdq_data %>%
      filter(characteristic == "SDQ average score", time_period == max(time_period), geographic_level == "Regional") %>%
      select(time_period, geo_breakdown, characteristic, number_num, score_label) %>%
      arrange(desc(number_num))

    data2 <- data %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `SDQ characteristic` = `characteristic`, `Average score` = `number_num`, `SDQ score` = `score_label`)

    reactable(
      data2,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Average score` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
      rename(`Average score` = `number_num`)

    max_y_lim <- (max(data$`Average score`) + 5)

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Average score", "Average SDQ score", yupperlim = max_y_lim, add_rect = TRUE) +
      scale_y_continuous(limits = c(0, max_y_lim))

    # p <- p + ggtitle("Average SDQ score by local authority")
    title <- paste0("Average SDQ score by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)


    ggplotly(
      p %>%
        config(displayModeBar = F),
      tooltip = "text",
      height = 420
    ) %>%
      layout(hovermode = "x") %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })
  # table
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
        select(time_period, geo_breakdown, characteristic, number_num, score_label) %>%
        arrange(desc(number_num))
    } else if (input$select_geography_o4 %in% c("Local authority", "National")) {
      data <- wellbeing_sdq_data %>%
        filter(geographic_level == "Local authority", time_period == max(wellbeing_sdq_data$time_period)) %>%
        filter(characteristic == "SDQ average score") %>%
        select(time_period, geo_breakdown, characteristic, number_num, score_label) %>%
        arrange(desc(number_num))
    }

    data2 <- data %>%
      arrange(desc(number_num)) %>%
      rename(`Time period` = `time_period`, `Local authority` = `geo_breakdown`, `SDQ characteristic` = `characteristic`, `Average score` = `number_num`, `SDQ score` = `score_label`)

    reactable(
      data2,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Average score` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })




  ## Care leavers ---------
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


  #### CL Time series chart ----
  output$care_activity_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = care_leavers_activity_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("activity" = "Total in education, employment or training", "age" = input$leavers_age)
    ) %>%
      rename("Care leavers in education, employment or training (%)" = "percent")

    # Set the max y-axis scale
    max_rate <- max(care_leavers_activity_data$`percent`[care_leavers_activity_data$activity == "Total in education, employment or training"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Care leavers in education, employment or training (%)", "Care leavers in education,\n employment or training (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in employment, education and training (", input$leavers_age, ")")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ##### CL timeseries table ----
  output$cl_activity_ts_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an assessment factor.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = care_leavers_activity_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("activity" = "Total in education, employment or training", "age" = input$leavers_age)
    ) %>%
      select(time_period, geo_breakdown, activity, age, percent) %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Activity` = `activity`, `Age range` = `age`, `Care leavers in education, employment or training (%)` = `percent`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Care leavers in education, employment or training (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE
    )
  })

  # care leavers activity by region
  output$cl_activity_region_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      #  need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )

    data <- care_leavers_activity_data %>%
      filter(age == input$leavers_age & activity == "Total in education, employment or training" & time_period == max(time_period) & geographic_level == "Regional") %>%
      rename("Care leavers in education, employment or training (%)" = "percent")

    max_rate <- max(care_leavers_activity_data$`percent`[care_leavers_activity_data$time_period == max(care_leavers_activity_data$time_period) &
      care_leavers_activity_data$geographic_level == "Regional" &
      care_leavers_activity_data$activity == "Total in education, employment or training"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Care leavers in education, employment or training (%)", "Care leavers in education,\n employment or training (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in employment, education and training (", input$leavers_age, ") by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  output$cl_activity_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      #   need(input$geographic_breakdown_o4 != "", "Select a location."),
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
      defaultColDef = colDef(align = "center"),
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

    max_rate <- max(care_leavers_activity_data$`percent`[care_leavers_activity_data$time_period == max(care_leavers_activity_data$time_period) &
      care_leavers_activity_data$geographic_level == "Local authority" &
      care_leavers_activity_data$activity == "Total in education, employment or training"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Care leavers in education, employment or training (%)", "Care leavers in education,\n employment or training (%)", max_rate, decimal_percentage = FALSE)
    age_title <- paste0("Care leavers in employment, education and training (", input$leavers_age, ") by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(age_title)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
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

  ##### CL Accom Time series chart ----
  output$care_accommodation_ts_plot <- renderPlotly({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = care_leavers_accommodation_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("accommodation_suitability" = "Accommodation considered suitable", "age" = input$leavers_age)
    ) %>%
      rename("Care leavers in suitable accommodation (%)" = "percent")

    # Set the max y-axis scale
    max_rate <- max(care_leavers_accommodation_data$`percent`[care_leavers_accommodation_data$accommodation_suitability == "Accommodation considered suitable"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o4, input$geographic_breakdown_o4, "Care leavers in suitable accommodation (%)", "Care leavers in suitable\n accommodation (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in suitable accommodation (", input$leavers_age, ")")
    p <- p + ggtitle(age_title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  #### CL Accom timeseries table ----
  output$cl_accommodation_ts_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = care_leavers_accommodation_data,
      select_geographic_level = input$select_geography_o4,
      select_geo_breakdown = input$geographic_breakdown_o4,
      check_compare_national = input$national_comparison_checkbox_o4,
      check_compare_regional = input$region_comparison_checkbox_o4,
      check_compare_sn = input$sn_comparison_checkbox_o4,
      dimensional_filters = list("accommodation_suitability" = "Accommodation considered suitable", "age" = input$leavers_age)
    ) %>%
      select(time_period, geo_breakdown, accommodation_suitability, age, percent) %>%
      rename("Care leavers in suitable accommodation (%)" = "percent") %>%
      rename(`Time period` = `time_period`, `Location` = `geo_breakdown`, `Accommodation suitability` = `accommodation_suitability`, `Age range` = `age`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
      #  need(input$geographic_breakdown_o4 != "", "Select a location."),
      need(input$leavers_age != "", "Select an age range.")
    )

    data <- care_leavers_accommodation_data %>%
      filter(age == input$leavers_age & accommodation_suitability == "Accommodation considered suitable" & time_period == max(time_period) & geographic_level == "Regional") %>%
      rename("Care leavers in suitable accommodation (%)" = "percent")

    max_rate <- max(care_leavers_accommodation_data$`percent`[care_leavers_accommodation_data$time_period == max(care_leavers_accommodation_data$time_period) &
      care_leavers_accommodation_data$geographic_level == "Regional" &
      care_leavers_accommodation_data$accommodation_suitability == "Accommodation considered suitable"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(data, "Care leavers in suitable accommodation (%)", "Care leavers in suitable\n accommodation (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in suitable accommodation (", input$leavers_age, ") by region ", "(", max(p$data$time_period), ")")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 430,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  output$cl_accommodation_region_tbl <- renderReactable({
    shiny::validate(
      need(input$select_geography_o4 != "", "Select a geography level."),
      #   need(input$geographic_breakdown_o4 != "", "Select a location."),
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
      defaultColDef = colDef(align = "center"),
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

    max_rate <- max(care_leavers_accommodation_data$`percent`[care_leavers_accommodation_data$time_period == max(care_leavers_accommodation_data$time_period) &
      care_leavers_accommodation_data$geographic_level == "Local authority" &
      care_leavers_accommodation_data$accommodation_suitability == "Accommodation considered suitable"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Care leavers in suitable accommodation (%)", "Care leavers in suitable\n accommodation (%)", max_rate, decimal_percentage = FALSE)
    age_title <- paste0("Care leavers in suitable accommodation (", input$leavers_age, ") by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(age_title)

    ggplotly(
      p %>%
        config(displayModeBar = F),
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Care leavers in suitable accommodation (%)` = colDef(cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # Enabler 1 ---------------------
  # There is currently no indicators/metrics for this enabler.
  # Working with the department and policy to improve this area.

  # Enabler 2 -------------------------------------------------------------------
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

  ## Confirmation sentence e2
  # This function gets the selected region to put into the confirmation text below

  workforce_region_e2 <- reactive({
    location_data %>%
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
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics for ", tags$b(input$geographic_breakdown_e2), ", in ", workforce_region_e2(), ".")
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


  ## Spending -----
  # Dynamic titles
  output$spending_header1 <- renderUI({
    h2(paste(input$spending_choice, " by region"))
  })
  output$spending_header2 <- renderUI({
    h2(paste(input$spending_choice, " by local authority"))
  })

  ##### Headline stats
  # Share of total spend on CS
  output$total_spending_txt <- renderText({
    stat <- format(spending_data %>% filter(time_period == "2024/25" &
      geo_breakdown %in% input$geographic_breakdown_e2) %>%
      select(`CS Share`), nsmall = 2)

    if (input$geographic_breakdown_e2 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(spending_data$time_period), ")", "</p>")
  })

  # Average spend per child
  output$avg_spend_per_child <- renderText({
    stat <- format(spending_per_capita %>% filter(time_period == max(spending_per_capita$time_period) &
      geo_breakdown %in% input$geographic_breakdown_e2) %>%
      select(`Cost per child`))

    if (input$geographic_breakdown_e2 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0("~ £", prettyNum(stat, big.mark = ","), " per child", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(spending_per_capita$time_period), ")", "</p>")
  })

  # Share of total spend on children's services minus CLA
  output$spend_minus_cla_txt <- renderText({
    stat <- format(spending_data_no_cla %>% filter(time_period == "2024/25" &
      geo_breakdown %in% input$geographic_breakdown_e2) %>%
      select(`Excluding CLA Share`), nsmall = 2)

    if (input$geographic_breakdown_e2 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(spending_data_no_cla$time_period), ")", "</p>")
  })

  ####### Regional plot for spending
  output$plot_spending_region <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      #  need(input$geographic_breakdown_e2 != "", "Select a location."),
      need(input$spending_choice != "", "Select a spending level.")
    )
    # Need an if statement to look at the spending level choice this will determine the data in the chart
    if (input$spending_choice == "Share of total local authority spend on children's services") {
      data <- spending_data %>%
        filter(geographic_level == "Regional", time_period == max(spending_data$time_period)) %>%
        select(time_period, geographic_level, geo_breakdown, cs_share) # %>%

      max_y_lim <- ceiling(max(data$cs_share) / 10) * 10
      p <- by_region_bar_plot(data, "cs_share", "Share spent on children's services (%)", max_y_lim, decimal_percentage = TRUE) %>%
        config(displayModeBar = F)
      title <- paste0("Share of total local authority spend on children's services (%) by region (", max(p$data$time_period), ")")
      p <- p + ggtitle(title)
    } else {
      data <- spending_per_capita %>%
        filter(geographic_level == "Regional", time_period == max(spending_data$time_period)) %>%
        select(time_period, geographic_level, geo_breakdown, cost_per_capita) %>%
        rename("Average spend per child (£)" = "cost_per_capita")

      max_y_lim <- ceiling(max(data$`Average spend per child (£)`) / 50) * 50

      p <- by_region_bar_plot(data, "Average spend per child (£)", "Average spend per child (£)", max_y_lim) %>%
        config(displayModeBar = F)
      # p <- p + ggtitle("Average spend per child (£) by region")
      title <- paste0("Average spend per child (£) by region (", max(p$data$time_period), ")")
      p <- p + ggtitle(title)
    }

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # region table alternative
  output$table_tot_spending_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      #  need(input$geographic_breakdown_e2 != "", "Select a location."),
      need(input$spending_choice != "", "Select a spending level.")
    )
    if (input$spending_choice == "Share of total local authority spend on children's services") {
      data <- spending_data %>%
        filter(geographic_level == "Regional", time_period == max(spending_data$time_period)) %>%
        select(time_period, geo_breakdown, cs_share) %>%
        arrange(desc(cs_share)) %>%
        rename("Time period" = "time_period", "Region" = "geo_breakdown", "Share of spending on children's services (%)" = "cs_share")

      table <- reactable(
        data,
        defaultColDef = colDef(align = "center"),
        columns = list(
          `Share of spending on children's services (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
        defaultColDef = colDef(align = "center"),
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
    if (input$spending_choice == "Share of total local authority spend on children's services") {
      data <- spending_data %>%
        filter(geographic_level == "Local authority", time_period == max(spending_data$time_period)) %>%
        select(time_period, geographic_level, geo_breakdown, cs_share) # %>%

      max_y_lim <- ceiling(max(data$cs_share) / 10) * 10
      p <- by_la_bar_plot(dataset = data, selected_geo_breakdown = input$geographic_breakdown_e2, selected_geo_lvl = input$select_geography_e2, yvalue = "cs_share", yaxis_title = "Share spent on children's services (%)", decimal_percentage = TRUE) %>%
        config(displayModeBar = F)
      title <- paste0("Share of total local authority spend on children's services (%) by local authority (", max(p$data$time_period), ")")
      p <- p + ggtitle(title) +

        scale_y_continuous(limits = c(0, max_y_lim))
    } else {
      data <- spending_per_capita %>%
        filter(geographic_level == "Local authority", time_period == max(spending_data$time_period)) %>%
        select(time_period, geographic_level, geo_breakdown, cost_per_capita) %>%
        rename("Spend per child (£)" = "cost_per_capita")

      max_y_lim <- ceiling(max(data$`Spend per child (£)`) / 50) * 50

      p <- by_la_bar_plot(dataset = data, selected_geo_breakdown = input$geographic_breakdown_e2, selected_geo_lvl = input$select_geography_e2, yvalue = "Spend per child (£)", yaxis_title = "Average spend per child (£)") %>%
        config(displayModeBar = F)
      # p <- p + ggtitle("Average spend per child (£) by local authority") +
      title <- paste0("Average spend per child (£) by local authority (", max(p$data$time_period), ")")
      p <- p + ggtitle(title) +
        scale_y_continuous(limits = c(0, max_y_lim))
    }
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$table_tot_spending_la <- renderReactable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
      need(input$spending_choice != "", "Select a spending level.")
    )
    if (input$spending_choice == "Share of total local authority spend on children's services") {
      data <- spending_data %>%
        filter(geographic_level == "Local authority", time_period == max(spending_data$time_period)) %>%
        select(time_period, geo_breakdown, cs_share) %>%
        arrange(desc(cs_share)) %>%
        rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Share of spending on children's services (%)" = "cs_share")

      table <- reactable(
        data,
        defaultColDef = colDef(align = "center"),
        columns = list(
          `Share of spending on children's services (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
        defaultColDef = colDef(align = "center"),
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
      # need(input$geographic_breakdown_e2 != "", "Select a location.")
    )

    data <- spending_data_no_cla %>%
      filter(geographic_level == "Regional", time_period == max(spending_data_no_cla$time_period)) %>%
      select(time_period, geographic_level, geo_breakdown, minus_cla_share) # %>%

    max_y_lim <- ceiling(max(data$minus_cla_share) / 10) * 10
    p <- by_region_bar_plot(data, "minus_cla_share", "Share spent on children's services\n excluding CLA (%)", max_y_lim, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Share of Children’s Services spend not on CLA (%) by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # region table alternative
  output$table_spend_excl_cla_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e2 != "", "Select a geography level."),
      # need(input$geographic_breakdown_e2 != "", "Select a location.")
    )
    data <- spending_data_no_cla %>%
      filter(geographic_level == "Regional", time_period == max(spending_data_no_cla$time_period)) %>%
      select(time_period, geo_breakdown, minus_cla_share) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Share of Children’s Services spend not on CLA (%)" = "minus_cla_share")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Share of Children’s Services spend not on CLA (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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

    p <- by_la_bar_plot(
      dataset = data, selected_geo_breakdown = input$geographic_breakdown_e2, selected_geo_lvl = input$select_geography_e2, yvalue = "Share minus CLA (%)",
      yaxis_title = "Share of Children’s Services spend not on CLA (%)", decimal_percentage = TRUE
    ) %>%
      config(displayModeBar = F)
    title <- paste0("Share of Children’s Services spend not on CLA (%) by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title) +
      scale_y_continuous(limits = c(0, max_y_lim))
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      rename("Time period" = "time_period", "Local authority" = "geo_breakdown", "Share of Children’s Services spend not on CLA (%)" = "minus_cla_share")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Share of Children’s Services spend not on CLA (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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

  # this reactive is used in the 4 headline boxes
  ofsted_ratings_data <- reactive({
    ofsted_leadership_data_long %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      )) %>%
      filter(geo_breakdown == input$geographic_breakdown_e2)
  })

  output$ofsted_outstanding_headline <- renderText({
    if (input$geographic_breakdown_e2 == "") {
      paste0("NA")
    } else {
      stat <- ofsted_ratings_data()
      stat_final <- stat$Count[which(stat$Rating == "Outstanding")]
      paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(stat$time_period), ")", "</p>")

      # if (input$geographic_breakdown_e2 %in% c("Inner London", "Outer London", "London")) {
      #   stat <- data %>%
      #     filter(geo_breakdown == "London")
      #   stat_final <- stat$Count[which(stat$Rating == "Outstanding")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$inspection_date), ")", "</p>")
      # } else if (input$geographic_breakdown_e2 %in% c("North East", "Yorkshire and The Humber")) {
      #   stat <- data %>%
      #     filter(geo_breakdown == "North East, Yorkshire and the Humber")
      #   stat_final <- stat$Count[which(stat$Rating == "Outstanding")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # } else {
      #   stat <- data %>%
      #     filter(geo_breakdown == input$geographic_breakdown_e2)
      #   stat_final <- stat$Count[which(stat$Rating == "Outstanding")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # }
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
      stat <- ofsted_ratings_data()
      stat_final <- stat$Count[which(stat$Rating == "Good")]
      paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(stat$time_period), ")", "</p>")
      # if (input$geographic_breakdown_e2 %in% c("Inner London", "Outer London", "London")) {
      #   stat <- data %>%
      #     filter(geo_breakdown == "London")
      #   stat_final <- stat$Count[which(stat$Rating == "Good")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # } else if (input$geographic_breakdown_e2 %in% c("North East", "Yorkshire and The Humber")) {
      #   stat <- data %>%
      #     filter(geo_breakdown == "North East, Yorkshire and the Humber")
      #   stat_final <- stat$Count[which(stat$Rating == "Good")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # } else {
      #   stat <- data %>%
      #     filter(geo_breakdown == input$geographic_breakdown_e2)
      #   stat_final <- stat$Count[which(stat$Rating == "Good")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # }
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
      stat <- ofsted_ratings_data()
      stat_final <- stat$Count[which(stat$Rating == "Requires Improvement")]
      paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(stat$time_period), ")", "</p>")
      # if (input$geographic_breakdown_e2 %in% c("Inner London", "Outer London", "London")) {
      #   stat <- data %>%
      #     filter(geo_breakdown == "London")
      #   stat_final <- stat$Count[which(stat$Rating == "Requires Improvement")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # } else if (input$geographic_breakdown_e2 %in% c("North East", "Yorkshire and The Humber")) {
      #   stat <- data %>%
      #     filter(geo_breakdown == "North East, Yorkshire and the Humber")
      #   stat_final <- stat$Count[which(stat$Rating == "Requires Improvement")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # } else {
      #   stat <- data %>%
      #     filter(geo_breakdown == input$geographic_breakdown_e2)
      #   stat_final <- stat$Count[which(stat$Rating == "Requires Improvement")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # }
    }
  })

  output$ofsted_inadequate_headline <- renderText({
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
      stat <- ofsted_ratings_data()
      stat_final <- stat$Count[which(stat$Rating == "Inadequate")]
      paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(stat$time_period), ")", "</p>")

      # if (input$geographic_breakdown_e2 %in% c("Inner London", "Outer London", "London")) {
      #   stat <- data %>%
      #     filter(geo_breakdown == "London")
      #   stat_final <- stat$Count[which(stat$Rating == "Inadequate")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # } else if (input$geographic_breakdown_e2 %in% c("North East", "Yorkshire and The Humber")) {
      #   stat <- data %>%
      #     filter(geo_breakdown == "North East, Yorkshire and the Humber")
      #   stat_final <- stat$Count[which(stat$Rating == "Inadequate")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # } else {
      #   stat <- data %>%
      #     filter(geo_breakdown == input$geographic_breakdown_e2)
      #   stat_final <- stat$Count[which(stat$Rating == "Inadequate")]
      #   paste0(stat_final, "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(data$time_period), ")", "</p>")
      # }
    }
  })

  output$ofsted_latest_ratings_tbl <- renderReactable({
    filtered_data <- ofsted_leadership_data %>%
      filter(is.na(region) == FALSE) %>%
      select(geo_breakdown, published_year) %>%
      arrange(`geo_breakdown`) %>%
      rename(`Latest Rating` = `published_year`, `Location` = `geo_breakdown`)

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
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
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$plot_ofsted_reg <- plotly::renderPlotly({
    p <- plot_ofsted_reg() %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Ofsted – The impact of leaders on social work practice with children and families by region")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
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
      defaultColDef = colDef(align = "center"),
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
      defaultColDef = colDef(align = "center"),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })

  # Enabler 3 ----
  # Geographic breakdown e3 (list of either LA names or Region names)
  observeEvent(eventExpr = {
    input$select_geography_e3
  }, {
    choices <- sort(unique(workforce_data[geographic_level == input$select_geography_e3 & time_period == max(workforce_data$time_period)]$geo_breakdown), decreasing = FALSE)

    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_e3",
      selected = choices[1],
      choices = choices
    )
  })

  # checkbox for enabler 3
  observeEvent(input$select_geography_e3, {
    if (input$select_geography_e3 == "Regional") {
      updateCheckboxInput(session, "Yes_national_e3", value = FALSE)
      updateCheckboxInput(session, "Yes_region_e3", value = FALSE)
    } else if (input$select_geography_e3 == "National") {
      updateCheckboxInput(session, "Yes_national_e3", value = FALSE)
      updateCheckboxInput(session, "Yes_region_e3", value = FALSE)
    }
  })

  ###### Confirmation sentence E3
  # This function gets the selected region to put into the confirmation text below
  workforce_region_e3 <- reactive({
    location_data_workforce %>%
      filter(la_name == input$geographic_breakdown_e3) %>%
      pull(region_name) %>%
      as.character() # Convert to character
  })

  output$enabler3_choice_text1 <- renderText({
    generate_choice_text1(input$select_geography_e3, input$geographic_breakdown_e3, workforce_region_e3())
  })

  output$enabler3_choice_text2 <- renderText({
    generate_choice_text2(input$national_comparison_checkbox_e3, input$region_comparison_checkbox_e3, input$sn_comparison_checkbox_e3)
  })

  ## Headline boxes -----------
  ## Social worker headline stat
  output$s_w_headline_txt <- renderText({
    stat <- format(workforce_data %>%
      filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
      select(turnover_rate_fte), nsmall = 1)

    if (input$geographic_breakdown_e3 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_data$time_period), ")", "</p>"
    )
  })
  ## Agency worker rates headline stat
  output$agency_rate_txt <- renderText({
    stat <- format(workforce_data %>%
      filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
      select(agency_rate_fte), nsmall = 1)

    if (input$geographic_breakdown_e3 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_data$time_period), ")", "</p>"
    )
  })
  ## Vacancy rates headline stat
  output$vacancy_rate_txt <- renderText({
    stat <- format(workforce_data %>%
      filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
      select(vacancy_rate_fte), nsmall = 1)

    if (input$geographic_breakdown_e3 == "" || nrow(stat) == 0) {
      stat <- "NA"
    }

    paste0(
      stat, "%", "<br>", "<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_data$time_period), ")", "</p>"
    )
  })

  ## Caseload headline box
  output$caseload_txt <- renderText({
    if (input$geographic_breakdown_e3 == "") {
      stat <- "NA"
      paste0(stat, "<br>")
    } else {
      previous_year_value <- workforce_data %>%
        filter(time_period == (max(workforce_data$time_period) - 1) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(caseload_fte)

      current_year_value <- workforce_data %>%
        filter(time_period == (max(workforce_data$time_period)) & geo_breakdown %in% input$geographic_breakdown_e3) %>%
        select(caseload_fte)

      if (nrow(previous_year_value) < 1) {
        context <- ""
      } else if ((current_year_value < previous_year_value)) {
        context <- paste0(" down from ", previous_year_value, " ", (max(workforce_data$time_period) - 1))
      } else if ((current_year_value > previous_year_value)) {
        context <- paste0(" up from ", previous_year_value, " ", (max(workforce_data$time_period) - 1))
      } else {
        context <- "No change"
      }
      stat <- format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e3) %>% select(caseload_fte), nsmall = 1)
      paste0(stat, "<br>", "<p style='font-size:16px; font-weight:500;'>", "in ", max(workforce_data$time_period), context, "</p>")
    }
  })

  ### Social worker turnover rate ------------
  #### Social worker turnover rate time series plot ----
  output$plot_s_w_turnover <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = workforce_data,
      select_geographic_level = input$select_geography_e3,
      select_geo_breakdown = input$geographic_breakdown_e3,
      check_compare_national = input$national_comparison_checkbox_e3,
      check_compare_regional = input$region_comparison_checkbox_e3,
      check_compare_sn = input$sn_comparison_checkbox_e3,
      dimensional_filters = list()
    )

    if (input$geographic_breakdown_e3 == "Isles of Scilly") {
      # Set the max y-axis scale with Isles of Scilly
      max_rate <- max(workforce_data$`Turnover Rate Fte`, na.rm = TRUE)
      max_rate <- ceiling(max_rate / 20) * 20
    } else {
      # Set the max y-axis scale without Isles of Scilly
      max_rate <- max(workforce_data$`Turnover Rate Fte`[workforce_data$geo_breakdown != "Isles of Scilly"], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 20) * 20
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e3, input$geographic_breakdown_e3, "Turnover Rate Fte", "Turnover rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)

    p <- p + ggtitle("Social worker turnover rate (FTE) %")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ##### Social worker turnover rate time series benchmarking table
  output$table_s_w_turnover <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = workforce_data,
      select_geographic_level = input$select_geography_e3,
      select_geo_breakdown = input$geographic_breakdown_e3,
      check_compare_national = input$national_comparison_checkbox_e3,
      check_compare_regional = input$region_comparison_checkbox_e3,
      check_compare_sn = input$sn_comparison_checkbox_e3,
      dimensional_filters = list()
    ) %>%
      select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Turnover rate (FTE) %" = "Turnover Rate Fte")

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Turnover Rate (FTE) %` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### turnover rate by region plot
  output$plot_turnover_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    max_rate <- max(workforce_data$`Turnover Rate Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Regional"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(workforce_data, "Turnover Rate Fte", "Turnover Rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Social worker turnover rate (FTE) % by region")
    title <- paste0("Social worker turnover rate (FTE) % by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # turnover rate by region table
  output$table_turnover_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      #   need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- workforce_data %>%
      filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
      select(time_period, geo_breakdown, `Turnover Rate Fte`) %>%
      arrange(desc(`Turnover Rate Fte`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Turnover rate (FTE) %" = "Turnover Rate Fte")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Turnover rate (FTE) %` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### Turnover Rate by LA plot
  output$plot_turnover_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    max_rate <- max(workforce_data$`Turnover Rate Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Turnover Rate Fte", "Turnover Rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Social worker turnover rate (FTE) % by local authority")
    title <- paste0("Social worker turnover rate (FTE) % by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Turnover rate (FTE) %` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ## Agency Worker Rate ----
  ### Agency worker rate benchmarking plot ----
  output$plot_agency_worker <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = workforce_data,
      select_geographic_level = input$select_geography_e3,
      select_geo_breakdown = input$geographic_breakdown_e3,
      check_compare_national = input$national_comparison_checkbox_e3,
      check_compare_regional = input$region_comparison_checkbox_e3,
      check_compare_sn = input$sn_comparison_checkbox_e3,
      dimensional_filters = list()
    )

    if (input$geographic_breakdown_e3 == "Isles of Scilly") {
      # Set the max y-axis scale with Isles of Scilly
      max_rate <- max(workforce_data$`Agency Rate Fte`, na.rm = TRUE)
      max_rate <- ceiling(max_rate / 20) * 20
    } else {
      # Set the max y-axis scale without Isles of Scilly
      max_rate <- max(workforce_data$`Agency Rate Fte`[workforce_data$geo_breakdown != "Isles of Scilly"], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 20) * 20
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e3, input$geographic_breakdown_e3, "Agency Rate Fte", "Agency worker rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Agency worker rate (FTE) %")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  #### Agency worker rate table alternative ----
  output$table_agency_worker <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = workforce_data,
      select_geographic_level = input$select_geography_e3,
      select_geo_breakdown = input$geographic_breakdown_e3,
      check_compare_national = input$national_comparison_checkbox_e3,
      check_compare_regional = input$region_comparison_checkbox_e3,
      check_compare_sn = input$sn_comparison_checkbox_e3,
      dimensional_filters = list()
    ) %>%
      select(time_period, geo_breakdown, "Agency Rate Fte") %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Agency worker rate (FTE) %" = "Agency Rate Fte")

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Agency worker rate (FTE) %` = colDef(cell = cellfunc_decimal_percent)
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### agency rate plot by region
  output$plot_agency_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    max_rate <- max(workforce_data$`Agency Rate Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Regional"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(workforce_data, "Agency Rate Fte", "Agency worker rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Agency worker rate (FTE) % by region")
    title <- paste0("Agency worker rate (FTE) % by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # agency rate table by region
  output$table_agency_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    data <- workforce_data %>%
      filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
      select(time_period, geo_breakdown, `Agency Rate Fte`) %>%
      arrange(desc(`Agency Rate Fte`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Agency worker rate (FTE) %" = "Agency Rate Fte")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Agency worker rate (FTE) %` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### agency rate by la plot
  output$plot_agency_rate_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    max_rate <- max(workforce_data$`Agency Rate Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Agency Rate Fte", "Agency worker rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Agency worker rate (FTE) % by local authority")
    title <- paste0("Agency worker rate (FTE) % by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Agency worker rate (FTE) %` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ## Vacancy rate ------------------------
  ### Vacancy Rate benchmarking plot ----
  output$plot_vacancy_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = workforce_data,
      select_geographic_level = input$select_geography_e3,
      select_geo_breakdown = input$geographic_breakdown_e3,
      check_compare_national = input$national_comparison_checkbox_e3,
      check_compare_regional = input$region_comparison_checkbox_e3,
      check_compare_sn = input$sn_comparison_checkbox_e3,
      dimensional_filters = list()
    )

    if (input$geographic_breakdown_e3 == "Isles of Scilly") {
      # Set the max y-axis scale with Isles of Scilly
      max_rate <- max(workforce_data$`Vacancy Rate Fte`, na.rm = TRUE)
      max_rate <- ceiling(max_rate / 20) * 20
    } else {
      # Set the max y-axis scale without Isles of Scilly
      max_rate <- max(workforce_data$`Vacancy Rate Fte`[workforce_data$geo_breakdown != "Isles of Scilly"], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 20) * 20
    }

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e3, input$geographic_breakdown_e3, "Vacancy Rate Fte", "Vacancy rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Vacancy rate (FTE) %")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### Vacancy Rate benchmarking table alternative ----
  output$table_vacancy_rate <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = workforce_data,
      select_geographic_level = input$select_geography_e3,
      select_geo_breakdown = input$geographic_breakdown_e3,
      check_compare_national = input$national_comparison_checkbox_e3,
      check_compare_regional = input$region_comparison_checkbox_e3,
      check_compare_sn = input$sn_comparison_checkbox_e3,
      dimensional_filters = list()
    ) %>%
      select(time_period, geo_breakdown, "Vacancy Rate Fte") %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Vacancy rate (FTE) %" = "Vacancy Rate Fte")

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Vacancy rate (FTE) %` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### vacancy rate plot by region
  output$plot_vacancy_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      #  need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    max_rate <- max(workforce_data$`Vacancy Rate Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Regional"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(workforce_data, "Vacancy Rate Fte", "Vacancy rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Vacancy rate (FTE) % by region")
    title <- paste0("Vacancy rate (FTE) % by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  ### vacancy rate table by region
  output$table_vacancy_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      #  need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- workforce_data %>%
      filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
      select(time_period, geo_breakdown, `Vacancy Rate Fte`) %>%
      arrange(desc(`Vacancy Rate Fte`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Vacancy rate (FTE) %" = "Vacancy Rate Fte")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Vacancy rate (FTE) %` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### vacancy rate by la plot
  output$plot_vacancy_rate_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    max_rate <- max(workforce_data$`Vacancy Rate Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Vacancy Rate Fte", "Vacancy rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Vacancy rate (FTE) % by local authority")
    title <- paste0("Vacancy rate (FTE) % by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  ### vacancy rate by la table alternative
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Vacancy rate (FTE) %` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ## Social worker Caseload --------------------------------------
  ### Caseload benchmarking plot ----
  output$caseload_plot <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = workforce_data,
      select_geographic_level = input$select_geography_e3,
      select_geo_breakdown = input$geographic_breakdown_e3,
      check_compare_national = input$national_comparison_checkbox_e3,
      check_compare_regional = input$region_comparison_checkbox_e3,
      check_compare_sn = input$sn_comparison_checkbox_e3,
      dimensional_filters = list()
    )

    # Set the max y-axis scale
    max_rate <- max(workforce_data$`Caseload Fte`, na.rm = TRUE)

    # Round the max_rate to the nearest 20
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e3, input$geographic_breakdown_e3, "Caseload Fte", "Average caseload (FTE)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average caseload (FTE)")

    ggplotly(p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(yaxis = list(range = c(0, max_rate), tickmode = "auto")) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })


  # caseload benchamrking table alternative
  output$table_caseload <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    # filter the dataset based on the context and user selections
    filtered_data <- filter_time_series_data(
      dataset_in = workforce_data,
      select_geographic_level = input$select_geography_e3,
      select_geo_breakdown = input$geographic_breakdown_e3,
      check_compare_national = input$national_comparison_checkbox_e3,
      check_compare_regional = input$region_comparison_checkbox_e3,
      check_compare_sn = input$sn_comparison_checkbox_e3,
      dimensional_filters = list()
    ) %>%
      select(time_period, geo_breakdown, `Caseload Fte`) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")

    reactable(
      filtered_data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Average caseload (FTE)` = colDef(cell = cellfunc_decimal_percent)
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### Caseload by region
  output$plot_caseload_reg <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    max_rate <- max(workforce_data$`Caseload Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Regional"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_region_bar_plot(workforce_data, "Caseload Fte", "Average caseload (FTE)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Average caseload (FTE) by region")
    title <- paste0("Average caseload (FTE) by region (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d", "hoverCompareCartesian"))
  })

  # Caseload by region table
  output$table_caseload_reg <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      # need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- workforce_data %>%
      filter(geographic_level == "Regional", time_period == max(workforce_data$time_period)) %>%
      select(time_period, geo_breakdown, "Caseload Fte") %>%
      arrange(desc(`Caseload Fte`)) %>%
      rename("Time period" = "time_period", "Region" = "geo_breakdown", "Average caseload (FTE)" = "Caseload Fte")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Average caseload (FTE)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ### caseload by la
  output$plot_caseload_la <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )

    max_rate <- max(workforce_data$`Caseload Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- by_la_bar_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Caseload Fte", "Average Caseload (FTE)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Average caseload (FTE) by local authority")
    title <- paste0("Average caseload (FTE) by local authority (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Average caseload (FTE)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  ## Ethnicity and Diversity Domain-----
  output$non_white_txt <- renderText({
    non_white_stat <- workforce_eth %>%
      filter(time_period == max(workforce_eth$time_period) &
        geo_breakdown %in% input$geographic_breakdown_e3 &
        role == "Total" &
        breakdown == "Non-white") %>%
      select(inpost_headcount_percentage)

    if (input$geographic_breakdown_e3 == "") {
      non_white_stat <- "NA"
    } else {
      non_white_stat <- format(as.numeric(non_white_stat), nsmall = 1)
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
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$table_ethnicity_rate <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- workforce_eth %>%
      filter(
        geo_breakdown %in% input$geographic_breakdown_e3,
        role == "Total",
        breakdown_topic == "Ethnicity major",
        breakdown != "Non-white"
      ) %>%
      select(time_period, geo_breakdown, breakdown, inpost_headcount, inpost_headcount_percentage) %>%
      rename("Time period" = "time_period", "Location" = "geo_breakdown", "Ethnicity" = "breakdown", "Headcount" = "inpost_headcount", "Headcount (%)" = "inpost_headcount_percentage")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Headcount` = colDef(cell = cellfunc),
        `Headcount (%)` = colDef(cell = cellfunc_social_ethnicity, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })

  output$plot_population_ethnicity_rate <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    p <- plot_population_ethnicity_rate(input$geographic_breakdown_e3) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Social worker ethnicity % vs. general population ethnicity %")
    title <- paste0("Social worker ethnicity % vs. general population ethnicity % (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420
      # This one does not need to have a customised tooltip
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$table_population_ethnicity_rate <- renderReactable({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    data <- combined_ethnicity_data %>%
      filter(geo_breakdown %in% input$geographic_breakdown_e3) %>%
      filter(breakdown != "Non-white") %>%
      select(geo_breakdown, breakdown, inpost_headcount_percentage, Percentage) %>%
      rename("Location" = "geo_breakdown", "Ethnicity group" = "breakdown", "Workforce (%)" = "inpost_headcount_percentage", "Population (%)" = "Percentage")

    reactable(
      data,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Workforce (%)` = colDef(cell = cellfunc_social_ethnicity),
        `Population (%)` = colDef(cell = cellfunc_social_ethnicity, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })
  ### Social worker ethnicity by seniority level
  output$plot_seniority_eth <- plotly::renderPlotly({
    shiny::validate(
      need(input$select_geography_e3 != "", "Select a geography level."),
      need(input$geographic_breakdown_e3 != "", "Select a location.")
    )
    p <- plot_seniority_eth(input$geographic_breakdown_e3, input$select_geography_e3) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Social worker ethnicity by seniority level %")
    title <- paste0("Social worker ethnicity by seniority level % (", max(p$data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Headcount` = colDef(cell = cellfunc),
        `Headcount (%)` = colDef(cell = cellfunc_social_ethnicity, defaultSortOrder = "desc")
      ),
      defaultPageSize = 10,
      searchable = TRUE,
    )
  })


  # ALL statistical neighbours -----
  # The following code has a layout of UI, stats neighbours plot and table alternative
  # and is repeated for each indicator
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Outcome 1 ------
  ### CLA ------
  output$SN_cla <- renderUI({
    if (input$cla_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cla_rate_la"),
        br(),
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_cla_rate_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_cla_rate_la", filename = "cla_rates_all_LAs.csv"),
              reactableOutput("table_cla_rate_la")
            ))
          )
        ),
        details(
          inputId = "cla_rate_la_info",
          label = "Additional information:",
          help_text = (
            tagList(
              tags$ul(
                tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                tags$li("Only the first occasion on which a child started to be looked after in the LA during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
                tags$li("Figures exclude children looked after under a series of short-term placements."),
                tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication.")
              ),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.", target = "_blank")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("cla_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cla",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_cla_tbl", filename = paste0("cla_rates_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_cla_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location."),
    )
    # Set the max y-axis scale
    max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    filtered_data <- cla_rates %>%
      filter(population_count == "Children starting to be looked after each year") %>%
      rename("Rate per 10,000" = "Rate Per 10000")

    # Set the max y-axis scale
    max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year" &
      cla_rates$time_period == max(cla_rates$time_period) &
      cla_rates$geographic_level == "Local authority"], na.rm = TRUE)

    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, "Rate per 10,000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("CLA rate per 10,000 by statistical neighbours")
    title <- paste0("CLA rate per 10,000 by statistical neighbours (", max(filtered_data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })





  # cla stats neighbour tables
  output$SN_cla_tbl <- renderReactable({
    filtered_data <- cla_rates %>% filter(population_count == "Children starting to be looked after each year")

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = "number", yvalue = "rate_per_10000"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_uasc_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_uasc_la", filename = "cla_UASC_rates_all_LAs.csv"),
              reactableOutput("table_uasc_la")
            ))
          )
        ),
        details(
          inputId = "cla_UASC_rate_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Rates are calculated using published number of children starting to be looked after each year, who are UASC and non-UASC, which have been rounded to the nearest 10 at national and regional level (unrounded for local authority figures)."),
              tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
              tags$li("Only the first occasion on which a child started to be looked after in the LA during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
              tags$li("Following the introduction of the National Transfer Scheme (NTS) in 2016, there has been an agreement between local authorities to transfer UASC to ensure a more equitable distribution of UASC across all local authorities. This means that some UASC will be counted more than once in the national and regional CLA starting figures if they started to be looked after within more than 1 local
                                  authority during the year. In 2019 we estimate that nationally, the number of UASC starts was overestimated by 9%, this increased to 15% in 2023 following the mandation of the NTS in February 2022."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.", target = "_blank")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("UASC_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_uasc",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_uasc_tbl", filename = paste0("cla_UASC_rates_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_uasc_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location."),
    )

    # Set the max y-axis scale
    max_rate <- max(
      combined_cla_data$`Placement Rate Per 10000`[combined_cla_data$population_count == "Children starting to be looked after each year" &
        combined_cla_data$characteristic %in% c("UASC", "Non-UASC") &
        combined_cla_data$time_period == max(combined_cla_data$time_period) &
        combined_cla_data$geographic_level == "Local authority"],
      na.rm = TRUE
    )

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot_uasc(combined_cla_data, input$geographic_breakdown_o1, input$select_geography_o1, "Placement Rate Per 10000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("CLA rate per 10,000 with Unaccompanied asylum-seeking children breakdown by statistical neighbours")
    title <- paste0("CLA rate per 10,000 with Unaccompanied asylum-seeking children breakdown by statistical neighbours (", max(combined_cla_data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # cla UASC stats neighbour tables
  output$SN_uasc_tbl <- renderReactable({
    filtered_data <- combined_cla_data %>%
      filter(population_count == "Children starting to be looked after each year", characteristic %in% c("UASC", "Non-UASC")) %>%
      mutate(characteristic = case_when(
        characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
        characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
        TRUE ~ as.character(characteristic)
      ))
    reactable(
      stats_neighbours_table_uasc(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, yvalue = "Placement Rate Per 10000"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Placement Rate Per 10000` = colDef(name = "Rate per 10,000", cell = cellfunc, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  output$SN_uasc_31_march <- renderUI({
    if (input$uasc_31_march_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_uasc_31_march_la"),
        br(),
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_uasc_31_march_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_uasc_31_march_la", filename = "cla_UASC_31_March_rates_all_LAs.csv"),
              reactableOutput("table_uasc_31_march_la")
            ))
          )
        ),
        details(
          inputId = "cla_UASC_31_march_rate_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Rates are calculated using published number of children starting to be looked after each year, who are UASC and non-UASC, which have been rounded to the nearest 10 at national and regional level (unrounded for local authority figures)."),
              tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
              tags$li("Only the first occasion on which a child started to be looked after in the LA during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
              tags$li("Following the introduction of the National Transfer Scheme (NTS) in 2016, there has been an agreement between local authorities to transfer UASC to ensure a more equitable distribution of UASC across all local authorities. This means that some UASC will be counted more than once in the national and regional CLA starting figures if they started to be looked after within more than 1 local
                                  authority during the year. In 2019 we estimate that nationally, the number of UASC starts was overestimated by 9%, this increased to 15% in 2023 following the mandation of the NTS in February 2022."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.", target = "_blank")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("UASC_31_march_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_uasc_31_march",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_uasc_31_march_tbl", filename = paste0("cla_UASC_31_march_rates_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_uasc_31_march_tbl")
            ))
          )
        ),
        details(
          inputId = "sn_usac_31_march_info",
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
              ),
            )
          )
        )
      )
    }
  })

  # UASC stats neighbours chart and table here
  output$UASC_31_march_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location."),
    )

    # Set the max y-axis scale
    max_rate <- max(
      combined_cla_31_march_data$`Placement Rate Per 10000`[combined_cla_31_march_data$population_count == "Children looked after at 31 March each year" &
        combined_cla_31_march_data$characteristic %in% c("UASC", "Non-UASC") &
        combined_cla_31_march_data$time_period == max(combined_cla_31_march_data$time_period) &
        combined_cla_31_march_data$geographic_level == "Local authority"],
      na.rm = TRUE
    )

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot_uasc_31_march(combined_cla_31_march_data, input$geographic_breakdown_o1, input$select_geography_o1, "Placement Rate Per 10000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("CLA rate per 10,000 with Unaccompanied asylum-seeking children breakdown by statistical neighbours")
    title <- paste0("CLA rate on 31st March per 10,000 with Unaccompanied asylum-seeking children breakdown by statistical neighbours (", max(combined_cla_31_march_data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # cla UASC 31st March stats neighbour tables
  output$SN_uasc_31_march_tbl <- renderReactable({
    filtered_data <- combined_cla_31_march_data %>%
      filter(population_count == "Children looked after at 31 March each year", characteristic %in% c("UASC", "Non-UASC")) %>%
      mutate(characteristic = case_when(
        characteristic == "UASC" ~ "Unaccompanied asylum-seeking children",
        characteristic == "Non-UASC" ~ "Non-unaccompanied asylum-seeking children",
        TRUE ~ as.character(characteristic)
      ))
    reactable(
      stats_neighbours_table_uasc(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, yvalue = "Placement Rate Per 10000"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_cla_march_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_cla_march_la", filename = "cla_march_rates_all_LAs.csv"),
              reactableOutput("table_cla_march_la")
            ))
          )
        ),
        details(
          inputId = "cla_rate_march_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.", target = "_blank")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("cla_march_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cla_march",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_cla_march_tbl", filename = paste0("cla_march_rates_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_cla_march_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
              ),
            )
          )
        )
      )
    }
  })

  # cla march stats neighbours chart and table here
  output$cla_march_SN_plot <- plotly::renderPlotly({
    validate(need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."))

    # Set the max y-axis scale
    max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children looked after at 31 March each year" &
      cla_rates$time_period == max(cla_rates$time_period) &
      cla_rates$geographic_level == "Local authority"], na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 10) * 10

    filtered_data <- cla_rates %>%
      filter(population_count == "Children looked after at 31 March each year") %>%
      rename("Rate per 10,000" = "Rate Per 10000")

    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, "Rate per 10,000", "Rate per 10,000 children", max_rate) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("CLA rate per 10,000 on 31 March by statistical neighbours")
    title <- paste0("CLA rate per 10,000 on 31 March by statistical neighbours (", max(filtered_data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # # cla March stats neighbour tables
  output$SN_cla_march_tbl <- renderReactable({
    filtered_data <- cla_rates %>% filter(population_count == "Children looked after at 31 March each year")

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = "number", yvalue = "rate_per_10000"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_cin_rates_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_cin_rates_la", filename = "cin_rates_all_LAs.csv"),
              reactableOutput("table_cin_rates_la")
            ))
          )
        ),
        details(
          inputId = "CIN_la_info",
          label = "Additional information:",
          help_text = tags$ul(
            tags$li("Rate of children as at 31 March 2024 assessed as needing help and protection as a result of risks to their development or health."),
            tags$li(
              "Rates per 10,000 children are calculated based on ONS",
              a(
                href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland",
                "ONS mid-year population estimates (opens in a new tab)", target = "_blank"
              ),
              "for children aged 0 to 17 years. Revised/new population estimates for 2012 to 2022, based on 2021 Census data, were used to calculate revised rates for 2013 to 2023 in this publication. The rates for each year were calculated based on population estimates for the preceding year. For example, population estimates for 2023 were used to calculate 2024 rates."
            ),
            tags$li(
              "The impact of these revisions at a national level has resulted in changes to the following rates, per 10,000 children, ranging from:",
              tags$ul(
                tags$li("For children in need, a decrease of 3.6 in 2023 (from 342.7 to 339.1) to an increase of 8.4 in 2021 (from 321.2 to 329.6)."),
                tags$li("For children on protection plans, a decrease of 0.5 in 2023 (from 43.2 to 42.7) to an increase of 1.0 in 2021 (from 41.4 to 42.4).")
              )
            ),
            tags$li("The rates in the 2023 release for the 2023 year were calculated based on 2021 population estimates as estimates for 2022 were not available at the time of publication; this should be considered alongside the impact of the revisions."),
            tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in the 2021 and 2022 national totals and regional totals."),
            tags$li("Data for the year ending 31 March 2024 is not available for Hampshire local authority, therefore 2023 data for Hampshire has been included in the 2024 national and regional totals."),
            tags$li("Hampshire local authority moved to a new case management and reporting system and their return in 2024 had significant data quality issues and was assessed to not be sufficiently reliable to use. Therefore, their data for 2024 is presented as ‘u’ to indicate low reliability and 2023 figures for Hampshire are included in the 2024 totals for the South East region and England"),
            tags$li("Hackney had a cyberattack in December 2020, which had a significant impact on their information management systems. As a result, 2020 figures for Hackney have been included in the 2021 and 2022 national and regional totals, but data for Hackney has been presented as ‘x’ to indicate not available"),
            tags$br(),
            p(
              "For more information on the data and definitions, please refer to the",
              a(
                href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance",
                "Children in need data guidance.", target = "_blank"
              ),
              tags$br(),
              "For more information on the methodology, please refer to the",
              a(
                href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology",
                "Children in need methodology.", target = "_blank"
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
        need(
          nrow(cin_rates %>% filter(time_period == max(cin_rates$time_period) & geo_breakdown %in% input$geographic_breakdown_o1)) > 0,
          "This local authority has no data for the current year"
        )
      )
      tagList(
        plotlyOutput("cin_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cin",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_cin_tbl", filename = paste0("cin_rates_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_cin_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )

    # Set the max y-axis scale
    max_rate <- max(cin_rates$CIN_rate, na.rm = TRUE)

    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50

    data <- cin_rates %>%
      rename("CIN rate per 10,000" = "CIN_rate")

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "CIN rate per 10,000", "CIN rate per 10,000", max_rate) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("CIN rate per 10,000 by statistical neighbours")
    title <- paste0("CIN rate per 10,000 by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # # cin stats neighbours tables
  output$SN_cin_tbl <- renderReactable({
    # renaming column
    data <- cin_rates %>% rename("CIN_rate_per_10000" = "At31_episodes_rate")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = "At31_episodes", yvalue = "CIN_rate_per_10000"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_cin_referral_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_cin_referral_la", filename = "cin_re_referrals_all_LAs.csv"),
              reactableOutput("table_cin_referral_la")
            ))
          )
        ),
        details(
          inputId = "CIN_referral_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in the 2021 and 2022 national totals and regional totals. Data for the year ending 31 March 2024 is not available for Hampshire local authority, therefore 2023 data for Hampshire has been included in the 2024 national and regional totals. Refer to", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-in-need", "Children in need data methodology", target = "_blank"), "for more information"),
              tags$li("Herefordshire local authority considerably underreported their data on referrals, and therefore re-referrals, in 2024. Impacted data is shown as ‘u’ to indicate low reliability but are included in the national totals and regional totals."),
              tags$li("For Herefordshire, it was determined at the end of the 2024 collection that the re-referrals data initially reported for 2023 was unreliable, so data on re-referrals for Herefordshire for 2023 was replaced with ‘u’ to indicate low reliability within the 2024 statistics release."),
              tags$li("Re-referrals for the year ending 31 March 2020 exclude Dorset and exclude Bournemouth, Christchurch and Poole local authorities due to the reorganisation of these areas on 1 April 2019."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
        need(
          nrow(cin_referrals %>% filter(time_period == max(cin_referrals$time_period) & geo_breakdown %in% input$geographic_breakdown_o1)) > 0,
          "This local authority has no data for the current year"
        )
      )
      tagList(
        plotlyOutput("cin_referral_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cin_referral",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_cin_referral_tbl", filename = paste0("cin_re_referrals_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_cin_referral_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location.")
    )
    p <- statistical_neighbours_plot(cin_referrals, input$geographic_breakdown_o1, input$select_geography_o1, "Re-referrals (%)", "Re-referrals (%)", 100, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Re-referrals (%) by statistical neighbours")
    title <- paste0("Re-referrals (%) by statistical neighbours (", max(cin_referrals$time_period), ")")
    p <- p + ggtitle(title)


    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # # cin stats neighbours tables
  output$SN_cin_referral_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(cin_referrals, input$geographic_breakdown_o1, input$select_geography_o1, yvalue = "Re-referrals (%)"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Re-Referrals (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_absence_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_absence_la", filename = "absence_rates_all_LAs.csv"),
              reactableOutput("table_absence_la")
            ))
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
                a(href = "https://explore-education-statistics.service.gov.uk/methodology/pupil-absence-in-schools-in-england#section3-1", "3.1 Overall absence methodology.", target = "_blank"),
              ),
              tags$li(
                "No absence data relating to the full 2019/20 academic year is available due to COVID-19.
                                  Due to the disruption during the 2020/21 and 2021/22 academic years, caution should be taken when comparing data to previous years. For more detailed information on this see ",
                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England.", target = "_blank"),
              ),
              tags$li("CINO refers to children In need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
              tags$li("CLA refers to children looked after (excludes children who are in respite care in their most recent episode during the reporting year)."),
              tags$li("Children in need data is not available for Hackney local authority for both the 2020 to 2021 and 2021 to 2022 collection years and Hampshire local authority for the 2023 to 2024 collection year. Hackney was unable to provide a return for both the 2021 and 2022 children in need census collections, due to a cyberattack which had a significant impact on their management information systems. Hampshire provided a CIN return for the 2024 collection, however, due to a transition to a new case management and reporting system, there were significant data quality issues affecting the coverage of Hampshire's 2024 return. Refer to the methodology section for more information."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("absence_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_absence",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_absence_tbl", filename = paste0("absence_rates_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_absence_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location."),
    )
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_absence$`Overall absence (%)`[outcomes_absence$time_period == max(outcomes_absence$time_period) &
      outcomes_absence$geographic_level == "Local authority" &
      !outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Overall absence (%)", "Overall absence (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Overall absence rate (%) by statistical neighbours", " (", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })
  # Absence SN table
  output$SN_absence_tbl <- renderReactable({
    filtered_data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      rename(`OA%` = `Overall absence (%)`, `Overall absence (%)` = `pt_overall`) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("social_care_group", "school_type", "Total pupils"), yvalue = "Overall absence (%)"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `social_care_group` = colDef(name = "Social care group"),
        `school_type` = colDef(name = "School type"),
        `Total pupils` = colDef(name = "Total number of pupils"),
        `Overall Absence (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_persistent_absence_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_persistent_absence_la", filename = "persistent_absence_all_LAs.csv"),
              reactableOutput("table_persistent_absence_la")
            ))
          )
        ),
        details(
          inputId = "Persistent_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li(
                "Persistent absence is when a pupil enrolment’s overall absence equates to 10% or more of their possible sessions. For further information see ",
                a(href = "https://explore-education-statistics.service.gov.uk/methodology/pupil-absence-in-schools-in-england#section3-2", "3.2 Overall absence methodology.", target = "_blank"),
              ),
              tags$li(
                "No absence data relating to the full 2019/20 academic year is available due to COVID-19.
                                  Due to the disruption during the 2020/21 and 2021/22 academic years, caution should be taken when comparing data to previous years. For more detailed information on this see ",
                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England.", target = "_blank"),
              ),
              tags$li("CINO refers to children In need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
              tags$li("CLA refers to children looked after (excludes children who are in respite care in their most recent episode during the reporting year)."),
              tags$li("Children in need data is not available for Hackney local authority for both the 2020 to 2021 and 2021 to 2022 collection years and Hampshire local authority for the 2023 to 2024 collection year. Hackney was unable to provide a return for both the 2021 and 2022 children in need census collections, due to a cyberattack which had a significant impact on their management information systems. Hampshire provided a CIN return for the 2024 collection, however, due to a transition to a new case management and reporting system, there were significant data quality issues affecting the coverage of Hampshire's 2024 return. Refer to the methodology section for more information."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("persistent_absence_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_persistent_abs",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_persistent_absence_tbl", filename = paste0("persistent_absence_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_persistent_absence_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location."),
    )
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_absence$`Persistent absentees (%)`[outcomes_absence$time_period == max(outcomes_absence$time_period) &
      outcomes_absence$geographic_level == "Local authority" &
      !outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Persistent absentees (%)", "Persistent absentees (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Persistent absentees (%) by statistical neighbours ", "(", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # Persistent Absence SN table
  output$SN_persistent_absence_tbl <- renderReactable({
    filtered_data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      rename(`PA%` = `Persistent absentees (%)`, `Persistent absentees (%)` = `pt_pupils_pa_10_exact`) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("social_care_group", "school_type", "Total pupils"), yvalue = "Persistent absentees (%)"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `social_care_group` = colDef(name = "Social care group"), `school_type` = colDef(name = "School type"), `Total pupils` = colDef(name = "Total number of pupils"), `Persistent Absentees (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### Severe Absence --------

  output$SN_severe_abs <- renderUI({
    if (input$severe_abs_stats_toggle == "All local authorities") {
      validate(
        need(input$wellbeing_school_breakdown != "", "Select a school type"),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      # define this once here and reuse it on both sections
      details_severe_abs_la_add_info <- details(
        inputId = "Severe_la_info",
        label = "Additional information:",
        help_text = (
          tags$ul(
            tags$li(
              "A pupil is identified as severely absent if they miss 50% or more of possible sessions. For further information see ",
              a(href = "https://explore-education-statistics.service.gov.uk/methodology/pupil-absence-in-schools-in-england#section3-2", "3.2 Overall absence methodology.", target = "_blank"),
            ),
            tags$li(
              "No absence data relating to the full 2019/20 academic year is available due to COVID-19.
                                  Due to the disruption during the 2020/21 and 2021/22 academic years, caution should be taken when comparing data to previous years. For more detailed information on this see ",
              a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England.", target = "_blank"),
            ),
            tags$li("CINO refers to children In need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
            tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
            tags$li("CLA refers to children looked after (excludes children who are in respite care in their most recent episode during the reporting year)."),
            tags$li("Children in need data is not available for Hackney local authority for both the 2020 to 2021 and 2021 to 2022 collection years and Hampshire local authority for the 2023 to 2024 collection year. Hackney was unable to provide a return for both the 2021 and 2022 children in need census collections, due to a cyberattack which had a significant impact on their management information systems. Hampshire provided a CIN return for the 2024 collection, however, due to a transition to a new case management and reporting system, there were significant data quality issues affecting the coverage of Hampshire's 2024 return. Refer to the methodology section for more information."),
            tags$br(),
            p(
              "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
              tags$br(),
              "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
            )
          )
        )
      )

      if (input$wellbeing_school_breakdown != "State-funded primary") {
        tagList(
          plotlyOutput("plot_severe_absence_la"),
          br(),
          p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
          br(),
          details(
            inputId = "tbl_severe_absence_la",
            label = "View chart as a table",
            help_text = (
              HTML(paste0(
                csvDownloadButton("table_severe_absence_la", filename = "severe_absence_all_LAs.csv"),
                reactableOutput("table_severe_absence_la")
              ))
            )
          ),
          details_severe_abs_la_add_info,
        )
      } else {
        tagList(
          br(),
          p("This table is reactive to the local authority and regional filters at the top and will not react to the national filter. The table will display all local authorities overall or every local authority in the selected region."),
          br(),
          tagAppendAttributes(
            details(
              inputId = "tbl_severe_absence_la",
              label = "View table",
              help_text = (
                HTML(paste0(
                  csvDownloadButton("table_severe_absence_la", filename = "severe_absence_all_LAs.csv"),
                  reactableOutput("table_severe_absence_la")
                ))
              )
            ),
            open = ""
          ),
          details_severe_abs_la_add_info
        )
      }
    } else { # this is the SN plot/table
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      details_severe_abs_SN_add_info <- details(
        inputId = "sn_severe_abs_info",
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
            ),
          )
        )
      )

      if (input$wellbeing_school_breakdown != "State-funded primary") { # table and chart
        tagList(
          plotlyOutput("severe_absence_SN_plot"),
          br(),
          details(
            inputId = "tbl_sn_severe_abs",
            label = "View chart as a table",
            help_text = (
              HTML(paste0(
                csvDownloadButton("SN_severe_absence_tbl", filename = paste0("severe_absence_SN_", input$geographic_breakdown_o1, ".csv")),
                reactableOutput("SN_severe_absence_tbl")
              ))
            )
          ),
          details_severe_abs_SN_add_info
        )
      } else { # table only
        tagList(
          br(),
          tagAppendAttributes(
            details(
              inputId = "tbl_sn_severe_abs",
              label = "View chart as a table",
              help_text = (
                HTML(paste0(
                  csvDownloadButton("SN_severe_absence_tbl", filename = paste0("severe_absence_SN_", input$geographic_breakdown_o1, ".csv")),
                  reactableOutput("SN_severe_absence_tbl")
                ))
              )
            ),
            open = ""
          ),
          details_severe_abs_SN_add_info
        )
      }
    }
  })


  # Severe absence stats neighbours chart
  output$severe_absence_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location."),
    )
    data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_absence$`Severe absentees (%)`[outcomes_absence$time_period == max(outcomes_absence$time_period) &
      outcomes_absence$geographic_level == "Local authority" &
      !outcomes_absence$school_type %in% c("Special", "State-funded AP school")], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Severe absentees (%)", "Severe absentees (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Severe absentees (%) by statistical neighbours ", "(", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  # Severe Absence SN table
  output$SN_severe_absence_tbl <- renderReactable({
    filtered_data <- outcomes_absence %>%
      filter(school_type %in% input$wellbeing_school_breakdown, social_care_group %in% input$wellbeing_extra_breakdown) %>%
      rename(`PA%` = `Severe absentees (%)`, `Severe absentees (%)` = `pt_pupils_pa_50_exact`) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))


    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o1, input$select_geography_o1, selectedcolumn = c("social_care_group", "school_type", "Total pupils"), yvalue = "Severe absentees (%)"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `social_care_group` = colDef(name = "Social care group"), `school_type` = colDef(name = "School type"), `Total pupils` = colDef(name = "Total number of pupils"), `Severe Absentees (%)` = colDef(cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_KS2_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_KS2_la", filename = "ks2_attainment_all_LAs.csv"),
              reactableOutput("table_KS2_la")
            ))
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
                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment", "Key stage 2 attainment.", target = "_blank"),
              ),
              tags$li("CINO refers to children In need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
              tags$li("CLA refers to children looked after (excludes children who are in respite care in their most recent episode during the reporting year)."),
              tags$li("Children in need data is not available for Hackney local authority for both the 2020 to 2021 and 2021 to 2022 collection years and Hampshire local authority for the 2023 to 2024 collection year. Hackney was unable to provide a return for both the 2021 and 2022 children in need census collections, due to a cyberattack which had a significant impact on their management information systems. Hampshire provided a CIN return for the 2024 collection, however, due to a transition to a new case management and reporting system, there were significant data quality issues affecting the coverage of Hampshire's 2024 return. Refer to the methodology section for more information."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("ks2_attain_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_ks2",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_ks2_attain_tbl", filename = paste0("ks2_attainment_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_ks2_attain_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location."),
    )
    data <- outcomes_ks2 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_ks2$`Expected standard reading writing maths (%)`[outcomes_ks2$time_period == max(outcomes_ks2$time_period) &
      outcomes_ks2$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Expected standard reading writing maths (%)", "Expected standard combined (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    title <- paste0("Percentage meeting combined expected standard (KS2) by statistical neighbours ", "(", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_KS4_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_KS4_la", filename = "ks4_attainment_all_LAs.csv"),
              reactableOutput("table_KS4_la")
            ))
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
                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance", "Key stage 4 performance.", target = "_blank"),
              ),
              tags$li("In 2022/23 there was a return to pre-pandemic standards for GCSEs, with protection built into the grading process to recognise the disruption that students have faced. Therefore, the more meaningful comparison is with 2019, the last year that summer exams were taken before the pandemic, as 2023 saw a return to pre-pandemic grading, with some protections.
                                  In 2022 outcomes broadly reflected a mid-point between 2019 and 2021, to take account of the impact of the pandemic and in line with Ofqual’s approach to grading in 2022. It is expected that performance in 2023 will generally be lower than in 2022. Users need to exercise extreme caution when considering comparisons over time, as they may not reflect changes in pupil performance alone."),
              tags$li("CINO refers to children In need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
              tags$li("CLA refers to children looked after (excludes children who are in respite care in their most recent episode during the reporting year)."),
              tags$li("Children in need data is not available for Hackney local authority for both the 2020 to 2021 and 2021 to 2022 collection years and Hampshire local authority for the 2023 to 2024 collection year. Hackney was unable to provide a return for both the 2021 and 2022 children in need census collections, due to a cyberattack which had a significant impact on their management information systems. Hampshire provided a CIN return for the 2024 collection, however, due to a transition to a new case management and reporting system, there were significant data quality issues affecting the coverage of Hampshire's 2024 return. Refer to the methodology section for more information."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o1 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("ks4_attain_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_ks4",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_ks4_attain_tbl", filename = paste0("ks4_attainment_SN_", input$geographic_breakdown_o1, ".csv")),
              reactableOutput("SN_ks4_attain_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o1 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o1 != "", "Select a location."),
    )
    data <- outcomes_ks4 %>%
      filter(social_care_group %in% input$attainment_extra_breakdown) %>%
      mutate(time_period = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, nchar(time_period))))

    max_rate <- max(outcomes_ks4$`Average Attainment 8`[outcomes_ks4$time_period == max(outcomes_ks4$time_period) &
      outcomes_ks4$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o1, input$select_geography_o1, "Average Attainment 8", "Average Attainment 8 score", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Average attainment 8 score (KS4) by statistical neighbours ", "(", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
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
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Total number of pupils` = colDef(cell = cellfunc),
        `Average Attainment 8 Score` = colDef(cell = cellfunc_decimal_percent)
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_sgo_ceased_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_sgo_la", filename = "ceased_CLA_SGO_all_LAs.csv"),
              reactableOutput("table_sgo_la")
            ))
          )
        ),
        details(
          inputId = "sgo_la_info",
          label = "Additional information:",
          help_text = (
            p(
              tags$li("Only one reason for children ceased to be looked after during the year shown. See ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "children looked after publication ", target = "_blank"), "for full list of reasons."),
              tags$li("Percentages rounded to the nearest whole number."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$li("Figures exclude children looked after under a series of short-term placements."),
              tags$li("Only the last occasion on which a child ceased to be looked after in the year has been counted."),
              tags$br(),
              "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after guidance.", target = "_blank"),
              tags$br(),
              "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
          ))
        )
      )
    } else {
      validate(
        need(input$select_geography_o2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o2 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("sgo_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_sgo",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_sgo_tbl", filename = paste0("ceased_CLA_SGO_SN_", input$geographic_breakdown_o2, ".csv")),
              reactableOutput("SN_sgo_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o2 != "", "Select a location."),
    )
    filtered_data <- ceased_cla_data %>% filter(characteristic == "Special guardianship orders")

    max_rate <- max(ceased_cla_data$`Ceased (%)`[ceased_cla_data$time_period == max(ceased_cla_data$time_period) &
      ceased_cla_data$geographic_level == "Local authority" &
      ceased_cla_data$characteristic == "Special guardianship orders"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased due to SGO (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage ceased CLA due to SGO by statistical neighbours")
    title <- paste0("Percentage ceased CLA due to SGO by statistical neighbours (", max(filtered_data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$SN_sgo_tbl <- renderReactable({
    filtered_data <- ceased_cla_data %>%
      filter(characteristic == "Special guardianship orders") %>%
      rename(`Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, selectedcolumn = c("Reason ceased", "Number ceased", "Total ceased"), yvalue = "Ceased (%)"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_cao_ceased_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_cao_la", filename = "ceased_CLA_CAO_all_LAs.csv"),
              reactableOutput("table_cao_la")
            ))
          )
        ),
        details(
          inputId = "cao_la_info",
          label = "Additional information:",
          help_text = (
            p(
              tags$li("Only one reason for children ceased to be looked after during the year shown. See ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "children looked after publication ", target = "_blank"), "for full list of reasons."),
              tags$li("Percentages rounded to the nearest whole number."),
              tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
              tags$li("Figures exclude children looked after under a series of short-term placements."),
              tags$li("Only the last occasion on which a child ceased to be looked after in the year has been counted."),
              tags$br(),
              "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after guidance.", target = "_blank"),
              tags$br(),
              "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
          ))
        )
      )
    } else {
      validate(
        need(input$select_geography_o2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o2 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("cao_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cao",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_cao_tbl", filename = paste0("ceased_CLA_CAO_SN_", input$geographic_breakdown_o2, ".csv")),
              reactableOutput("SN_cao_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o2 != "", "Select a location."),
    )
    filtered_data <- ceased_cla_data %>% filter(characteristic == "Residence order or child arrangement order granted")

    max_rate <- max(ceased_cla_data$`Ceased (%)`[ceased_cla_data$time_period == max(ceased_cla_data$time_period) &
      ceased_cla_data$geographic_level == "Local authority" &
      ceased_cla_data$characteristic == "Residence order or child arrangement order granted"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, "Ceased (%)", "Ceased due to CAO (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage ceased CLA due to CAO by statistical neighbours")
    title <- paste0("Percentage ceased CLA due to CAO by statistical neighbours (", max(filtered_data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$SN_cao_tbl <- renderReactable({
    filtered_data <- ceased_cla_data %>%
      filter(characteristic == "Residence order or child arrangement order granted") %>%
      rename(`Reason ceased` = `characteristic`, `Total ceased` = `Total_num`)

    reactable(
      stats_neighbours_table(filtered_data, input$geographic_breakdown_o2, input$select_geography_o2, selectedcolumn = c("Reason ceased", "Number ceased", "Total ceased"), yvalue = "Ceased (%)"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_repeat_cpp_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_cpp_repeat_la", filename = "repeat_CPP_rates_all_LAs.csv"),
              reactableOutput("table_cpp_repeat_la")
            ))
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
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance.", target = "_blank"),
                tags$br(),
                "For more information about child protection plans, please refer to", a(href = "https://assets.publishing.service.gov.uk/media/65cb4349a7ded0000c79e4e1/Working_together_to_safeguard_children_2023_-_statutory_guidance.pdf", "Working together to safeguard children - statutory guidance.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o3 != "", "Select a location."),
        need(
          nrow(repeat_cpp %>% filter(time_period == max(repeat_cpp$time_period) & geo_breakdown %in% input$geographic_breakdown_o3)) > 0,
          "This local authority has no data for the current year"
        )
      )
      tagList(
        plotlyOutput("cpp_repeat_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cpp",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_cpp_repeat_tbl", filename = paste0("reapeat_CPP_SN_", input$geographic_breakdown_o3, ".csv")),
              reactableOutput("SN_cpp_repeat_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    filtered_data <- repeat_cpp %>%
      rename("Repeat CPP (%)" = "Repeat_CPP_percent")

    max_rate <- max(repeat_cpp$`Repeat_CPP_percent`[repeat_cpp$time_period == max(repeat_cpp$time_period) &
      repeat_cpp$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o3, input$select_geography_o3, "Repeat CPP (%)", "Repeat CPP (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Repeat CPP (%) by statistical neighbours")
    title <- paste0("Repeat CPP (%) by statistical neighbours (", max(filtered_data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })


  output$SN_cpp_repeat_tbl <- renderReactable({
    data <- repeat_cpp %>%
      rename("Repeat CPP (%)" = "Repeat_CPP_percent", "CPP starts" = "CPP_start", "Repeat CPP" = "CPP_subsequent")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, selectedcolumn = c("CPP starts", "Repeat CPP"), yvalue = "Repeat CPP (%)"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `CPP starts` = colDef(name = "CPP Starts", cell = cellfunc),
        `Repeat CPP` = colDef(name = "Repeat CPP", cell = cellfunc),
        `Repeat Cpp (%)` = colDef(name = "Repeat CPP (%)", cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })

  ### CPP for 2+ years by LA ----

  # output all LA chart or stats neighbour chart for CPP duration
  output$SN_CPP_duration <- renderUI({
    if (input$CPP_duration_stats_toggle == "All local authorities") {
      tagList(
        plotlyOutput("plot_cpp_duration_la"),
        br(),
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_duration_cpp_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_cpp_duration_la", filename = "CPP_more_than_2years_rates_region.csv"),
              reactableOutput("table_cpp_duration_la")
            ))
          )
        ),
        details(
          inputId = "cpp_duration_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("The metric shown in the graph refers to the percentage of children who have been on a child protection plan (CPP) for longer than 2 years."),
              tags$br(),
              p(
                "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance.", target = "_blank"),
                tags$br(),
                "For more information about child protection plans, refer to", a(href = "https://assets.publishing.service.gov.uk/media/65cb4349a7ded0000c79e4e1/Working_together_to_safeguard_children_2023_-_statutory_guidance.pdf", "Working together to safeguard children - statutory guidance.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o3 != "", "Select a location."),
        need(
          nrow(duration_cpp %>% filter(time_period == max(duration_cpp$time_period) & geo_breakdown %in% input$geographic_breakdown_o3)) > 0,
          "This local authority has no data for the current year"
        )
      )
      tagList(
        plotlyOutput("cpp_duration_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_duration_cpp",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_cpp_duration_tbl", filename = paste0("duration_CPP_SN_", input$geographic_breakdown_o3, ".csv")),
              reactableOutput("SN_cpp_duration_tbl")
            ))
          )
        ),
        details(
          inputId = "sn_cpp_duration_info",
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
              ),
            )
          )
        )
      )
    }
  })

  # CPP duration SN plot and table alternative
  output$cpp_duration_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o3 != "", "Select a location.")
    )
    filtered_data <- duration_cpp %>%
      rename("CPP 2+ years (%)" = "CPP_2_years_or_more_percent", "CPP 2+ years" = "X2_years_or_more")

    max_rate <- max(duration_cpp$`CPP_2_years_or_more_percent`[duration_cpp$time_period == max(duration_cpp$time_period) &
      duration_cpp$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(filtered_data, input$geographic_breakdown_o3, input$select_geography_o3, "CPP 2+ years (%)", "CPP 2+ years (%)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)

    # p <- p + ggtitle("Repeat CPP (%) by statistical neighbours")
    title <- paste0("Repeat CPP (%) by statistical neighbours (", max(filtered_data$time_period), ")")
    p <- p + ggtitle(title)
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })


  output$SN_cpp_duration_tbl <- renderReactable({
    data <- duration_cpp %>%
      rename("CPP 2+ years (%)" = "CPP_2_years_or_more_percent", "CPP 2+ years" = "X2_years_or_more")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, selectedcolumn = c("CPP 2+ years"), yvalue = "CPP 2+ years (%)"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `CPP 2+ years` = colDef(name = "CPP 2+ years", cell = cellfunc),
        `Cpp 2+ Years (%)` = colDef(name = "Cpp 2+ years (%)", cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 11, # 11 for stats neighbours, 10 for others?
      searchable = TRUE,
    )
  })


  ### Hospital admissions -----
  output$SN_hosp_admissions <- renderUI({
    if (input$hosp_admission_toggle == "All local authorities") {
      tagList(
        plotlyOutput("admissions_la_plot"),
        br(),
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_hosp_admission_la",
          label = "View chart as table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("admissions_la_tbl", filename = "hospital_admissions_all_LAs.csv"),
              reactableOutput("admissions_la_tbl")
            ))
          )
        ),
        details(
          inputId = "admissions_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("All sub national counts are rounded to the nearest 5. Rates are calculated using unrounded counts."),
              tags$li("For time points prior to 2012, all values between 1 and 5 have been suppressed and, where necessary, other LAs and comparators have also been suppressed in order to prevent possible disclosure and disclosure by differencing."),
              tags$li("For time points from 2012, all sub national counts are rounded to the nearest 5, and counts of 1 to 7 are suppressed. Rates and confidence intervals are calculated using unrounded counts."),
              tags$li("Values relating to City of London and Isles of Scilly have been combined with Hackney and Cornwall respectively."),
              tags$li(
                "In 2023, NHS England announced a ",
                a(
                  href = "https://eur03.safelinks.protection.outlook.com/?url=https:%2f%2fdigital.nhs.uk%2fdata-and-information%2ffind-data-and-publications%2fstatement-of-administrative-sources%2fmethodological-changes%2fimpact-of-changes-to-recording-of-same-day-emergency-care-activity-to-hospital-episode-statistics-hes-data&data=05%7c02%7cLaura.Powell%40dhsc.gov.uk%7c22feb52393f04a2270bc08dc587f7b14%7c61278c3091a84c318c1fef4de8973a1c%7c1%7c0%7c638482551742842317%7cUnknown%7cTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7c0%7c%7c%7c&sdata=Ic0fzE6wEChYvL5zD4LnrOnvsXvYJ%2Bwkql7DoTnjRY4%3D&reserved=0",
                  "methodological change", target = "_blank"
                ),
                " to require Trusts to report Same Day Emergency Care (SDEC) to the Emergency Care Data Set (ECDS) by July 2024. Early adopter sites began to report SDEC to ECDS from 2021/22, with other Trusts changing their reporting in 2022/23 or 2023/24. Some Trusts had previously reported this activity as part of the Admitted Patient Care data set, and moving to report to ECDS may reduce the number of admissions reported for this indicator. NHSE have advised it is not possible accurately to identify SDEC in current data flows, but the impact of the change is expected to vary by diagnosis, with indicators related to injuries and external causes potentially most affected."
              ),
              tags$li(
                "When considering if SDEC recording practice has reduced the number of admissions reported for this indicator at local level, please refer to the ",
                a(
                  href = "https://eur03.safelinks.protection.outlook.com/?url=https:%2f%2fdigital.nhs.uk%2fdata-and-information%2fdata-collections-and-data-sets%2fdata-sets%2femergency-care-data-set-ecds%2fsame-day-emergency-care&data=05%7c02%7cLaura.Powell%40dhsc.gov.uk%7c22feb52393f04a2270bc08dc587f7b14%7c61278c3091a84c318c1fef4de8973a1c%7c1%7c0%7c638482551742856428%7cUnknown%7cTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7c0%7c%7c%7c&sdata=qiwVNfRx2vQW6ZLF0CKyGJL2mqmLgt%2fZWKiqa8ufy18%3D&reserved=0",
                  "published list", target = "_blank"
                ),
                " of sites who have reported when they began to report SDEC to ECDS."
              ),
              tags$br(),
              p(
                "For more information on the data, please refer to the", a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/502/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0", "Public health data explorer.", target = "_blank"),
                tags$br(),
                "For more information on the definitions and methodology, please refer to the ", a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/6/gid/1938133230/pat/159/par/K02000001/ati/15/are/E92000001/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1", "Indicator definitions and supporting information page.", target = "_blank")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o3 != "", "Select a location."),
      )

      tagList(
        plotlyOutput("hosp_admissions_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_hosp_ad",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("hosp_admissions_SN_tbl", filename = paste0("hospital_admissions_SN_", input$geographic_breakdown_o3, ".csv")),
              reactableOutput("hosp_admissions_SN_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
              ),
            )
          )
        )
      )
    }
  })

  output$hosp_admissions_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
    )
    data <- hospital_admissions %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename(`Rate per 10,000` = `Value`)

    max_y_lim <- max(data$`Rate per 10,000`) + 50

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o3, input$select_geography_o3, "Rate per 10,000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Hospital admissions caused by unintentional and deliberate injuries to young people (0 to 14 years), by statistical neighbours")
    title <- paste0("Hospital admissions caused by unintentional and deliberate injuries to young people (0 to 14 years), by\nstatistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$hosp_admissions_SN_tbl <- renderReactable({
    data <- hospital_admissions %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename(`Rate per 10,000` = `Value`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, yvalue = "Rate per 10,000"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_child_ab_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_child_ab_neg_la", filename = "child_abuse_rates_all_LAs.csv"),
              reactableOutput("table_child_ab_neg_la")
            ))
          )
        ),
        details(
          inputId = "child_abuse_add_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("These figures are based on assessment factors recorded against individual episodes of need, which begin when a child is referred to children’s social care services and is assessed as being in need of children’s social care services. Each unique factor is counted once against a given episode, irrespective of the number of times the same factor was recorded in that episode. However, as a child can have more than one episode of need during the year (ending 31 March), the same child can be recorded more than once for a given factor."),
              tags$li("Information on child on child and adult on child physical and sexual abuse was collected and reported on for the fourth time in 2024. Previously physical abuse and sexual abuse was collected and reported on (irrespective of whether it was child on child or adult on child) and some local authorities have provided information on the old basis only, or a mixture of the old and new basis, since 2021. The old physical and sexual abuse categories have therefore been included to provide a more complete account of this category of assessment."),
              tags$li(
                "Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in the 2021 and 2022 national totals and regional totals. Data for the year ending 31 March 2024 is not available for Hampshire local authority, therefore 2023 data for Hampshire has been included in the 2024 national and regional totals. Refer to",
                a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-in-need", "Children in need methodology", target = "_blank"),
                "for more information."
              ),
              tags$li("Herefordshire local authority considerably underreported their data on factors identified at the end of assessment. Impacted data is shown as ‘u’ to indicate low reliability but are included in the national totals and regional totals."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o3 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("abuse_neg_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_ch_ab_neg",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("abuse_neg_SN_tbl", filename = paste0("child_abuse_rates_SN_", input$geographic_breakdown_o3, ".csv")),
              reactableOutput("abuse_neg_SN_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$assessment_factors_1 != "", "Select an assessment factor."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
    )
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_1, geographic_level == "Local authority", time_period == max(time_period))

    max_y_lim <- max(data$rate_per_10000) + 100
    p <- statistical_neighbours_plot_factors(data, input$geographic_breakdown_o3, input$select_geography_o3, "rate_per_10000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste0(input$assessment_factors_1, " cases (rate per 10,000), by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(title_factor, " cases (rate per 10,000), by statistical neighbours")

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$abuse_neg_SN_tbl <- renderReactable({
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_1, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Rate per 10,000" = "rate_per_10000", "Assessment factor" = `assessment_factor`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, selectedcolumn = c("Assessment factor"), yvalue = "Rate per 10,000"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_extra_fam_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_efh_la", filename = "EFH_rates_all_LAs.csv"),
              reactableOutput("table_efh_la")
            ))
          )
        ),
        details(
          inputId = "efh_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("These figures are based on assessment factors recorded against individual episodes of need, which begin when a child is referred to children’s social care services and is assessed as being in need of children’s social care services. Each unique factor is counted once against a given episode, irrespective of the number of times the same factor was recorded in that episode. However, as a child can have more than one episode of need during the year (ending 31 March), the same child can be recorded more than once for a given factor."),
              tags$li(
                "Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in the 2021 and 2022 national totals and regional totals. Data for the year ending 31 March 2024 is not available for Hampshire local authority, therefore 2023 data for Hampshire has been included in the 2024 national and regional totals. Refer to",
                a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-in-need", "Children in need methodology", target = "_blank"),
                "for more information."
              ),
              tags$li("Herefordshire local authority considerably underreported their data on factors identified at the end of assessment. Impacted data is shown as ‘u’ to indicate low reliability but are included in the national totals and regional totals."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.", target = "_blank")
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o3 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("efh_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_efh",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("efh_SN_tbl", filename = paste0("EFH_rates_SN_", input$geographic_breakdown_o3, ".csv")),
              reactableOutput("efh_SN_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$assessment_factors_2 != "", "Select an assessment factor."),
      need(input$geographic_breakdown_o3 != "", "Select a location."),
    )
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_2, geographic_level == "Local authority", time_period == max(time_period))

    max_y_lim <- max(data$rate_per_10000) + 10

    p <- statistical_neighbours_plot_factors(data, input$geographic_breakdown_o3, input$select_geography_o3, "rate_per_10000", "Rate per 10,000", max_y_lim) %>%
      config(displayModeBar = F)
    title_factor <- paste(input$assessment_factors_2, " cases (rate per 10,000), by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(title_factor)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$efh_SN_tbl <- renderReactable({
    data <- assessment_factors %>%
      filter(assessment_factor == input$assessment_factors_2, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Rate per 10,000" = "rate_per_10000", "Assessment factor" = "assessment_factor")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o3, input$select_geography_o3, selectedcolumn = "Assessment factor", yvalue = "Rate per 10,000"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_placement_type_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("placement_type_la_tbl", filename = "placement_type_all_LAs.csv"),
              reactableOutput("placement_type_la_tbl")
            ))
          )
        ),
        details(
          inputId = "placement_type_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Numbers have been rounded to the nearest 10. Percentages rounded to the nearest whole number. Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to check for the equivalent table in earlier releases of this publication. Figures exclude children looked after under a series of short-term placements."),
              tags$li("For the placement type of children who are looked after, the Dashboard provides information on foster care, secure homes and children’s homes, independent and semi-independent living arrangements / supported accommodation. It does not provide a separate breakdown for the smaller number of other placements, which include placed for adoption, placed with parents or other person with parental responsibility, other residential settings.  These figures can be found on ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after in England data guidance.", target = "_blank")),
              tags$li("From 28 April 2023, supported accommodation providers in England were able to register with Ofsted and it became illegal for a supported accommodation provider to operate if they had not submitted a complete application which had been accepted by 28 October 2023. For 2024, figures in the category 'Independent and semi-independent living arrangements/supported accommodation' include young people, before 28 October 2023, who were placed in former semi independent living accommodation or who formerly lived independently, and also young people in placements from 28 April 2023 onwards at a provider that had submitted a complete application to Ofsted to operate as a supported accommodation provider by the deadline. Former semi-independent and former independent living providers who did not submit a completed application that was accepted by Ofsted by the end of 27 October 2023 who continued to provide placements are classified as ‘Other placements’ from 28 October 2023 onwards"),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after in England data guidance.", target = "_blank"),
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o4 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("placement_type_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_placement_type",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("placement_type_SN_tbl", filename = paste0("placement_type_SN_", input$geographic_breakdown_o4, ".csv")),
              reactableOutput("placement_type_SN_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$placement_type_breakdown != "", "Select a placement type."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    data <- placement_data %>%
      filter(characteristic == input$placement_type_breakdown, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Placements (%)" = "Percent")

    max_rate <- max(placement_data$`Percent`[placement_data$time_period == max(placement_data$time_period) &
      placement_data$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Placements (%)", "Placements (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    placements_title <- paste("Children living in selected placement type (%) by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(placements_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$placement_type_SN_tbl <- renderReactable({
    data <- placement_data %>%
      filter(characteristic == input$placement_type_breakdown, geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Placements (%)" = "Percent", "Placement Type" = "characteristic")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, selectedcolumn = "Placement Type", yvalue = "Placements (%)"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_placement_changes_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("placement_changes_la_tbl", filename = "cla_more_than_3_placements_all_LAs.csv"),
              reactableOutput("placement_changes_la_tbl")
            ))
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
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after in England data guidance.", target = "_blank"),
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o4 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("placement_changes_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_placement_changes",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("placement_changes_SN_tbl", filename = paste0("cla_more_than_3_placements_SN_", input$geographic_breakdown_o4, ".csv")),
              reactableOutput("placement_changes_SN_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$placement_type_breakdown != "", "Select a placement type."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year", geographic_level == "Local authority", time_period == max(time_period))

    max_rate <- max(placement_changes_data$`Percent`[placement_changes_data$time_period == max(placement_changes_data$time_period) &
      placement_changes_data$geographic_level == "Local authority" &
      placement_changes_data$placement_stability == "With 3 or more placements during the year"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Percent", "Percentage", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage of CLA with 3 or more placements during the year by statistical neighbours")
    title <- paste0("Percentage of CLA with 3 or more placements during the year by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$placement_changes_SN_tbl <- renderReactable({
    data <- placement_changes_data %>%
      filter(placement_stability == "With 3 or more placements during the year", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename(`Percentage2` = `Percentage`, `Percentage` = `Percent`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Percentage"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_placement_changes_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("placement_dist_la_tbl", filename = "placements_more_than_20_miles_from_home_all_LAs.csv"),
              reactableOutput("placement_dist_la_tbl")
            ))
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
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after in England data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o4 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("placement_dist_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_placement_changes",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("placement_dist_SN_tbl", filename = paste0("placements_more_than_20_miles_from_home_SN_", input$geographic_breakdown_o4, ".csv")),
              reactableOutput("placement_dist_SN_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    data <- placement_data %>%
      filter(characteristic == "Placed more than 20 miles from home", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Placements more then 20 miles from home (%)" = "Percent")

    max_rate <- max(placement_data$`Percent`[placement_data$time_period == max(placement_data$time_period) &
      placement_data$geographic_level == "Local authority" &
      placement_data$characteristic == "Placed more than 20 miles from home"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Placements more then 20 miles from home (%)", "Placements (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Percentage of placements more then 20 miles from home by statistical neighbours")
    title <- paste0("Percentage of placements more then 20 miles from home by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$placement_dist_SN_tbl <- renderReactable({
    data <- placement_data %>%
      filter(characteristic == "Placed more than 20 miles from home", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename(`Placements (%)` = `Percent`, `Placement Distance` = `characteristic`)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, selectedcolumn = "Placement Distance", yvalue = "Placements (%)"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_sdq_score_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("sdq_by_la_tbl", filename = "wellbeing_sdq_score_all_LAs.csv"),
              reactableOutput("sdq_by_la_tbl")
            ))
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
            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after guidance.", target = "_blank"),
            tags$br(),
            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
          ))
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o4 != "", "Select a location.")
      )
      tagList(
        plotlyOutput("SN_sdq_plot"),
        br(),
        details(
          inputId = "tbl_sn_sdq_score",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_sdq_table", filename = paste0("wellbeing_sdq_score_SN_", input$geographic_breakdown_o4, ".csv")),
              reactableOutput("SN_sdq_table")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
              ),
            )
          )
        )
      )
    }
  })

  output$SN_sdq_plot <- renderPlotly({
    validate(
      need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    data <- wellbeing_sdq_data %>%
      filter(characteristic == "SDQ average score", geographic_level == "Local authority", time_period == max(time_period)) %>%
      rename("Average score" = "number_num")

    max_y_lim <- (max(data$`Average score`) + 5)
    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Average score", "Average SDQ score", max_y_lim, add_rect = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Average SDQ score by statistical neighbours")
    title <- paste0("Average SDQ score by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      layout(hovermode = "x") %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$SN_sdq_table <- renderReactable({
    data <- wellbeing_sdq_data %>%
      filter(characteristic == "SDQ average score" & geographic_level == "Local authority" & time_period == max(time_period)) %>%
      rename("SDQ characteristic" = characteristic, "Average score" = "number_num", "SDQ score" = score_label)

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, selectedcolumn = c("SDQ characteristic", "SDQ score"), yvalue = "Average score"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Average Score` = colDef(name = "Average score", cell = cellfunc_decimal_percent, defaultSortOrder = "desc"),
        `SDQ score` = colDef(name = "SDQ score")
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_cl_activity_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_cl_activity_la", filename = "care_leavers_activity_all_LAs.csv"),
              reactableOutput("table_cl_activity_la")
            ))
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
              "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after guidance.", target = "_blank"),
              tags$br(),
              "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
          ))
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o4 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("cl_activity_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cl_act",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("cl_activity_SN_tbl", filename = paste0("care_leavers_activity_SN_", input$geographic_breakdown_o4, ".csv")),
              reactableOutput("cl_activity_SN_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$leavers_age != "", "Select an age range."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    data <- care_leavers_activity_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & activity == "Total in education, employment or training") %>%
      rename("Care leavers in education, employment or training (%)" = "percent")

    max_rate <- max(care_leavers_activity_data$`percent`[care_leavers_activity_data$time_period == max(care_leavers_activity_data$time_period) &
      care_leavers_activity_data$geographic_level == "Local authority" &
      care_leavers_activity_data$activity == "Total in education, employment or training"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Care leavers in education, employment or training (%)", "Care leavers in education,\n employment or training (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    age_title <- paste0("Care leavers in employment, education and training (", input$leavers_age, ") by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$cl_activity_SN_tbl <- renderReactable({
    data <- care_leavers_activity_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & activity == "Total in education, employment or training") %>%
      rename("Percent" = "percent")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Percent"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_cl_accommodation_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_cl_accommodation_la", filename = "care_leavers_accomm_all_LAs.csv"),
              reactableOutput("table_cl_accommodation_la")
            ))
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
              "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after guidance.", target = "_blank"),
              tags$br(),
              "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_o4 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_o4 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("cl_accommodation_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_cl_accomm",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("cl_acccomm_SN_tbl", filename = paste0("care_leavers_accomm_SN_", input$geographic_breakdown_o4, ".csv")),
              reactableOutput("cl_acccomm_SN_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$leavers_age != "", "Select an age range."),
      need(input$geographic_breakdown_o4 != "", "Select a location."),
    )
    data <- care_leavers_accommodation_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & accommodation_suitability == "Accommodation considered suitable") %>%
      rename("Care leavers in suitable accommodation (%)" = "percent")

    max_rate <- max(care_leavers_accommodation_data$`percent`[care_leavers_accommodation_data$time_period == max(care_leavers_accommodation_data$time_period) &
      care_leavers_accommodation_data$geographic_level == "Local authority" &
      care_leavers_accommodation_data$accommodation_suitability == "Accommodation considered suitable"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_o4, input$select_geography_o4, "Care leavers in suitable accommodation (%)", "Care leavers in suitable\n accommodation (%)", max_rate, decimal_percentage = FALSE) %>%
      config(displayModeBar = F)
    age_title <- paste("Care leavers in suitable accommodation (", input$leavers_age, ") by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(age_title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$cl_acccomm_SN_tbl <- renderReactable({
    data <- care_leavers_accommodation_data %>%
      filter(age == input$leavers_age & geographic_level == "Local authority" & time_period == max(time_period) & accommodation_suitability == "Accommodation considered suitable") %>%
      rename("Care leavers in suitable accommodation (%)" = "percent")

    reactable(
      stats_neighbours_table(data, input$geographic_breakdown_o4, input$select_geography_o4, yvalue = "Care leavers in suitable accommodation (%)"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_turnover_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_turnover_la", filename = "social_worker_turnover_all_LAs.csv"),
              reactableOutput("table_turnover_la")
            ))
          )
        ),
        details(
          inputId = "turnover_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
              tags$li("The turnover rate is calculated as (the number of) children and family social worker leavers in the year to 30 September divided by children and family social workers in post at 30 September. The turnover rate is a measure of churn in the workforce (although it doesn’t capture the movement of social workers to different children and family social work positions within the same local authority)."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
              )
            )

          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_e3 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("turnover_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_turnover",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_turnover_tbl", filename = paste0("social_worker_turnover_SN_", input$geographic_breakdown_e3, ".csv")),
              reactableOutput("SN_turnover_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e3 != "", "Select a location."),
    )

    max_rate <- max(workforce_data$`Turnover Rate Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Turnover Rate Fte", "Turnover Rate %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Social worker turnover (FTE) % by statistical neighbours")
    title <- paste0("Social worker turnover (FTE) % by statistical neighbours (", max(workforce_data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$SN_turnover_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, yvalue = "Turnover Rate Fte"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Turnover Rate Fte` = colDef(name = "Turnover rate (FTE) %", cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_agency_rate_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_agency_rate_la", filename = "agency_worker_rate_all_LAs.csv"),
              reactableOutput("table_agency_rate_la")
            ))
          )
        ),
        details(
          inputId = "agency_worker_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
              tags$li(
                "After the 2024 collection had closed, Birmingham local authority informed the Department that there were data quality issues with the figures they reported in the collection. This affects their data on agency workers, caseload and absence. To reflect these issues:",
                tags$ul(
                  tags$li("For the national and regional figures, 2024 data for Birmingham has been included in the caseload figures/rates and agency worker counts but excluded from the sickness absence figures/rates and agency worker rates."),
                  tags$li("2024 data for Birmingham has been provided as ‘u’ in the underlying data for these measures to indicate low reliability.
")
                )
              ),
              tags$li("The decision to include or exclude Birmingham's figures from the regional and national figures is based on assessments of under and over reporting in these statistics, with included figures not being deemed to have a considerable impact on national/regional trends and excluded figures deemed to have a greater impact. The Department will further investigate these data quality issues with the local authority and revise the data in this statistical release if necessary in due course."),
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
    } else {
      validate(
        need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_e3 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("agency_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_agency",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_agency_tbl", filename = paste0("agency_worker_rate_SN_", input$geographic_breakdown_e3, ".csv")),
              reactableOutput("SN_agency_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e3 != "", "Select a location."),
    )

    max_rate <- max(workforce_data$`Agency Rate Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Agency Rate Fte", "Agency worker rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Agency worker rate (FTE) % by statistical neighbours")
    title <- paste0("Agency worker rate (FTE) % by statistical neighbours (", max(workforce_data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$SN_agency_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, yvalue = "Agency Rate Fte"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Agency Rate Fte` = colDef(name = "Agency worker rate (FTE) %", cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_vacancy_rate_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_vacancy_rate_la", filename = "vacancy_rates_all_LAs.csv"),
              reactableOutput("table_vacancy_rate_la")
            ))
          )
        ),
        details(
          inputId = "vacancy_rate_la_info",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
              tags$li("The vacancy rate, as at 30 September per year, is calculated as (the number of) FTE (full-time equivalent) vacancies divided by the sum of FTE vacancies and FTE social workers."),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                tags$br(),
                "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
              )
            )
          )
        ),
      )
    } else {
      validate(
        need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_e3 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("vacancy_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_vacancy",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_vacancy_tbl", filename = paste0("vacancy_rates_SN_", input$geographic_breakdown_e3, ".csv")),
              reactableOutput("SN_vacancy_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
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
      need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e3 != "", "Select a location."),
    )

    max_rate <- max(workforce_data$`Vacancy Rate Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Vacancy Rate Fte", "Vacancy rate (FTE) %", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Vacancy rate (FTE) % by statistical neighbours")
    title <- paste0("Vacancy rate (FTE) % by statistical neighbours (", max(workforce_data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$SN_vacancy_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, yvalue = "Vacancy Rate Fte"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Vacancy Rate Fte` = colDef(name = "Vacancy rate (FTE) %", cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_caseload_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_caseload_la", filename = "avg_caseload_all_LAs.csv"),
              reactableOutput("table_caseload_la")
            ))
          )
        ),
        details(
          inputId = "caseload_la_info",
          label = "Additional information:",
          help_text = (
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
    } else {
      validate(
        need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_e3 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("caseload_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_caseload",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_caseload_tbl", filename = paste0("avg_caseload_SN_", input$geographic_breakdown_e3, ".csv")),
              reactableOutput("SN_caseload_tbl")
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
      need(input$select_geography_e3 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e3 != "", "Select a location."),
    )

    max_rate <- max(workforce_data$`Caseload Fte`[workforce_data$time_period == max(workforce_data$time_period) &
      workforce_data$geographic_level == "Local authority"], na.rm = TRUE)
    max_rate <- ceiling(max_rate / 10) * 10

    p <- statistical_neighbours_plot(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, "Caseload Fte", "Average Caseload (FTE)", max_rate, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    # p <- p + ggtitle("Average caseload (FTE) by statistical neighbours")
    title <- paste0("Average caseload (FTE) by statistical neighbours (", max(workforce_data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$SN_caseload_tbl <- renderReactable({
    reactable(
      stats_neighbours_table(workforce_data, input$geographic_breakdown_e3, input$select_geography_e3, yvalue = "Caseload Fte"),
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Caseload Fte` = colDef(name = "Average caseload (FTE)", cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
      ),
      defaultPageSize = 15,
      searchable = TRUE,
    )
  })



  # Enabler 2 ----
  ### Ofsted leadership rating --------
  output$ofsted_rating_SN_ui <- renderUI({
    tagList(
      h2("Ofsted – The impact of leaders on social work practice with children and families with statistical neighbours"),
      p("Hover over each data point to see the year of their last Ofsted inspection."),
      plotlyOutput("ofsted_SN_plot"),
      br(),
      details(
        inputId = "tbl_ofsted_SN",
        label = "View chart as a table",
        help_text = (
          HTML(paste0(
            csvDownloadButton("ofsted_SN_tbl", filename = paste0("Ofsted_leadership_ratings_SN_", input$geographic_breakdown_e2, ".csv")),
            reactableOutput("ofsted_SN_tbl")
          ))
        )
      ),
      details(
        inputId = "ofsted_stat_neighbours_info",
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
            ),
          )
        )
      )
    )
  })



  output$ofsted_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
    )

    p <- statistical_neighbours_plot_ofsted(ofsted_leadership_data_long, input$geographic_breakdown_e2) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Ofsted – The impact of leaders on social work practice with children and families with statistical neighbours")
    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$ofsted_SN_tbl <- renderReactable({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this table, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
    )
    data <- ofsted_leadership_data_long %>%
      mutate(Rating = recode(Rating,
        "inadequate_count" = "Inadequate",
        "requires_improvement_count" = "Requires Improvement",
        "good_count" = "Good",
        "outstanding_count" = "Outstanding"
      )) %>%
      group_by(geo_breakdown) %>%
      mutate(latest_rating = max(published_year)) %>%
      ungroup()
    reactable(
      stats_neighbours_table_ofsted(data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Rating"),
      defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_tot_spend_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("table_tot_spending_la", filename = "spend_on_CSC_all_LAs.csv"),
              reactableOutput("table_tot_spending_la")
            ))
          )
        ),
        details(
          inputId = "tot_spend_la_information",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Share of spend is calculated by taking total children’s services expenditure divided by total local authority expenditure"),
              tags$li("Average per capita (of all children in a local authority) spend on children’s services is calculated based on", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales", "ONS published mid-2024 population estimates", target = "_blank"), "for children aged 0 to 17 years and total children’s services expenditure."),
              tags$li("Average per capita (of all children in a local authority) spend on children’s services has been rounded to the nearest whole number."),
              tags$li("Spending data is based on the RO3 and RSX data files from the", a(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2024-to-2025-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2024 to 2025 individual local authority data – outturn", target = "_blank")),
              tags$br(),
              p(
                "For more information on the data and definitions, refer to the", a(href = "https://www.gov.uk/government/publications/general-fund-revenue-account-outturn/general-fund-revenue-account-outturn-general-guidance-notes", "General fund revenue account outturn: general guidance notes.", target = "_blank"),
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_e2 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("total_spending_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_total_spending",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_tot_spend_tbl", filename = paste0("spend_on_CSC_SN_", input$geographic_breakdown_e2, ".csv")),
              reactableOutput("SN_tot_spend_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
              ),
            )
          )
        )
      )
    }
  })

  output$total_spending_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
    )

    # Need an if statement to look at the spending level choice this will determine the data in the chart
    if (input$spending_choice == "Share of total local authority spend on children's services") {
      data <- spending_data

      max_y_lim <- ceiling(max(data$cs_share) / 10) * 10
      p <- statistical_neighbours_plot(data, input$geographic_breakdown_e2, input$select_geography_e2, "cs_share", "Share spent on children's services (%)", max_y_lim, decimal_percentage = TRUE) %>%
        config(displayModeBar = F)
      title <- paste0("Share of total LA spend on children's services (%) by statistical neighbours (", max(data$time_period), ")")
      p <- p + ggtitle(title)
    } else {
      data <- spending_per_capita %>%
        rename("Spend per child (£)" = "cost_per_capita")

      max_y_lim <- ceiling(max(data$`Spend per child (£)`) / 50) * 50

      p <- statistical_neighbours_plot(data, input$geographic_breakdown_e2, input$select_geography_e2, "Spend per child (£)", "Average spend per child (£)", max_y_lim, decimal_percentage = TRUE) %>%
        config(displayModeBar = F)
      # p <- p + ggtitle("Average spend per child (£) by statistical neighbours")
      title <- paste0("Average spend per child (£) by statistical neighbours (", max(data$time_period), ")")
      p <- p + ggtitle(title)
    }


    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$SN_tot_spend_tbl <- renderReactable({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
    )
    # Need an if statement to look at the spending level choice this will determine the data in the chart
    if (input$spending_choice == "Share of total local authority spend on children's services") {
      data <- spending_data %>%
        rename("Children's services share (%)" = "cs_share")

      table <- stats_neighbours_table(data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Children's services share (%)")

      reactable(
        table,
        defaultColDef = colDef(align = "center"),
        columns = list(
          `Children's Services Share (%)` = colDef(name = "Children's Services share (%)", cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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
        defaultColDef = colDef(align = "center"),
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
        p("This chart is reactive to the local authority and regional filters at the top and will not react to the national filter. The chart will display all local authorities overall or every local authority in the selected region."),
        br(),
        details(
          inputId = "tbl_tot_no_cla_spend_la",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("spend_excl_cla_la_tbl", filename = "spend_on_CSC_excl_CLA_all_LAs.csv"),
              reactableOutput("spend_excl_cla_la_tbl")
            ))
          )
        ),
        details(
          inputId = "no_cla_spend_information",
          label = "Additional information:",
          help_text = (
            tags$ul(
              tags$li("Share of spend is calculated by taking total children’s services expenditure minus total CLA expenditure, divided by total children’s services expenditure"),
              tags$li("Spending data is based on the RO3 and RSX data files from the", a(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2024-to-2025-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2024 to 2025 individual local authority data – outturn", target = "_blank")),
              tags$br(),
              p(
                "For more information on the data and definitions, please refer to the", a(href = "https://www.gov.uk/government/publications/general-fund-revenue-account-outturn/general-fund-revenue-account-outturn-general-guidance-notes", "General fund revenue account outturn: general guidance notes.", target = "_blank"),
              )
            )
          )
        )
      )
    } else {
      validate(
        need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
        need(input$geographic_breakdown_e2 != "", "Select a location."),
      )
      tagList(
        plotlyOutput("spend_excl_cla_SN_plot"),
        br(),
        details(
          inputId = "tbl_sn_spending",
          label = "View chart as a table",
          help_text = (
            HTML(paste0(
              csvDownloadButton("SN_spend_no_cla_tbl", filename = paste0("spend_on_CSC_excl_CLA_SN_", input$geographic_breakdown_e2, ".csv")),
              reactableOutput("SN_spend_no_cla_tbl")
            ))
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
                "For information on the Children’s services statistical neighbour benchmarking tool, please refer to the", a(href = "https://www.gov.uk/government/publications/local-authority-interactive-tool-lait", "Local Authority Interactive Tool (LAIT) publication.", target = "_blank"),
                tags$br(),
                "The Children’s services statistical neighbour benchmarking is also available", a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "here.", target = "_blank")
              ),
            )
          )
        )
      )
    }
  })


  output$spend_excl_cla_SN_plot <- plotly::renderPlotly({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
    )

    data <- spending_data_no_cla
    max_y_lim <- ceiling(max(data$minus_cla_share) / 10) * 10

    p <- statistical_neighbours_plot(data, input$geographic_breakdown_e2, input$select_geography_e2, "minus_cla_share", "Share of Children’s Services spend\n not on CLA (%)", max_y_lim, decimal_percentage = TRUE) %>%
      config(displayModeBar = F)
    title <- paste0("Share of Children’s Services spend not on CLA (%) by statistical neighbours (", max(data$time_period), ")")
    p <- p + ggtitle(title)

    ggplotly(
      p,
      height = 420,
      tooltip = "text"
    ) %>%
      config(displayModeBar = T, modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "lasso2d"))
  })

  output$SN_spend_no_cla_tbl <- renderReactable({
    validate(
      need(input$select_geography_e2 == "Local authority", "To view this chart, you must select \"Local authority\" level and select a local authority."),
      need(input$geographic_breakdown_e2 != "", "Select a location."),
    )

    data <- spending_data_no_cla %>%
      rename("Share of Children’s Services spend not on CLA (%)" = "minus_cla_share")

    table <- stats_neighbours_table(data, input$geographic_breakdown_e2, input$select_geography_e2, yvalue = "Share of Children’s Services spend not on CLA (%)")

    reactable(
      table,
      defaultColDef = colDef(align = "center"),
      columns = list(
        `Share Of Children’s Services Spend Not On Cla (%)` = colDef(name = "Share of Children’s Services spend not on CLA (%)", cell = cellfunc_decimal_percent, defaultSortOrder = "desc")
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

  observeEvent(input$go_to_user_guide, {
    updateTabsetPanel(session, "navlistPanel", selected = "user_guide")
  })
}
