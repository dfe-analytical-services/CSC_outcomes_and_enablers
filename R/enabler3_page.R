enabler3_tab <- function() {
  nav_panel(
    value = "enabler3_page",
    "Workforce",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Enabler: The workforce is equipped and effective"),
        )
      ),
      gov_row(
        div(
          class = "geo_input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_e3",
              label = "Select a geographical level:",
              choices = unique(workforce_data %>% filter(geographic_level != "Statistical neighbours (median)") %>% pull("geographic_level")),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(condition = "input.select_geography_e3 != 'National'", selectizeInput(
              inputId = "geographic_breakdown_e3",
              label = "Select a location: ",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = NULL
            )),
            panel(),
            col_widths = c(4, 4, 4)
          ),
          layout_columns(
            conditionalPanel(
              condition = "input.select_geography_e3 != 'National'",
              column(
                width = 12,
                checkbox_Input(
                  inputId = "national_comparison_checkbox_e3",
                  cb_labels = "Compare with national",
                  checkboxIds = "Yes_national_e3",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "(input.select_geography_e3 == 'Local authority')",
              column(
                width = 12,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_e3",
                  cb_labels = "Compare with region",
                  checkboxIds = "Yes_region_e3",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "(input.select_geography_e3 == 'Local authority')",
              column(
                width = 12,
                checkbox_Input(
                  inputId = "sn_comparison_checkbox_e3",
                  cb_labels = "Compare with statistical neighbours",
                  checkboxIds = "Yes_sn_e3",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            col_widths = c(4, 4, 4)
          ),
        )
      ),
      gov_row(
        p(htmlOutput("enabler3_choice_text1"), htmlOutput("enabler3_choice_text2")),
        conditionalPanel(
          condition = "(input.geographic_breakdown_e3 == 'Kingston upon Thames / Richmond upon Thames')",
          p("Kingston upon Thames and Richmond upon Thames submit a joint workforce return each year, and their data is reported together.")
        ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_e3 == 'North Northamptonshire / West Northamptonshire')",
          p("North Northamptonshire and West Northamptonshire submitted a joint workforce return in 2021 and onwards, and their data is reported together")
        ),
        div(
          tabsetPanel(
            id = "enabler3_panels",
            type = "tabs",
            tabPanel(
              "Workforce stability",
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Turnover rate (FTE)",
                    value = htmlOutput("s_w_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Agency worker rate (FTE)",
                    value = htmlOutput("agency_rate_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Vacancy rate (FTE)",
                    value = htmlOutput("vacancy_rate_txt")
                  )
                ),
                br(),
              ),
              accordion(
                # Social Worker Turnover --------------
                accordion_panel(
                  "Social worker turnover",
                  gov_row(
                    h2("Social worker turnover"),
                    p("Prioritising a stable workforce allows children, young people and families to maintain consistent relationships with practitioners."),
                    insert_text(inputId = "social_work_turnover_definition", text = paste(
                      "The ", "<b>", "turnover rate", "</b>", " is calculated as (the number of) FTE (full-time equivalent) children and family social worker leavers in the year to 30 September divided by FTE children and family social workers in
                      post at 30 September. The turnover rate is a measure of churn in the workforce (although it doesn’t capture the movement of social workers to different children and family social work positions within the same local authority)."
                    )),
                    plotlyOutput("plot_s_w_turnover"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_s_w_turnover",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_s_w_turnover", filename = "social_worker_turnover.csv"),
                          reactableOutput("table_s_w_turnover")
                        ))
                      )
                    ),
                    details(
                      inputId = "turnover_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
                          tags$li("The turnover rate is calculated as (the number of) children and family social worker leavers in the year to 30 September divided by children and family social workers in post at 30 September. The turnover rate is a measure of churn in the workforce (although it doesn’t capture the movement of social workers to different children and family social work positions within the same local authority)."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )

                      )
                    ),
                  ),
                  gov_row(
                    h2("Turnover rates by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_turnover_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_turnover_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_turnover_reg", filename = "social_worker_turnover_region.csv"),
                          reactableOutput("table_turnover_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "turnover_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
                          tags$li("The turnover rate is calculated as (the number of) children and family social worker leavers in the year to 30 September divided by children and family social workers in post at 30 September. The turnover rate is a measure of churn in the workforce (although it doesn’t capture the movement of social workers to different children and family social work positions within the same local authority)."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )

                      )
                    ),
                  ),
                  gov_row(
                    h2("Turnover rates by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(workforce_data$time_period))),
                    radioGroupButtons(
                      "turnover_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_turnover"),
                  ),
                ),
                # Agency worker rates ---------------------------
                accordion_panel(
                  "Agency rates",
                  gov_row(
                    h2("Agency rates"),
                    p("Prioritising a stable and permanent workforce allows children, young people and families to maintain consistent relationships with practitioners.
                           Agency workers should only be used as per the national agency rules from Autumn 2024."),
                    insert_text(inputId = "agency_rates_definition", text = paste(
                      "<b>", "Agency workers", "</b>", "are children and family social workers not directly paid by the local authority. These may be social workers who are paid by an agency rather than the local authority or who are self-employed.", "<br>", "<br>",
                      "The ", "<b>", "agency worker rate", "</b>", ", as at 30 September, is calculated as (the number of) FTE (full-time equivalent) agency (children and family) social workers divided by the sum of FTE agency social workers and FTE social workers."
                    )),
                    br(),
                    plotlyOutput("plot_agency_worker"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_agency_worker",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_agency_worker", filename = "agency_worker_rate.csv"),
                          reactableOutput("table_agency_worker")
                        ))
                      )
                    ),
                    details(
                      inputId = "agency_worker_info",
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
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Agency rates by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_agency_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_agency_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_agency_reg", filename = "agency_worker_rate_regions.csv"),
                          reactableOutput("table_agency_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "agency_worker_reg_info",
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
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Agency rates by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(workforce_data$time_period))),
                    radioGroupButtons(
                      "agency_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_agency"),
                  ),
                ),
                # Vacancy rates ------------------
                accordion_panel(
                  "Vacancy rates",
                  gov_row(
                    h2("Vacancy rates"),
                    p("A workforce strategy should develop and maintain an effective workforce. With a well-supported workforce vacancy rates should remain low."),
                    insert_text(inputId = "vacancy_rates_definition", text = paste(
                      "<b>", "Vacancies", "</b>", "  are defined as any FTE (child and family social worker) vacancy at 30 September within a local authority’s organisational structure, including vacancies that are not being actively recruited for, and those covered by agency workers.", "<br>", "<br>",
                      "The ", "<b>", "vacancy rate", "</b>", ", as at 30 September, is calculated as (the number of) FTE (full-time equivalent) vacancies divided by the sum of FTE vacancies and FTE social workers."
                    )),
                    br(),
                    plotlyOutput("plot_vacancy_rate"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_vacancy_rate",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_vacancy_rate", filename = "vacancy_rates.csv"),
                          reactableOutput("table_vacancy_rate")
                        ))

                      )
                    ),
                    details(
                      inputId = "vacancy_rate_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
                          tags$li("The vacancy rate, as at 30 September per year, is calculated as (the number of) FTE (full-time equivalent) vacancies divided by the sum of FTE vacancies and FTE social workers."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    ),
                  ),
                  gov_row(
                    h2("Vacancy rates by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_vacancy_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_vacancy_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_vacancy_reg", filename = "vacancy_rates_regions.csv"),
                          reactableOutput("table_vacancy_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "vacancy_rate_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
                          tags$li("The vacancy rate, as at 30 September per year, is calculated as (the number of) FTE (full-time equivalent) vacancies divided by the sum of FTE vacancies and FTE social workers."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    ),
                  ),
                  gov_row(
                    h2("Vacancy rates by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(workforce_data$time_period))),
                    radioGroupButtons(
                      "vacancy_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_vacancy"),
                  ),
                ),
                open = FALSE
              )
            ),
            # Second Domain - "quality of support for children and families" -------------

            tabPanel(
              "Quality of support for children and families",
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Average caseload (FTE)",
                    value = htmlOutput("caseload_txt")
                  ),
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Percentage of children with 3 or more social workers in past 12 months",
                    value = htmlOutput("sw_stability_txt")
                  ),
                )
              ),
              accordion(
                ## caseload -------------
                accordion_panel(
                  "Social worker caseloads",
                  gov_row(
                    h2("Social worker caseloads"),
                    p("Ensuring that practitioners have an appropriate caseload supports recruitment and
                         retention and allows practitioners to deliver impactful services."),
                    insert_text(inputId = "caseload_definition", text = paste(
                      "A", "<b>", " case ", "</b>", " is defined as any person allocated to a named social worker, where the work involves child and family social work. Cases may be held by social workers regardless of their role in the organisation and not just those specifically in a ‘case holder’ role.", "<br>", "<br>",
                      "<b>", "Average caseload", "</b>", "at 30 September is calculated as the total number of cases held by FTE (Full-time Equivalent) social workers, including agency workers, in post divided by the number of FTE social workers, including agency workers, in post that held one or more cases.", "<br><br>",
                      "The number of cases held doesn’t account for the complexity of the cases held and this should also be taken into consideration when interpreting the caseload figures."
                    )),
                    br(),
                    plotlyOutput("caseload_plot"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_caseload",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_caseload", filename = "avg_caseload.csv"),
                          reactableOutput("table_caseload")
                        ))
                      )
                    ),
                    details(
                      inputId = "caseload_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
                          tags$li("Average caseload at 30 September per year is calculated as the total number of cases held by FTE social workers, including agency workers, in post divided by the number of FTE social workers, including agency workers, in post that held one or more cases."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Social worker caseloads by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_caseload_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_caseload_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_caseload_reg", filename = "avg_caseload_regions.csv"),
                          reactableOutput("table_caseload_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "caseload_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
                          tags$li("Average caseload at 30 September per year is calculated as the total number of cases held by FTE social workers, including agency workers, in post divided by the number of FTE social workers, including agency workers, in post that held one or more cases."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Social worker caseloads by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(workforce_data$time_period))),
                    radioGroupButtons(
                      "caseload_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_caseload"),
                  )
                ),
                accordion_panel(
                  ## social worker stability (new indicator) ----
                  "Social worker stability",
                  gov_row(
                    h2("Social worker stability"),
                    p("Ensuring that practitioners have an appropriate caseload supports recruitment and
                         retention and allows practitioners to deliver impactful services."),
                    insert_text(inputId = "caseload_definition", text = paste(
                      "A", "<b>", " case ", "</b>", " is defined as any person allocated to a named social worker, where the work involves child and family social work. Cases may be held by social workers regardless of their role in the organisation and not just those specifically in a ‘case holder’ role.", "<br>", "<br>",
                      "<b>", "Average caseload", "</b>", "at 30 September is calculated as the total number of cases held by FTE (Full-time Equivalent) social workers, including agency workers, in post divided by the number of FTE social workers, including agency workers, in post that held one or more cases.", "<br><br>",
                      "The number of cases held doesn’t account for the complexity of the cases held and this should also be taken into consideration when interpreting the caseload figures."
                    )),
                    br(),
                    # here is the call to the module to display timeseries chart, table and download button
                    timeseries_section_ui("sw_stability"),
                    details(
                      inputId = "caseload_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
                          tags$li("Average caseload at 30 September per year is calculated as the total number of cases held by FTE social workers, including agency workers, in post divided by the number of FTE social workers, including agency workers, in post that held one or more cases."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Social worker stability by region"),
                    regional_barchart_section_ui("sw_stability"),
                    details(
                      inputId = "caseload_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
                          tags$li("Average caseload at 30 September per year is calculated as the total number of cases held by FTE social workers, including agency workers, in post divided by the number of FTE social workers, including agency workers, in post that held one or more cases."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Social worker stability by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(sw_stability_data$time_period))),
                    la_and_sn_toggle_section_ui("sw_stability")
                  )
                ),
                open = FALSE
              ),
            ),

            # Social worker ethnicity -----
            tabPanel(
              "Social worker ethnicity",
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Social workers from ethnic minority backgrounds",
                    value = htmlOutput("non_white_txt")
                  )
                )
              ),
              accordion(
                accordion_panel(
                  "Social worker ethnicity",
                  gov_row(
                    h2("Social worker ethnicity"),
                    p("A diverse workforce, across all levels, should enable practice which reflects the cultural, linguistic, and religious needs of the communities practitioners serve."),
                    insert_text(inputId = "Ethnicity_definition", text = paste(
                      "<b>", "Ethnicity (headcount)", "</b><br>",
                      "Headcount percentage by ethnicity group, for children and family social workers in post at 30 September with known ethnicity."
                    )),
                    plotlyOutput("plot_ethnicity_rate"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_ethnicity",
                      label = "View Chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_ethnicity_rate", filename = "social_worker_ethnicity.csv"),
                          reactableOutput("table_ethnicity_rate")
                        ))
                      )
                    ),
                    details(
                      inputId = "ethnicity_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li(tags$b("Ethnicity groups"), " are based on ethnic origin and are provided on a headcount basis."),
                          tags$li(tags$b("Ethnicity"), sprintf(" was known for 81%% of child and family social workers nationally in %s.", max(workforce_eth$time_period)), "Headcount percentage by ethnicity group calculated using the headcount of social workers with known ethnicity as the denominator."),
                          tags$li(tags$b("Headcount"), "is a count of all individual children and family social workers, regardless of their working pattern."),
                          tags$li(tags$b("Ethnic minority backgrounds"), " exclude white British, white Irish, or any other white background."),
                          tags$li(tags$b("White"), " comprises white British, white Irish, or any other white background."),
                          tags$li(tags$b("Black or Black British"), " comprises black Caribbean, black African or any other black background."),
                          tags$li(tags$b("Asian or Asian British"), " comprises Indian, Pakistani, Bangladeshi, Chinese or any other Asian background."),
                          tags$li(tags$b("Mixed"), " comprises white and black Caribbean, white and black African, white and Asian, or any other mixed background."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  )
                ),
                accordion_panel(
                  "Social worker ethnicity vs. general population",
                  gov_row(
                    h2("Social worker ethnicity vs. general population"),
                    insert_text(inputId = "Ethnicity_vs_general_definition", text = paste(
                      "<b>", "Ethnicity of social workers (headcount) vs. ethnicity of general population", "</b><br>",
                      "Headcount percentage by ethnicity group, for children and family social workers in post at 30 September with known ethnicity, compared to known ethnicity breakdown of general population."
                    )),
                    br(),
                    plotlyOutput("plot_population_ethnicity_rate"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_population_ethnicity",
                      label = "View Chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_population_ethnicity_rate", filename = "social_worker_ethnicity_vs_population.csv"),
                          reactableOutput("table_population_ethnicity_rate")
                        ))
                      )
                    ),
                    details(
                      inputId = "population_ethnicity_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Population data is taken from the latest available", a(href = "https://www.ons.gov.uk/datasets/TS021/editions/2021/versions/3", "ONS Census data (2021).", target = "_blank")),
                          tags$li(sprintf("The Workforce data comparison uses the latest available collection year in the Workforce diversity dataset (%s).", max(workforce_eth$time_period))),
                          tags$li(tags$b("Ethnicity"), sprintf(" was known for 81%% of child and family social workers nationally in %s.", max(workforce_eth$time_period)), "Headcount percentage by ethnicity group calculated using the headcount of social workers with known ethnicity as the denominator."),
                          tags$li(tags$b("White"), " comprises white British, white Irish, or any other white background."),
                          tags$li(tags$b("Black or Black British"), " comprises black Caribbean, black African or any other black background."),
                          tags$li(tags$b("Asian or Asian British"), " comprises Indian, Pakistani, Bangladeshi, Chinese or any other Asian background."),
                          tags$li(tags$b("Mixed"), " comprises white and black Caribbean, white and black African, white and Asian, or any other mixed background."),
                          tags$li("General population ethnicity covers all ages of population living in the geographical area in question."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  )
                ),
                accordion_panel(
                  "Social worker ethnicity by seniority level",
                  gov_row(
                    h2("Social worker ethnicity by seniority level"),
                    insert_text(inputId = "Ethnicity_by_role_definition", text = paste(
                      "<b>", "Ethnicity (headcount)", "</b><br>",
                      "Headcount percentage by ethnicity group and social worker role, for children and family social workers in post at 30 September with known ethnicity."
                    )),
                    br(),
                    plotlyOutput("plot_seniority_eth"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_seniority_eth",
                      label = "View Chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_seniority_eth", filename = "social_worker_seniority.csv"),
                          reactableOutput("table_seniority_eth")
                        ))
                      )
                    ),
                    details(
                      inputId = "seniority_ethnicity_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li(sprintf("The data used is from the latest available collection year in the Workforce diversity dataset (%s).", max(workforce_eth$time_period))),
                          tags$li(tags$b("Ethnicity"), sprintf(" was known for 81%% of child and family social workers nationally in %s.", max(workforce_eth$time_period)), "Headcount percentage by ethnicity group are calculated using the headcount of social workers with known ethnicity as the denominator."),
                          tags$li("Seniority level relates to social worker role. Manager roles have been grouped and include first line managers, middle managers and senior managers."),
                          tags$li("A Senior Practitioner works in a local authority in a children’s services department as a team leader, supervising social worker or senior social worker."),
                          tags$li("A case holder is a children and family social worker that manages cases, but is not in a manager or senior practitioner role (however, cases can be hold by those not in case holder roles)."),
                          tags$li("Qualified without cases includes all other qualified and registered social workers, including those without cases (for example Independent Reviewing Officer (IRO), Chairs of Child Protection Conferences, Youth Custody worker, Family Support) and those not currently practicing (for example, those in learning and development or quality assurance roles)."),
                          tags$li(tags$b("White"), " comprises white British, white Irish, or any other white background."),
                          tags$li(tags$b("Black or Black British"), " comprises black Caribbean, black African or any other black background."),
                          tags$li(tags$b("Asian or Asian British"), " comprises Indian, Pakistani, Bangladeshi, Chinese or any other Asian background."),
                          tags$li(tags$b("Mixed"), " comprises white and black Caribbean, white and black African, white and Asian, or any other mixed background."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  )
                ),
                open = FALSE
              ),
            )
          )
        )
      )
    )
  )
}
