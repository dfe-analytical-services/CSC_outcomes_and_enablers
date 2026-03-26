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
          p("Kingston upon Thames and Richmond upon Thames submit a joint workforce return each year, and their data is reported together for all indicators on this page except Social Worker Stability.
            To see Social Worker Stability data, please select either Kingston upon Thames or Richmond upon Thames from the dropdown.")
        ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_e3 == 'Kingston upon Thames' | input.geographic_breakdown_e3 == 'Richmond upon Thames')",
          p("Kingston upon Thames and Richmond upon Thames submit a joint workforce return each year, and their data is reported together for all indicators on this page except Social Worker Stability.
            To see the workforce indicators for the combined return, please select Kingston upon Thames / Richmond upon Thames from the dropdown.")
        ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_e3 == 'North Northamptonshire / West Northamptonshire')",
          p("North Northamptonshire and West Northamptonshire submit a joint workforce return each year, and their data is reported together for all indicators on this page except Social Worker Stability.
            To see Social Worker Stability data, please select either North Northamptonshire or West Northamptonshire from the dropdown.")
        ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_e3 == 'North Northamptonshire' | input.geographic_breakdown_e3 == 'West Northamptonshire')",
          p("North Northamptonshire and West Northamptonshire submit a joint workforce return each year, and their data is reported together for all indicators on this page except Social Worker Stability.
            To see the workforce indicators for the combined return, please select North Northamptonshire / West Northamptonshire from the dropdown.")
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
                        get_additional_info("workforce_rates_and_caseload")
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
                        get_additional_info("workforce_rates_and_caseload")
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
                        get_additional_info("workforce_rates_and_caseload")
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
                        get_additional_info("workforce_rates_and_caseload")
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
                        get_additional_info("workforce_rates_and_caseload")
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
                        get_additional_info("workforce_rates_and_caseload")
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
                    title = "Percentage of CLA with 3 or more social workers in past 12 months",
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
                        get_additional_info("workforce_rates_and_caseload")
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
                        get_additional_info("workforce_rates_and_caseload")
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
                    p("Ensuring children have minimal changes to their assigned social worker through the year so that they receive consistent support."),
                    insert_text(inputId = "sw_stability_definition", text = paste(
                      "High social worker instability is defined as a looked after child experiencing 3 or more social workers during the year ending 31 March."
                    )),
                    br(),
                    # here is the call to the module to display timeseries chart, table and download button
                    timeseries_section_ui("sw_stability")
                  ),
                  gov_row(
                    h2("Social worker stability by region"),
                    regional_barchart_section_ui("sw_stability")
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
                        get_additional_info("workforce_eth")
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
                        get_additional_info("workforce_eth")
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
                        get_additional_info("workforce_eth")
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
