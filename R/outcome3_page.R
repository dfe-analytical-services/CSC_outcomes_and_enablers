outcome3_tab <- function() {
  tabPanel(
    value = "outcome3_page",
    "3 - Safety",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Outcome 3: Children and young people are safe in and outside of their home")
        )
      ),
      gov_row(
        # Input boxes
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_o3",
              label = "Select a geographical level:",
              choices = unique(cla_rates %>% pull("geographic_level")),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(condition = "input.select_geography_o3 != 'National'", selectizeInput(
              inputId = "geographic_breakdown_o3",
              label = "Select a location: ",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = NULL
            )),
            col_widths = c(4, 8)
          ),
          # checkboxes for comparisons
          layout_columns(
            conditionalPanel(
              condition = "input.select_geography_o3 != 'National'",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "national_comparison_checkbox_o3",
                  cb_labels = "Compare with National",
                  checkboxIds = "Yes_national_o3",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "(input.select_geography_o3 == 'Local authority')",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_o3",
                  cb_labels = "Compare with Region",
                  checkboxIds = "Yes_region_o3",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              ),
            ),
            col_widths = c(4, 8)
          )
        )
      ),
      br(),
      # confirmation sentence
      gov_row(
        br(),
        p(htmlOutput("outcome3_choice_text1"), htmlOutput("outcome3_choice_text2")),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o3 == 'Cumbria')",
          p("Cumbria are still in the latest statistics (except for hospital admissions) because they relate to the year ending 31 March 2023. Cumbria local authority was replaced with two new unitary authorities, Cumberland and Westmorland and Furness, in April 2023.")
        ),
      ),
      gov_row(
        br(),
        div(
          tabsetPanel(
            id = "outcome3_panels",
            type = "tabs",
            tabPanel(
              "Child safety – general",
              fluidRow(
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Percentage of Child Protection Plans (CPP) starting during year, which were a second or subsequent plan",
                    value = htmlOutput("cpp_in_year_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Percentage of Child Protection Plans (CPP) longer than 2 years",
                    value = htmlOutput("cpp_duration_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = HTML("Hospital admissions caused by unintentional and deliberate injuries to children and young people <br> (0 to 14 years)"),
                    value = htmlOutput("hosp_admissions_txt")
                  )
                ),
                br(),
                p("Child protection is focused on investigating and addressing
                  significant harm that children might experience. Remaining
                  on a CPP for a longer period of time or having multiple
                  repeat plans, suggests that children and families are not
                  receiving the help that they need to address their issues.")
              ),
              ### Repeat CPP ------------------------
              accordion(
                accordion_panel(
                  "Percentage of Child Protection Plans (CPP) starting during year, which were a second or subsequent plan",
                  h2("Percentage of Child Protection Plans (CPP) starting during year, which were a second or subsequent plan"),
                  insert_text(inputId = "CIN_CPP_definition", text = paste(
                    "<b>", "Child Protection Plan (CPP)", "</b><br>",
                    "A child becomes the subject of a child protection plan if they are assessed as being at risk of harm, at an initial child protection conference."
                  )),
                  plotlyOutput("repeat_cpp_time_series"),
                  br(),
                  # Expandable for the table alternative
                  details(
                    inputId = "tbl_repeat_cpp",
                    label = "View chart as a table",
                    help_text = (
                      HTML(paste0(
                        csvDownloadButton("table_repeat_cpp", filename = "Repeat_CPP_rates.csv"),
                        reactableOutput("table_repeat_cpp")
                      ))
                    )
                  ),
                  details(
                    inputId = "cpp_in_year_info",
                    label = "Additional information:",
                    help_text = (
                      tags$ul(
                        tags$li("The metric shown in the graph refers to the percentage of children who have been entered into a CPP during the year, where this plan was at least their second."),
                        tags$br(),
                        p(
                          "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/2023/data-guidance", "Children in need data guidance."),
                          tags$br(),
                          "For more information about child protection plans, please refer to", a(href = "https://assets.publishing.service.gov.uk/media/65cb4349a7ded0000c79e4e1/Working_together_to_safeguard_children_2023_-_statutory_guidance.pdf", "Working together to safeguard children - statutory guidance.")
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Repeat Child Protection Plan (CPP) by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_cpp_repeat_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_CPP_repeat_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cpp_repeat_reg", filename = "Repeat_CPP_rates_regions.csv"),
                          reactableOutput("table_cpp_repeat_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "cpp_in_year_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("The metric shown in the graph refers to the percentage of children who have been entered into a CPP during the year, where this plan was at least their second."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/2023/data-guidance", "Children in need data guidance."),
                            tags$br(),
                            "For more information about child protection plans, please refer to", a(href = "https://assets.publishing.service.gov.uk/media/65cb4349a7ded0000c79e4e1/Working_together_to_safeguard_children_2023_-_statutory_guidance.pdf", "Working together to safeguard children - statutory guidance.")
                          )
                        )
                      )
                    ),
                  ),
                  gov_row(
                    h2("Repeat Child Protection Plan (CPP)  by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(repeat_cpp$time_period))),
                    radioGroupButtons(
                      "CPP_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 Statistical Neighbours"),
                      selected = "All local authorities"
                    ),
                    uiOutput("SN_CPP"),
                  )
                ),
                ### Repeat CPP 2+ ---------------------------------------------------
                accordion_panel(
                  "Percentage of Child Protection Plans (CPP) longer than 2 years",
                  h2("Percentage of Child Protection Plans (CPP) longer than 2 years"),
                  insert_text(inputId = "CIN_CPP__longdefinition", text = paste(
                    "<b>", "Child Protection Plan (CPP)", "</b><br>",
                    "A child becomes the subject of a child protection plan if they are assessed as being at risk of harm, at an initial child protection conference."
                  )),
                  plotlyOutput("duration_cpp_time_series"),
                  br(),
                  details(
                    inputId = "tbl_duration_cpp",
                    label = "View chart as a table",
                    help_text = (
                      HTML(paste0(
                        csvDownloadButton("table_duration_cpp", filename = "CPP_more_than_2years_rates.csv"),
                        reactableOutput("table_duration_cpp")
                      ))
                    )
                  ),
                  details(
                    inputId = "cpp_longer_info",
                    label = "Additional information:",
                    help_text = (
                      tags$ul(
                        tags$li("The metric shown in the graph refers to the percentage of children who have been on a Child Protection Plan (CPP) for longer than 2 years."),
                        tags$li("Local authority data is not available for this metric as there are a large number of local authorities with suppressed data."),
                        tags$br(),
                        p(
                          "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/2023/data-guidance", "Children in need data guidance."),
                          tags$br(),
                          "For more information about child protection plans, please refer to", a(href = "https://assets.publishing.service.gov.uk/media/65cb4349a7ded0000c79e4e1/Working_together_to_safeguard_children_2023_-_statutory_guidance.pdf", "Working together to safeguard children - statutory guidance.")
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Child Protection Plans (CPP) longer than 2 years, by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_cpp_duration_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_CPP_duration_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cpp_duration_reg", filename = "CPP_more_than_2years_rates_region.csv"),
                          reactableOutput("table_cpp_duration_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "cpp_longer_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("The metric shown in the graph refers to the percentage of children who have been on a Child Protection Plan (CPP) for longer than 2 years."),
                          tags$li("Local authority data is not available for this metric as there are a large number of local authorities with suppressed data."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/2023/data-guidance", "Children in need data guidance."),
                            tags$br(),
                            "For more information about child protection plans, please refer to", a(href = "https://assets.publishing.service.gov.uk/media/65cb4349a7ded0000c79e4e1/Working_together_to_safeguard_children_2023_-_statutory_guidance.pdf", "Working together to safeguard children - statutory guidance.")
                          )
                        )
                      )
                    ),
                  ),
                ),
                # No local authority level data for cpp longer than 2 years

                ### Hospital admissions ------
                accordion_panel(
                  "Hospital admissions caused by unintentional and deliberate injuries to children and young people",
                  gov_row(
                    insert_text(
                      inputId = "admissions_warning",
                      text = paste("This indicator shows the data for ages 0 to 14 years for the year", max(hospital_admissions$time_period), ", and does not have historical data available for comparison.")
                    ),
                  ),
                  
                  gov_row(
                    h2("Hospital admissions caused by unintentional and deliberate injuries to children and young people (0 to 14 years), by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("admissions_region_plot"),
                    br(),
                    details(
                      inputId = "admissions_region_table",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("admissions_region_tbl", filename = "hospital_admissions_rates_region.csv"),
                          reactableOutput("admissions_region_tbl")
                        ))
                      )
                    ),
                    details(
                      inputId = "admissions_region_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("All sub national counts are rounded to the nearest 5. Rates are calculated using unrounded counts."),
                          tags$li("For time points from 2012, all sub national counts are rounded to the nearest 5, and counts of 1 to 7 are suppressed. Rates and confidence intervals are calculated using unrounded counts."),
                          tags$li("Values relating to City of London and Isles of Scilly have been combined with Hackney and Cornwall."),
                          tags$br(),
                          p(
                            "For more information on the data, please refer to the", a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/502/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0", "Public health data explorer."),
                            tags$br(),
                            "For more information on the definitions and methodology, please refer to the", a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/6/gid/1938133230/pat/159/par/K02000001/ati/15/are/E92000001/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1", "Indicator definitions and supporting information page.")
                          )
                        )
                      )
                    )
                  ),
                  
                  gov_row(
                    h2("Hospital admissions caused by unintentional and deliberate injuries to children and young people (0 to 14 years) by local authority"),
                    br(),
                    radioGroupButtons(
                      "hosp_admission_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities"
                    ),
                    uiOutput("SN_hosp_admissions")
                  )
                ),
                open = FALSE
              ),
            ),
            # Child abuse / neglect -----
            tabPanel(
              "Child abuse / neglect",
              fluidRow(
                br()
              ),
              gov_row(
                h2("Factors identified at the end of assessment in the year to 31 March 2023 that include child abuse or neglect."),
                warning_text(inputId = "assessment_factors_def1", text = paste(
                  "Factors identified at the end of assessment are additional factors that social workers record as being relevant in a case.
                    The majority of children have more than one factor recorded for each episode of need.
                    It should be noted that not all episodes have factors recorded, but this has improved over time.
                    Nonetheless, there can be differences in the recording practices between local authorities therefore this data should be treated with a degree of caution."
                )),
                plotlyOutput("child_abuse_all_af_plot"),
                br(),
                # Expandable for the table alternative
                details(
                  inputId = "tbl_all_child_abuse_factors",
                  label = "View chart as a table",
                  help_text = (
                    HTML(paste0(
                      csvDownloadButton("child_abuse_all_af_tbl", filename = "child_abuse_all_factors.csv"),
                      reactableOutput("child_abuse_all_af_tbl")
                    ))
                  )
                ),
                details(
                  inputId = "child_abuse_add_info",
                  label = "Additional information:",
                  help_text = (
                    tags$ul(
                      tags$li("Figures exclude the category ‘no factors identified’."),
                      tags$li("An episode of need may have more than one factor recorded."),
                      tags$li("Information on child on child and adult on child physical and sexual abuse was collected and reported on for the third time in 2023. Previously physical abuse and sexual abuse was collected and reported on (irrespective of whether it was child on child or adult on child) and some local authorities have provided information on the old basis only, or a mixture of the old and new basis, since 2021. The old physical and sexual abuse categories have therefore been included to provide a more complete account of this category of assessment."),
                      tags$li("The ‘Domestic violence’ factor was renamed as ‘Domestic abuse’ in the 2022 release. This is a change to the description of the factor and is not a change to the information collected for this factor."),
                      tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology for more information."),
                      tags$br(),
                      p(
                        "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                        tags$br(),
                        "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
                      )
                    )
                  )
                ),
              ),
              # extra dropdown for the assessment factors
              gov_row(
                div(
                  class = "input_box",
                  style = "min-height:100%; height = 100%; overflow-y: visible",
                  p("This domain contains breakdowns of data for the following assessment factors: ", paste(unique(af_child_abuse_extra_filter %>% str_sort()), collapse = ", "), "."),
                  p("Please use the dropdown below to select which assessment factor you would like to see in the below accordions:"),
                  selectizeInput(
                    inputId = "assessment_factors_1",
                    label = "Select an assessment factor:",
                    choices = unique(af_child_abuse_extra_filter %>% str_sort()),
                    selected = NULL,
                    multiple = FALSE,
                    options = NULL
                  ),
                ),
                br(),
              ),
              accordion(
                accordion_panel(
                  "Assessment factor includes child abuse or neglect",
                  gov_row(
                    uiOutput("ca_header1"),
                    plotlyOutput("child_abuse_ts_plot"),
                    br(),
                    details(
                      inputId = "table_child_abuse",
                      label = "View the chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("ca_ts_tbl", filename = "child_abuse_rates.csv"),
                          reactableOutput("ca_ts_tbl")
                        ))
                      )
                    ),
                    details(
                      inputId = "ca_ts_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Figures exclude the category ‘no factors identified’."),
                          tags$li("An episode of need may have more than one factor recorded."),
                          tags$li("Information on child on child and adult on child physical and sexual abuse was collected and reported on for the third time in 2023. Previously physical abuse and sexual abuse was collected and reported on (irrespective of whether it was child on child or adult on child) and some local authorities have provided information on the old basis only, or a mixture of the old and new basis, since 2021. The old physical and sexual abuse categories have therefore been included to provide a more complete account of this category of assessment."),
                          tags$li("The ‘Domestic violence’ factor was renamed as ‘Domestic abuse’ in the 2022 release. This is a change to the description of the factor and is not a change to the information collected for this factor."),
                          tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology for more information."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    uiOutput("ca_header2"),
                    plotlyOutput("child_abuse_region_plot"),
                    details(
                      inputId = "ca_region_tbl",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("child_abuse_region_tbl", filename = "child_abuse_rates_regions.csv"),
                          reactableOutput("child_abuse_region_tbl")
                        ))
                      )
                    ),
                    details(
                      inputId = "ca_region_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Figures exclude the category ‘no factors identified’."),
                          tags$li("An episode of need may have more than one factor recorded."),
                          tags$li("Information on child on child and adult on child physical and sexual abuse was collected and reported on for the third time in 2023. Previously physical abuse and sexual abuse was collected and reported on (irrespective of whether it was child on child or adult on child) and some local authorities have provided information on the old basis only, or a mixture of the old and new basis, since 2021. The old physical and sexual abuse categories have therefore been included to provide a more complete account of this category of assessment."),
                          tags$li("The ‘Domestic violence’ factor was renamed as ‘Domestic abuse’ in the 2022 release. This is a change to the description of the factor and is not a change to the information collected for this factor."),
                          tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology for more information."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    uiOutput("ca_header3"),
                    radioGroupButtons(
                      "child_abuse_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities"
                    ),
                    uiOutput("SN_child_ab_neg")
                  )
                ),
                open = FALSE
              )
            ),

            # Harms outside the home ----
            tabPanel(
              "Harms outside the home",
              fluidRow(
                br(),
              ),
              gov_row(
                h2("Factors identified at the end of assessment in the year to 31 March 2023 that include extra familial harm."),
                warning_text(
                  inputId = "assessment_factors_def2", text = paste(
                    "Factors identified at the end of assessment are additional factors that social workers record as being relevant in a case.
                  The majority of children have more than one factor recorded for each episode of need.
                  It should be noted that not all episodes have factors recorded, but this has improved over time.
                  Nonetheless, there can be differences in the recording practices between local authorities therefore this data should be treated with a degree of caution."
                  )
                ),
                plotlyOutput("extra_familial_all_af_plot"),
                br(),
                details(
                  inputId = "tbl_all_extra_fam_factors",
                  label = "View chart as a table",
                  help_text = (
                    HTML(paste0(
                      csvDownloadButton("extra_familial_all_af_tbl", filename = "EFH_all_factors.csv"),
                      reactableOutput("extra_familial_all_af_tbl")
                    ))
                  )
                ),
                details(
                  inputId = "efh_all_info",
                  label = "Additional information:",
                  help_text = (
                    tags$ul(
                      tags$li("Figures exclude the category ‘no factors identified’."),
                      tags$li("An episode of need may have more than one factor recorded."),
                      tags$li("Information on child on child and adult on child physical and sexual abuse was collected and reported on for the third time in 2023. Previously physical abuse and sexual abuse was collected and reported on (irrespective of whether it was child on child or adult on child) and some local authorities have provided information on the old basis only, or a mixture of the old and new basis, since 2021. The old physical and sexual abuse categories have therefore been included to provide a more complete account of this category of assessment."),
                      tags$li("The ‘Domestic violence’ factor was renamed as ‘Domestic abuse’ in the 2022 release. This is a change to the description of the factor and is not a change to the information collected for this factor."),
                      tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology for more information."),
                      tags$br(),
                      p(
                        "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                        tags$br(),
                        "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
                      )
                    )
                  )
                )
              ),
              gov_row(
                div(
                  class = "input_box",
                  style = "min-height:100%; height = 100%; overflow-y: visible",
                  p("This domain contains breakdowns of data for the following assessment factors: ", paste(unique(extra_familial_harm_af %>% str_sort()), collapse = ", "), "."),
                  p("Please use the dropdown below to select which assessment factor you would like to see in the below accordions:"),
                  selectizeInput(
                    inputId = "assessment_factors_2",
                    label = "Select an assessment factor:",
                    choices = unique(extra_familial_harm_af %>% str_sort()),
                    selected = NULL,
                    multiple = FALSE,
                    options = NULL
                  ),
                ),
                br(),
              ),
              accordion(
                accordion_panel(
                  "Assessment factor includes extra familial harm",
                  gov_row(
                    uiOutput("efh_header1"),
                    plotlyOutput("efh_ts_plot"),
                    br(),
                    details(
                      inputId = "table_child_abuse",
                      label = "View the chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("efh_ts_tbl", filename = "EFH_rates.csv"),
                          reactableOutput("efh_ts_tbl")
                        ))
                      )
                    ),
                    details(
                      inputId = "efh_ts_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Figures exclude the category ‘no factors identified’."),
                          tags$li("An episode of need may have more than one factor recorded."),
                          tags$li("Information on child on child and adult on child physical and sexual abuse was collected and reported on for the third time in 2023. Previously physical abuse and sexual abuse was collected and reported on (irrespective of whether it was child on child or adult on child) and some local authorities have provided information on the old basis only, or a mixture of the old and new basis, since 2021. The old physical and sexual abuse categories have therefore been included to provide a more complete account of this category of assessment."),
                          tags$li("The ‘Domestic violence’ factor was renamed as ‘Domestic abuse’ in the 2022 release. This is a change to the description of the factor and is not a change to the information collected for this factor."),
                          tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology for more information."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    uiOutput("efh_header2"),
                    plotlyOutput("efh_region_plot"),
                    br(),
                    details(
                      inputId = "efh_region_table",
                      label = "View chart as a table",
                      help_text = (
                        # For some reason this table needs to be wrapped in a taglist, unlike the others
                        # I have no idea why but if we use the same code as the others, it causes all of the tables to not render.
                        tagList(
                          csvDownloadButton("efh_region_tbl", filename = "EFH_rates_regions.csv"),
                          reactableOutput("efh_region_tbl")
                        )
                      )
                    ),
                    details(
                      inputId = "efh_region_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Figures exclude the category ‘no factors identified’."),
                          tags$li("An episode of need may have more than one factor recorded."),
                          tags$li("Information on child on child and adult on child physical and sexual abuse was collected and reported on for the third time in 2023. Previously physical abuse and sexual abuse was collected and reported on (irrespective of whether it was child on child or adult on child) and some local authorities have provided information on the old basis only, or a mixture of the old and new basis, since 2021. The old physical and sexual abuse categories have therefore been included to provide a more complete account of this category of assessment."),
                          tags$li("The ‘Domestic violence’ factor was renamed as ‘Domestic abuse’ in the 2022 release. This is a change to the description of the factor and is not a change to the information collected for this factor."),
                          tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology for more information."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology.")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    # by la and stats neighbours
                    uiOutput("efh_header3"),
                    radioGroupButtons(
                      "extra_familial_harm_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities"
                    ),
                    uiOutput("SN_extra_familial_harm")
                  )
                ),
                open = FALSE
              )
            )
          )
        )
      )
    )
  )
}
