outcome4_tab <- function() {
  tabPanel(
    value = "outcome4_page",
    "Outcome 4",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Outcome 4: Children in care and care leavers have stable, loving homes")
        )
      ),
      gov_row(
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_o4",
              label = "Select a geographical level:",
              # Change this to look at the relevant dataset for outcome 4
              choices = unique(placement_data %>% pull("geographic_level")),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(
              condition = "input.select_geography_o4 != 'National'",
              selectizeInput(
                inputId = "geographic_breakdown_o4",
                label = "Select a location: ",
                choices = NULL,
                selected = NULL,
                multiple = FALSE,
                options = NULL
              )
            ),
            col_widths = c(4, 8)
          ),
          layout_columns(
            conditionalPanel(
              condition = "input.select_geography_o4 != 'National'",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "national_comparison_checkbox_o4",
                  cb_labels = "Compare with National",
                  checkboxIds = "Yes_national_o4",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "(input.select_geography_o4 == 'Local authority')",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_o4",
                  cb_labels = "Compare with Region",
                  checkboxIds = "Yes_region_o4",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            col_widths = c(4, 8)
          )
        )
      ),
      br(),
      gov_row(
        br(),
        h2("Confirmation Sentence"),
        br(),
        div(
          tabsetPanel(
            id = "outcome4_panels",
            type = "tabs",
            # Domain 1 ----
            tabPanel(
              "Stability and quality of where a child lives",
              fluidRow(
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "% CLA on 31 March with 3 or more placements during the year",
                    value = htmlOutput("placement_changes_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "% CLA on 31 March placed more than 20 miles from home",
                    value = p("Headline stats 2")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Average time between an LA receiving court authority to place a child and the LA deciding on a match to an adoptive family? Q1-Q4 2022/23",
                    value = p("stats")
                  )
                ),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "% CLA on 31 March living in foster placements",
                    value = htmlOutput("foster_placement_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "% living in secure units, children's homes or semi-independent living accommodation",
                    value = htmlOutput("secure_unit_placement_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "% living in other residential settings",
                    value = htmlOutput("residential_placement_txt"),
                  )
                ),
                br(),
                p("Care leavers should be supported to access education, employment and
                  training that supports them and allows them to achieve their aspirations and goals.")
              ),
              accordion(
                accordion_panel(
                  "Percentage of CLA with 3 or more placements during the year",
                  gov_row(
                    plotlyOutput("placement_changes_ts_plot"),
                    br(),
                    details(
                      inputId = "tbl_placement_type",
                      label = "View chart as a table",
                      help_text = (
                        reactableOutput("placement_changes_tbl")
                      )
                    ),
                    details(
                      inputId = "placement_changes_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Numbers have been rounded to the nearest 10. Percentages rounded to the nearest whole number. Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to check for the equivalent table in earlier releases of this publication. Figures exclude children looked after under a series of short-term placements."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after in England data guidance."),
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Percentage of CLA with 3 or more placements during the year by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("placement_changes_region_plot"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_placement_type_reg",
                      label = "View chart as a table",
                      help_text = (
                        reactableOutput("placement_changes_region_tbl")
                      )
                    )
                  ),
                  gov_row(
                    h2("Percentage of CLA with 3 or more placements during the year by LA"),
                    p(sprintf("The charts below represent data from %s.", max(placement_changes_data$time_period))),
                    radioGroupButtons(
                      "placement_changes_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 Statistical Neighbours"),
                      selected = "All local authorities"
                    ),
                    uiOutput("SN_placement_changes"),
                  )
                ),
                accordion_panel(
                  "Distance of placements from home",
                  p("contents for panel 2"),
                  gov_row(
                    h2("Time Series")
                  ),
                  gov_row(
                    h2("By Region")
                  ),
                  gov_row(
                    h2("By local authority")
                  )
                ),
                accordion_panel(
                  "Percentage of children living in foster, residential care, or secure children’s homes",
                  gov_row(
                    # Box here to have an extra dropdown just for this section to split the percentage for each placement
                    p("This indicator contains breakdowns of data for the following assessment factors: ", paste(unique(placement_type_filter %>% str_sort()), collapse = ", ")),
                    div(
                      class = "input_box",
                      style = "min-height:100%; height = 100%; overflow-y: visible",
                      p("Please use the dropdown below to select which placement type you would like to see in this accordion:"),
                      selectizeInput(
                        inputId = "placement_type_breakdown",
                        label = "Select a placement type:",
                        choices = unique(placement_type_filter %>% str_sort()),
                        selected = NULL,
                        multiple = FALSE,
                        options = NULL
                      ),
                    ),
                    br(),
                  ),
                  gov_row(
                    plotlyOutput("placement_type_ts_plot"),
                    br(),
                    details(
                      inputId = "tbl_placement_type",
                      label = "View chart as a table",
                      help_text = (
                        reactableOutput("placement_type_tbl")
                      )
                    ),
                    details(
                      inputId = "placement_type_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Numbers have been rounded to the nearest 10. Percentages rounded to the nearest whole number. Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to check for the equivalent table in earlier releases of this publication. Figures exclude children looked after under a series of short-term placements."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after in England data guidance."),
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Percentage of children living in foster, residential care, or secure children’s homes by region"),
                    p("This chart will only react to the placement type filter, not the geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("placement_type_region_plot"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_placement_type_reg",
                      label = "View chart as a table",
                      help_text = (
                        reactableOutput("placement_type_region_tbl")
                      )
                    )
                  ),
                  gov_row(
                    h2("Percentage of children living in foster, residential care, or secure children’s homes by LA"),
                    p(sprintf("The charts below represent data from %s.", max(placement_data$time_period))),
                    radioGroupButtons(
                      "placement_type_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 Statistical Neighbours"),
                      selected = "All local authorities"
                    ),
                    uiOutput("SN_placement_type"),
                  )
                ),
                accordion_panel(
                  "Average time between placement order and match for those children who are adopted",
                  p("contents for panel 4"),
                  gov_row(
                    h2("Time Series")
                  ),
                  gov_row(
                    h2("By Region")
                  ),
                  gov_row(
                    h2("By local authority")
                  )
                ),
                open = FALSE
              )
            ),
            # Domain 2 -----
            tabPanel(
              "Wellbeing of child",
              fluidRow(
                # p("testing"),
                br()
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Average SDQ score",
                    value = htmlOutput("wellbeing_score_stat")
                  )
                ),
                # column(
                #   width = 6,
                #   value_box(
                #     title = "Headline stat 2",
                #     value = p("Headline stats 2")
                #   )
                # ),
                br(),
              ),
              fluidRow(
                br(),
                p("Understanding the emotional and behavioural need of CLA is important to ensure that they are receiving the care and support they need to thrive. The SDQ score uses a series of carer-reported measures to calculate an overall score to assess the emotional wellbeing of CLA."),
              ),
              accordion(
                accordion_panel(
                  "Strengths and difficulties questionnaire (SDQ score)",
                  # p("contents for panel 1"),
                  gov_row(
                    h2("Strengths and difficulties questionnaire (SDQ score)"),
                    insert_text(
                      inputId = "sdq_definition",
                      text = paste(
                        tags$b("Strengths and Difficulties Questionnaire (SDQ) scores"), " -  The SDQ is a behavioural screening questionnaire. Its primary purpose is to give social workers and health professionals information about a child’s wellbeing. A score of 0 to 13 is considered normal, 14 to 16 is borderline, and 17 to 40 is a cause for concern."
                      )
                    ),
                    plotlyOutput("sdq_time_series_plot"),
                    br(),
                    details(
                      inputId = "sdq_ts_tbl",
                      label = "View chart as table",
                      help_text = reactableOutput("sqd_ts_table")
                    ),
                    details(
                      inputId = "ts_additional_info",
                      label = "Additional information:",
                      help_text = p("additional info")
                    )
                  ),
                  gov_row(
                    h2("SDQ score by Region"),
                    br(),
                  ),
                  gov_row(
                    h2("SDQ score by local authority"),
                    br(),
                  )
                ),
                open = FALSE
              )
            ),
            # Domain 3 ------
            tabPanel(
              "Quality of life for care experienced people",
              fluidRow(
                #  p("testing"),
                br()
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Care leavers in education, employment or training (17 to 18 years)",
                    value = htmlOutput("care_leavers_employment_txt1")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Care leavers in education, employment or training (19 to 21 years)",
                    value = htmlOutput("care_leavers_employment_txt2")
                  )
                ),
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Care leavers in accommodation considered suitable (17 to 18 years)",
                    value = htmlOutput("care_leavers_accommodation_txt1")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Care leavers in accommodation considered suitable (19 to 21 years)",
                    value = htmlOutput("care_leavers_accommodation_txt2")
                  )
                ),
                br(),
                p("Care leavers should be supported to access education, employment and
                  training that supports them and allows them to achieve their aspirations and goals."),
                insert_text(
                  inputId = "care_leavers_def",
                  text = paste(
                    tags$b("Data collected on care leavers"), tags$br(), "Local authorities provide information about children who were previously looked after, who turned 17 to 21 in the year.
                              These were CLA for at least 13 weeks after their 14th birthday, including some time after their 16th birthday.
                              The information provided relates to contact around their birthday in the year."
                  )
                ),
              ),
              fluidRow(
                div(
                  class = "input_box",
                  style = "min-height:100%; height = 100%; overflow-y: visible",
                  p("This domain contains breakdowns of data for the following age ranges: 17 to 18 years and 19 to 21 years."),
                  p("Please use the dropdown below to select which age range of care leavers you would like to see in the accordions below:"),
                  selectizeInput(
                    inputId = "leavers_age",
                    label = "Select an age range:",
                    choices = unique(care_leavers_accommodation_data$age %>% str_sort()),
                    selected = NULL,
                    multiple = FALSE,
                    options = NULL
                  ),
                ),
                br()
              ),
              fluidRow(
                br()
              ),
              accordion(
                accordion_panel(
                  "Care leavers employment, education and training rate",
                  gov_row(
                    uiOutput("care_leavers_header1"),
                    plotlyOutput("care_activity_ts_plot"),
                    br(),
                    details(
                      inputId = "cl_activity_tbl",
                      label = "View chart as table",
                      help_text = (
                        reactableOutput("cl_activity_ts_tbl")
                      )
                    ),
                    details(
                      inputId = "cl_activity_info",
                      label = "Additional information:",
                      help_text = (
                        p(
                          tags$li("Numbers have been rounded to the nearest 10. Percentages rounded to the nearest whole number. Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to check for the equivalent table in earlier releases of this publication. Figures exclude young people who were looked after under an agreed series of short term placements, those who have died since leaving care, those who have returned home to parents or someone with parental responsibility for a continuous period of at least 6 months and those whose care was transferred to another local authority. Figures for the number of care leavers who have died each year can be found in the methodology document."),
                          tags$li("'Local authority not in touch' excludes young people where activity information is known, as a third party provided it even though the local authority is not directly in touch with the young person."),
                          tags$li("In touch, activity and accommodation information for 17-21 year old care leavers relates to contact around their birthday."),
                          tags$li("Figures for 2023 exclude Barnsley who were unable to provide data in time for publication."),
                          tags$br(),
                          "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance."),
                          tags$br(),
                          "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
                      ))
                    )
                  ),
                  gov_row(
                    uiOutput("care_leavers_header2"),
                    plotlyOutput("cl_activity_region_plot"),
                    br(),
                    details(
                      inputId = "cl_act_region_tbl",
                      label = "View chart as table",
                      help_text = (
                        reactableOutput("cl_activity_region_tbl")
                      )
                    ),
                    details(
                      inputId = "cl_act_region_info",
                      label = "Additional information:",
                      help_text = (
                        p(
                          tags$li("Numbers have been rounded to the nearest 10. Percentages rounded to the nearest whole number. Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to check for the equivalent table in earlier releases of this publication. Figures exclude young people who were looked after under an agreed series of short term placements, those who have died since leaving care, those who have returned home to parents or someone with parental responsibility for a continuous period of at least 6 months and those whose care was transferred to another local authority. Figures for the number of care leavers who have died each year can be found in the methodology document."),
                          tags$li("'Local authority not in touch' excludes young people where activity information is known, as a third party provided it even though the local authority is not directly in touch with the young person."),
                          tags$li("In touch, activity and accommodation information for 17-21 year old care leavers relates to contact around their birthday."),
                          tags$li("Figures for 2023 exclude Barnsley who were unable to provide data in time for publication."),
                          tags$br(),
                          "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance."),
                          tags$br(),
                          "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
                      ))
                    )
                  ),
                  gov_row(
                    uiOutput("care_leavers_header3"),
                    radioGroupButtons(
                      "cl_activity_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 Statistical Neighbours"),
                      selected = "All local authorities"
                    ),
                    uiOutput("SN_care_leavers_activity")
                  )
                ),
                # Accommodation panel
                accordion_panel(
                  "Percentage of care leavers in suitable accommodation",
                  # p("contents for panel 2"),
                  gov_row(
                    # h2("Time Series"),
                    uiOutput("care_leavers_header4"),
                    plotlyOutput("care_accommodation_ts_plot"),
                    br(),
                    details(
                      inputId = "cl_accommodation_tbl",
                      label = "View chart as table",
                      help_text = (
                        reactableOutput("cl_accommodation_ts_tbl")
                      )
                    ),
                    details(
                      inputId = "cl_accommodation_info",
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
                          "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance."),
                          tags$br(),
                          "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
                      ))
                    )
                  ),
                  gov_row(
                    # h2("By Region"),
                    uiOutput("care_leavers_header5"),
                    plotlyOutput("cl_accommodation_region_plot"),
                    br(),
                    details(
                      inputId = "cl_accommodation_reg_tbl",
                      label = "View chart as table",
                      help_text = (
                        reactableOutput("cl_accommodation_region_tbl")
                      )
                    ),
                    details(
                      inputId = "cla_accomm_reg_info",
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
                          "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance."),
                          tags$br(),
                          "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.")
                      ))
                    )
                  ),
                  gov_row(
                    uiOutput("care_leavers_header6"),
                    radioGroupButtons(
                      "cl_accommodation_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 Statistical Neighbours"),
                      selected = "All local authorities"
                    ),
                    uiOutput("SN_care_leavers_accommodation")
                  )
                ),
                open = FALSE
              )
            ),
          )
        )
      )
    )
  )
}
