outcome1_tab <- function() {
  tabPanel(
    value = "outcome1_page",
    "Outcome 1",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Outcome 1: Children, young people and families stay together and get the help they need")
        )
      ),
      gov_row(
        # Input boxes
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_o1",
              label = "Select a geographical level:",
              choices = unique(dropdown_choices %>% pull("geographic_level")),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(condition = "input.select_geography_o1 != 'National'", selectizeInput(
              inputId = "geographic_breakdown_o1",
              label = "Select a location: ",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = NULL
            )),
            col_widths = c(4, 8)
          ),
          layout_columns(
            conditionalPanel(
              condition = "input.select_geography_o1 != 'National'",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "national_comparison_checkbox_o1",
                  cb_labels = "Compare with National",
                  checkboxIds = "Yes_national",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "(input.select_geography_o1 == 'Local authority')",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_o1",
                  cb_labels = "Compare with Region",
                  checkboxIds = "Yes_region",
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
      gov_row(
        br(),
        p(htmlOutput("outcome1_choice_text1"), htmlOutput("outcome1_choice_text2")),
        # conditionalPanel(
        #   condition = "(input.geographic_breakdown_o1 == 'Northamptonshire')",
        #   p("To view 2021 and onwards data select ", strong("North Northamptonshire"), "or", strong("West Northamptonshire"), ". Northamptonshire local authority was replaced with two new unitary authorities, North Northamptonshire and West Northamptonshire, in April 2021.")
        # ),
        # conditionalPanel(
        #   condition = "(input.geographic_breakdown_o1 == 'Poole')",
        #   p("To view 2020 and onwards data select ", strong("Bournemouth, Christchurch and Poole"), ". Bournemouth, Christchurch and Poole local authority was formed in April 2019.")
        # ),
        # conditionalPanel(
        #   condition = "(input.geographic_breakdown_o1 == 'Bournemouth')",
        #   p("To view 2020 and onwards data select ", strong("Bournemouth, Christchurch and Poole"), ". Bournemouth, Christchurch and Poole local authority was formed in April 2019.")
        # ),
      ),
      gov_row(
        br(),
        div(
          tabsetPanel(
            id = "outcome1_panels",
            type = "tabs",
            tabPanel(
              "Family Stability",
              fluidRow(
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Rate of children starting in care, per 10,000 children",
                    value = htmlOutput("cla_rate_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Rate of children starting in care who were UASC, per 10,000 children",
                    value = htmlOutput("uasc_rate_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Rate of children in care on 31 March, per 10,000 children",
                    value = htmlOutput("cla_march_rate_headline_txt")
                  )
                ),
                br(),
              ),
              accordion(
                accordion_panel(
                  "Rate of new entrants to care",
                  gov_row(
                    insert_text(inputId = "cla_rate_definition", text = paste(
                      "<b>", "Rate of children who started to be looked after", "</b><br>",
                      "The children in care rate is calculated as the number of children in care per 10,000 children in the general population."
                    )),
                    # p("plots go here"),
                    plotlyOutput("plot_cla_rate"),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_cla_rate",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cla_rate")
                      )
                    ),
                    details(
                      inputId = "cla_rate_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated using published number of children starting in care figures which have been rounded to the nearest 10 at national and regional level (unrounded for local authority figures)."),
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Figures exclude children looked after under a series of short-term placements. Only the first occasion on which a child started to be looked after in the year has been counted."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children in care data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children in care methodology.")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Rate of children starting in care during the year by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_cla_rate_reg"),
                    br(),
                    details(
                      inputId = "tbl_cla_rate_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cla_rate_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Rate of children starting in care by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(cla_rates$time_period))),
                    br(),
                    plotlyOutput("plot_cla_rate_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cla_rate_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cla_rate_la")
                      )
                    )
                  )
                ),
                accordion_panel(
                  "Rate of new entrants to care, with a breakdown by whether new entrants to care are Unaccompanied Asylum Seeking Children (UASC)",
                  gov_row(
                    h2("Rate of children starting in care who were UASC"),
                    br(),
                    plotlyOutput("plot_uasc"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_uasc",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_uasc")
                      )
                    ),
                  ),
                  gov_row(
                    h2("Rate of children starting in care by region who were UASC"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    p(sprintf("The graph represents data from %s.", max(combined_cla_data$time_period))),
                    br(),
                    plotlyOutput("plot_uasc_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_uasc_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_uasc_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Rate of children starting in care by LA who were UASC"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(combined_cla_data$time_period))),
                    br(),
                    plotlyOutput("plot_uasc_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_uasc_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_uasc_la")
                      )
                    )
                  )
                ),
                accordion_panel(
                  "Rate of children in care on 31 March",
                  gov_row(
                    h2("Rate of children in care on 31 March"),
                    br(),
                    plotlyOutput("plot_cla_rate_march"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cla_rate_march",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cla_rate_march")
                      )
                    ),
                    details(
                      inputId = "cla_rate_march_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children in care data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children in care methodology.")
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Rate of children in care on 31 March by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_cla_march_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cla_march_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cla_march_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Rate of children in care on 31 March by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(cla_rates$time_period))),
                    br(),
                    plotlyOutput("plot_cla_march_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cla_march_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cla_march_la")
                      )
                    )
                  )
                ),
                open = FALSE
              )
            ),
            tabPanel(
              "Access to support and getting help",
              fluidRow(
                br()
              ),
              fluidRow(
                br(),
                column(
                  width = 6,
                  value_box(
                    title = "Children In Need rate per 10,000 children",
                    value = htmlOutput("cin_rate_headline_txt")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Re-referrals within 12 months of a previous referral",
                    value = htmlOutput("cin_referral_headline_txt")
                  )
                ),
              ),
              accordion(
                accordion_panel(
                  "Rate of Child In Need (CIN)",
                  gov_row(
                    h2("Rate of Child In Need (CIN)"),
                    p("Helping children to stay together with their families means ensuring the right support is in place at earlier stages of intervention.
                    Looking at the flow of children who become a CIN will show children being supported by the wider system. Combined with family stability indicators, this will reflect a broad view of flow into and through the children’s social care system."),
                    # style ="font-family: GDS Transport, arial, sans-serif; font-size :19px; padding-left: 4px;"),

                    insert_text(inputId = "CIN_definition", text = paste(
                      "<b>", "Children In Need (CIN) rate", "</b><br>",
                      "Rate of Children In Need at 31 March, per 10,000 children in the population."
                    )),
                    # p("plots go here"),
                    plotlyOutput("plot_cin_rate"),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_cin_rate",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cin_rate")
                      )
                    ),
                    # expandable for the additional info links
                    details(
                      inputId = "CIN_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rate of children as at 31 March 2023 assessed as needing help and protection as a result of risks to their devlopment or health."),
                          tags$li("Rates per 10,000 children are calculated based on ONS", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2021", "mid-year population estimates."), "for children aged 0 to 17 years. The rates for 2022 and 2023 are based on 2021 population estimates which in turn are based on 2021 Census data."),
                          tags$li("The rates for 2023 have been calculated based on 2021 population estimates as 2022 estimates were not available at the time of publication. Therefore, some caution is needed when interpreting the 2023 rates, either in isolation or in comparison with other years. The 2023 rates will be revised as part of the next 2024 publication."),
                          tags$li("Revised population estimates for 2012 to 2020 based on 2021 Census data, to calculate revised 2013 to 2021 rates, were not available at the time of publication. Therefore, some caution is needed when interpreting these rates, either in isolation or in comparison with other years. The 2013 to 2021 rates will be revised as part of the next 2024 publication."),
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
                  gov_row(
                    h2("Children In Need rates by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_cin_rate_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cin_rates_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cin_rates_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("CIN rates by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(cin_rates$time_period))),
                    br(),
                    plotlyOutput("plot_cin_rates_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cin_rates_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cin_rates_la")
                      )
                    ),
                  )
                ),
                accordion_panel(
                  "Repeat referrals (within 12 months)",
                  gov_row(
                    h2("Repeat referrals (within 12 months)"),
                    p("If children are being referred to services repeatedly, this suggests that they and their families may not be receiving
                   the support necessary to allow them to thrive  independently as a family unit. Multiple referrals can be inefficient and
                   cause additional upset and trauma for the child and family, therefore reducing the rate of repeat referrals will result in better outcomes."),
                    # style ="font-family: GDS Transport, arial, sans-serif; font-size :19px; padding-left: 4px;"),

                    insert_text(inputId = "CIN_referrals_definition", text = paste(
                      "<b>", "Re-referrals within 12 months", "</b><br>",
                      "Percentage of re-referrals within 12 months of a previous referral in the year to 31 March."
                    )),
                    # p("plots go here"),
                    plotlyOutput("plot_cin_referral"),
                    br(),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_cin_referral",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cin_referral")
                      )
                    ),
                    details(
                      inputId = "CIN_referral_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("If a child has more than one referral in a reporting year, then each referral is counted."),
                          tags$li("Data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology section for more information."),
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
                  gov_row(
                    h2("Re-referrals by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_cin_referral_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cin_referral_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cin_referral_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Re-referrals by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(cin_referrals$time_period))),
                    br(),
                    plotlyOutput("plot_cin_referral_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cin_referral_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cin_referral_la")
                      )
                    )
                  )
                ),
                open = FALSE
              ),
            ),
            tabPanel(
              "Child wellbeing and development",
              br(),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Overall absence for CINO at 31 March",
                    value = htmlOutput("absence_CIN_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Overall absence for CPPO at 31 March",
                    value = htmlOutput("absence_CPP_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Overall absence for CLA 12 months on 31 March",
                    value = htmlOutput("absence_CLA_headline_txt")
                  )
                ),
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Persistent absentees for CINO at 31 March",
                    value = htmlOutput("persistent_CIN_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Persistent absentees for CPPO at 31 March",
                    value = htmlOutput("persistent_CPP_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Persistent absentees for CLA 12 months on 31 March",
                    value = htmlOutput("persistent_CLA_headline_txt")
                  )
                ),
                br(),
              ),
              gov_row(
                div(
                  class = "input_box",
                  style = "min-height:100%; height = 100%; overflow-y: visible",
                  p("This domain contains three breakdowns of data for the following social care groups: Child in Need (CINO), Child Protection Plan (CPPO) and Children Looked After (CLA)."),
                  p("Please use this dropdown to select which social care group you would like to see in the below accordians:"),
                  selectizeInput(
                    inputId = "wellbeing_extra_breakdown",
                    label = "Select a social care group:",
                    choices = c("CINO at 31 March", "CPPO at 31 March", "CLA 12 months at 31 March"),
                    selected = NULL,
                    multiple = FALSE,
                    options = NULL
                  )
                ),
                br(),
              ),
              accordion(
                accordion_panel(
                  "School attendance",
                  gov_row(
                    h2("School attendance of Children In Need and Children Looked After"),
                    p("Attending an education setting is a key component of long term development and wellbeing for
                      children and young people, which affects their outcomes. Barriers to attendance can be prevalent
                      amongst children in need, and children’s social care has an important role in helping to overcome
                      those barriers."),
                    insert_text(inputId = "Absence_definition", text = paste(
                      "<b>", "Absence rate", "</b><br>",
                      htmlOutput("outcome1_choice_social_care_group_text")
                    )),
                    plotlyOutput("absence_time_series"),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_absence",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_absence_rate")
                      )
                    ),
                    details(
                      inputId = "Attendance_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li(
                            "No absence data relating to the full 2019/20 academic year is available due to COVID-19.
                                  Due to the disruption during the 2020/21 and 2021/22 academic years, caution should be taken when comparing data to previous years. For more detailed information on this see ",
                            a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England."),
                          ),
                          tags$li("CINO refers to Children In Need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                          tags$li("CPPO refers to children on a Child Protection Plan, excluding children looked after."),
                          tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.")
                          )
                        )
                      )
                    ),
                  ),
                  gov_row(
                    h2("Absence rate by region"),
                    p("This chart will react to social care group selection but it will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_absence_reg"),
                    br(),
                    details(
                      inputId = "tbl_absence_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_absence_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Absence rate by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top, aswell as the social care group filter, and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(outcomes_absence$time_period))),
                    br(),
                    plotlyOutput("plot_absence_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_absence_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_absence_la")
                      )
                    )
                  ),
                ),
                accordion_panel(
                  "Persistent absence",
                  gov_row(
                    h2("Educational engagement: persistent absence"),
                    p("Engaging in education is a key component of long-term development and wellbeing for children and young people,
                      which affects their outcomes. Barriers to engagement can be prevalent amongst children in need,
                      and children’s social care has an important role in helping to overcome those barriers."),
                    insert_text(inputId = "Persistent_absence_definition", text = paste(
                      "<b>", "Persistent absentees", "</b><br>",
                      htmlOutput("outcome1_choice_social_care_group_text_1")
                    )),
                    plotlyOutput("persistence_time_series"),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_persistence",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_persistent_rate")
                      )
                    ),
                    details(
                      inputId = "Persistent_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("A pupil enrolment is identified as persistently absent if they have missed 10% or more of their possible sessions in the year to date."),
                          tags$li(
                            "No absence data relating to the full 2019/20 academic year is available due to COVID-19.
                                  Due to the disruption during the 2020/21 and 2021/22 academic years, caution should be taken when comparing data to previous years. For more detailed information on this see ",
                            a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England."),
                          ),
                          tags$li("CINO refers to Children In Need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                          tags$li("CPPO refers to children on a Child Protection Plan, excluding children looked after."),
                          tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.")
                          )
                        )
                      )
                    ),
                  ),
                  gov_row(
                    h2("Persistent absence by region"),
                    p("This chart will react to social care group selection but it will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_persistent_reg"),
                    br(),
                    details(
                      inputId = "tbl_persistence_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_persistent_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Persistent absence rate by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top, aswell as the social care group filter, and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(outcomes_absence$time_period))),
                    br(),
                    plotlyOutput("plot_persistent_absence_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_persistent_absence_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_persistent_absence_la")
                      )
                    )
                  ),
                ),
                open = FALSE
              )
            ),
            tabPanel(
              "Educational attainment",
              br(),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "CINO at 31 March percentage of pupils achieving expected standard in reading, writing and mathematics (combined)",
                    value = htmlOutput("KS2_CIN_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "CPPO at 31 March percentage of pupils achieving expected standard in reading, writing and mathematics (combined)",
                    value = htmlOutput("KS2_CPP_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "CLA 12 months on 31 March percentage of pupils achieving expected standard in reading, writing and mathematics (combined)",
                    value = htmlOutput("KS2_CLA_headline_txt")
                  )
                ),
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Average attainment 8 for CINO at 31 March",
                    value = htmlOutput("KS4_CIN_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Average attainment 8 for CPPO at 31 March",
                    value = htmlOutput("KS4_CPP_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Average attainment 8 for CLA 12 months on 31 March",
                    value = htmlOutput("KS4_CLA_headline_txt")
                  )
                ),
                br(),
              ),
              gov_row(
                div(
                  class = "input_box",
                  style = "min-height:100%; height = 100%; overflow-y: visible",
                  p("This domain contains three breakdowns of data for the following social care groups: Child in Need (CINO), Child Protection Plan (CPPO) and Children Looked After (CLA)."),
                  p("Please use this dropdown to select which social care group you would like to see in the below accordians:"),
                  selectizeInput(
                    inputId = "attainment_extra_breakdown",
                    label = "Select a social care group:",
                    choices = c("CINO at 31 March", "CPPO at 31 March", "CLA 12 months at 31 March"),
                    selected = NULL,
                    multiple = FALSE,
                    options = NULL
                  )
                ),
                br(),
              ),
              accordion(
                accordion_panel(
                  "Key stage 2",
                  gov_row(
                    h2("Key stage 2 attainment"),
                    p("Educational attainment is a key component of long-term development and wellbeing for children and young people, which affects their outcomes.
                      Children should be supported to access and make progress in education to support their development and life chances.
                      Virtual School Heads have a statutory duty to promote the educational attainment of all children in their care. This
                       includes ensuring suitable and timely educational provision and managing Pupil Premium Plus funding aligned to
                        objectives in the child’s Personal Education Plan."),
                    insert_text(inputId = "ks2_definition", text = paste(
                      "<b>", "Expected standard for year 6 pupils (mostly aged 11)", "</b><br>",
                      htmlOutput("outcome1_choice_social_care_group_text_2")
                    )),
                    plotlyOutput("plot_ks2_expected"),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_ks2",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_ks2_expected")
                      )
                    ),
                    details(
                      inputId = "ks2_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("No attainment data related to 2019/20 and 2020/21 academic year is available due to COVID-19."),
                          tags$li(
                            "Attainment in reading, writing and maths combined is not directly comparable to some earlier years (2016/17) because of changes to writing teacher assessment frameworks in 2018. For more detailed information on this see ",
                            a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment", "Key stage 2 attainment."),
                          ),
                          tags$li("CINO refers to Children In Need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                          tags$li("CPPO refers to children on a Child Protection Plan, excluding children looked after."),
                          tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.")
                          )
                        )
                      )
                    ),
                  ),
                  gov_row(
                    h2("KS2 attainment by region"),
                    p("This chart will react to social care group selection but it will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_ks2_reg"),
                    br(),
                    details(
                      inputId = "tbl_ks2_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_ks2_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("KS2 attainment by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top, aswell as the social care group filter, and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(outcomes_ks2$time_period))),
                    br(),
                    plotlyOutput("plot_KS2_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_KS2_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_KS2_la")
                      )
                    )
                  ),
                ),
                accordion_panel(
                  "Key stage 4",
                  gov_row(
                    h2("Key stage 4 attainment"),
                    p("Educational attainment is a key component of long-term development and wellbeing for children and young people, which affects their outcomes.
                      Children should be supported to access and make progress in education to support their development and life chances.
                      Virtual School Heads have a statutory duty to promote the educational attainment of all children in their care. This
                       includes ensuring suitable and timely educational provision and managing Pupil Premium Plus funding aligned to
                        objectives in the child’s Personal Education Plan."),
                    insert_text(inputId = "ks4_definition", text = paste(
                      "<b>", "Attainment 8 for pupils finishing GCSEs (mostly aged 16)", "</b><br>",
                      htmlOutput("outcome1_choice_social_care_group_text_3")
                    )),
                    plotlyOutput("plot_ks4"),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_ks4",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_ks4")
                      )
                    ),
                    details(
                      inputId = "ks4_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li(
                            "Due to the impact of the COVID-19 pandemic, the summer examination series was cancelled in both 2020 and 2021, and alternative processes set up to award grades.
                                 The method to award grades was different in 2021 to that in 2020. The changes to the way GCSE grades were awarded in these two years means 2019/20 and 2020/21 pupil attainment data should not be
                                 directly compared to pupil attainment data from previous or later years for the purposes of measuring year on year changes in pupil performance. For more detailed information on this see ",
                            a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance", "Key stage 4 performance."),
                          ),
                          tags$li("CINO refers to Children In Need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                          tags$li("CPPO refers to children on a Child Protection Plan, excluding children looked after."),
                          tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.")
                          )
                        )
                      )
                    ),
                  ),
                  gov_row(
                    h2("KS4 attainment by region"),
                    p("This chart will react to social care group selection but it will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_ks4_reg"),
                    br(),
                    details(
                      inputId = "tbl_ks4_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_ks4_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("KS4 attainment by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top, aswell as the social care group filter, and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(outcomes_ks4$time_period))),
                    br(),
                    plotlyOutput("plot_KS4_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_KS4_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_KS4_la")
                      )
                    )
                  ),
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
