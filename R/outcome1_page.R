outcome1_tab <- function() {
  tabPanel(
    value = "outcome1_page",
    "Family support",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Outcome 1: Children, young people and families stay together and get the help they need")
        )
      ),
      gov_row(
        # Input boxes for geographic level and geographic breakdown
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_o1",
              label = "Select a geographical level:",
              choices = unique(cla_rates %>% pull("geographic_level")),
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
            col_widths = c(5, 7)
          ),
          # checkboxes for comparisons
          layout_columns(
            conditionalPanel(
              condition = "input.select_geography_o1 != 'National'",
              column(
                width = 5,
                checkbox_Input(
                  inputId = "national_comparison_checkbox_o1",
                  cb_labels = "Compare with national",
                  checkboxIds = "Yes_national_o1",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "(input.select_geography_o1 == 'Local authority')",
              column(
                width = 7,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_o1",
                  cb_labels = "Compare with region",
                  checkboxIds = "Yes_region_o1",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              ),
            ),
            col_widths = c(5, 7)
          )
        )
      ),
      br(),
      gov_row(
        br(),
        # Confirmation of user selection
        p(htmlOutput("outcome1_choice_text1"), htmlOutput("outcome1_choice_text2")),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o1 == 'Cumbria')",
          p("Cumbria are still in the latest statistics because they relate to the year ending 31 March 2023. Cumbria local authority was replaced with two new unitary authorities, Cumberland and Westmorland and Furness, in April 2023.")
        ),
      ),
      gov_row(
        br(),
        div(
          tabsetPanel(
            id = "outcome1_panels",
            type = "tabs",
            # Domain 1 --------------
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
                    title = "Children in need rate per 10,000 children",
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
                ### CIN ------------------
                accordion_panel(
                  "Rate of children in need (CIN)",
                  gov_row(
                    h2("Rate of children in need (CIN)"),
                    p("Helping children to stay together with their families means ensuring the right support is in place at earlier stages of intervention.
                    Looking at the flow of children who become CIN will show children being supported by the wider system. Combined with family stability indicators, this will reflect a broad view of flow into and through the children’s social care system."),
                    insert_text(inputId = "CIN_definition", text = paste(
                      "<b>", "Rate of children in need (CIN)", "</b><br>",
                      "The rate of children in need is calculated as the number of children in need at 31 March, per 10,000 children in the population."
                    )),
                    plotlyOutput("plot_cin_rate"),
                    br(),
                    details(
                      inputId = "tbl_cin_rate",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cin_rate", filename = "CIN_rates.csv"),
                          reactableOutput("table_cin_rate")
                        ))
                      )
                    ),
                    details(
                      inputId = "CIN_info",
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
                  ),
                  # cin by region
                  gov_row(
                    h2("CIN rates by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_cin_rate_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cin_rates_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cin_rates_reg", filename = "CIN_rates_regions.csv"),
                          reactableOutput("table_cin_rates_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "CIN_reg_info",
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
                  ),
                  # cin by local authority
                  gov_row(
                    h2("CIN rates by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(cin_rates$time_period))),
                    radioGroupButtons(
                      "cin_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_cin"),
                  )
                ),
                ### Repeat referrals --------------
                accordion_panel(
                  "Repeat referrals (within 12 months)",
                  gov_row(
                    h2("Repeat referrals (within 12 months)"),
                    p("If children are being referred to services repeatedly, this suggests that they and their families may not be receiving
                   the support necessary to allow them to thrive  independently as a family unit. Multiple referrals can be inefficient and
                   cause additional upset and trauma for the child and family, therefore reducing the rate of repeat referrals will result in better outcomes."),
                    insert_text(inputId = "CIN_referrals_definition", text = paste(
                      "<b>", "Re-referrals within 12 months", "</b><br>",
                      "Percentage of referrals within 12 months of a previous referral in the year to 31 March."
                    )),
                    plotlyOutput("plot_cin_referral"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cin_referral",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cin_referral", filename = "CIN_re_referral_rates.csv"),
                          reactableOutput("table_cin_referral")
                        ))
                      )
                    ),
                    details(
                      inputId = "CIN_referral_info",
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
                  ),
                  # re-referrals by region
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
                        HTML(paste0(
                          csvDownloadButton("table_cin_referral_reg", filename = "CIN_re_referral_rates_regions.csv"),
                          reactableOutput("table_cin_referral_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "CIN_referral_reg_info",
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
                  ),
                  # re-referrals by local authority
                  gov_row(
                    h2("Re-referrals by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(cin_referrals$time_period))),
                    radioGroupButtons(
                      "cin_referral_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_cin_referral"),
                  )
                ),
                open = FALSE
              ),
            ),
            # Domain 2 --------------
            tabPanel(
              "Family stability",
              fluidRow(
                br(),
              ),
              # Headline stats boxes
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Rate of children starting to be looked after, per 10,000 children",
                    value = htmlOutput("cla_rate_headline_txt")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Rate of children looked after on 31 March, per 10,000 children",
                    value = htmlOutput("cla_march_rate_headline_txt")
                  )
                ),
                br(),
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Rate of children starting to be looked after who were UASC, per 10,000 children",
                    value = htmlOutput("uasc_rate_headline_txt")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Rate of children looked after on 31 March who were UASC, per 10,000 children",
                    value = htmlOutput("uasc_31_march_rate_headline_txt")
                  )
                ),
                br(),
              ),
              # Accordion for indicators
              accordion(
                ## CLA rate ------------
                accordion_panel(
                  "Children starting to be looked after each year",
                  gov_row(
                    h2("Children starting to be looked after each year"),
                    p("This measures the flow of those children moving into care."),
                    insert_text(inputId = "cla_rate_definition", text = paste(
                      "<b>", "Rate of children who started to be looked after", "</b><br>",
                      "The children starting to be looked after rate is calculated as the number of children starting to be looked after each year, per 10,000 children in the population."
                    )),
                    plotlyOutput("plot_cla_rate"),
                    br(),
                    details(
                      inputId = "tbl_cla_rate",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cla_rate", filename = "cla_rates.csv"),
                          reactableOutput("table_cla_rate")
                        ))
                      )
                    ),
                    details(
                      inputId = "cla_rate_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Only the first occasion on which a child started to be looked after in the local authority during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
                          tags$li("Figures exclude children looked after under a series of short-term placements."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  # CLA rate region
                  gov_row(
                    h2("Rate of children starting to be looked after by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_cla_rate_reg"),
                    br(),
                    details(
                      inputId = "tbl_cla_rate_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cla_rate_reg", filename = "cla_rates_regions.csv"),
                          reactableOutput("table_cla_rate_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "cla_rate_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Only the first occasion on which a child started to be looked after in the local authority during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
                          tags$li("Figures exclude children looked after under a series of short-term placements."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  ## cla rate by local authority
                  gov_row(
                    h2("Rate of children starting to be looked after by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(cla_rates$time_period))),
                    radioGroupButtons(
                      "cla_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_cla"),
                  )
                ),
                ## CLA on 31 March ---------
                accordion_panel(
                  "Rate of children looked after on 31 March",
                  gov_row(
                    h2("Rate of children looked after on 31 March"),
                    p("This metric measures the rate of children in care as a proportion of the 0-17 population. Avoiding permanent placements in care is a good indicator of supporting families to remain together"),
                    insert_text(inputId = "cla_31_March_rate_definition", text = paste(
                      "<b>", "Rate of children looked after on 31 March", "</b><br>",
                      "The children looked after rate is calculated as the number of children looked after on 31 March, per 10,000 children in the population."
                    )),
                    br(),
                    plotlyOutput("plot_cla_rate_march"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cla_rate_march",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cla_rate_march", filename = "cla_march_rates.csv"),
                          reactableOutput("table_cla_rate_march")
                        ))
                      )
                    ),
                    details(
                      inputId = "cla_rate_march_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  # cla on 31 march by region
                  gov_row(
                    h2("Rate of children looked after on 31 March by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_cla_march_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_cla_march_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cla_march_reg", filename = "cla_march_rates_regions.csv"),
                          reactableOutput("table_cla_march_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "cla_rate_march_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  # cla on 31 march by local authority
                  gov_row(
                    h2("Rate of children looked after on 31 March by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(cla_rates$time_period))),
                    br(),
                    radioGroupButtons(
                      "cla_march_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_cla_march"),
                  )
                ),
                ## CLA UASC -------------
                accordion_panel(
                  "Children starting to be looked after each year, with a breakdown by whether they are Unaccompanied Asylum Seeking Children (UASC)",
                  gov_row(
                    h2("Rate of children starting to be looked after who were Unaccompanied Asylum Seeking Children"),
                    p("This measures the flow of those children moving into care who are UASC. UASC are children, who have applied for asylum in their own right and are separated from both parents and/or any other responsible adult. Local authorities have a legal duty to provide accommodation for these children. This breakdown is provided for context."),
                    insert_text(inputId = "cla_rate__starting_definition", text = paste(
                      "<b>", "Rate of children who started to be looked after", "</b><br>",
                      "The children starting to be looked after rate is calculated as the number of children starting to be looked after each year, per 10,000 children in the population."
                    )),
                    br(),
                    plotlyOutput("plot_uasc"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_uasc",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_uasc", filename = "cla_UASC_rates.csv"),
                          reactableOutput("table_uasc")
                        ))
                      )
                    ),
                    details(
                      inputId = "cla_UASC_rate_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated using published number of children starting to be looked after each year, who are UASC and non-UASC, which have been rounded to the nearest 10 at national and regional level (unrounded for local authority figures)."),
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Only the first occasion on which a child started to be looked after in the local authority during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
                          tags$li("Following the introduction of the National Transfer Scheme (NTS) in 2016, there has been an agreement between local authorities to transfer UASC to ensure a more equitable distribution of UASC across all local authorities. This means that some UASC will be counted more than once in the national and regional CLA starting figures if they started to be looked after within more than 1 local
                                  authority during the year. In 2019 we estimate that nationally, the number of UASC starts was overestimated by 9%, this increased to 15% in 2023 following the mandation of the NTS in February 2022."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  ## CLA UASC region
                  gov_row(
                    h2("Rate of children starting to be looked after by region who were Unaccompanied Asylum Seeking Children"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    p(sprintf("The chart represents data from %s.", max(combined_cla_data$time_period))),
                    br(),
                    plotlyOutput("plot_uasc_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_uasc_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_uasc_reg", filename = "cla_UASC_rates_regions.csv"),
                          reactableOutput("table_uasc_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "cla_UASC_rate_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated using published number of children starting to be looked after each year, who are UASC and non-UASC, which have been rounded to the nearest 10 at national and regional level (unrounded for local authority figures)."),
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Only the first occasion on which a child started to be looked after in the local authority during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
                          tags$li("Following the introduction of the National Transfer Scheme (NTS) in 2016, there has been an agreement between local authorities to transfer UASC to ensure a more equitable distribution of UASC across all local authorities. This means that some UASC will be counted more than once in the national and regional CLA starting figures if they started to be looked after within more than 1 local
                                  authority during the year. In 2019 we estimate that nationally, the number of UASC starts was overestimated by 9%, this increased to 15% in 2023 following the mandation of the NTS in February 2022."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  ## CLA UASC by local authority
                  gov_row(
                    h2("Rate of children starting to be looked after by local authority who were Unaccompanied Asylum Seeking Children"),
                    p(sprintf("The charts below represent data from %s.", max(combined_cla_data$time_period))),
                    radioGroupButtons(
                      "uasc_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_uasc"),
                  )
                ),
                ## CLA UASC 31 March -------------
                accordion_panel(
                  "Children looked after each year on 31 March, with a breakdown by whether they are Unaccompanied Asylum Seeking Children (UASC)",
                  gov_row(
                    h2("Rate of children looked after on the 31st March who were Unaccompanied Asylum Seeking Children"),
                    p("This measures the stock number in care who are UASC. UASC are children, who have applied for asylum in their own right and are separated from both parents and/or any other responsible adult. Local authorities have a legal duty to provide accommodation for these children. This breakdown is provided for context."),
                    insert_text(inputId = "cla_rate_31_march_definition", text = paste(
                      "<b>", "Rate of children looked after on the 31st March", "</b><br>",
                      "The children looked after rate is calculated as the number of children looked after on 31 March, per 10,000 children in the population."
                    )),
                    br(),
                    plotlyOutput("plot_uasc_31_march"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_uasc_31_march",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_uasc_31_march", filename = "cla_UASC_rates_31_march.csv"),
                          reactableOutput("table_uasc_31_march")
                        ))
                      )
                    ),
                    details(
                      inputId = "cla_UASC_31_march_rate_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated using published number of children looked after at the 31st March each year, who are UASC and non-UASC, which have been rounded to the nearest 10 at national and regional level (unrounded for local authority figures)."),
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Only the first occasion on which a child started to be looked after in the local authority during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
                          tags$li("Following the introduction of the National Transfer Scheme (NTS) in 2016, there has been an agreement between local authorities to transfer UASC to ensure a more equitable distribution of UASC across all local authorities. This means that some UASC will be counted more than once in the national and regional CLA starting figures if they started to be looked after within more than 1 local
                                  authority during the year. In 2019 we estimate that nationally, the number of UASC starts was overestimated by 9%, this increased to 15% in 2023 following the mandation of the NTS in February 2022."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  ## CLA UASC region
                  gov_row(
                    h2("Rate of children looked after on 31st March by region who were Unaccompanied Asylum Seeking Children"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    p(sprintf("The chart represents data from %s.", max(combined_cla_data$time_period))),
                    br(),
                    plotlyOutput("plot_uasc_31_march_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_uasc_31_march_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_uasc_31_march_reg", filename = "cla_UASC_31_march_rates_regions.csv"),
                          reactableOutput("table_uasc_31_march_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "cla_UASC_31_march_rate_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Rates are calculated using published number of children starting to be looked after each year, who are UASC and non-UASC, which have been rounded to the nearest 10 at national and regional level (unrounded for local authority figures)."),
                          tags$li("Rates are calculated based on ", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2022#:~:text=We%20estimate%20the%20population%20of,mid%2D1962%20(1.0%25)", "ONS published mid-2022 population estimates", target = "_blank"), "and rebased population estimates for mid-2012 to mid-2021 for children aged 0 to 17 years."),
                          tags$li("Only the first occasion on which a child started to be looked after in the local authority during year has been counted. The care of a small number of children each year is transferred between LAs, in national figures these children will be counted as starting once within each LA. For more information see the methodology document (link below)."),
                          tags$li("Following the introduction of the National Transfer Scheme (NTS) in 2016, there has been an agreement between local authorities to transfer UASC to ensure a more equitable distribution of UASC across all local authorities. This means that some UASC will be counted more than once in the national and regional CLA starting figures if they started to be looked after within more than 1 local
                                  authority during the year. In 2019 we estimate that nationally, the number of UASC starts was overestimated by 9%, this increased to 15% in 2023 following the mandation of the NTS in February 2022."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "children looked after data guidance.", target = "_blank"),
                            tags$br(),
                            "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "children looked after methodology.", target = "_blank")
                          )
                        )
                      )
                    )
                  ),
                  ## CLA UASC 31st March by local authority
                  gov_row(
                    h2("Rate of children looked after on 31st March by local authority who were Unaccompanied Asylum Seeking Children"),
                    p(sprintf("The charts below represent data from %s.", max(combined_cla_data$time_period))),
                    radioGroupButtons(
                      "uasc_31_march_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_uasc_31_march"),
                  )
                ),
                open = FALSE
              )
            ),
            # Domain 3 --------------
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
                    title = "Persistent absentees for CINO at 31 March (overall absence 10% or more)",
                    value = htmlOutput("persistent_CIN_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Persistent absentees for CPPO at 31 March (overall absence 10% or more)",
                    value = htmlOutput("persistent_CPP_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Persistent absentees for CLA 12 months on 31 March (overall absence 10% or more)",
                    value = htmlOutput("persistent_CLA_headline_txt")
                  )
                ),
                br(),
              ),
              gov_row(
                div(
                  class = "input_box",
                  style = "min-height:100%; height = 100%; overflow-y: visible",
                  p("This domain contains breakdowns of data for the following social care groups: children in need, excluding children on a child protection plan and children looked after (CINO), children on a child protection plan, excluding children looked after (CPPO), children looked after (CLA) for 12 months."),
                  p("It also breakdowns the data by school type: total, primary, secondary (the headline boxes above show data for 'total' school type)."),
                  p("Use the dropdowns to select which social care group and school type you would like to see in the below accordions:"),
                  selectizeInput(
                    inputId = "wellbeing_extra_breakdown",
                    label = "Select a social care group:",
                    choices = c("CINO at 31 March", "CPPO at 31 March", "CLA 12 months at 31 March"),
                    selected = NULL,
                    multiple = FALSE,
                    options = NULL
                  ),
                  selectizeInput(
                    inputId = "wellbeing_school_breakdown",
                    label = "Select a school type:",
                    choices = c("Total", "State-funded primary", "State-funded secondary"),
                    selected = NULL,
                    multiple = FALSE,
                    options = NULL
                  ),
                  details(
                    inputId = "social_definitions_info",
                    label = "Social care group definitions:",
                    help_text = (
                      tags$ul(
                        tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                        tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                        tags$li("CLA 12 months refers to children looked after for 12 months (excludes children who are in respite care in their most recent episode during the reporting year)."),
                        tags$br(),
                        p(
                          "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                          tags$br(),
                          "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                        )
                      )
                    )
                  ),
                  details(
                    inputId = "school_definitions_info",
                    label = "School type information:",
                    help_text = (
                      tags$ul(
                        tags$li("Total school type includes state-funded primary and secondary schools as well as special schools and state-funded alternative provision schools."),
                        tags$li("Breakdowns for special schools and state-funded alternative provision schools are not available on the dashboard due to high levels of suppression in the published local authority level data.
                                You can view data for all breakdowns via the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england", "Outcomes publication.", target = "_blank"), ),
                        tags$br(),
                        p(
                          "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                          tags$br(),
                          "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                        )
                      )
                    )
                  ),
                  ### absence -----
                  accordion(
                    accordion_panel(
                      "School attendance",
                      gov_row(
                        h2("School attendance of children in need and children looked after"),
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
                        details(
                          inputId = "table_absence",
                          label = "View chart as a table",
                          help_text = (
                            HTML(paste0(
                              csvDownloadButton("table_absence_rate", filename = "absence_rates.csv"),
                              reactableOutput("table_absence_rate")
                            ))
                          )
                        ),
                        details(
                          inputId = "Attendance_info",
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
                              tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                              tags$li("CLA refers to children looked after (excludes children who are in respite care in their most recent episode during the reporting year)."),
                              tags$li("Children in need data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology section for more information."),
                              tags$br(),
                              p(
                                "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                                tags$br(),
                                "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                              )
                            )
                          )
                        ),
                      ),
                      # absence by region
                      gov_row(
                        h2("Absence rate by region"),
                        p("This chart will react to social care group selection but it will not react to geographical level and location selected in the filters at the top."),
                        br(),
                        insert_text(inputId = "Absence_definition", text = paste(
                          "<b>", "Absence rate", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_by_region_text")
                        )),
                        plotlyOutput("plot_absence_reg"),
                        br(),
                        details(
                          inputId = "tbl_absence_reg",
                          label = "View chart as a table",
                          help_text = (
                            HTML(paste0(
                              csvDownloadButton("table_absence_reg", filename = "absence_rates_regions.csv"),
                              reactableOutput("table_absence_reg")
                            ))
                          )
                        ),
                        details(
                          inputId = "Attendance_reg_info",
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
                              tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                              tags$li("CLA refers to children looked after (excludes children who are in respite care in their most recent episode during the reporting year)."),
                              tags$li("Children in need data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology section for more information."),
                              tags$br(),
                              p(
                                "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                                tags$br(),
                                "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                              )
                            )
                          )
                        ),
                      ),
                      # absence by local authority
                      gov_row(
                        h2("Absence rate by local authority"),
                        # p("This chart is reactive to the Local Authority and Regional filters at the top, aswell as the social care group filter, and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                        # p(sprintf("The charts below represent data from %s.", max(outcomes_absence$time_period))),
                        htmlOutput("outcome1_time_period_text"),
                        br(),
                        insert_text(inputId = "Absence_definition", text = paste(
                          "<b>", "Absence rate", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_by_la_text")
                        )),
                        radioGroupButtons(
                          "absence_stats_toggle",
                          label = NULL,
                          choices = c("All local authorities", "10 statistical neighbours"),
                          selected = "All local authorities",
                          justified = TRUE
                        ),
                        uiOutput("SN_absence"),
                      ),
                    ),
                    ### Persistent absence ----
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
                        details(
                          inputId = "table_persistence",
                          label = "View chart as a table",
                          help_text = (
                            HTML(paste0(
                              csvDownloadButton("table_persistent_rate", filename = "persistence_absence_rates.csv"),
                              reactableOutput("table_persistent_rate")
                            ))
                          )
                        ),
                        details(
                          inputId = "Persistent_info",
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
                              tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                              tags$li("Children in need data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology section for more information."),
                              tags$br(),
                              p(
                                "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                                tags$br(),
                                "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                              )
                            )
                          )
                        ),
                      ),
                      ## persistent absence by region
                      gov_row(
                        h2("Persistent absence by region"),
                        p("This chart will react to social care group selection but it will not react to geographical level and location selected in the filters at the top."),
                        br(),
                        insert_text(inputId = "Persistent_absence_definition", text = paste(
                          "<b>", "Persistent absentees", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_by_region_text_1")
                        )),
                        plotlyOutput("plot_persistent_reg"),
                        br(),
                        details(
                          inputId = "tbl_persistence_reg",
                          label = "View chart as a table",
                          help_text = (
                            HTML(paste0(
                              csvDownloadButton("table_persistent_reg", filename = "persistence_absence_rates_regions.csv"),
                              reactableOutput("table_persistent_reg")
                            ))
                          )
                        ),
                        details(
                          inputId = "Persistent_reg_info",
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
                              tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                              tags$li("Children in need data for the years ending 31 March 2021 and 2022 is not available for Hackney local authority, therefore 2020 data for Hackney has been included in 2021 and 2022 national totals, and regional totals for inner London and London. Refer to the methodology section for more information."),
                              tags$br(),
                              p(
                                "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                                tags$br(),
                                "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                              )
                            )
                          )
                        ),
                      ),
                      gov_row(
                        h2("Persistent absence rate by local authority"),
                        # p("This chart is reactive to the Local Authority and Regional filters at the top, aswell as the social care group filter, and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                        # p(sprintf("The charts below represent data from %s.", max(outcomes_absence$time_period))),
                        htmlOutput("outcome1_time_period_text_2"),
                        br(),
                        insert_text(inputId = "Persistent_absence_definition", text = paste(
                          "<b>", "Persistent absentees", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_by_la_text_1")
                        )),
                        radioGroupButtons(
                          "persis_abs_stats_toggle",
                          label = NULL,
                          choices = c("All local authorities", "10 statistical neighbours"),
                          selected = "All local authorities",
                          justified = TRUE
                        ),
                        uiOutput("SN_persistent_abs"),
                      ),
                    ),
                    open = FALSE
                  )
                ),
                br(),
              ),
            ),
            # Domain 4 ----
            tabPanel(
              "Educational attainment",
              br(),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "CINO at 31 March pupils achieving expected standard in reading, writing and mathematics combined (KS2)",
                    value = htmlOutput("KS2_CIN_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "CPPO at 31 March pupils achieving expected standard in reading, writing and mathematics combined (KS2)",
                    value = htmlOutput("KS2_CPP_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "CLA 12 months on 31 March pupils achieving expected standard in reading, writing and mathematics combined (KS2)",
                    value = htmlOutput("KS2_CLA_headline_txt")
                  )
                ),
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Average attainment 8 for CINO at 31 March (KS4)",
                    value = htmlOutput("KS4_CIN_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Average attainment 8 for CPPO at 31 March (KS4)",
                    value = htmlOutput("KS4_CPP_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Average attainment 8 for CLA 12 months on 31 March (KS4)",
                    value = htmlOutput("KS4_CLA_headline_txt")
                  )
                ),
                br(),
              ),
              gov_row(
                div(
                  class = "input_box",
                  style = "min-height:100%; height = 100%; overflow-y: visible",
                  p("This domain contains breakdowns of data for the following social care groups: children in need, excluding children on a child protection plan and children looked after (CINO), children on a child protection plan, excluding children looked after (CPPO), children looked after (CLA) for 12 months."),
                  p("Use this dropdown to select which social care group you would like to see in the below accordions:"),
                  selectizeInput(
                    inputId = "attainment_extra_breakdown",
                    label = "Select a social care group:",
                    choices = c("CINO at 31 March", "CPPO at 31 March", "CLA 12 months at 31 March"),
                    selected = NULL,
                    multiple = FALSE,
                    options = NULL
                  ),
                  details(
                    inputId = "social_definitions_info_2",
                    label = "Social care group definitions:",
                    help_text = (
                      tags$ul(
                        tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                        tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                        tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                        tags$br(),
                        p(
                          "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                          tags$br(),
                          "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                        )
                      )
                    )
                  ),
                  ### ks2 attainment -----
                  accordion(
                    accordion_panel(
                      "Key stage 2 (KS2)",
                      gov_row(
                        h2("Key stage 2 attainment"),
                        p("Educational attainment is a key component of long-term development and wellbeing for children and young people, which affects their outcomes. Children should be supported to access and make progress in education to support their development and life chances. Virtual school heads have a statutory duty to promote the educational attainment of all children in their care.
                          This includes ensuring suitable and timely educational provision and managing pupil premium plus funding aligned to objectives in the child’s personal education plan"),
                        insert_text(inputId = "ks2_definition", text = paste(
                          "<b>", "Expected standard for year 6 pupils (mostly aged 11)", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_text_2")
                        )),
                        plotlyOutput("plot_ks2_expected"),
                        br(),
                        details(
                          inputId = "table_ks2",
                          label = "View chart as a table",
                          help_text = (
                            HTML(paste0(
                              csvDownloadButton("table_ks2_expected", filename = "ks2_attainment_rates.csv"),
                              reactableOutput("table_ks2_expected")
                            ))
                          )
                        ),
                        details(
                          inputId = "ks2_info",
                          label = "Additional information:",
                          help_text = (
                            tags$ul(
                              tags$li("No attainment data related to 2019/20 and 2020/21 academic year is available due to COVID-19."),
                              tags$li(
                                "Writing teacher assessment and reading, writing and maths (combined) measures from 2018 onwards are not directly comparable to previous years due to changes in the writing teacher assessment frameworks. For more detailed information on this see ",
                                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment", "Key stage 2 attainment.", target = "_blank"),
                              ),
                              tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                              tags$li("Children in need data is not available for Hackney local authority for the 2020 to 2021 or the 2021 to 2022 collection year. Hackney was unable to provide a return for both the 2021 and the 2022 children in need census, due to a cyberattack which had a significant impact on their management information systems. Refer to the methodology section for more information."),
                              tags$br(),
                              p(
                                "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                                tags$br(),
                                "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                              )
                            )
                          )
                        ),
                      ),
                      gov_row(
                        h2("KS2 attainment by region"),
                        p("This chart will react to social care group selection but it will not react to geographical level and location selected in the filters at the top."),
                        br(),
                        insert_text(inputId = "ks2_definition", text = paste(
                          "<b>", "Expected standard for year 6 pupils (mostly aged 11)", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_by_region_text_2")
                        )),
                        plotlyOutput("plot_ks2_reg"),
                        br(),
                        details(
                          inputId = "tbl_ks2_reg",
                          label = "View chart as a table",
                          help_text = (
                            HTML(paste0(
                              csvDownloadButton("table_ks2_reg", filename = "ks2_attainment_rates_region.csv"),
                              reactableOutput("table_ks2_reg")
                            ))
                          )
                        ),
                        details(
                          inputId = "ks2_reg_info",
                          label = "Additional information:",
                          help_text = (
                            tags$ul(
                              tags$li("No attainment data related to 2019/20 and 2020/21 academic year is available due to COVID-19."),
                              tags$li(
                                "Writing teacher assessment and reading, writing and maths (combined) measures from 2018 onwards are not directly comparable to previous years due to changes in the writing teacher assessment frameworks. For more detailed information on this see ",
                                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment", "Key stage 2 attainment.", target = "_blank"),
                              ),
                              tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                              tags$li("Children in need data is not available for Hackney local authority for the 2020 to 2021 or the 2021 to 2022 collection year. Hackney was unable to provide a return for both the 2021 and the 2022 children in need census, due to a cyberattack which had a significant impact on their management information systems. Refer to the methodology section for more information."),
                              tags$br(),
                              p(
                                "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                                tags$br(),
                                "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                              )
                            )
                          )
                        ),
                      ),
                      gov_row(
                        h2("KS2 attainment by local authority"),
                        # p("This chart is reactive to the Local Authority and Regional filters at the top, aswell as the social care group filter, and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                        # p(sprintf("The charts below represent data from %s.", max(outcomes_ks2$time_period))),
                        htmlOutput("outcome1_time_period_text_3"),
                        br(),
                        insert_text(inputId = "ks2_definition", text = paste(
                          "<b>", "Expected standard for year 6 pupils (mostly aged 11)", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_by_la_text_2")
                        )),
                        radioGroupButtons(
                          "ks2_attainment_stats_toggle",
                          label = NULL,
                          choices = c("All local authorities", "10 statistical neighbours"),
                          selected = "All local authorities",
                          justified = TRUE
                        ),
                        uiOutput("SN_ks2_attainment"),
                      ),
                    ),
                    ### KS4 attainment ---------
                    accordion_panel(
                      "Key stage 4 (KS4)",
                      gov_row(
                        h2("Key stage 4 attainment"),
                        p("Educational attainment is a key component of long-term development and wellbeing for children and young people, which affects their outcomes. Children should be supported to access and make progress in education to support their development and life chances.
                          Virtual school heads have a statutory duty to promote the educational attainment of all children in their care. This includes ensuring suitable and timely educational provision and managing pupil premium plus funding aligned to objectives in the child’s personal education plan."),
                        insert_text(inputId = "ks4_definition", text = paste(
                          "<b>", "Attainment 8 for pupils finishing GCSEs (mostly aged 16)", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_text_3")
                        )),
                        plotlyOutput("plot_ks4"),
                        br(),
                        details(
                          inputId = "tbl_ks4",
                          label = "View chart as a table",
                          help_text = (
                            HTML(paste0(
                              csvDownloadButton("table_ks4", filename = "ks4_attainment_rates.csv"),
                              reactableOutput("table_ks4")
                            ))
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
                                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance", "Key stage 4 performance.", target = "_blank"),
                              ),
                              tags$li("In 2022/23 there was a return to pre-pandemic standards for GCSEs, with protection built into the grading process to recognise the disruption that students have faced. Therefore, the more meaningful comparison is with 2019, the last year that summer exams were taken before the pandemic, as 2023 saw a return to pre-pandemic grading, with some protections.
                                  In 2022 outcomes broadly reflected a mid-point between 2019 and 2021, to take account of the impact of the pandemic and in line with Ofqual’s approach to grading in 2022. It is expected that performance in 2023 will generally be lower than in 2022. Users need to exercise extreme caution when considering comparisons over time, as they may not reflect changes in pupil performance alone."),
                              tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                              tags$li("Children in need data is not available for Hackney local authority for the 2020 to 2021 or the 2021 to 2022 collection year. Hackney was unable to provide a return for both the 2021 and the 2022 children in need census, due to a cyberattack which had a significant impact on their management information systems. Refer to the methodology section for more information."),
                              tags$br(),
                              p(
                                "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                                tags$br(),
                                "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                              )
                            )
                          )
                        ),
                      ),
                      gov_row(
                        h2("KS4 attainment by region"),
                        p("This chart will react to social care group selection but it will not react to geographical level and location selected in the filters at the top."),
                        br(),
                        insert_text(inputId = "ks4_definition", text = paste(
                          "<b>", "Attainment 8 for pupils finishing GCSEs (mostly aged 16)", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_by_region_text_3")
                        )),
                        plotlyOutput("plot_ks4_reg"),
                        br(),
                        details(
                          inputId = "tbl_ks4_reg",
                          label = "View chart as a table",
                          help_text = (
                            HTML(paste0(
                              csvDownloadButton("table_ks4_reg", filename = "ks4_attainment_rates_regions.csv"),
                              reactableOutput("table_ks4_reg")
                            ))
                          )
                        ),
                        details(
                          inputId = "ks4_reg_info",
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
                              tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
                              tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
                              tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
                              tags$li("Children in need data is not available for Hackney local authority for the 2020 to 2021 or the 2021 to 2022 collection year. Hackney was unable to provide a return for both the 2021 and the 2022 children in need census, due to a cyberattack which had a significant impact on their management information systems. Refer to the methodology section for more information."),
                              tags$br(),
                              p(
                                "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
                                tags$br(),
                                "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
                              )
                            )
                          )
                        ),
                      ),
                      gov_row(
                        h2("KS4 attainment by local authority"),
                        # p("This chart is reactive to the Local Authority and Regional filters at the top, aswell as the social care group filter, and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                        # p(sprintf("The charts below represent data from %s.", max(outcomes_ks4$time_period))),
                        htmlOutput("outcome1_time_period_text_4"),
                        br(),
                        insert_text(inputId = "ks4_definition", text = paste(
                          "<b>", "Attainment 8 for pupils finishing GCSEs (mostly aged 16)", "</b><br>",
                          htmlOutput("outcome1_choice_social_care_group_by_la_text_3")
                        )),
                        radioGroupButtons(
                          "ks4_attainment_stats_toggle",
                          label = NULL,
                          choices = c("All local authorities", "10 statistical neighbours"),
                          selected = "All local authorities",
                          justified = TRUE
                        ),
                        uiOutput("SN_ks4_attainment"),
                      ),
                    ),
                    open = FALSE
                  )
                ),
                br(),
              ),
            )
          )
        )
      )
    )
  )
}
