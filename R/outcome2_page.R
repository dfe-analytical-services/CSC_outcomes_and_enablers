outcome2_tab <- function() {
  nav_panel(
    value = "outcome2_page",
    "Family network",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Outcome 2: Children and young people are supported by their family network",class = "h2-27px"),
        )
      ),
      # dropdown boxes
      gov_row(
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_o2",
              label = "Select a geographical level:",
              choices = unique(ceased_cla_data %>% filter(geographic_level != "Statistical neighbours (median)") %>% pull("geographic_level")),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(
              condition = "input.select_geography_o2 != 'National'",
              selectizeInput(
                inputId = "geographic_breakdown_o2",
                label = "Select a location: ",
                choices = NULL,
                selected = NULL,
                multiple = FALSE,
                options = NULL
              )
            ),
            panel(),
            col_widths = c(4, 4, 4)
          ),
          # checkboxes for comparisons
          layout_columns(
            conditionalPanel(
              condition = "input.select_geography_o2 != 'National'",
              column(
                width = 12,
                checkbox_Input(
                  inputId = "national_comparison_checkbox_o2",
                  cb_labels = "Compare with national",
                  checkboxIds = "Yes_national_o2",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "(input.select_geography_o2 == 'Local authority')",
              column(
                width = 12,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_o2",
                  cb_labels = "Compare with region",
                  checkboxIds = "Yes_region_o2",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "(input.select_geography_o2 == 'Local authority')",
              column(
                width = 12,
                checkbox_Input(
                  inputId = "sn_comparison_checkbox_o2",
                  cb_labels = "Compare with statistical neighbours",
                  checkboxIds = "Yes_sn_o2",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            col_widths = c(4, 4, 4)
          )
        )
      ),
     # br(),
      # confirmation choice
      gov_row(
      #  br(),
        p(htmlOutput("outcome2_choice_text1"), htmlOutput("outcome2_choice_text2")),
        # conditionalPanel(
        #   condition = "(input.geographic_breakdown_o2 == 'Cumbria')",
        #   p("Cumbria are included in the latest statistics because there is historic data available to review before Cumbria local authority was replaced with two new unitary authorities, Cumberland and Westmorland and Furness, in April 2023.")
        # ),
      ),
      gov_row(
      #  br(),
        div(
          tabsetPanel(
            id = "outcome2_panels",
            type = "tabs",
            tabPanel(
              "Families engaging and receiving support from their family network",
              fluidRow(
                br()
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Percentage of children who cease being looked after due to special guardianship order (SGO)",
                    value = htmlOutput("SGO_headline_txt")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Percentage of children who cease being looked after due to residence order or child arrangements order (CAO)",
                    value = htmlOutput("CAO_headline_txt")
                  )
                ),
                br(),
                p("Unlocking the potential of family networks and kinship care is key to building a system where children who cannot live with their parents are supported to live with people who are known to them and love them."),
                p("Moving children from care arrangements to a SGO or CAO shows that permanence outside of the care system is being prioritised."),
              ),
              accordion(
                ## SGO ------
                accordion_panel(
                  "Percentage of children who cease being looked after due to special guardianship order (SGO)",
                  gov_row(
                    h2("Special guardianship order (SGO)"),
                    p("Children ceasing to be looked after during the year due to the reason of special guardianship order."),
                    insert_text(inputId = "SGO_def", text = paste(
                      tags$b("Special guardianship order"), " - A special guardianship order is a private law order (under section14A Children Act 1989) appointing one or more individuals to be a child's 'special guardian'."
                    )),
                    br(),
                    plotlyOutput("SGO_time_series"),
                    br(),
                    details(
                      inputId = "tbl_sgo_ceased_cla",
                      label = "View chart as table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_sgo_ceased", filename = "ceased_CLA_SGO.csv"),
                          reactableOutput("table_sgo_ceased")
                        ))
                      )
                    ),
                    details(
                      inputId = "sgo_info",
                      label = "Additional information:",
                      help_text = (
                        p(
                          tags$li("Only one reason for children ceased to be looked after during the year shown. See ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after publication ", target = "_blank"), "for full list of reasons."),
                          tags$li("Percentages rounded to the nearest whole number."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$li("Figures exclude children looked after under a series of short-term placements."),
                          tags$li("Only the last occasion on which a child ceased to be looked after in the year has been counted."),
                          tags$br(),
                          "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance.", target = "_blank"),
                          tags$br(),
                          "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.", target = "_blank")
                      ))
                    )
                  ),
                  gov_row(
                    h2("Special guardianship order (SGO) by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top.

                      The chart represents data from 2023."),
                    br(),
                    plotlyOutput("plot_sgo_ceased_reg"),
                    br(),
                    details(
                      inputId = "tbl_sgo_ceased_cla_reg",
                      label = "View chart as table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_sgo_ceased_reg", filename = "ceased_CLA_SGO_regions.csv"),
                          reactableOutput("table_sgo_ceased_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "sgo_reg_info",
                      label = "Additional information:",
                      help_text = (
                        p(
                          tags$li("Only one reason for children ceased to be looked after during the year shown. See ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after publication ", target = "_blank"), "for full list of reasons."),
                          tags$li("Percentages rounded to the nearest whole number."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$li("Figures exclude children looked after under a series of short-term placements."),
                          tags$li("Only the last occasion on which a child ceased to be looked after in the year has been counted."),
                          tags$br(),
                          "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance.", target = "_blank"),
                          tags$br(),
                          "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.", target = "_blank")
                      ))
                    )
                  ),
                  gov_row(
                    h2("Special guardianship order (SGO) by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(ceased_cla_data$time_period))),
                    radioGroupButtons(
                      "sgo_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_sgo"),
                  )
                ),
                ## CAO ----------------
                accordion_panel(
                  "Percentage of children who cease being looked after due to residence order or child arrangements order (CAO)",
                  gov_row(
                    h2("Residence order or child arrangements order (CAO)"),
                    p("Children ceasing to be looked after during the year due to the reason of child arrangements order."),
                    insert_text(inputId = "CAO_def", text = paste(
                      tags$b("Child arrangements order"), " - An order from court which details the arrangements for a child, including where and with whom the child will live, and who else they will spend time or have contact with. A child arrangements order is usually used to determine arrangements between parents but can also be used to order that a child lives with, or otherwise has contact with, another person, such as a family member or friend."
                    )),
                    plotlyOutput("CAO_time_series"),
                    br(),
                    details(
                      inputId = "tbl_cao_ceased_cla",
                      label = "View chart as table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cao_ceased", filename = "ceased_CLA_CAO.csv"),
                          reactableOutput("table_cao_ceased")
                        ))
                      )
                    ),
                    details(
                      inputId = "cao_info",
                      label = "Additional information:",
                      help_text = (
                        p(
                          tags$li("Only one reason for children ceased to be looked after during the year shown. See ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after publication ", target = "_blank"), "for full list of reasons."),
                          tags$li("Percentages rounded to the nearest whole number."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$li("Figures exclude children looked after under a series of short-term placements."),
                          tags$li("Only the last occasion on which a child ceased to be looked after in the year has been counted."),
                          tags$br(),
                          "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance.", target = "_blank"),
                          tags$br(),
                          "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.", target = "_blank")
                      ))
                    )
                  ),
                  gov_row(
                    h2("Residence order or child arrangements order (CAO) by region"),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top.

                      The chart represents data from 2023."),
                    br(),
                    plotlyOutput("plot_cao_ceased_reg"),
                    br(),
                    details(
                      inputId = "tbl_cao_ceased_reg",
                      label = "View chart as table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_cao_ceased_reg", filename = "ceased_CLA_CAO_regions.csv"),
                          reactableOutput("table_cao_ceased_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "cao_reg_info",
                      label = "Additional information:",
                      help_text = (
                        p(
                          tags$li("Only one reason for children ceased to be looked after during the year shown. See ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after publication ", target = "_blank"), "for full list of reasons."),
                          tags$li("Percentages rounded to the nearest whole number."),
                          tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to view the equivalent data in earlier releases of the publication."),
                          tags$li("Figures exclude children looked after under a series of short-term placements."),
                          tags$li("Only the last occasion on which a child ceased to be looked after in the year has been counted."),
                          tags$br(),
                          "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after guidance.", target = "_blank"),
                          tags$br(),
                          "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after methodology.", target = "_blank")
                      ))
                    )
                  ),
                  gov_row(
                    h2("Residence order or child arrangements order (CAO) by local authority"),
                    p(sprintf("The charts below represent data from %s.", max(ceased_cla_data$time_period))),
                    radioGroupButtons(
                      "cao_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_cao"),
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
