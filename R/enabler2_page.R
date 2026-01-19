enabler2_tab <- function() {
  nav_panel(
    value = "enabler2_page",
    "Leadership",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Enabler: Leaders drive conditions for effective practice"),
        )
      ),
      gov_row(
        div(
          class = "geo_input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_e2",
              label = "Select a geographical level:",
              choices = c("National", "Regional", "Local authority"),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(
              condition = "input.select_geography_e2 != 'National'",
              selectizeInput(
                inputId = "geographic_breakdown_e2",
                label = "Select a location: ",
                choices = NULL,
                selected = NULL,
                multiple = FALSE,
                options = NULL
              )
            ),
            col_widths = c(5, 7)
          ),
          insert_text(
            inputId = "no_checkboxes",
            text = "The indicators in this enabler do not have regional or national comparisons over time"
          )
        )
      ),
      gov_row(
        p(htmlOutput("enabler2_choice_text1"), htmlOutput("enabler2_choice_text2")),
      ),
      gov_row(
        div(
          tabsetPanel(
            id = "enabler2_panels",
            type = "tabs",
            ## Spending ----------------
            tabPanel(
              "Spending",
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Share of total local authority spend on children's services",
                    value = htmlOutput("total_spending_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Average per capita (of all children in a local authority) spend on children’s services",
                    value = htmlOutput("avg_spend_per_child")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Share of children’s services spend not on CLA",
                    value = htmlOutput("spend_minus_cla_txt")
                  )
                ),
                br(),
              ),
              # conditionalPanel(
              #   condition = "input.geographic_breakdown_e2 == 'London'",
              #   p("Inner London and Outer London have been grouped together as London.")
              # ),
              accordion(
                accordion_panel(
                  "Share of local authority total spend on children’s services, and per child spend",
                  gov_row(
                    h2("Share of local authority total spend on children’s services, and per child spend"),
                    p("This metric provides contextual information on the resource prioritisation within each local authority."),
                    # extra dropdown for choice to view per child spend or share of spending
                    div(
                      class = "input_box",
                      style = "min-height:100%; height = 100%; overflow-y: visible",
                      p("This domain contains two levels of data: Share of total LA spend on children's services and average per capita (of all children in a local authority) spend on children's services"),
                      p("Use the dropdown below to select which level of spending data you would like to see in the charts below:"),
                      selectizeInput(
                        inputId = "spending_choice",
                        label = "Select a spending level:",
                        choices = c("Share of total local authority spend on children's services", "Spend per child on children's services"),
                        selected = NULL,
                        multiple = FALSE,
                        options = NULL,
                        width = "50%"
                      ),
                      gov_row(
                        htmlOutput("spending_header1"),
                        br(),
                        plotlyOutput("plot_spending_region"),
                        br(),
                        details(
                          inputId = "spend1_region_tbl",
                          label = "View chart as table",
                          help_text = (
                            HTML(paste0(
                              csvDownloadButton("table_tot_spending_reg", filename = "spend_on_CSC_regions.csv"),
                              reactableOutput("table_tot_spending_reg")
                            ))
                          )
                        ),
                        details(
                          inputId = "spend1_region_info",
                          label = "Additional information:",
                          help_text = (
                            tags$ul(
                              tags$li("Share of spend is calculated by taking total children’s services expenditure divided by total local authority expenditure"),
                              tags$li("Share of spend at a national level includes lower tier local authorities. Share of spend at a regional level aggregates the spend of the local authorities in the dashboard and therefore does not include lower tier local authorities."),
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
                      ),
                      gov_row(
                        htmlOutput("spending_header2"),
                        radioGroupButtons(
                          "spending1_stats_toggle",
                          label = NULL,
                          choices = c("All local authorities", "10 statistical neighbours"),
                          selected = "All local authorities",
                          justified = TRUE
                        ),
                        uiOutput("SN_total_spending"),
                      ),
                    ),
                    br(),
                  ),
                ),
                accordion_panel(
                  "Share of children’s services spend not on CLA",
                  gov_row(
                    h2("Share of children’s services spend not on CLA by region"),
                    p("Prioritising funding and resources that help families early helps children and young people thrive. This metric looks at the resource prioritisation between early and later statutory intervention."),
                    plotlyOutput("plot_spend_excl_cla_region"),
                    br(),
                    details(
                      inputId = "minus_cla_reg_tbl",
                      label = "View chart as table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("table_spend_excl_cla_reg", filename = "spend_on_CSC_excl_CLA_regions.csv"),
                          reactableOutput("table_spend_excl_cla_reg")
                        ))
                      )
                    ),
                    details(
                      inputId = "minus_cla_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Share of spend is calculated by taking total children’s services expenditure minus total CLA expenditure, divided by total children’s services expenditure."),
                          tags$li("Share of spend at a national level includes lower tier local authorities. Share of spend at a regional level aggregates the spend of the local authorities in the dashboard and therefore does not include lower tier local authorities."),
                          tags$li("Spending data is based on the RO3 and RSX data files from the", a(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2024-to-2025-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2024 to 2025 individual local authority data – outturn", target = "_blank")),
                          tags$br(),
                          p(
                            "For more information on the data and definitions, refer to the", a(href = "https://www.gov.uk/government/publications/general-fund-revenue-account-outturn/general-fund-revenue-account-outturn-general-guidance-notes", "General fund revenue account outturn: general guidance notes.", target = "_blank"),
                          )
                      ))
                    )
                  ),
                  gov_row(
                    h2("Share of children’s services spend not on CLA by local authority"),
                    radioGroupButtons(
                      "spending2_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 statistical neighbours"),
                      selected = "All local authorities",
                      justified = TRUE
                    ),
                    uiOutput("SN_spending_minus_cla"),
                  )
                ),
                open = FALSE
              )
            ),
            ## Ofsted ---------------------
            tabPanel(
              "Culture focused on outcomes from children and families and continually improving services",
              fluidRow(
                conditionalPanel(
                  condition = "input.select_geography_e2 == 'Local authority'",
                  column(
                    width = 4,
                    value_box(
                      title = "LA Ofsted leadership rating",
                      value = htmlOutput("ofsted_la_headline")
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.select_geography_e2 != 'Local authority'",
                fluidRow(
                  column(
                    width = 6,
                    value_box(
                      title = "Number of local authorities with an Ofsted Leadership Rating of Outstanding",
                      value = htmlOutput("ofsted_outstanding_headline")
                    )
                  ),
                  column(
                    width = 6,
                    value_box(
                      title = "Number of local authorities with an Ofsted Leadership Rating of Good",
                      value = htmlOutput("ofsted_good_headline")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    value_box(
                      title = "Number of local authorities with an Ofsted Leadership Rating of Requires Improvement",
                      value = htmlOutput("ofsted_improvement_headline")
                    )
                  ),
                  column(
                    width = 6,
                    value_box(
                      title = "Number of local authorities with an Ofsted Leadership Rating of Inadequate",
                      value = htmlOutput("ofsted_inadequate_headline")
                    )
                  )
                ),
                fluidRow(
                  p("The culture of leadership drives effective and efficient practice. Ofsted rating for leadership provides a summary judgement of the assessed culture and practice of leadership within each authority."),
                )
              ),
              accordion(
                accordion_panel(
                  "Culture focused on outcomes from children and families and continually improving services",
                  gov_row(
                    warning_text(inputId = "warn", text = "Latest leadership rating years for each local authority may differ. View the table below to see the latest year for each LA."),
                    details(
                      inputId = "tbl_ofsted_latest_ratings",
                      label = "View latest ratings",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("ofsted_latest_ratings_tbl", filename = "Ofsted_leadership_ratings_all_LA_rating_date.csv"),
                          reactableOutput("ofsted_latest_ratings_tbl")
                        ))
                      )
                    )
                  ),
                  gov_row(
                    h2("Ofsted – The impact of leaders on social work practice with children and families nationally"),
                    warning_text(inputId = "warn", text = "Latest leadership rating years for each local authority may differ. View the table at the top to see the latest year for each LA."),
                    p("This is a static chart and will not react to geographical level, location, or comparison checkboxes selected in the filters at the top."),
                    plotlyOutput("plot_ofsted"),
                    br(),
                    details(
                      inputId = "tbl_ofsted",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("ofsted_tbl", filename = "Ofsted_leadership_ratings_national.csv"),
                          reactableOutput("ofsted_tbl")
                        ))
                      )
                    ),
                    details(
                      inputId = "ofsted_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("For more information on how Ofsted rate local authorities on their leadership, see the ", a(href = "https://www.gov.uk/government/publications/inspecting-local-authority-childrens-services-from-2018/inspecting-local-authority-childrens-services", "'Inspecting local authority children’s services'", target = "_blank"), " guidance."),
                          tags$li(
                            "For more information on the data and definitions, see here: ", a(href = "https://www.gov.uk/government/statistics/local-authority-inspection-outcomes-as-at-31-march-2025", "Children’s social care data in England 2025.", target = "_blank"),
                          ),
                          tags$li("The Ofsted leadership rating is sub-judgement rating and not the overall Ofsted rating.")
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Ofsted – The impact of leaders on social work practice with children and families by region"),
                    warning_text(inputId = "warn", text = "Latest leadership rating years for each local authority may differ. View the table at the top to see the latest year for each LA."),
                    p("This is a static chart and will not react to geographical level, location, or comparison checkboxes selected in the filters at the top."),
                    plotlyOutput("plot_ofsted_reg"),
                    br(),
                    details(
                      inputId = "tbl_ofsted_reg",
                      label = "View chart as a table",
                      help_text = (
                        HTML(paste0(
                          csvDownloadButton("ofsted_reg_tbl", filename = "Ofsted_leadership_ratings_regions.csv"),
                          reactableOutput("ofsted_reg_tbl")
                        ))
                      )
                    ),
                    details(
                      inputId = "ofsted_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("For more information on how Ofsted rate local authorities on their leadership, see the ", a(href = "https://www.gov.uk/government/publications/inspecting-local-authority-childrens-services-from-2018/inspecting-local-authority-childrens-services", "'Inspecting local authority children’s services'", target = "_blank"), " guidance."),
                          tags$li(
                            "For more information on the data and definitions, see here: ", a(href = "https://www.gov.uk/government/statistics/local-authority-inspection-outcomes-as-at-31-march-2025", "Children’s social care data in England 2025.", target = "_blank"),
                          ),
                          tags$li("The Ofsted leadership rating is sub-judgement rating and not the overall Ofsted rating.")
                        )
                      )
                    )
                  ),
                  gov_row(
                    uiOutput("ofsted_rating_SN_ui"),
                  )
                ),
                open = FALSE,
              )
            )
          )
        )
      )
    )
  )
}
