enabler3_tab <- function() {
  tabPanel(
    value = "enabler3_page",
    "Enabler 3",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Enabler 3: Leaders drive conditions for effective practice")
        )
      ),
      gov_row(
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_e3",
              label = "Select a geographical level:",
              choices = unique(ofsted_leadership_data %>% pull("geographic_level")),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(
              condition = "input.select_geography_e3 != 'National'",
              selectizeInput(
                inputId = "geographic_breakdown_e3",
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
              condition = "input.select_geography_e3 != 'National'",
              column(
                width = 5,
                checkbox_Input(
                  inputId = "national_comparison_checkbox_e3",
                  cb_labels = "Compare with National",
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
                width = 5,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_e3",
                  cb_labels = "Compare with Region",
                  checkboxIds = "Yes_region_e3",
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
        p(htmlOutput("enabler3_choice_text1"), htmlOutput("enabler3_choice_text2")),
        br(),
      ),
      gov_row(
        div(
          tabsetPanel(
            id = "enabler3_panels",
            type = "tabs",
            tabPanel(
              "Spending",
              fluidRow(
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Share of total spend on Children's services",
                    value = htmlOutput("total_spending_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Average per child spend",
                    value = "value"
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Share of total spend on Children's services
                    minus CLA",
                    value = "value"
                  )
                ),
                br(),
              ),
              accordion(
                accordion_panel(
                  "Share of local authority total spend on Children’s Services, and per child spend",
                  gov_row(
                    h2("Share of local authority total spend on Children’s Services, and per child spend"),
                    p("This metric provides contextual information on the resource prioritisation within each Local Authority."),
                    # extra dropdown for choice to view per child spend or share of spending
                    div(
                      class = "input_box",
                      style = "min-height:100%; height = 100%; overflow-y: visible",
                      p("This domain contains two levels of data: Share of total spend on children's services and spend per child on children's services"),
                      p("Please use the dropdown below to select which level of spending data you would like to see in the charts below:"),
                      selectizeInput(
                        inputId = "spending_choice",
                        label = "Select a spending level:",
                        choices = c("Share of total spend on children's services", "Spend per child on children's services"),
                        selected = NULL,
                        multiple = FALSE,
                        options = NULL,
                        width = "50%"
                      ),
                    ),
                    br(),
                    #
                    br(),
                    # plotlyOutput("plot_spending_ts"),
                    br(),
                    # details(
                    #   inputId = "",
                    #   label = "",
                    #   help_text = (p(
                    #
                    #   ))
                    # )
                  ),
                  gov_row(
                    h2("by region"),
                  ),
                  gov_row(
                    h2("by la"),
                    # p(),
                    radioGroupButtons(
                      "spending1_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 Statistical Neighbours"),
                      selected = "All local authorities"
                    ),
                    # uiOutput("SN_sgo"),
                  )
                ),
                accordion_panel(
                  "Share of Children and Young People Services spend minus spend on CLA",
                  gov_row(
                    h2("time series")
                  ),
                  gov_row(
                    h2("by region")
                  ),
                  gov_row(
                    h2("by la"),
                    radioGroupButtons(
                      "spending2_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 Statistical Neighbours"),
                      selected = "All local authorities"
                    ),
                  )
                ),
                open = FALSE
              )
            ),
            tabPanel(
              "Culture focused on outcomes from children and families and continually improving services",
              fluidRow(
                br()
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.select_geography_e3 == 'Local authority'",
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
                condition = "input.select_geography_e3 != 'Local authority'",
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
              ),
              accordion(
                accordion_panel(
                  "Culture focused on outcomes from children and families and continually improving services",
                  gov_row(
                    h2("Ofsted – The impact of leaders on social work practice with children and families nationally"),
                    warning_text(inputId = "warn", text = "Latest leadership rating years for each LA may differ. View the table at the top to see the latest year for each LA."),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    p("This chart will also not react to the comparison checkboxes at the top."),
                    plotlyOutput("plot_ofsted"),
                    br(),
                    details(
                      inputId = "tbl_ofsted",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("ofsted_tbl")
                      )
                    ),
                    details(
                      inputId = "ofsted_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("For more information on how Ofsted rate local authorities on their leadership, please see the ", a(href = "https://www.gov.uk/government/publications/inspecting-local-authority-childrens-services-from-2018/inspecting-local-authority-childrens-services", "'Inspecting local authority children’s services'"), " guidance."),
                          tags$li(
                            "For more information on the data and definitions, please see here: ", a(href = "https://www.gov.uk/government/statistics/childrens-social-care-data-in-england-2023", "Children’s social care data in England 2023."),
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Ofsted – The impact of leaders on social work practice with children and families by region"),
                    warning_text(inputId = "warn", text = "Latest leadership rating years for each LA may differ. View the table at the top to see the latest year for each LA."),
                    p("This is a static chart and will not react to geographical level and location selected in the filters at the top."),
                    p("This chart will also not react to the comparison checkboxes at the top."),
                    plotlyOutput("plot_ofsted_reg"),
                    br(),
                    details(
                      inputId = "tbl_ofsted_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("ofsted_reg_tbl")
                      )
                    ),
                    details(
                      inputId = "ofsted_reg_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("For more information on how Ofsted rate local authorities on their leadership, please see the ", a(href = "https://www.gov.uk/government/publications/inspecting-local-authority-childrens-services-from-2018/inspecting-local-authority-childrens-services", "'Inspecting local authority children’s services'"), " guidance."),
                          tags$li(
                            "For more information on the data and definitions, please see here: ", a(href = "https://www.gov.uk/government/statistics/childrens-social-care-data-in-england-2023", "Children’s social care data in England 2023."),
                          )
                        )
                      )
                    )
                  ),
                  gov_row(
                    h2("Ofsted – The impact of leaders on social work practice with children and families with statistical neighbours"),
                    p("Hover over each data point to see the year of their last Ofsted inspection."),
                    plotlyOutput("ofsted_SN_plot"),
                    br(),
                    details(
                      inputId = "tbl_ofsted_SN",
                      label = "View chart as a table",
                      help_text = (
                        reactableOutput("ofsted_SN_tbl")
                      )
                    ),
                    details(
                      inputId = "ofsted_stat_neighbours_info",
                      label = "Additional information:",
                      help_text = (
                        p("Statistical neighbours info")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
