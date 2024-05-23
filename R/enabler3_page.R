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
                  width = 6,
                  value_box(
                    title = "title",
                    value = "value"
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "title",
                    value = "value"
                  )
                ),
                br(),
                p("Rationale here")
              ),
              accordion(
                accordion_panel(
                  "Accordion one",
                  gov_row(
                    h2("Share of local authority total spend on Children’s Services, and per child spend."),
                    #
                    #
                    br(),
                    plotlyOutput(""),
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
                )
              )
            ),
            tabPanel(
              "Culture focused on outcomes from children and families and continually improving services",
              fluidRow(
                # p("testing")
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "title",
                    value = "value"
                  )
                ),
                br(),
                p("Rationale"),
              ),
              accordion(
                accordion_panel(
                  "Culture focused on outcomes from children and families and continually improving services",
                  gov_row(
                    h2("Ofsted – The impact of leaders on social work practice with children and families"),
                  ),
                  gov_row(
                    h2("by region"),
                  ),
                  gov_row(
                    h2("by la"),
                    radioGroupButtons(
                      "ofted_stats_toggle",
                      label = NULL,
                      choices = c("All local authorities", "10 Statistical Neighbours"),
                      selected = "All local authorities"
                    ),
                    # uiOutput("SN_sgo"),
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
