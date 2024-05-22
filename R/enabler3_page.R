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
            conditionalPanel(condition = "input.select_geography_e3 != 'National'", selectizeInput(
              inputId = "geographic_breakdown_e3",
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
              ),
            ),
            col_widths = c(4, 8)
          ),
        )
      ),
      br(),
      gov_row(
        br(),
        p(htmlOutput("enabler3_choice_text1"), htmlOutput("enabler3_choice_text2")),
        br(),
        div(
          tabsetPanel(
            id = "outcome3_panels",
            type = "tabs",
            tabPanel(
              "Spending",
              fluidRow()
            ),
            tabPanel(
              "Culture focused on outcomes from children and families and continually improving services",
              fluidRow(
                plotlyOutput("plot_ofsted"),
                br(),
              )
            )
          )
        )
      )
    )
  )
}
