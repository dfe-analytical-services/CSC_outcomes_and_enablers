outcome3_tab <- function() {
  tabPanel(
    value = "outcome3_page",
    "Outcome 3",
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
              choices = unique(dropdown_choices %>% pull("geographic_level")),
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
      gov_row(
        br(),
        h2("Confirmation Sentence"),
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
                    title = "Percentage of Child Protection Plans starting during year, which were a second or subsequent plan",
                    value = htmlOutput("cpp_in_year_txt")
                  )
                ),
                br(),
              ),
              accordion(
                accordion_panel(
                  "Percentage of Child Protection Plans (CPP) longer than 2 years, and repeat CPP",
                  # p("plots go here"),
                  plotlyOutput("cpp_time_series"),
                  br(),
                  # Expandable for the table alternative
                  details(
                    inputId = "tbl_cpp_in_year",
                    label = "View chart as a table",
                    help_text = (
                      dataTableOutput("")
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
                  )
                ),
              ),
            ),
            tabPanel(
              "Child abuse / neglect",
              fluidRow(
                p("testing")
              )
            ),
            tabPanel(
              "Harms outside the home",
              fluidRow(
                p("testing")
              )
            )
          )
        )
      )
    )
  )
}
