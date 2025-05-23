summary_page_tab <- function() {
  nav_panel(
    value = "summary_page",
    "Summary Page",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Summary Page")
        )
      ),
      gov_row(
        # Input boxes for geographic level and geographic breakdown
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_sp",
              label = "Select a geographical level:",
              choices = unique(cla_rates %>% filter(geographic_level != "Statistical neighbours (median)") %>% pull("geographic_level")),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(condition = "input.select_geography_sp != 'National'", selectizeInput(
              inputId = "geographic_breakdown_sp",
              label = "Select a location: ",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = NULL
            )),
            panel(),
            col_widths = c(4, 4, 4)
          )
        )
      ),
      br(),
      gov_row(
        br(),
        # Confirmation of user selection
        p(htmlOutput("summary_page_choice_text1"), htmlOutput("summary_page_choice_text2")),
        conditionalPanel(
          condition = "(input.geographic_breakdown_sp == 'Cumbria')",
          p("Cumbria are still in the latest statistics because they relate to the year ending 31 March 2023. Cumbria local authority was replaced with two new unitary authorities, Cumberland and Westmorland and Furness, in April 2023.")
        ),
      ),
      gov_row(
        br(),
        div(
          tabsetPanel(
            id = "summary_page_panels",
            type = "tabs",
            # Domain 1 --------------
            tabPanel(
              "Outcomes",
              fluidRow(
                br()
              ),
              fluidRow(
                reactableOutput("tbl_summary_page_outcomes")
              ),
            ),

            # Domain 2 --------------
            tabPanel(
              "Enablers",
              fluidRow(
                br()
              ),
              fluidRow(
                reactableOutput("tbl_summary_page_enablers")
              )
            )
          )
        )
      )
    )
  )
}
