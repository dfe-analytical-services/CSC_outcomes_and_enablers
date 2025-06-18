summary_page_tab <- function() {
  nav_panel(
    value = "summary_page",
    "Summary",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Summary of National Framework indicators")
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
            col_widths = c(6, 6)
          )
        )
      ),
      br(),
      gov_row(
        br(),
        # Confirmation of user selection
        p(htmlOutput("summary_page_choice_text1"), htmlOutput("summary_page_choice_text2")),
        conditionalPanel(
          condition = "(input.geographic_breakdown_sp %in% c('Kingston upon Thames', 'Richmond upon Thames')",
          p("Workforce data is unavailable on the summary page. Kingston upon Thames and Richmond upon Thames submit a joint workforce return each year. The workforce data for the Combined Authorities is reported together on the workforce enabler page")
        ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_sp %in% c('North Northamptonshire','West Northamptonshire')",
          p("Workforce data is unavailable on the summary page. North Northamptonshire and West Northamptonshire submit a joint workforce return each year. The workforce data for the Combined Authorities is reported together on the workforce enabler page.")
        ),
      ),
      # now the main body of the page with 2 tabs containing accordions (4 outcomes, 2 enablers) and domain sections within each.
      # The tables are within the domains
      gov_row(
        br(),
        div(
          div(
            style = "position:absolute;right:1em;margin-top:-20px",
            downloadButton(
              "summary_page_download",
              label = "Download CSV",
              class = "btn btn-default",
              icon = shiny::icon("download")
            ) # actionButton('load_inputs', 'Load inputs') #CSSDownloader
          ),
          tabsetPanel(
            id = "summary_page_panels",
            type = "tabs",
            # Domain 1 --------------
            tabPanel(
              "Outcomes",
              accordion(
                id = "summary_page_outcomes",
                open = TRUE,
                multiple = TRUE,
                # Social Worker Turnover --------------
                accordion_panel(
                  "Outcome 1: Children, young people and families stay together",
                  # add the header row here (generate on the fly based on the user selections )
                  gov_row(
                    # module for a single heading (i.e. pass the data and the parameters, get a heading and a table
                    sp_accordion_cols_ui("outcome1"),
                    sp_domain_ui(id = "Access to support and getting help"),
                    sp_domain_ui(id = "Family stability"),
                    sp_domain_ui(id = "Child wellbeing and development"),
                    sp_domain_ui(id = "Educational attainment"),
                  )
                ),
                accordion_panel(
                  "Outcome 2: Children and young people are supported by their family network",
                  # add the header row here (generate on the fly based on the user selections )
                  gov_row(
                    # module for a single heading (i.e. pass the data and the parameters, get a heading and a table
                    sp_accordion_cols_ui("outcome2"),
                    sp_domain_ui(id = "Families engaging and receiving support from their family network"),
                  )
                ),
                accordion_panel(
                  "Outcome 3: Children and young people are safe in and outside of their home",
                  # add the header row here (generate on the fly based on the user selections )
                  gov_row(
                    # module for a single heading (i.e. pass the data and the parameters, get a heading and a table
                    sp_accordion_cols_ui("outcome3"),
                    sp_domain_ui(id = "Child safety – general"),
                    sp_domain_ui(id = "Child abuse / neglect"),
                    sp_domain_ui(id = "Harms outside the home"),
                  )
                ),
                accordion_panel(
                  "Outcome 4: Children in care and care leavers have stable, loving homes",
                  # add the header row here (generate on the fly based on the user selections )
                  gov_row(
                    # module for a single heading (i.e. pass the data and the parameters, get a heading and a table
                    sp_accordion_cols_ui("outcome4"),
                    sp_domain_ui(id = "Stability and quality of where a child lives"),
                    sp_domain_ui(id = "Child wellbeing"),
                    sp_domain_ui(id = "Quality of life for care experienced people")
                  )
                )
              )
            ),


            # Domain 2 --------------
            tabPanel(
              "Enablers",
              accordion(
                id = "summary_page_enablers",
                open = TRUE,
                multiple = TRUE,
                # Social Worker Turnover --------------
                accordion_panel(
                  "Enabler: Leaders drive conditions for effective practice",
                  # add the header row here (generate on the fly based on the user selections )
                  gov_row(
                    # module for a single heading (i.e. pass the data and the parameters, get a heading and a table
                    sp_accordion_cols_ui("enabler1"),
                    sp_domain_ui(id = "Spending"),
                    sp_domain_ui(id = "Culture focused on outcomes from children and families and continually improving services")
                  )
                ),
                accordion_panel(
                  "Enabler: The workforce is equipped and effective.",
                  # add the header row here (generate on the fly based on the user selections )
                  gov_row(
                    # module for a single heading (i.e. pass the data and the parameters, get a heading and a table
                    sp_accordion_cols_ui("enabler2"),
                    sp_domain_ui(id = "Workforce stability"),
                    sp_domain_ui(id = "Quality of support for children and families"),
                    sp_domain_ui(id = "Social worker ethnicity")
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
