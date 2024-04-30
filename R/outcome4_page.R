outcome4_tab <- function() {
  tabPanel(
    value = "outcome4_page",
    "Outcome 4",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Outcome 4: Children in care and care leavers have stable, loving homes")
        )
      ),
      gov_row(
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_o4",
              label = "Select a geographical level:",
              # Change this to look at the relevant dataset for outcome 4
              choices = unique(placement_data %>% pull("geographic_level")),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(
              condition = "input.select_geography_o4 != 'National'",
              selectizeInput(
                inputId = "geographic_breakdown_o4",
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
              condition = "input.select_geography_o4 != 'National'",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "national_comparison_checkbox_o4",
                  cb_labels = "Compare with National",
                  checkboxIds = "Yes_national_o4",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "(input.select_geography_o4 == 'Local authority')",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_o4",
                  cb_labels = "Compare with Region",
                  checkboxIds = "Yes_region_o4",
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
        h2("Confirmation Sentence"),
        br(),
        div(
          tabsetPanel(
            id = "outcome4_panels",
            type = "tabs",
            # Domain 1 ----
            tabPanel(
              "Stability and quality of where a child lives",
              fluidRow(
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "% CLA on 31 March with 3 or more placements during the year",
                    value = p("Headline stats 1")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "% CLA on 31 March placed more than 20 miles from home",
                    value = p("Headline stats 2")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Average time between an LA receiving court authority to place a child and the LA deciding on a match to an adoptive family? Q1-Q4 2022/23",
                    value = p("stats")
                  )
                ),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "% CLA on 31 March living in foster placements",
                    value = htmlOutput("foster_placement_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "% living in in secure units, children's homes or semi-independent living accommodation",
                    value = htmlOutput("secure_unit_placement_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "% living in other residential settings",
                    value = htmlOutput("residential_placement_txt"),
                  )
                ),
                br(),
                p("Care leavers should be supported to access education, employment and
                  training that supports them and allows them to achieve their aspirations and goals.")
              ),
              accordion(
                accordion_panel(
                  "Average number of placement changes children have",
                  p("contents for panel 1"),
                  gov_row(
                    h2("Time Series")
                  ),
                  gov_row(
                    h2("By Region")
                  ),
                  gov_row(
                    h2("By local authority")
                  )
                ),
                accordion_panel(
                  "Distance of placements from home",
                  p("contents for panel 2"),
                  gov_row(
                    h2("Time Series")
                  ),
                  gov_row(
                    h2("By Region")
                  ),
                  gov_row(
                    h2("By local authority")
                  )
                ),
                accordion_panel(
                  "Percentage of children living in foster, residential care, or secure children’s homes",
                  gov_row(
                    # Box here to have an extra dropdown just for this section to split the percentage for each placement
                    p("This indicator contains breakdowns of data for the following assessment factors: ", paste(unique(placement_type_filter %>% str_sort()), collapse = ", ")),
                    div(
                      class = "input_box",
                      style = "min-height:100%; height = 100%; overflow-y: visible",
                      p("Please use the dropdown below to select which placement type you would like to see in this accordion:"),
                      selectizeInput(
                        inputId = "placement_type_breakdown",
                        label = "Select a placement type:",
                        choices = unique(placement_type_filter %>% str_sort()),
                        selected = NULL,
                        multiple = FALSE,
                        options = NULL
                      ),
                    ),
                    br(),
                  ),
                  gov_row(
                    plotlyOutput("placement_type_ts_plot"),
                    br(),
                    details(
                      inputId = "tbl_placement_type",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("placement_type_tbl")
                      )
                    ),
                  ),
                  gov_row(
                    h2("By Region")
                  ),
                  gov_row(
                    h2("By local authority")
                  )
                ),
                accordion_panel(
                  "Average time between placement order and match for those children who are adopted",
                  p("contents for panel 4"),
                  gov_row(
                    h2("Time Series")
                  ),
                  gov_row(
                    h2("By Region")
                  ),
                  gov_row(
                    h2("By local authority")
                  )
                ),
                open = FALSE
              )
            ),
            # Domain 2 -----
            tabPanel(
              "Wellbeing of child",
              fluidRow(
                p("testing"),
                br()
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Headline stat 1",
                    value = p("Headline stats 1")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Headline stat 2",
                    value = p("Headline stats 2")
                  )
                ),
                br(),
              ),
              fluidRow(
                br()
              ),
              accordion(
                accordion_panel(
                  "Strengths and difficulties questionnaire (SDQ score)",
                  p("contents for panel 1"),
                  p("Understanding the emotional and behavioural need of CLA is important to ensure that they are receiving the care and support they need to thrive. The SDQ score uses a series of carer-reported measures to calculate an overall score to assess the emotional wellbeing of CLA."),
                  gov_row(
                    h2("Time Series")
                  ),
                  gov_row(
                    h2("By Region")
                  ),
                  gov_row(
                    h2("By local authority")
                  )
                ),
                open = FALSE
              )
            ),
            # Domain 3 ------
            tabPanel(
              "Quality of life for care experienced people",
              fluidRow(
                p("testing"),
                br()
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Care leavers % Total in education, employment or training (17 to 18)",
                    value = p("Headline stats 1")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Care leavers % Total in education, employment or training (19 to 21)",
                    value = p("Headline stats 2")
                  )
                ),
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Care leavers % Accommodation considered suitable as quite a few LAs supressed for not suitable. (17 to 18)",
                    value = p("headline stats 3")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Care leavers % Accommodation considered suitable as quite a few LAs supressed for not suitable. (19 to 21)",
                    value = p("headline stats 4")
                  )
                ),
                br(),
                p("Care leavers should be supported to access education, employment and
                  training that supports them and allows them to achieve their aspirations and goals.")
              ),
              fluidRow(
                div(
                  class = "input_box",
                  style = "min-height:100%; height = 100%; overflow-y: visible",
                  # p("This domain contains breakdowns of data for the following assessment factors: ", paste(unique(af_child_abuse_extra_filter %>% str_sort()), collapse = ", "), "."),
                  p("Please use the dropdown below to select which age range of care leavers you would like to see in the accordions below:"),
                  selectizeInput(
                    inputId = "leavers_age",
                    label = "Select an age range:",
                    choices = c("17 to 18", "19 to 21"), # unique(af_child_abuse_extra_filter %>% str_sort()),
                    selected = NULL,
                    multiple = FALSE,
                    options = NULL
                  ),
                ),
                br()
              ),
              fluidRow(
                br()
              ),
              accordion(
                accordion_panel(
                  "Care leavers employment, education and training rate",
                  p("contents for panel 1"),
                  gov_row(
                    h2("Time Series")
                  ),
                  gov_row(
                    h2("By Region")
                  ),
                  gov_row(
                    h2("By local authority")
                  )
                ),
                accordion_panel(
                  "Percentage of care leavers in unsuitable accommodation",
                  p("contents for panel 2"),
                  gov_row(
                    h2("Time Series")
                  ),
                  gov_row(
                    h2("By Region")
                  ),
                  gov_row(
                    h2("By local authority")
                  )
                ),
                open = FALSE
              )
            ),
          )
        )
      )
    )
  )
}
