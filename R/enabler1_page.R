enabler1_tab <- function() {
  tabPanel(
    value = "enabler1_page",
    "Multi-Agency",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Enabler 1: Multi-agency working is prioritised and effective")
        )
      ),
      br(),
      gov_row(
        br(),
        h2("We will work with the sector and other experts to develop indicators this National Framework enabler."),
        br(),
      )
    )
  )
}
