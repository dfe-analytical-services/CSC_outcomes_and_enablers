enabler1_tab <- function() {
  tabPanel(
    value = "enabler1_page",
    "Multi-agency",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Enabler: Multi-agency working is prioritised and effective")
        )
      ),
      br(),
      gov_row(
        br(),
        h2("We will work with the sector and other experts to develop indicators for this National Framework enabler."),
        br(),
      )
    )
  )
}
