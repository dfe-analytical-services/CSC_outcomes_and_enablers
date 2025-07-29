omitted_data_tab <- function() {
  nav_panel(
    value = "omitted_data",
    "Omitted data reasons",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Omitted data reason codes"),
        )
      ),
      br(),
      gov_row(
        tags$ul(
          tags$li("c - confidential data which has been suppressed.", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"),
          tags$li("k - rounds to 0 but is not 0.", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"),
          tags$li("u – observation is of low reliability.", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"),
          tags$li("x - data is unavailable for other reason(s).", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"),
          tags$li("z - observation is not applicable.", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")
        )
      )
    )
  )
}
