disclaimer_tab <- function() {
  tabPanel(
    value = "disclaimer",
    "Disclaimer",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Disclaimer")
        )
      ),
      br(),
      gov_row(
        p("The CSC Dashboard service displays data indicators to help both local and central
        government understand progress towards the outcomes and enablers set out in the
        Children’s Social Care National Framework. Users can view the progress for England by
        geography (national, regional and local authority) over time, and benchmark performance
        across geographies including local authority statistical neighbours."),
        p("This service has been built based on official statistics which uphold the standards
        of trustworthiness, quality and value as per the Code of Practice for Statistics. All of
        the data used in this service, as well as methodological information and caveats, are
        available transparently at the sources linked from this page.  Information on how each
        indicator has been calculated "),
        p("The data sources used to underpin each indicator by outcome and enabler are children’s
        social care statistics published by DfE and are available on Explore Education
        Statistics.:"),
        p("The publications include: "),
        tags$ul(
          tags$li("Children looked after in England including adoptions (last updated: 25 April 2024)."),
          tags$li("Children in need (last updated: 26 October 2023)."),
          tags$li("Public health data explorer (last updated: 08 May 2024)."),
          tags$li("Children's social work workforce (last updated: 29 February 2024)"),
          tags$li("ONS - Census, Ethnic groups (last updated: 28 March 2023)"),
          tags$li("Estimates of the population for the UK, England, Wales, Scotland, and Northern Ireland (last updated: 26 March 2024)"),
          tags$li("Children’s social care data in England 2023 (last updated: 8 September 2023)"),
          tags$li("Local authority revenue expenditure and financing England: 2022 to 2023 (last updated: 14 February 2024)"),
          tags$li("Children's services statistical neighbour benchmarking tool (last updated: April 2021)"),
          tags$li("Outcomes for children in need including children looked after by local authorities in england (last updated: 18th Apr 2024)")
        ),
        p("The  specific indicators (policy annex) for each outcome and enabler have been
          chosen based on available official published statistics and review with policy.
          The CSC dashboard indictors will be updated when new official statistics become
          available.")
      )
    )
  )
}
