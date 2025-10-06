disclaimer_tab <- function() {
  nav_panel(
    value = "disclaimer",
    "Disclaimer",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Disclaimer"),
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
        available transparently at the sources linked from this page."),
        p("The data sources used to underpin each indicator by outcome and enabler are children’s
        social care statistics published by DfE and are available on Explore Education
        Statistics:"),
        p("The publications include: "),
        tags$ul(
          tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after in England including adoptions (last updated:	14 November 2024) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need", "Children in need (last updated: 	1 November 2024) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/502/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0", "Public health data explorer (last updated: 04 February 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce", "Children's social work workforce (last updated:	27 February 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://www.ons.gov.uk/datasets/TS021/editions/2021/versions/3", "ONS - Census, Ethnic groups (last updated: 28 March 2023) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales", "Estimates of the population for the UK, England, Wales, Scotland, and Northern Ireland (last updated: 30 July 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://www.gov.uk/government/collections/childrens-social-care-statistics", "Children’s social care data in England 2024 (last updated: 25 September 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2024-to-2025-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2024 to 2025 (last updated: 18 September 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "Children's services statistical neighbour benchmarking tool (last updated: May 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/2024", "Outcomes for children in need including children looked after (last updated 10 April 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"))
        ),
        p("The  specific indicators ", a(href = "https://assets.publishing.service.gov.uk/media/670699d892bb81fcdbe7b5c3/Children_s_social_care_dashboard_supporting_information_October_2024.pdf", "(Children's social care dashboard supporting information (publishing.service.gov.uk)(opens in a new tab))", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"), " for each outcome and enabler have been
          chosen based on available official published statistics and review with policy.
          The CSC dashboard indictors will be updated when new official statistics become
          available.")
      )
    )
  )
}
