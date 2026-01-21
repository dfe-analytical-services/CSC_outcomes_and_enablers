support_links <- function() {
  nav_panel(
    "Support and feedback",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Support and feedback"),
          h2("Give us feedback"),
          p(
            "If you have any feedback or suggestions for improvements, submit them using our ",
            a(
              href = "https://forms.office.com/e/dMDRycTXcU", target = "_blank",
              "feedback form (opens in a new tab)", .noWS = c("after")
            ), "."
          ),
          p(
            "If you spot any errors or bugs while using this dashboard, screenshot and email them to",
            a(href = "mailto:CSCDashboard.FEEDBACK@education.gov.uk", target = "_blank", "CSCDashboard.FEEDBACK@education.gov.uk", .noWS = c("after")), "."
          ),
          h2("Find more information on the data"),
          p("Most data included in the dashboard is children’s social care statistics published by DfE and available on", a(href = "https://explore-education-statistics.service.gov.uk/", "Explore Education Statistics.", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"), " The publications include:"),
          tags$ul(
            tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after in England including adoptions (last updated:	26 November 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need", "Children in need (last updated: 30 October 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/502/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0", "Public health data explorer (last updated: 08 May 2024) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce", "Children's social work workforce (last updated:	29 February 2024) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://www.ons.gov.uk/datasets/TS021/editions/2021/versions/3", "ONS - Census, Ethnic groups (last updated: 28 March 2023) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales", "Estimates of the population for the UK, England, Wales, Scotland, and Northern Ireland (last updated: 30 July 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://www.gov.uk/government/collections/childrens-social-care-statistics", "Children’s social care data in England 2025 (last updated: 25 September 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2024-to-2025-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2024 to 2025 (last updated: 4 December 2025) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "Children's services statistical neighbour benchmarking tool (last updated: April 2021) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england", "Outcomes for children in need including children looked after (last updated 18 April 2024) (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"))
          ),
          h2("Contact us"),
          p(
            "If you have questions about the dashboard or data within it, contact us at ",
            a(href = "mailto:CSCDashboard.FEEDBACK@education.gov.uk", target = "_blank", "CSCDashboard.FEEDBACK@education.gov.uk", .noWS = c("after"))
          ),
          h2("See the source code"),
          p(
            "The source code for this dashboard is available in our ",
            a(href = "https://github.com/dfe-analytical-services/CSC_outcomes_and_enablers", target = "_blank", "GitHub repository (opens in a new tab)", .noWS = c("after")),
            "."
          )
        )
      )
    )
  )
}
