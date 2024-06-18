a11y_panel <- function() {
  tabPanel(
    "Accessibility",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Accessibility statement"),
          p("This accessibility statement applies to the Children's Social Care - Outcomes and Enablers dashboard.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind.", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"),
          h2("WCAG 2.1 compliance"),
          p("We follow the recommendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. ", onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"),
          tags$div(tags$ul(
            tags$li("uses colours that have sufficient contrast"),
            tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
            tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users"),
            style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"
          )),
          h2("Limitations"),
          p("We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"),
          tags$div(tags$ul(
            tags$li("Screen reader and keyboard users cannot navigate through the interactive graphs effectively. An accessible alternative table is provided to view and a CSV format is available for users to download."),
            tags$li("Some users may have difficultly reading the graph due to the use of colour. An accessible alternative table is provided to view and a CSV format is available for users to download."),
            style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"
          )),
          h2("Feedback"),
          p(
            "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
            a(href = "mailto:CSCDashboard.FEEDBACK@education.gov.uk", "CSCDashboard.FEEDBACK@education.gov.uk"),
            style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"
          )
        )
      )
    )
  )
}

support_links <- function() {
  tabPanel(
    "Support and feedback",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Support and feedback"),
          h2("Give us feedback"),
          p(
            "If you have any feedback or suggestions for improvements, please submit them using our ",
            a(
              href = "https://forms.office.com",
              "feedback form", .noWS = c("after")
            ), "."
          ),
          p(
            "If you spot any errors or bugs while using this dashboard, please screenshot and email them to",
            a(href = "mailto:CSCDashboard.FEEDBACK@education.gov.uk", "CSCDashboard.FEEDBACK@education.gov.uk", .noWS = c("after")), "."
          ),
          h2("Find more information on the data"),
          p("Most data included in the dashboard is children’s social care statistics published by DfE and available on", a(href = "https://explore-education-statistics.service.gov.uk/", "Explore Education Statistics.", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"), " The publications include:"),
          tags$ul(
            tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after in England including adoptions (last updated:	25 April 2024).", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need", "Children in need (last updated: 26 October 2023).", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/502/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0", "Public health data explorer (last updated: 08 May 2024).", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce", "Children's social work workforce (last updated:	29 February 2024)", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://www.ons.gov.uk/datasets/TS021/editions/2021/versions/3", "ONS - Census, Ethnic groups (last updated: 28 March 2023)", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland", "Estimates of the population for the UK, England, Wales, Scotland, and Northern Ireland (last updated: 26 March 2024)", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://www.gov.uk/government/statistics/childrens-social-care-data-in-england-2023", "Children’s social care data in England 2023 (last updated: 8 September 2023)", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2022-to-2023-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2022 to 2023 (last updated: 14 February 2024)", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
            tags$li(a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "Children's services statistical neighbour benchmarking tool (last updated: April 2021)", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"))
          ),
          h2("Contact us"),
          p(
            "If you have questions about the dashboard or data within it, please contact us at ",
            a(href = "mailto:CSCDashboard.FEEDBACK@education.gov.uk", "CSCDashboard.FEEDBACK@education.gov.uk", .noWS = c("after"))
          ),
          h2("See the source code"),
          p(
            "The source code for this dashboard is available in our ",
            a(href = "https://github.com/dfe-analytical-services/CSC_outcomes_and_enablers", "GitHub repository", .noWS = c("after")),
            "."
          )
        ),
        column(
          12,
          h2("Use of cookies"),
          textOutput("cookie_status"),
          actionButton("remove", "Reset cookie consent"),
        )
      )
    )
  )
}
