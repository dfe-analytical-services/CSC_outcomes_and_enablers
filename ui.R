# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#

ui <- function(input, output, session) {
  bslib::page(
    # use_tota11y(),
    title = tags$head(
      tags$link(
        rel = "shortcut icon",
        href = "dfefavicon.png"
      ),
      # Add title for browser tabs
      tags$title("Children's Social Care - Outcomes and Enablers")
    ),
    use_shiny_title(),
    tags$html(lang = "en"),
    # Add meta description for search engines
    meta() %>%
      meta_general(
        application_name = "Children's Social Care - Outcomes and Enablers",
        description = "Children's Social Care - Outcomes and Enablers",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "stats development",
        rating = "General",
        referrer = "no-referrer"
      ),
    shinyjs::useShinyjs(),
    dfe_cookies_script(),
    cookies_banner_ui(
      name = "Children's Social Care - Outcomes and Enablers"
    ),
    dfeshiny::custom_disconnect_message(
      links = c(site_primary, site_overflow)
    ),
    # Setting up cookie consent based on a cookie recording the consent:
    # https://book.javascript-for-r.com/shiny-cookies.html
    tags$head(
      tags$script(
        src = paste0(
          "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
          "dist/js.cookie.min.js"
        )
      ),
      tags$script(src = "cookie-consent.js"),
    ),
    tags$head(includeScript("www/downloader.js")),
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    shinyGovstyle::header(
      main_text = "",
      main_link = "https://www.gov.uk/government/organisations/department-for-education",
      secondary_text = "Children's Social Care - Outcomes and Enablers",
      logo = "images/DfE_logo_landscape.png",
      logo_width = 150,
      logo_height = 32
    ),
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      paste0(
        "This is a new service - your <a class = 'govuk-link' href = 'https://forms.office.com/e/dMDRycTXcU'>feedback</a> will help us to improve it."
      )
    ),
    shiny::navlistPanel(
      "",
      id = "navlistPanel",
      widths = c(2, 8),
      well = FALSE,
      introductionPanel(),
      "Outcomes",
      outcome1_tab(),
      outcome2_tab(),
      outcome3_tab(),
      outcome4_tab(),
      "Enablers",
      enabler1_tab(),
      enabler2_tab(),
      enabler3_tab(),
      "Support",
      disclaimer_tab(),
      omitted_data_tab(),
      data_sources_tab(),
      a11y_panel(),
      shiny::tabPanel(
        value = "cookies_panel_ui",
        "Cookies",
        gov_main_layout(
          cookies_panel_ui(google_analytics_key = google_analytics_key)
        )
      ),
      shiny::tabPanel(
        value = "support_panel",
        "Support and feedback",
        gov_main_layout(
          dfeshiny::support_panel(
            team_email = "CSCDashboard.FEEDBACK@education.gov.uk",
            repo_name = "https://github.com/dfe-analytical-services/CSC_outcomes_and_enablers",
            form_url = "https://forms.office.com/e/dMDRycTXcU",
            custom_data_info = tagList(
              p(
                "Most data included in the dashboard is children’s social care statistics published by DfE and available on",
                external_link(
                  href = "https://explore-education-statistics.service.gov.uk/",
                  "Explore Education Statistics"
                ),
                ". The publications include:"
              ),
              tags$ul(
                tags$li(external_link(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after in England including adoptions (last updated:	25 April 2024)")),
                tags$li(external_link(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need", "Children in need (last updated: 26 October 2023)")),
                tags$li(external_link(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/502/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0", "Public health data explorer (last updated: 08 May 2024)")),
                tags$li(external_link(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce", "Children's social work workforce (last updated:	29 February 2024)")),
                tags$li(external_link(href = "https://www.ons.gov.uk/datasets/TS021/editions/2021/versions/3", "ONS - Census, Ethnic groups (last updated: 28 March 2023)")),
                tags$li(external_link(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland", "Estimates of the population for the UK, England, Wales, Scotland, and Northern Ireland (last updated: 26 March 2024)")),
                tags$li(external_link(href = "https://www.gov.uk/government/statistics/childrens-social-care-data-in-england-2023", "Children’s social care data in England 2023 (last updated: 8 September 2023)")),
                tags$li(external_link(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2022-to-2023-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2022 to 2023 (last updated: 14 February 2024)")),
                tags$li(external_link(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "Children's services statistical neighbour benchmarking tool (last updated: April 2021)"))
              )
            )
          )
        )
      ),
      tutorialPanel(),
    ),
    tags$div(
      style = "postion: relative; text-align: center; margin-bottom: 50px;",
      tags$a(href = "#top", "Return to top")
    ),
    footer(full = TRUE)
  )
}
