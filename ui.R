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
    shinyGovstyle::banner(
      "banner",
      "",
      paste0(
        "We have updated the Dashboard with the latest published data for Children in Need (1st Nov 2024), Children Looked After (14th Nov 2024) and Ofsted Leadership Ratings (up to 30th August 2024)"
      )
    ),
    bslib::navset_pill_list(
      id = "navlistPanel",
      well = FALSE,
      fluid = TRUE,
      widths = c(2, 10),
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
        cookies_panel_ui(google_analytics_key = google_analytics_key)
      ),
      support_links(),
      tutorialPanel(),
    ),
    tags$div(
      style = "postion: relative; text-align: center; margin-bottom: 50px;",
      tags$a(href = "#top", "Return to top")
    ),
    footer(full = TRUE)
  )
}
