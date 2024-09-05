data_sources <- function() {
  tabPanel(
    value = "data_sources",
    "Data Sources",
    gov_main_layout(
      br(),
      gov_row(
        h2("Data sources"),
        p("Most data included in the dashboard is children’s social care statistics published by DfE and available on", a(href = "https://explore-education-statistics.service.gov.uk/", "Explore Education Statistics.", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"), " The publications include:"),
        tags$ul(
          tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions", "Children looked after in England including adoptions (last updated:	25 April 2024).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need", "Children in need (last updated: 26 October 2023).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/502/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0", "Public health data explorer (last updated: 08 May 2024).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce", "Children's social work workforce (last updated:	29 February 2024)", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://www.ons.gov.uk/datasets/TS021/editions/2021/versions/3", "ONS - Census, Ethnic groups (last updated: 28 March 2023)", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland", "Estimates of the population for the UK, England, Wales, Scotland, and Northern Ireland (last updated: 26 March 2024)", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://www.gov.uk/government/statistics/childrens-social-care-data-in-england-2023", "Children’s social care data in England 2023 (last updated: 8 September 2023)", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2022-to-2023-individual-local-authority-data-outturn", "Local authority revenue expenditure and financing England: 2022 to 2023 (last updated: 14 February 2024)", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://assets.publishing.service.gov.uk/media/606458acd3bf7f0c8d06b7e2/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx", "Children's services statistical neighbour benchmarking tool (last updated: April 2021)", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;")),
          tags$li(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england", "Outcomes for Children in Need including Children Looked After (last updated 18 April 2024)", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"))
        )
      )
    )
  )
}
