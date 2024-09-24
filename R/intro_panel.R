# This page is a hidden page for the User Guide
introductionPanel <- function() {
  tabPanel(
    value = "intro_panel",
    title = "Introduction",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          heading_text("Introduction", size = "l"),
          p(
            "This dashboard displays data indicators to help both local and central government understand progress towards the outcomes and enablers set out in the",
            a(href = "https://www.gov.uk/government/publications/childrens-social-care-national-framework", "Children’s Social Care National Framework.", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"),
            "You can view the progress for England nationally or choose a specific region or local authority in England to view its progress.",
            actionLink("go_to_user_guide", "Read the user guide"), "for further information on how to do this after reading this introduction page."
          ),
          p("The data indicators included in the dashboard are contained within domains (themes). The domains sit within the below enablers and outcomes."),
        ),
      ),
      gov_row(
        layout_columns(
          column(
            width = 12,
            h2("Outcomes"),
            p("The outcomes included in the dashboard are what children’s social care should achieve for the children, young people and families they support.
              They reflect the core purpose of children’s social care. The outcomes included are:"),
            tags$ol(
              tags$li("Children, young people and families stay together and get the help they need"),
              tags$li("Children and young people are supported by their family network"),
              tags$li("Children and young people are safe in and outside of their home"),
              tags$li("Children in care and care leavers have stable, loving homes"),
              style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
            ),
          ),
          column(
            width = 12,
            h2("Enablers"),
            p("The enablers included in the dashboard refer to aspects of the children’s social care system that facilitate effective support for children, young people and families.
              The enablers are foundational to good practice. The enablers included are:"),
            tags$ul(
              tags$li("Multi-agency working is prioritised and effective"),
              tags$li("Leaders drive conditions for effective practice"),
              tags$li("The workforce is equipped and effective"),
              style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
            ),
          )
        ),
      ),
      gov_row(
        h2("Use and limitations"),
        p("This dashboard is intended to be used by local authorities, regions, safeguarding partners, central government and the general public."),
        # br(),
        p("This dashboard is not intended to be a tool to measure local authority performance on children’s social care.
          The dashboard and the data included is intended to be the start of a conversation around understanding the outcomes and enablers and generating learning to improve practice with children, young people and families.
          The dashboard will not prompt inspection, and this first iteration does not include any new data that is not already in the public domain. The limitations of the data in not fully measuring the outcomes are recognised.
          The indicators will evolve over time as a more robust outcomes-based set of measures is developed."),
        # br(),
        p("This dashboard allows for comparisons to be made between local authorities; however, all data is provided with the caveat that the system is complex, and that indicators and trends should not be viewed in isolation.
          Specific data limitations are included in the dashboard."),
      ),
    )
  )
}
