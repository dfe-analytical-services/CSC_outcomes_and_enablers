# This page is a hidden page for the User Guide
introductionPanel <- function() {
  nav_panel(
    value = "intro_panel",
    title = "Introduction",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Introduction"),
          p(
            "This dashboard displays data indicators to help both local and central government understand progress towards the outcomes and enablers set out in the",
            a(href = "https://www.gov.uk/government/publications/childrens-social-care-national-framework", "Children’s Social Care National Framework (opens in a new tab).", target = "_blank", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"),
            "You can view the progress for England nationally or choose a specific region or local authority in England to view its progress.",
            actionLink("go_to_user_guide", "Read the user guide"), "for further information on how to do this after reading this introduction page."
          ),
          p("The data indicators included in the dashboard are contained within domains (themes). The domains sit within the below enablers and outcomes."),
        ),
      ),
      gov_row(
        div(
          tabsetPanel(
            id = "intro_page_panels",
            type = "tabs",
            tabPanel(
              "Visual Guide",
              tags$img(id = "national_framework_image", src = "images/national_framework_visual_guide.jpg", alt = "National Framework Visual Guide")
            ),
            tabPanel(
              "Text-only Version",
              gov_row(
                column(
                  width = 12,
                  h2("Outcomes"),
                  p("The outcomes included in the dashboard are what children’s social care should achieve for the children, young people and families they support.
          They reflect the core purpose of children’s social care."),
                  # outcome 1
                  h4("Children, young people and families stay together and get the help they need"),
                  h5("Access to support and getting help"),
                  tags$ul(
                    tags$li("Rate of children in need per 10k"),
                    tags$li("Repeat referrals (within 12 months)"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Family stability"),
                  tags$ul(
                    tags$li("Rate of children starting to be looked after per 10k"),
                    tags$li("Rate of children looked after who were UASC per 10k"),
                    tags$li("Rate of children starting to be looked who were UASC per 10k"),
                    tags$li("Rate of children looked after on 31 March per 10k"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Child wellbeing and development"),
                  tags$ul(
                    tags$li("Overall Absence for CINO, CPPO and CLA on 31 March by school type"),
                    tags$li("Persistent absentees for CINO, CPPO and CLA at 31 March (overall absence 10% or more) by school type"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Educational attainment"),
                  tags$ul(
                    tags$li("Pupils (CINO, CPPO, CLA) achieving expected standard in reading, writing and maths combined (KS2)"),
                    tags$li("Average attainment 8 (CINO, CPPO, CLA) at 31 March (KS4)"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  # outcome 2
                  h4("Children and young people are supported by their network"),
                  h5("Families engaging and receiving support from their family network"),
                  tags$ul(
                    tags$li("% children who cease to be looked after due to special guardianship order"),
                    tags$li("% children who cease to be looked after due to residence order or child arrangement order"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  # outcome 3
                  h4("Children and young people are safe in and outside of their home"),
                  h5("Child safety - general"),
                  tags$ul(
                    tags$li("% of child protection plans starting during year which were a second or subsequent plan"),
                    tags$li("% of child protection plans longer than 2 years"),
                    tags$li("Hospital admissions caused by unintentional and deliberate injuries to children and young people per 10K (0-14 years)"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Child abuse / neglect"),
                  tags$ul(
                    tags$li("Factors identified at the end of assessment in the year to 31 March related to child abuse or neglect per 10k by factor"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Harms outside the home"),
                  tags$ul(
                    tags$li("Factors identified at the end of assessment in the year to 31 March related to specific types of harms outside the home per 10k by factor"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  # outcome 4
                  h4("Children in care and care leavers have stable, loving homes"),
                  h5("Stability and quality of where a child lives"),
                  tags$ul(
                    tags$li("% children looked after with 3 or more placements during the year"),
                    tags$li("% children looked after placed more than 20 miles from home"),
                    tags$li("Average number of months between placement order and match for children who are adopted"),
                    tags$li("% children looked after on 31 March living in foster care"),
                    tags$li("% children looked after living in secure homes and children's homes"),
                    tags$li("% children looked after living in independent and semi-independent living arrangements / supported accommodation"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Child wellbeing"),
                  tags$ul(
                    tags$li("Average SDQ score"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Quality of life for care experienced people"),
                  tags$ul(
                    tags$li("% care leavers in education, employment or training (17 to 18 years)"),
                    tags$li("% care leavers in accommodation considered suitable (17 to 18 years)"),
                    tags$li("% care leavers in education, employment or training (19 - 21 years)"),
                    tags$li("% care leavers in accommodation considered suitable (19 to 21 years)"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                ),
              ),
              ### enablers text ----
              gov_row(
                column(
                  width = 12,
                  h2("Enablers"),
                  p("The enablers included in the dashboard refer to aspects of the children's social care system that facilitate effective support for children, young people and families. The enablers are foundational to good practice."),
                  h4("Multi-agency working is prioritised and effective"),
                  p("We will work with the sector and other experts to develop indicators"),
                  # enabler 2
                  h4("Leaders drive conditions for effective practice"),
                  h5("Spending"),
                  tags$ul(
                    tags$li("Share of total local authority spend on children's services"),
                    tags$li("Average per capita (of all children in a local authority) spend on children's services"),
                    tags$li("Share of children's services spend not on CLA"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Culture focused on outcomes from children and families and continually improving services"),
                  tags$ul(
                    tags$li("Ofsted leadership rating"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  # enabler 3
                  h4("The workforce is equipped and effective"),
                  h5("Workforce stability"),
                  tags$ul(
                    tags$li("Turnover rate (FTE)"),
                    tags$li("Agency worker rate (FTE)"),
                    tags$li("Vacancy rate (FTE)"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Quality of support for children and families"),
                  tags$ul(
                    tags$li("Average caseload"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                  h5("Social worker ethnicity"),
                  tags$ul(
                    tags$li("Ethnicity (headcount)"),
                    tags$li("Ethnicity (headcount) vs general population ethnicity"),
                    tags$li("Ethnicity (headcount) by role seniority"),
                    style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"
                  ),
                ),
              ),
            )
          )
        )
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
