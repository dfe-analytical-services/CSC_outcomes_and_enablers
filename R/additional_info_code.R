# Utility function for getting the additional info help_text from a database.



# very simple accessor function to get the additional information for a section/indicator
get_additional_info <- function(id) {
  add_info <- additional_info_help_text[[id]]
  if (is.null(add_info)) add_info <- ""

  return(add_info)
}






# this is the database of additional information
additional_info_help_text <- list(
  "caseload" =
    tags$ul(
      tags$li("Full-time Equivalent (FTE) figures are calculated by aggregating the total number of hours that social workers are contracted to work and dividing by the standard hours for their grade. FTE figures exclude social workers for whom FTE information was missing or not known."),
      tags$li("Average caseload at 30 September per year is calculated as the total number of cases held by FTE social workers, including agency workers, in post divided by the number of FTE social workers, including agency workers, in post that held one or more cases."),
      tags$br(),
      p(
        "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance.", target = "_blank"),
        tags$br(),
        "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology.", target = "_blank")
      )
    ),
  "hospital_admissions" =
    tags$ul(
      tags$li("All sub national counts are rounded to the nearest 5. Rates are calculated using unrounded counts."),
      tags$li("For time points prior to 2012, all values between 1 and 5 have been suppressed and, where necessary, other LAs and comparators have also been suppressed in order to prevent possible disclosure and disclosure by differencing."),
      tags$li("For time points from 2012, all sub national counts are rounded to the nearest 5, and counts of 1 to 7 are suppressed. Rates and confidence intervals are calculated using unrounded counts."),
      tags$li("Values relating to City of London and Isles of Scilly have been combined with Hackney and Cornwall respectively."),
      tags$li(
        "In 2023, NHS England announced a ",
        a(
          href = "https://eur03.safelinks.protection.outlook.com/?url=https:%2f%2fdigital.nhs.uk%2fdata-and-information%2ffind-data-and-publications%2fstatement-of-administrative-sources%2fmethodological-changes%2fimpact-of-changes-to-recording-of-same-day-emergency-care-activity-to-hospital-episode-statistics-hes-data&data=05%7c02%7cLaura.Powell%40dhsc.gov.uk%7c22feb52393f04a2270bc08dc587f7b14%7c61278c3091a84c318c1fef4de8973a1c%7c1%7c0%7c638482551742842317%7cUnknown%7cTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7c0%7c%7c%7c&sdata=Ic0fzE6wEChYvL5zD4LnrOnvsXvYJ%2Bwkql7DoTnjRY4%3D&reserved=0",
          "methodological change", target = "_blank"
        ),
        " to require Trusts to report Same Day Emergency Care (SDEC) to the Emergency Care Data Set (ECDS) by July 2024. Early adopter sites began to report SDEC to ECDS from 2021/22, with other Trusts changing their reporting in 2022/23 or 2023/24. Some Trusts had previously reported this activity as part of the Admitted Patient Care data set, and moving to report to ECDS may reduce the number of admissions reported for this indicator. NHSE have advised it is not possible accurately to identify SDEC in current data flows, but the impact of the change is expected to vary by diagnosis, with indicators related to injuries and external causes potentially most affected."
      ),
      tags$li(
        "When considering if SDEC recording practice has reduced the number of admissions reported for this indicator at local level, please refer to the ",
        a(
          href = "https://eur03.safelinks.protection.outlook.com/?url=https:%2f%2fdigital.nhs.uk%2fdata-and-information%2fdata-collections-and-data-sets%2fdata-sets%2femergency-care-data-set-ecds%2fsame-day-emergency-care&data=05%7c02%7cLaura.Powell%40dhsc.gov.uk%7c22feb52393f04a2270bc08dc587f7b14%7c61278c3091a84c318c1fef4de8973a1c%7c1%7c0%7c638482551742856428%7cUnknown%7cTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7c0%7c%7c%7c&sdata=qiwVNfRx2vQW6ZLF0CKyGJL2mqmLgt%2fZWKiqa8ufy18%3D&reserved=0",
          "published list", target = "_blank"
        ),
        " of sites who have reported when they began to report SDEC to ECDS."
      ),
      tags$br(),
      p(
        "For more information on the data, please refer to the", a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/502/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0", "Public health data explorer.", target = "_blank"),
        tags$br(),
        "For more information on the definitions and methodology, please refer to the ", a(href = "https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/6/gid/1938133230/pat/159/par/K02000001/ati/15/are/E92000001/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1", "Indicator definitions and supporting information page.", target = "_blank")
      )
    ),
  "school_stability" = tags$ul(
    tags$li("Percentages rounded to the nearest whole number."),
    tags$li("Historical data may differ from older publications which is mainly due to amendments made by local authorities after the previous publication. However, users looking for a longer time series may wish to check for the equivalent table which can be found in earlier releases of the publication 'Children Looked After in England including Adoptions'. Figures exclude children looked after under a series of short-term placements"),
    tags$li("The summer census in 2020 was not carried out due to the coronavirus pandemic. Therefore it has not been possible to produce data for 2020, and 2021 data excludes children aged 15 at the beginning of the 2019/20 academic year."),
    tags$br()
  ),
  "severe_absence" =
    tags$ul(
      tags$li(
        "A pupil is identified as severely absent if they miss 50% or more of possible sessions. For further information see ",
        a(href = "https://explore-education-statistics.service.gov.uk/methodology/pupil-absence-in-schools-in-england#section3-2", "3.2 Overall absence methodology.", target = "_blank"),
      ),
      tags$li(
        "No absence data relating to the full 2019/20 academic year is available due to COVID-19.
                                  Due to the disruption during the 2020/21 and 2021/22 academic years, caution should be taken when comparing data to previous years. For more detailed information on this see ",
        a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England.", target = "_blank"),
      ),
      tags$li("CINO refers to children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed."),
      tags$li("CPPO refers to children on a child protection plan, excluding children looked after."),
      tags$li("CLA refers to Children Looked After (excludes children who are in respite care in their most recent episode during the reporting year)."),
      tags$li("Children in need data is not available for Hackney local authority for both the 2020 to 2021 and 2021 to 2022 collection years and Hampshire local authority for the 2023 to 2024 collection year. Hackney was unable to provide a return for both the 2021 and 2022 children in need census collections, due to a cyberattack which had a significant impact on their management information systems. Hampshire provided a CIN return for the 2024 collection, however, due to a transition to a new case management and reporting system, there were significant data quality issues affecting the coverage of Hampshire's 2024 return. Refer to the methodology section for more information."),
      tags$br(),
      p(
        "For more information on the data and definitions, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england/data-guidance", "Outcomes for children in need, including children looked after data guidance.", target = "_blank"),
        tags$br(),
        "For more information on the methodology, refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england-methodology", "Outcomes for children in need, including children looked after methodology.", target = "_blank")
      )
    ),
  "sw_stability" =
    tags$ul(
      tags$li("Percentages rounded to the nearest whole number. Figures exclude children looked after under a series of short-term placements."),
      tags$li("Social workers are only counted once for each child during the year. For example, if a child had a social worker, moved to a different social worker, then back to the original social worker during the course of the year, then this would count as 2 social workers during the year."),
      tags$br()
    )
)
