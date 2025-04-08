test_that("{shinytest2} recording: CSC_outcomes_and_enablers_error_test", {
  app <- AppDriver$new(
    name = "CSC_outcomes_and_enablers_error_test",
    load_timeout = 240 * 1000,
    timeout = 120 * 1000,
    wait = FALSE,
    height = 886, width = 1203,
    variant = NULL,
    expect_values_screenshot_args = FALSE
  )

  # Outcome 1 ----
  app$set_inputs(navlistPanel = "outcome1_page")
  app$set_inputs(select_geography_o1 = "")
  app$set_inputs(select_geography_o1 = "Regional")
  app$set_inputs(geographic_breakdown_o1 = "East Midlands")
  app$wait_for_idle(500)
  Sys.sleep(2)
  app$expect_values(input = c("navlistPanel"), output = c("outcome1_choice_text1", "outcome1_choice_text2"))

  # outcome 1 - checking family stability tab
  app$set_inputs(
    navlistPanel = "outcome1_page",
    select_geography_o1 = "Local authority",
    geographic_breakdown_o1 = "Kent",
    national_comparison_checkbox_o1 = TRUE,
    regional_comparison_checkbox_o1 = FALSE,
    outcome1_panels = "Family stability"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o1", "outcome1_panels",
      "national_comparison_checkbox_o1", "regional_comparison_checkbox_o1"
    ),
    output = c(
      "outcome2_choice_text1", "outcome2_choice_text2",
      "cla_rate_headline_txt", "uasc_rate_headline_txt", "cla_march_rate_headline_txt"
    )
  )

  # outcome 1 - checking child wellbeing and development tab
  app$set_inputs(
    navlistPanel = "outcome1_page",
    select_geography_o1 = "Local authority",
    national_comparison_checkbox_o1 = TRUE,
    regional_comparison_checkbox_o1 = TRUE,
    outcome1_panels = "Child wellbeing and development"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o1", "national_comparison_checkbox_o1",
      "regional_comparison_checkbox_o1", "outcome1_panels"
    ),
    output = c(
      "outcome1_choice_text1", "outcome1_choice_text2", "absence_CIN_headline_txt", "absence_CPP_headline_txt",
      "absence_CLA_headline_txt", "persistent_CIN_headline_txt", "persistent_CPP_headline_txt", "persistent_CLA_headline_txt"
    )
  )

  # outcome 1 - checking educational attainment tab
  app$set_inputs(
    navlistPanel = "outcome1_page",
    select_geography_o1 = "Local authority",
    national_comparison_checkbox_o1 = FALSE,
    regional_comparison_checkbox_o1 = TRUE,
    outcome1_panels = "Educational attainment"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o1", "national_comparison_checkbox_o1",
      "regional_comparison_checkbox_o1", "outcome1_panels"
    ),
    output = c(
      "outcome1_choice_text1", "outcome1_choice_text2", "KS2_CIN_headline_txt", "KS2_CPP_headline_txt",
      "KS2_CLA_headline_txt", "KS4_CIN_headline_txt", "KS4_CPP_headline_txt", "KS4_CLA_headline_txt"
    )
  )

  # Outcome 2 ----

  # Demo of using input and output flags to limit the range of shiny elements being
  # captured by a test.
  # Test 2: Switch to Outcomes page 2, select regional and check the national
  # comparison checkbox
  app$set_inputs(
    navlistPanel = "outcome2_page",
    select_geography_o2 = "Regional",
    national_comparison_checkbox_o2 = TRUE,
    outcome2_panels = "Families engaging and receiving support from their family network"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o2", "outcome2_panels",
      "national_comparison_checkbox_o2", "regional_comparison_checkbox_o2"
    ),
    output = c(
      "outcome2_choice_text1", "outcome2_choice_text2",
      "SGO_headline_txt", "CAO_headline_txt"
    )
  )

  # Outcome 2 - domain 2
  app$set_inputs(
    navlistPanel = "outcome2_page",
    select_geography_o2 = "National",
    outcome2_panels = "  Percentage of children who cease being looked after due to Special Guardianship Order (SGO)"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o2", "outcome2_panels"
    ),
    output = c(
      "outcome2_choice_text1", "outcome2_choice_text2",
      "SGO_headline_txt", "CAO_headline_txt"
    )
  )

  # Outcome 3 ----

  # Outcome 3 - looking at local authority level and no checkboxes
  app$set_inputs(
    navlistPanel = "outcome3_page",
    select_geography_o3 = "Local authority",
    geographic_breakdown_o3 = "Barking and Dagenham",
    national_comparison_checkbox_o3 = FALSE,
    regional_comparison_checkbox_o3 = FALSE,
    outcome3_panels = "Child safety - general"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o3", "geographic_breakdown_o3", "outcome3_panels",
      "national_comparison_checkbox_o3", "regional_comparison_checkbox_o3"
    ),
    output = c(
      "outcome3_choice_text1", "outcome3_choice_text2", "cpp_in_year_txt", "cpp_duration_txt",
      "hosp_admissions_txt"
    )
  )

  # outcome 3 - second domain tab
  app$set_inputs(
    navlistPanel = "outcome3_page",
    select_geography_o3 = "Regional",
    geographic_breakdown_o3 = "North East",
    national_comparison_checkbox_o3 = TRUE,
    outcome3_panels = "Child abuse / neglect"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o3", "geographic_breakdown_o3", "outcome3_panels",
      "national_comparison_checkbox_o3"
    ),
    output = c(
      "outcome3_choice_text1", "outcome3_choice_text2"
    )
  )

  # outcome 3 - third domain tab
  app$set_inputs(
    navlistPanel = "outcome3_page",
    select_geography_o3 = "Regional",
    geographic_breakdown_o3 = "North West",
    national_comparison_checkbox_o3 = FALSE,
    outcome3_panels = "Harms outside the home"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o3", "geographic_breakdown_o3", "outcome3_panels",
      "national_comparison_checkbox_o3"
    ),
    output = c(
      "outcome3_choice_text1", "outcome3_choice_text2"
    )
  )


  # Outcome 4 - Looking at local authority level with national checkbox
  app$set_inputs(
    navlistPanel = "outcome4_page",
    select_geography_o4 = "Local authority",
    geographic_breakdown_o4 = "Bedford",
    national_comparison_checkbox_o4 = FALSE,
    regional_comparison_checkbox_o4 = TRUE,
    outcome4_panels = "Child wellbeing"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o3", "geographic_breakdown_o3", "outcome4_panels",
      "national_comparison_checkbox_o3", "regional_comparison_checkbox_o3"
    ),
    output = c("outcome4_choice_text1", "outcome4_choice_text2", "wellbeing_score_stat")
  )

  # Outcome 4 - Looking at local authority level with national checkbox
  app$set_inputs(
    navlistPanel = "outcome4_page",
    select_geography_o4 = "Local authority",
    geographic_breakdown_o4 = "Halton",
    national_comparison_checkbox_o4 = TRUE,
    regional_comparison_checkbox_o4 = TRUE,
    outcome4_panels = "Quality of life for care experienced people"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_o3", "geographic_breakdown_o3", "outcome4_panels",
      "national_comparison_checkbox_o3", "regional_comparison_checkbox_o3"
    ),
    output = c(
      "outcome4_choice_text1", "outcome4_choice_text2", "care_leavers_employment_txt1", "care_leavers_employment_txt2",
      "care_leavers_accommodation_txt1", "care_leavers_accommodation_txt2"
    )
  )

  # Enabler 1 - Nothing here

  # Enabler 2 ----
  # Enabler 2 - Local authority (this page has no checkboxes)
  app$set_inputs(
    navlistPanel = "outcome4_page",
    select_geography_e2 = "Local authority",
    geographic_breakdown_e2 = "Bexley",
    enabler2_panels = "Spending"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_e2", "geographic_breakdown_e2", "enabler2_panels"
    ),
    output = c(
      "enabler2_choice_text1", "enabler2_choice_text2",
      "total_spending_txt", "avg_spend_per_child", "spend_minus_cla_txt"
    )
  )

  # Enabler 2 - Local authority (this page has no checkboxes)
  app$set_inputs(
    navlistPanel = "outcome4_page",
    select_geography_e2 = "National",
    enabler2_panels = "Culture focused on outcomes from children and families and continually improving services"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_e2", "geographic_breakdown_e2", "enabler2_panels"
    ),
    output = c(
      "enabler2_choice_text1", "enabler2_choice_text2", "ofsted_la_headline", "ofsted_outstanding_headline",
      "ofsted_good_headline", "ofsted_improvement_headline", "ofsted_inadequate_headline"
    )
  )

  # Enabler 3 ----
  # Enabler 3 - Domain 1
  app$set_inputs(
    navlistPanel = "outcome4_page",
    select_geography_e2 = "National",
    enabler3_panels = "Workforce Stability"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_e3", "geographic_breakdown_e3", "enabler3_panels",
      "national_comparison_checkbox_e3", "regional_comparison_checkbox_e3"
    ),
    output = c(
      "enabler3_choice_text1", "enabler3_choice_text2", "s_w_headline_txt",
      "agency_rate_txt", "vacancy_rate_txt"
    )
  )

  # Enabler 3 - Local authority with regional checkbox and national checkbox
  app$set_inputs(
    navlistPanel = "outcome4_page",
    select_geography_e2 = "Local authority",
    geographic_breakdown_e2 = "Medway",
    national_comparison_checkbox_e3 = TRUE,
    regional_comparison_checkbox_e3 = TRUE,
    enabler3_panels = "Social worker ethnicity"
  )
  Sys.sleep(2)
  app$expect_values(
    input = c(
      "navlistPanel", "select_geography_e3", "geographic_breakdown_e3", "enabler3_panels",
      "national_comparison_checkbox_e3", "regional_comparison_checkbox_e3"
    ),
    output = c("enabler3_choice_text1", "enabler3_choice_text2", "non_white_txt")
  )
})
