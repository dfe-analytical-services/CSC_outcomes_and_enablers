
source("./global.R")
source("./R/generate_qa_files.R")

sn_long = copy(stats_neighbours_long)


## similar code chunks exist for various datasets which need QA of stat neighbour calculations

#
#
# CIN RATES ----

output_identifier <- "cin_rates_test_automation"
dataset_calc <- copy(cin_rates)
csv_file <- "./data-raw/b1_children_in_need_2013_to_2025.csv"
cin_rate_data <- fread(csv_file)

# initial cleansing steps
cin_rate_data <- cin_rate_data %>%
  colClean() %>%
  insert_geo_breakdown() %>%
  remove_cumbria_data()

# additional step to use the population data to calculate rates
ons_population_data <- data.table::fread(file = "./data-raw/ons_mid-year_population_estimates_2012_to_2024.csv")
ons_population_data <- ons_population_data[, .(time_period, country_code, region_code, old_la_code, population_estimate)]
ons_population_data[, population_estimate := as.integer(population_estimate)]

# step to align the calendars
ons_population_data[, time_period := time_period + 1]

# merge in the population data and perform the calculation, formatting to 1 dp
cin_rate_data <- merge(cin_rate_data, ons_population_data, all.x = TRUE) %>%
  mutate(At31_episodes_rate = as.character((as.numeric(At31_episodes) / as.numeric(population_estimate)) * 10000)) %>%
  mutate(At31_episodes_rate = sapply(as.character(At31_episodes_rate), decimal_rounding, 0))

# copy through the redacted values with their reason codes
cin_rate_data[is.na(At31_episodes_rate) & !(is.na(population_estimate)), At31_episodes_rate := At31_episodes]
cin_rate_data[is.na(At31_episodes_rate), At31_episodes_rate := "z"]

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("At31_episodes", "At31_episodes_rate")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")

calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"

output_data <- create_qa_dataset_sn(output_identifier, dataset_calc = cin_rates, sn_long, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                     write_data = FALSE, dataset_raw = cin_rate_data)

# # CLA RATES ----
# updated for 2025 publication
output_identifier <- "cla_rates_test_automation_2025"
dataset_calc <- copy(cla_rates)
#csv_file <- "./data-raw/cla_number_and_rate_per_10k_children.csv"
dataset_raw <- read_cla_rate_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]
id.vars_raw <- c("time_period", "old_la_code", "la_name", "population_count")
measure.vars_raw <- c("number", "rate_per_10000")
raw_melt_cols <- c("time_period", "old_la_code", "population_count", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "population_count", "variable")
by.y <- c("time_period", "old_la_code", "population_count", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "population_count")

calc_qa_formula <- "old_la_code + geo_breakdown + population_count + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + population_count + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + population_count + variable + time_period + value_la ~ SN_LA_name"


output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw)

# Outcomes Absence ----
sn_long <- copy(stats_neighbours_long)
output_identifier <- "outcomes_absence_test_automation"
dataset_calc <- copy(outcomes_absence)
csv_file <- "./data-raw/absence_la.csv"
id.vars_raw <- c("time_period", "old_la_code", "la_name", "social_care_group", "school_type")
measure.vars_raw <- c("t_pupils", "pt_overall", "pt_pupils_pa_10_exact", "pt_pupils_pa_50_exact")
raw_melt_cols <- c("time_period", "old_la_code", "social_care_group", "school_type", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "social_care_group", "school_type", "variable")
by.y <- c("time_period", "old_la_code", "social_care_group", "school_type", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "social_care_group", "school_type")

calc_qa_formula <- "old_la_code + geo_breakdown + social_care_group + school_type + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + social_care_group + school_type + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + social_care_group + school_type + variable + time_period + value_la ~ SN_LA_name"


#strsplit("old_la_code + geo_breakdown + social_care_group + school_type + variable + time_period ~ geographic_level", c(" + ", fixed = TRUE)
#strsplit("old_la_code + la_name + social_care_group + school_type + variable + time_period + value_la ~ SN_LA_name", " + ", fixed = TRUE)

output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE)

# # Outcomes KS2 ----
#
output_identifier <- "outcomes_ks2_test_automation"
dataset_calc <- copy(outcomes_ks2)
csv_file <- "./data-raw/ks2_la.csv"
id.vars_raw <- c("time_period", "old_la_code", "la_name", "social_care_group")
measure.vars_raw <- c("pt_rwm_met_expected_standard", "t_rwm_eligible_pupils")
raw_melt_cols <- c("time_period", "old_la_code", "social_care_group", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "social_care_group", "variable")
by.y <- c("time_period", "old_la_code", "social_care_group", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "social_care_group")

calc_qa_formula <- "old_la_code + geo_breakdown + social_care_group + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + social_care_group + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + social_care_group + variable + time_period + value_la ~ SN_LA_name"


output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE)


## Outcomes KS4 ----

output_identifier <- "outcomes_ks4_test_automation"
dataset_calc <- copy(outcomes_ks4)
csv_file <- "./data-raw/ks4_la.csv"
id.vars_raw <- c("time_period", "old_la_code", "la_name", "social_care_group")
measure.vars_raw <- c("avg_att8", "t_pupils")
raw_melt_cols <- c("time_period", "old_la_code", "social_care_group", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "social_care_group", "variable")
by.y <- c("time_period", "old_la_code", "social_care_group", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "social_care_group")

calc_qa_formula <- "old_la_code + geo_breakdown + social_care_group + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + social_care_group + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + social_care_group + variable + time_period + value_la ~ SN_LA_name"


output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
  write_data = TRUE
)


## NEW INDICATOR - School  Stability ----

output_identifier <- "school_stability"
dataset_calc <- copy(school_stability_data)

dataset_raw <- school_stability_data[geographic_level == "Local authority"]

setDT(dataset_raw)

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("percentage")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")


calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"


#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)



# Ceased CLA  ----

# updated MW 2025 - done, but why was this commented out before?

output_identifier <- "outcomes_ceased_cla_test_automation_2025"
dataset_calc <- copy(ceased_cla_data)
#csv_file <- "./data/la_children_who_ceased_during_the_year.csv"
dataset_raw <- read_outcome2(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]
#dataset_raw <- read_cla_31_march_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]
id.vars_raw <- c("time_period", "old_la_code", "la_name", "cla_group", "characteristic")
measure.vars_raw <- c("percentage")
raw_melt_cols <- c("time_period", "old_la_code", "cla_group", "characteristic", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "cla_group", "characteristic", "variable")
by.y <- c("time_period", "old_la_code", "cla_group", "characteristic", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "cla_group", "characteristic")

calc_qa_formula <- "old_la_code + geo_breakdown + cla_group + characteristic + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + cla_group + characteristic + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + cla_group + characteristic + variable + time_period + value_la ~ SN_LA_name"


output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file=NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw)

# # Repeat CPP  ----
#
output_identifier <- "outcomes_repeat_cpp_test_automation"
dataset_calc <- copy(repeat_cpp)
csv_file <- "./data-raw/d3_cpps_subsequent_plan_2013_to_2025.csv"
id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("CPP_subsequent_percent")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")

calc_qa_formula <- "old_la_code + geo_breakdown  + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"



output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE)



#
# # CPP Duration ----

output_identifier <- "outcomes_cpp_duration_test_automation"
dataset_calc <- copy(duration_cpp)
csv_file <- "./data-raw/d5_cpps_at31march_by_duration_2013_to_2025.csv"
id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("X2_years_or_more_percent")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")

calc_qa_formula <- "old_la_code + geo_breakdown  + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"



output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE)

#
# CIN Re-referrals ----

output_identifier <- "outcomes_cin_re_referrals_test_automation"
dataset_calc <- copy(cin_referrals)
csv_file <- "./data-raw/c1_children_in_need_referrals_and_rereferrals_2013_to_2025.csv"

cin_referral_data <- fread(csv_file)

# initial cleansing steps
cin_referral_data <- cin_referral_data %>%
  colClean() %>%
  insert_geo_breakdown() %>%
  remove_cumbria_data()

# additional steps due to column renaming in the file (easier to put this in than refactor all references to the new field)
cin_referral_data <- cin_referral_data %>%
  mutate(
    Re_referrals_percent = rereferral_percent,
    Referrals = referral_count,
    Re_referrals = rereferral_count
  )

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("Re_referrals_percent")
measure.vars_calc <- c("Re_referrals_percent")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")

calc_qa_formula <- "old_la_code + geo_breakdown  + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"



output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE,measure.vars_calc = measure.vars_calc, dataset_raw = cin_referral_data)

#
# # assessment_factors ----
#
output_identifier <- "outcomes_assessment_factors_test_automation"
dataset_calc <- copy(assessment_factors)
dataset_raw <- copy(ass_fac_data[geographic_level == "Local authority"])
csv_file <- "./data-raw/c3_factors_identified_at_end_of_assessment_2018_to_2025.csv"
id.vars_raw <- c("time_period", "old_la_code", "la_name", "assessment_factor")
measure.vars_raw = "rate_per_10000_char"
# measure.vars_raw <- c("Episodes_with_assessment_factor",
#   "Alcohol_Misuse_child", "Alcohol_Misuse_parent", "Alcohol_Misuse_person", "Drug_Misuse_child",
#   "Drug_Misuse_parent", "Drug_Misuse_person", "Domestic_Abuse_child", "Domestic_Abuse_parent",
#   "Domestic_Abuse_person", "Mental_Health_child", "Mental_Health_parent", "Mental_Health_person", "Learning_Disability_child",
#   "Learning_Disability_parent", "Learning_Disability_person", "Physical_Disability_child",
#   "Physical_Disability_parent", "Physical_Disability_person", "Young_Carer", "Privately_fostered",
#   "Unaccompanied_asylum_seeker", "Going_missing", "Child_sexual_exploitation", "Trafficking", "Gangs",
#   "Socially_unacceptable_behaviour", "Self_harm", "Neglect", "Emotional_Abuse", "Physical_Abuse_unknown",
#   "Physical_Abuse_child_on_child", "Physical_Abuse_adult_on_child", "Sexual_Abuse_unknown", "Sexual_Abuse_child_on_child",
#   "Sexual_Abuse_adult_on_child", "Female_Genital_Mutilation", "Faith_linked_abuse", "Child_criminal_exploitation", "Other"
# )

raw_melt_cols <- c("time_period", "old_la_code", "assessment_factor", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "assessment_factor", "variable")
by.y <- c("time_period", "old_la_code", "assessment_factor", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "assessment_factor")

calc_qa_formula <- "old_la_code + geo_breakdown  + assessment_factor + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + assessment_factor + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + assessment_factor + variable + time_period + value_la ~ SN_LA_name"



output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw)


# CLA  Placement changes ----
# revised 2025
output_identifier <- "outcomes_placement_changes_data_test_automation_2025"
dataset_calc <- copy(placement_changes_data)
#csv_file <- "./data/la_cla_placement_stability.csv"
dataset_raw <- read_number_placements_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]
id.vars_raw <- c("time_period", "old_la_code", "la_name", "cla_group", "placement_stability")
measure.vars_raw <- c("Percentage")
raw_melt_cols <- c("time_period", "old_la_code", "cla_group", "placement_stability", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "cla_group", "placement_stability", "variable")
by.y <- c("time_period", "old_la_code", "cla_group", "placement_stability", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "cla_group", "placement_stability")
measure.vars_calc <- c("Percentage")

calc_qa_formula <- "old_la_code + geo_breakdown + cla_group + placement_stability + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + cla_group + placement_stability + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + cla_group + placement_stability + variable + time_period + value_la ~ SN_LA_name"



output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    measure.vars_calc, write_data = TRUE, dataset_raw = dataset_raw)


#
#
#
# CLA  Placement distance ----
# revised 2025
sn_long <- copy(stats_neighbours_long)
output_identifier <- "outcomes_cla_placement_distance_test_automation_2025"
dataset_calc <- copy(placement_data)
#csv_file <- "./data/la_cla_on_31_march_by_characteristics.csv"
dataset_raw <- read_placement_info_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]
id.vars_raw <- c("time_period", "old_la_code", "la_name", "cla_group", "characteristic")
measure.vars_raw <- c("percentage")
raw_melt_cols <- c("time_period", "old_la_code", "cla_group", "characteristic", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "cla_group", "characteristic", "variable")
by.y <- c("time_period", "old_la_code", "cla_group", "characteristic", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "cla_group", "characteristic")

calc_qa_formula <- "old_la_code + geo_breakdown + cla_group + characteristic + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + cla_group + characteristic + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + cla_group + characteristic + variable + time_period + value_la ~ SN_LA_name"



output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
  write_data = TRUE, dataset_raw = dataset_raw
)

#
#
# Care Leavers Activity ----
# updated 2025
output_identifier <- "outcomes_care_leavers_activity_test_automation_2025"
dataset_calc <- copy(care_leavers_activity_data)
#csv_file <- "./data/la_care_leavers_activity.csv"
dataset_raw <- read_care_leavers_activity_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]
id.vars_raw <- c("time_period", "old_la_code", "la_name", "age", "activity")
measure.vars_raw <- c("percentage")
raw_melt_cols <- c("time_period", "old_la_code", "age", "activity", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "age", "activity", "variable")
by.y <- c("time_period", "old_la_code", "age", "activity", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "age", "activity")

calc_qa_formula <- "old_la_code + geo_breakdown + age + activity + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + age + activity + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + age + activity + variable + time_period + value_la ~ SN_LA_name"



output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
  write_data = TRUE, dataset_raw = dataset_raw
)


# Care Leavers Accomodation ----
# updated 2025
output_identifier <- "outcomes_care_leavers_accomodation_test_automation_2025"
dataset_calc <- copy(care_leavers_accommodation_data)
#csv_file <- "./data/la_care_leavers_accommodation_suitability.csv"
dataset_raw <- read_care_leavers_accommodation_suitability(sn_long = stats_neighbours_long)
id.vars_raw <- c("time_period", "old_la_code", "la_name", "age", "accommodation_suitability")
measure.vars_raw <- c("percentage")
raw_melt_cols <- c("time_period", "old_la_code", "age", "accommodation_suitability", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "age", "accommodation_suitability", "variable")
by.y <- c("time_period", "old_la_code", "age", "accommodation_suitability", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "age", "accommodation_suitability")

calc_qa_formula <- "old_la_code + geo_breakdown + age + accommodation_suitability + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + age + accommodation_suitability + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + age + accommodation_suitability + variable + time_period + value_la ~ SN_LA_name"



output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw)


# # Workforce data ----

output_identifier <- "outcomes_workforce_data_test_automation"
dataset_calc <- copy(workforce_data)
#dataset_calc[, old_la_code := as.numeric(old_la_code)]
csv_file <- "./data-raw/csww_indicators_2017_to_2025.csv"
id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("turnover_rate_fte", "agency_rate_fte", "vacancy_rate_fte", "caseload_fte")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")

calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"



output_data <- create_qa_dataset_sn(output_identifier, dataset_calc, sn_long, csv_file, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE)



workforce_data



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NEW BATCH OF DATASETS
#===================================================================================================================================


## Spending and ofsted ----

output_identifier <- "outcomes_spending_data_test_automation"

dataset_calc <- copy(spending_data)
# csv_file <- "./data/csww_indicators_2017_to_2024.csv"
dataset_raw <- read_spending_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical_neighbours (median)"]

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("CS Share")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")

calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"


output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)


### outcomes_spending_per_capita_data_test_automation ---- 
output_identifier <- "outcomes_spending_per_capita_data_test_automation"

dataset_calc <- copy(spending_per_capita)
# csv_file <- "./data/csww_indicators_2017_to_2024.csv"
dataset_raw <- read_per_capita_spending(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("Cost per child")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")


calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"


#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)



output_identifier <- "outcomes_spending_data_no_cla_test_automation"

dataset_calc <- copy(spending_data_no_cla)
# csv_file <- "./data/csww_indicators_2017_to_2024.csv"
dataset_raw <- read_spending_data2(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("Excluding CLA Share")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")


calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"


#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)


### enablers_ofsted_ratings_test_automation ----
output_identifier <- "enablers_ofsted_ratings_test_automation"

dataset_calc <- copy(ofsted_leadership_data)
# csv_file <- "./data/csww_indicators_2017_to_2024.csv"
dataset_raw <- read_ofsted_leadership_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]
#dataset_raw <- pivot_ofsted_data(dataset_raw)
setDT(dataset_raw)

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("inadequate_count", "requires_improvement_count", "good_count", "outstanding_count")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")


calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"


output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)



### wokforce Ethnicity ----
output_identifier <- "enablers_ethnicity"

dataset_calc <- copy(workforce_eth)[role == "Total" & breakdown == "Non-white" & time_period == "2025"]
# csv_file <- "./data/csww_indicators_2017_to_2025.csv"
dataset_raw <- read_workforce_eth_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"][role == "Total" & breakdown == "Non-white" & time_period == "2025"]

setDT(dataset_raw)

id.vars_raw <- c("time_period", "old_la_code", "la_name", "role", "breakdown_topic", "breakdown")
measure.vars_raw <- c("inpost_headcount_percentage")
raw_melt_cols <- c("time_period", "old_la_code", "role", "breakdown", "breakdown_topic" , "variable", "value")
by.x <- c("time_period", "SN_LA_number", "role", "breakdown", "breakdown_topic", "variable")
by.y <- c("time_period", "old_la_code", "role", "breakdown", "breakdown_topic", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "role", "breakdown", "breakdown_topic")


calc_qa_formula <- "old_la_code + geo_breakdown + role + breakdown + breakdown_topic + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + role + breakdown + breakdown_topic + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + role + breakdown + breakdown_topic + variable + time_period + value_la ~ SN_LA_name"

#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)



# SDQ ----
output_identifier <- "outcomes_wellbeing_sdq_2025"

dataset_calc <- copy(wellbeing_sdq_data)[characteristic == "SDQ average score"]   # & time_period == "2024"]
# csv_file <- "./data/csww_indicators_2017_to_2024.csv"
dataset_raw <- read_wellbeing_child_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"][characteristic == "SDQ average score"]   # & time_period == "2024"]

setDT(dataset_raw)

id.vars_raw <- c("time_period", "old_la_code", "la_name", "cla_group", "characteristic")
measure.vars_raw <- c("number")
raw_melt_cols <- c("time_period", "old_la_code","cla_group", "characteristic" , "variable", "value")
by.x <- c("time_period", "SN_LA_number", "cla_group", "characteristic", "variable")
by.y <- c("time_period", "old_la_code", "cla_group", "characteristic", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level", "cla_group", "characteristic")


calc_qa_formula <- "old_la_code + geo_breakdown + cla_group + characteristic + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + cla_group + characteristic + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + cla_group + characteristic + variable + time_period + value_la ~ SN_LA_name"


#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)

### hospital_admissions_revised ----
output_identifier <- "hospital_admissions_revised"

dataset_calc <- copy(hospital_admissions)
# csv_file <- "./data/csww_indicators_2017_to_2024.csv"
dataset_raw <- read_a_and_e_data(sn_long = stats_neighbours_long)[geographic_level != "Statistical neighbours (median)"]

#========get the raw data - it's long winded!!

# read the raw data from 2 csv files
la_admissions <- read.csv("data-raw/la_hospital_admissions_2324.csv") # la_file)
region_admissions <- read.csv("data-raw/region_hospital_admissions_2324.csv") # region_file)

# remove North and West Northamptonshire for pre 2022
# remove Cumberland & Westmorland and Furness for pre 2023
# remove BCP and Dorset for pre 2019
# remove Buckinghamshire for pre 2020
la_admissions <- la_admissions %>%
  filter(!(Area.Name %in% c("North Northamptonshire", "West Northamptonshire") & Time.period < "2022")) %>%
  filter(!(Area.Name %in% c("Cumberland", "Westmorland and Furness") & Time.period < "2023")) %>%
  filter(!(Area.Name %in% c("Dorset", "Bournemouth, Christchurch and Poole") & Time.period < "2019")) %>%
  filter(!(Area.Name %in% c("Buckinghamshire UA") & Time.period < "2020"))


# additional step to clean dots out of the coumn names
setnames(la_admissions, "Area.Name", "AreaName")
setnames(region_admissions, "Area.Name", "AreaName")
la_admissions$AreaName <- sub(" UA$", "", la_admissions$AreaName)
region_admissions$AreaName <- sub(" region \\(statistical\\)$", "", region_admissions$AreaName)


# note the hard-coded cleansing here as the input files provided require a couple of hacks
admissions_data_joined <- rbind(
  la_admissions[la_admissions$Category == "" & la_admissions$Sex == "Persons", c(1:13, 18, 19)],
  region_admissions[region_admissions$Parent.Code == "E92000001" & region_admissions$Category == "" & region_admissions$Sex == "Persons", c(1:13, 18, 19)]
) %>%
  select("Time.period", "Area.Type", "AreaName", "Area.Code", "Value", "Count", "Denominator") %>%
  rename(`time_period` = `Time.period`, `geographic_level` = `Area.Type`, `geo_breakdown` = `AreaName`, `new_la_code` = `Area.Code`) %>%
  distinct()

admissions_data_joined["geographic_level"][admissions_data_joined["geographic_level"] == "Regions (statistical)"] <- "Regional"
admissions_data_joined["geographic_level"][admissions_data_joined["geographic_level"] == "Counties & UAs (from Apr 2023)"] <- "Local authority"
admissions_data_joined["geographic_level"][admissions_data_joined["geographic_level"] == "England"] <- "National"
admissions_data_joined["geo_breakdown"][admissions_data_joined["geo_breakdown"] == "England"] <- "National"

admissions_data_joined <- remove_cumbria_data(admissions_data_joined)

# For the stats neighbours charts we need to have old la codes, not available in this data so just get it from another dataset
la_codes <- suppressWarnings(read_workforce_data(sn_long = sn_long)) %>%
  filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
  select(old_la_code, new_la_code) %>%
  distinct() %>%
  separate_rows(c("old_la_code", "new_la_code"), sep = " / ")

admissions_data2 <- left_join(admissions_data_joined, la_codes, by = c("new_la_code"))
# admissions_data2$Count <- as.numeric(gsub(",", "", admissions_data2$Count))
# admissions_data2$rate_per_10000 <- as.numeric(admissions_data2$rate_per_10000)

# Name changes

admissions_data2 <- admissions_data2 %>%
  mutate(geo_breakdown = case_when(
    geo_breakdown == "Yorkshire and the Humber" ~ "Yorkshire and The Humber",
    geo_breakdown == "Bristol" ~ "Bristol, City of",
    geo_breakdown == "Herefordshire" ~ "Herefordshire, County of",
    geo_breakdown == "Kingston upon Hull" ~ "Kingston upon Hull, City of",
    TRUE ~ as.character(geo_breakdown)
  ))

# Inner London Data

inner_london <- c(
  "Westminster",
  "Tower Hamlets",
  "Camden",
  "Hackney",
  "Kensington and Chelsea",
  "Southwark",
  "Lewisham",
  "Islington",
  "Wandsworth",
  "Hammersmith and Fulham",
  "Haringey",
  "Lambeth",
  "Newham",
  "City of London"
)
inner_london_data <- admissions_data2 %>%
  filter(geo_breakdown %in% inner_london)

inner_london_stat <- inner_london_data %>%
  group_by(time_period) %>%
  summarise(
    geographic_level = "Regional",
    geo_breakdown = "Inner London",
    new_la_code = "E13000001",
    Value = sum(Value, na.rm = TRUE),
    Count = sum(Count[Denominator >= 0], na.rm = TRUE),
    Denominator = sum(Denominator[Denominator >= 0], na.rm = TRUE),
    old_la_code = NA
  )

# Outer London Data

Outer_london <- c(
  "Bexley",
  "Greenwich",
  "Harrow",
  "Brent",
  "Waltham Forest",
  "Ealing",
  "Richmond upon Thames",
  "Hillingdon",
  "Kingston upon Thames",
  "Hounslow",
  "Bromley",
  "Barnet",
  "Croydon",
  "Enfield",
  "Merton",
  "Sutton",
  "Barking and Dagenham",
  "Redbridge",
  "Havering"
)

Outer_london_data <- admissions_data2 %>%
  filter(geo_breakdown %in% Outer_london)

Outer_london_stat <- Outer_london_data %>%
  group_by(time_period) %>%
  summarise(
    geographic_level = "Regional",
    geo_breakdown = "Outer London",
    new_la_code = "E13000002",
    Value = sum(Value, na.rm = TRUE),
    Count = sum(Count[Denominator >= 0], na.rm = TRUE),
    Denominator = sum(Denominator[Denominator >= 0], na.rm = TRUE),
    old_la_code = NA
  )

# Inner and Outer London

inner_and_outer_london <- rbind(inner_london_stat, Outer_london_stat)

# rate per 10000

inner_and_outer_london <- inner_and_outer_london %>%
  # mutate(rate_per_10000 = Count / (Denominator / 10000)) %>%
  mutate(Value = Count / (Denominator / 10000))

# Add Inner and Outer London to the data frame
admissions_data2 <- rbind(admissions_data2, inner_and_outer_london)


# Convert rate_per_10000 to a character for all rows
admissions_data2 <- admissions_data2 %>%
  mutate(rate_per_10000 = case_when(
    is.na(Value) ~ "x",
    TRUE ~ as.character(Value)
  ))


# add stats neighbours
# now calculate SN metrics and append to the bottom of the dataset
setDT(admissions_data2)
admissions_data2[, old_la_code := as.numeric(old_la_code)]



dataset_raw <- copy(admissions_data2)

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("rate_per_10000")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")


calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"


#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)


output_data


# UASC
# Rate of children starting to be looked after who were UASC per 10k
output_identifier <- "outcomes_uasc_starting_to_be_2025"
dataset_calc <- copy(combined_cla_data)[time_period <= "2025"][characteristic == "UASC"][population_count == "Children starting to be looked after each year"]

dataset_raw <- suppressWarnings(merge_cla_dataframes(sn_long = stats_neighbours_long))[geographic_level != "Statistical neighbours (median)"][time_period <= "2025"][characteristic == "UASC"][population_count == "Children starting to be looked after each year"]

setDT(dataset_raw)

id.vars_raw <- c("time_period", "old_la_code", "la_name", "population_count", "characteristic")
measure.vars_raw <- c("placement_per_10000")
raw_melt_cols <- c("time_period", "old_la_code", "population_count", "characteristic", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "population_count", "characteristic", "variable")
by.y <- c("time_period", "old_la_code", "population_count", "characteristic", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "population_count", "characteristic", "geographic_level")


calc_qa_formula <- "old_la_code + geo_breakdown + population_count + characteristic + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + population_count + characteristic + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + population_count + characteristic + variable + time_period + value_la ~ SN_LA_name"


#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)




# UASC
# Rate of children 31 march looked after who were UASC per 10k
output_identifier <- "outcomes_uasc_31_march_looked_after_2025"
dataset_calc <- copy(combined_cla_31_march_data)[time_period <= "2025"][characteristic == "UASC"][population_count == "Children looked after on 31 March each year"]

dataset_raw <- suppressWarnings(merge_cla_31_march_dataframes(sn_long = stats_neighbours_long))[geographic_level != "Statistical neighbours (median)"][time_period <= "2025"][characteristic == "UASC"][population_count == "Children looked after on 31 March each year"]

setDT(dataset_raw)

id.vars_raw <- c("time_period", "old_la_code", "la_name", "population_count", "characteristic")
measure.vars_raw <- c("placement_per_10000")
raw_melt_cols <- c("time_period", "old_la_code", "population_count", "characteristic", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "population_count", "characteristic", "variable")
by.y <- c("time_period", "old_la_code", "population_count", "characteristic", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "population_count", "characteristic", "geographic_level")


calc_qa_formula <- "old_la_code + geo_breakdown + population_count + characteristic + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + population_count + characteristic + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + population_count + characteristic + variable + time_period + value_la ~ SN_LA_name"


#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)







# Enabler 3

## NEW INDICATOR - Social Worker Stability

output_identifier <- "sw_stability"
dataset_calc <- copy(sw_stability_data)[time_period <= "2024"][cla_group == "CLA on 31 March"][sw_stability == "3 or more social workers during the year"]

dataset_raw <- sw_stability_data[geographic_level == "Local authority"][time_period <= "2024"][cla_group == "CLA on 31 March"][sw_stability == "3 or more social workers during the year"]

setDT(dataset_raw)

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("percentage")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")


calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"


#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)




# 

## NEW INDICATOR - S47 ICPC

output_identifier <- "s47_icpc"
dataset_calc <- copy(s47_to_ICPC_data)

dataset_raw <- copy(s47_to_ICPC_data)[geographic_level == "Local authority"]

setDT(dataset_raw)

id.vars_raw <- c("time_period", "old_la_code", "la_name")
measure.vars_raw <- c("percentage")
raw_melt_cols <- c("time_period", "old_la_code", "variable", "value")
by.x <- c("time_period", "SN_LA_number", "variable")
by.y <- c("time_period", "old_la_code", "variable")
id.vars_calc <- c("time_period", "old_la_code", "geo_breakdown", "geographic_level")


calc_qa_formula <- "old_la_code + geo_breakdown + variable + time_period ~ geographic_level"
raw_qa_formula_1 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_rank"
raw_qa_formula_2 <- "old_la_code + la_name + variable + time_period + value_la ~ SN_LA_name"


#browser()
output_data <- create_qa_dataset_sn(output_identifier, dataset_calc,  sn_long, csv_file = NULL, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                    write_data = TRUE, dataset_raw = dataset_raw, measure.vars_calc = NULL)

