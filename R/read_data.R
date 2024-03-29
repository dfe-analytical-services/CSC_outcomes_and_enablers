# Script where we provide functions to read in the data file(s).

# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.

# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.

# Function to clean column names
colClean <- function(x) {
  colnames(x) <- gsub("\\.", "perc", colnames(x))
  x
}

# function to convert str columns into numerical columns
convert_perc_cols_to_numeric <- function(x) {
  suppressWarnings({
    perc_cols <- grep("fte", colnames(x))
    x[, perc_cols] <- apply(x[, perc_cols], 2, function(x) as.numeric(as.character(x)))
  })
  return(x)
}

# sample data functions we dont need this~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_revenue_data <- function(file = "data/la_maintained_schools_revenue_reserve_final.csv") {
  # This reads in an example file. For the purposes of this demo, we're using the
  # latest LA expenditure data downloaded from the EES release.
  dfRevenue <- read.csv(file)
  # The time period column name has some non-ascii characters so we're just going to rename it here.
  colnames(dfRevenue)[1] <- "time_period"
  dfRevenue <- dfRevenue %>% mutate(
    year = as.numeric(paste0("20", substr(format(time_period), 5, 6))),
    area_name = case_when(
      geographic_level == "National" ~ country_name,
      geographic_level == "Regional" ~ region_name,
      TRUE ~ la_name
    )
  )
  return(dfRevenue)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test not important
read_definitions <- function(file = "data/definitions.csv") {
  definitions <- read.csv(file)
  # colnames(definitions) <- c("Outcome/Enabler", "Domain", "Indicator", "Rationale/Description")
  #  definitions <- definitions[,1:4]
  return(definitions)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce data
# read_workforce_data <- function(file = "data/csww_headline_measures_2017_to_2022.csv") {
#   workforce_data <- read.csv(file)
#   # Select only the columns we want
#   workforce_data <- colClean(workforce_data) %>% select(
#     "time_period", "geographic_level", "region_name", "la_name", "turnover_rate_fte_perc", "absence_rate_fte_perc",
#     "agency_worker_rate_fte_perc", "agency_cover_rate_fte_perc", "vacancy_rate_fte_perc", "vacancy_agency_cover_rate_fte_perc",
#     "turnover_rate_headcount_perc", "agency_worker_rate_headcount_perc", "caseload_fte"
#   )
#   workforce_data <- convert_perc_cols_to_numeric(workforce_data)
#   return(workforce_data)
# }


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Need a fact table for the LA's and their Regions
GET_location <- function(file = "data/la_children_who_started_to_be_looked_after_during_the_year.csv") {
  FACT_location <- read.csv(file)
  FACT_location <- FACT_location %>%
    select(region_name, la_name) %>%
    filter((la_name != "")) %>%
    unique()
}


# Need a fact table for the LA's and their Regions for workforce data as they have LAs combined
GET_location_workforce <- function(file = "data/csww_indicators_2017_to_2023.csv") {
  workforce_location <- read.csv(file)
  workforce_location <- read.csv(file)
  workforce_location <- workforce_location %>%
    select(region_name, la_name) %>%
    filter((la_name != "")) %>%
    unique()
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For filters to work nicely, we want to have two levels of grouping: geographic level (national, regional, LA)
# and level breakdown (region names and la names)

read_workforce_data <- function(file = "data/csww_indicators_2017_to_2023.csv") {
  workforce_data <- read.csv(file)
  workforce_data <- colClean(workforce_data) %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(
      geographic_level, geo_breakdown, turnover_rate_fte, time_period, "time_period", "turnover_rate_fte", "absence_rate_fte",
      "agency_rate_fte", "agency_cover_rate_fte", "vacancy_rate_fte", "vacancy_agency_cover_rate_fte",
      "turnover_rate_headcount", "agency_rate_headcount", "caseload_fte"
    ) %>%
    distinct()

  workforce_data <- convert_perc_cols_to_numeric(workforce_data)

  # colnames(workforce_data) <- c("Geographic Level","Geographic Breakdown", "Turnover Rate (FTE) %", "Time Period", "Absence Rate (FTE) %",
  #                             "Agency Worker Rate (FTE) %", "Agency Cover Rate (FTE) %", "Vacancy Rate (FTE) %", "Vacancy Agency Cover Rate (FTE) %",
  #                             "Turnover Rate Headcount %", "Agency Worker Rate Headcount %", "Caseload (FTE)")
  #
  #
  return(workforce_data)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce characteristics data
# read_workforce_char_data <- function(file = "data/csww_workforce_characteristics_2017_to_2022.csv") {
#   workforce_characteristics <- read.csv(file)
#   # Select only the columns we want
#   workforce_char_data <- colClean(workforce_characteristics)
#   workforce_char_data <- workforce_char_data %>% filter(characteristic_type != "Total") %>% select(
#     "time_period", "geographic_level", "region_name", "characteristic", "characteristic_type", "percentage"
#   )
#   workforce_char_data <- convert_perc_cols_to_numeric(workforce_char_data)
#   return(workforce_char_data)
# }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce ethnicity data
read_workforce_eth_data <- function(file = "data/csww_role_by_characteristics_inpost_2019_to_2023.csv") {
  workforce_ethnicity_data <- read.csv(file)
  # Select only columns we want
  # workforce_eth_data <- colCleanPerc(workforce_ethnicity_data)
  workforce_ethnicity_data <- workforce_ethnicity_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, time_period,
      "time_period", "geographic_level", "region_name", "role", breakdown_topic, breakdown,
      inpost_FTE, inpost_FTE_percentage, inpost_headcount, inpost_headcount_percentage
    )

  workforce_ethnicity_data$new_la_code[workforce_ethnicity_data$new_la_code == ""] <- NA
  workforce_ethnicity_data$region_code[workforce_ethnicity_data$region_code == ""] <- NA
  workforce_ethnicity_data <- mutate(workforce_ethnicity_data, code = coalesce(new_la_code, region_code, country_code))

  workforce_ethnicity_data <- convert_perc_cols_to_numeric(workforce_ethnicity_data)

  return(workforce_ethnicity_data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce ethnicity by seniority data
read_workforce_eth_seniority_data <- function(file = "data/csww_role_by_characteristics_inpost_2019_to_2023.csv") {
  workforce_ethnicity_seniority_data <- read.csv(file)
  # Select only columns we want
  # workforce_eth_data <- colCleanPerc(workforce_ethnicity_data)
  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, time_period,
      "time_period", "geographic_level", "region_name", "role", breakdown_topic, breakdown,
      inpost_FTE, inpost_FTE_percentage, inpost_headcount, inpost_headcount_percentage
    ) %>%
    filter(breakdown_topic == "Ethnicity major")

  workforce_ethnicity_seniority_data$new_la_code[workforce_ethnicity_seniority_data$new_la_code == ""] <- NA
  workforce_ethnicity_seniority_data$region_code[workforce_ethnicity_seniority_data$region_code == ""] <- NA
  workforce_ethnicity_seniority_data <- mutate(workforce_ethnicity_seniority_data, code = coalesce(new_la_code, region_code, country_code))

  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    mutate(seniority = case_when(
      role == "Total" ~ "Total",
      role == "Case holder" ~ "Case holder",
      role == "Qualified without cases" ~ "Qualified without cases",
      role == "Senior practitioner" ~ "Senior practitioner",
      role %in% c("First line manager", "Senior manager", "Middle manager") ~ "Manager"
    ))

  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    mutate(inpost_headcount = case_when(
      inpost_headcount == "Z" ~ NA,
      inpost_headcount == "x" ~ NA,
      TRUE ~ as.numeric(inpost_headcount)
    ))


  # #sum ethnicity counts to create grouped manager percents
  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    group_by(geographic_level, geo_breakdown, time_period, region_name, code, seniority, breakdown) %>%
    summarise_at(c("inpost_headcount"), sum) %>%
    filter(!(breakdown %in% c("Total", "Not known", "Known")))


  # sum ethnicity headcount to create total
  total_observation <- workforce_ethnicity_seniority_data %>%
    group_by(geographic_level, geo_breakdown, time_period, region_name, code, seniority) %>%
    summarise(Observation = sum(inpost_headcount), .groups = "keep")

  # Join the total observation back to the original dataframe
  workforce_ethnicity_seniority_data <- left_join(workforce_ethnicity_seniority_data, total_observation,
    by = c("geographic_level", "geo_breakdown", "time_period", "region_name", "code", "seniority")
  )

  # Create ethnicity percentages
  workforce_ethnicity_seniority_data <- workforce_ethnicity_seniority_data %>%
    mutate(Percentage = round(inpost_headcount / Observation * 100, 1))

  # Filter to include only the latest year of data
  latest_year <- max(workforce_ethnicity_seniority_data$time_period)
  workforce_ethnicity_seniority_data <- subset(workforce_ethnicity_seniority_data, time_period == latest_year)
  # workforce_ethnicity_seniority_data <- convert_perc_cols_to_numeric(workforce_ethnicity_seniority_data)

  return(workforce_ethnicity_seniority_data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# General population ethnicity data


read_ethnic_population_data <- function(file1 = "data/ons-ethnic-population-reg.csv", file2 = "data/ons-ethnic-population-nat.csv", file3 = "data/ons-ethnic-population-la.csv") {
  # Read the csv files
  df_regions <- read.csv(file1, check.names = FALSE)
  df_countries <- read.csv(file2, check.names = FALSE)
  df_authorities <- read.csv(file3, check.names = FALSE)
  df_Inner_London <- read.csv(file3, check.names = FALSE)
  df_Outer_London <- read.csv(file3, check.names = FALSE)
  df_Kingston_upon_Thames <- read.csv(file3, check.names = FALSE)
  df_North_Northamptonshire <- read.csv(file3, check.names = FALSE)

  # Rename the columns to make them consistent across all data frames
  names(df_regions) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_countries) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_authorities) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_Inner_London) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_Outer_London) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_Kingston_upon_Thames) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_North_Northamptonshire) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")

  # Add 'geographic_level' column to each dataframe
  df_regions$geographic_level <- "Regional"
  df_countries$geographic_level <- "National"
  df_authorities$geographic_level <- "Local authority"
  df_Kingston_upon_Thames$geographic_level <- "Local authority"
  df_North_Northamptonshire$geographic_level <- "Local authority"
  df_Inner_London$geographic_level <- "Regional"
  df_Outer_London$geographic_level <- "Regional"

  # include just England to make national data
  df_countries <- df_countries[df_countries$Code %in% "E92000001", ]

  # create England data
  df_countries <- df_countries %>%
    mutate(Name = "National", Code = "E92000001")

  # select just Richmond upon Thames and Kingston upon Thames
  df_Kingston_upon_Thames <- df_Kingston_upon_Thames[df_Kingston_upon_Thames$Code %in% c("E09000021", "E09000027"), ]

  # select just North Northamptonshire and West Northamptonshire
  df_North_Northamptonshire <- df_North_Northamptonshire[df_North_Northamptonshire$Code %in% c("E06000061", "E06000062"), ]

  # remove Richmond upon Thames, Kingston upon Thames, North Northmptonshire & West Northamptonshire from LA file
  df_authorities <- df_authorities[!(df_authorities$Code %in% c("E09000021", "E09000027", "E06000061", "E06000062")), ]

  # include just inner London LAs to make inner London data
  df_Inner_London <- df_Inner_London[df_Inner_London$Code %in% c(
    "E09000001",
    "E09000007",
    "E09000012",
    "E09000013",
    "E09000014",
    "E09000019",
    "E09000020",
    "E09000022",
    "E09000023",
    "E09000025",
    "E09000028",
    "E09000030",
    "E09000032",
    "E09000033"
  ), ]

  # include just outer London LAs to make outer London data
  df_Outer_London <- df_Outer_London[df_Outer_London$Code %in% c(
    "E09000002",
    "E09000003",
    "E09000004",
    "E09000005",
    "E09000006",
    "E09000008",
    "E09000009",
    "E09000010",
    "E09000011",
    "E09000015",
    "E09000016",
    "E09000017",
    "E09000018",
    "E09000021",
    "E09000024",
    "E09000026",
    "E09000027",
    "E09000029",
    "E09000031"
  ), ]


  # create Kingston upon Thames/Richmond data (they submit a joint workforce return)
  df_Kingston_upon_Thames <- df_Kingston_upon_Thames %>%
    mutate(Name = "Kingston upon Thames / Richmond upon Thames", Code = "E09000021 / E09000027") %>%
    group_by(Code, Name, EthnicGroupCode, EthnicGroup, geographic_level) %>%
    summarise(Observation = sum(Observation), .groups = "drop")

  # create N/W Northamptonshire data (they submit a joint workforce return)
  df_North_Northamptonshire <- df_North_Northamptonshire %>%
    mutate(Name = "North Northamptonshire / West Northamptonshire", Code = "E06000061 / E06000062") %>%
    group_by(Code, Name, EthnicGroupCode, EthnicGroup, geographic_level) %>%
    summarise(Observation = sum(Observation), .groups = "drop")

  # create outer London data
  df_Inner_London <- df_Inner_London %>%
    mutate(Name = "Inner London", Code = "E13000001") %>%
    group_by(Code, Name, EthnicGroupCode, EthnicGroup, geographic_level) %>%
    summarise(Observation = sum(Observation), .groups = "drop")

  # create inner London data
  df_Outer_London <- df_Outer_London %>%
    mutate(Name = "Outer London", Code = "E13000002") %>%
    group_by(Code, Name, EthnicGroupCode, EthnicGroup, geographic_level) %>%
    summarise(Observation = sum(Observation), .groups = "drop")

  # Combine the data frames
  ethnic_population_data <- rbind(df_regions, df_countries, df_authorities, df_Inner_London, df_Outer_London, df_Kingston_upon_Thames, df_North_Northamptonshire)
  ethnic_population_data <- subset(ethnic_population_data, select = -c(EthnicGroupCode))

  # Remove rows where 'EthnicGroup' equals 'Does not apply'
  ethnic_population_data <- ethnic_population_data[ethnic_population_data$EthnicGroup != "Does not apply", ]

  # Convert 'EthnicGroup' to character type
  ethnic_population_data$EthnicGroup <- as.character(ethnic_population_data$EthnicGroup)

  # Add a new column 'EthnicGroupShort' that has a value of the first 5 characters in the 'EthnicGroup' column
  ethnic_population_data <- ethnic_population_data %>%
    mutate(EthnicGroupShort = substr(EthnicGroup, 1, 5))

  # Calculate the total observation for each 'Name'
  total_observation <- ethnic_population_data %>%
    group_by(Name, geographic_level) %>%
    summarise(TotalObservation = sum(Observation), .groups = "drop")

  # Join the total observation back to the original dataframe
  ethnic_population_data <- left_join(ethnic_population_data, total_observation, by = c("Name", "geographic_level"))

  # Group by 'Name', 'geographic_level' and 'EthnicGroupShort', and calculate the percentage, select unique values
  ethnic_population_data <- ethnic_population_data %>%
    group_by(Code, Name, geographic_level, EthnicGroupShort) %>%
    summarise(Percentage = round(sum(Observation) / TotalObservation * 100, 1), .groups = "drop") %>%
    unique()

  return(ethnic_population_data)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merge_eth_dataframes <- function() {
  # Read the data
  workforce_eth <- read_workforce_eth_data()
  population_eth <- read_ethnic_population_data()


  # Filter to include only the latest year of data
  latest_year <- max(workforce_eth$time_period)
  workforce_eth <- subset(workforce_eth, time_period == latest_year)

  # Filter to only include the ethnicity groups for all social workers
  workforce_eth <- workforce_eth %>%
    filter(breakdown_topic == "Ethnicity major", role == "Total") %>%
    filter(!(breakdown %in% c("Total", "Not known", "Known")))

  # Amend names of ethnic groups to match ONS data
  workforce_eth <- workforce_eth %>%
    mutate(breakdown = case_when(
      breakdown == "Mixed / Multiple ethnic groups" ~ "Mixed",
      breakdown == "Asian / Asian British" ~ "Asian",
      breakdown == "Black / African / Caribbean / Black British" ~ "Black",
      breakdown == "Other ethnic group" ~ "Other",
      TRUE ~ breakdown
    ))

  # Merge the two data frames
  merged_data <- left_join(workforce_eth, population_eth, by = c("code" = "Code", "breakdown" = "EthnicGroupShort"))

  return(merged_data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CLA rate per 10k children data
read_cla_rate_data <- function(file = "data/cla_number_and_rate_per_10k_children.csv") {
  cla_rate_data <- read.csv(file)
  cla_rate_data <- colClean(cla_rate_data) %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    mutate(rate_per_10000 = case_when(
      rate_per_10000 == "z" ~ NA,
      rate_per_10000 == "x" ~ NA,
      TRUE ~ as.numeric(rate_per_10000)
    )) %>%
    filter(!is.na(rate_per_10000)) %>%
    select(geographic_level, geo_breakdown, time_period, region_code, region_name, new_la_code, la_name, population_count, population_estimate, number, rate_per_10000) %>%
    distinct()


  return(cla_rate_data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_cla_placement_data <- function(file = "data/la_children_who_started_to_be_looked_after_during_the_year.csv") {
  cla_placement_data <- read.csv(file)
  cla_placement_data <- colClean(cla_placement_data) %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    mutate(percentage = case_when(
      percentage == "z" ~ NA,
      percentage == "x" ~ NA,
      TRUE ~ as.numeric(percentage)
    )) %>%
    filter(!is.na(percentage)) %>%
    select(geographic_level, geo_breakdown, time_period, region_code, region_name, new_la_code, la_name, cla_group, characteristic, number, percentage) %>%
    distinct()

  return(cla_placement_data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merge_cla_dataframes <- function() {
  # Read the data
  cla_rates <- read_cla_rate_data()
  cla_placements <- read_cla_placement_data()


  # Merge the two data frames
  # merged_data <- left_join(workforce_eth, population_eth, by = c("code" = "Code"))

  # Rename the columns to make it clear which dataset they come from
  cla_rates <- rename(cla_rates,
    rates_number = number
  )

  cla_placements <- rename(cla_placements,
    placements_number = number
  )

  # merge two data frames
  merged_data <- merge(cla_rates, cla_placements,
    by.x = c("geo_breakdown", "time_period", "geographic_level", "region_code", "region_name", "new_la_code", "la_name"),
    by.y = c("geo_breakdown", "time_period", "geographic_level", "region_code", "region_name", "new_la_code", "la_name")
  )

  merged_data <- merged_data %>%
    mutate(placement_per_10000 = round((as.numeric(placements_number) / as.numeric(population_estimate)) * 10000, 0))


  return(merged_data)
}

# CIN rate per 10k children data
read_cin_rate_data <- function(file = "data/b1_children_in_need_2013_to_2023.csv") {
  cin_rate_data <- read.csv(file)
  cin_rate_data <- colClean(cin_rate_data) %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    mutate(At31_episodes = case_when(
      At31_episodes == "Z" ~ NA,
      At31_episodes == "x" ~ NA,
      At31_episodes == "c" ~ NA,
      TRUE ~ as.numeric(At31_episodes)
    )) %>%
    mutate(At31_episodes_rate = case_when(
      At31_episodes_rate == "Z" ~ NA,
      At31_episodes_rate == "x" ~ NA,
      At31_episodes_rate == "c" ~ NA,
      TRUE ~ as.numeric(At31_episodes_rate)
    )) %>%
    select(geographic_level, geo_breakdown, time_period, region_code, region_name, new_la_code, la_name, At31_episodes, At31_episodes_rate) %>%
    distinct() %>%
    rename(CIN_rate = At31_episodes_rate, CIN_number = At31_episodes)


  return(cin_rate_data)
}

# CIN referrals data
read_cin_referral_data <- function(file = "data/c1_children_in_need_referrals_and_rereferrals_2013_to_2023.csv") {
  cin_referral_data <- read.csv(file)
  cin_referral_data <- colClean(cin_referral_data) %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    mutate(Referrals = case_when(
      Referrals == "Z" ~ NA,
      Referrals == "x" ~ NA,
      Referrals == "c" ~ NA,
      TRUE ~ as.numeric(Referrals)
    )) %>%
    mutate(Re_referrals = case_when(
      Re_referrals == "Z" ~ NA,
      Re_referrals == "x" ~ NA,
      Re_referrals == "c" ~ NA,
      TRUE ~ as.numeric(Re_referrals)
    )) %>%
    mutate(Re_referrals_percent = case_when(
      Re_referrals_percent == "Z" ~ NA,
      Re_referrals_percent == "x" ~ NA,
      Re_referrals_percent == "c" ~ NA,
      TRUE ~ as.numeric(Re_referrals_percent)
    )) %>%
    select(time_period, geographic_level, geo_breakdown, region_code, region_name, new_la_code, la_name, Referrals, Re_referrals, Re_referrals_percent) %>%
    distinct()


  # Calculate the number of referrals not including re-referrals
  referrals <- cin_referral_data %>%
    group_by(time_period, geographic_level, geo_breakdown, region_code, region_name, new_la_code, la_name) %>%
    summarise(
      referrals_not_including_re_referrals_perc = round((Referrals - Re_referrals) / Referrals * 100, 1),
      referrals_not_including_re_referrals = Referrals - Re_referrals,
      .groups = "drop"
    )

  # Join the referall back to the original dataframe
  cin_referral_data <- merge(referrals, cin_referral_data) %>%
    arrange(desc(time_period))


  return(cin_referral_data)
}
# Outcome 2 ----
# read_outcome2 <- function(file = "data/la_children_who_ceased_during_the_year.csv"){
#   ceased_cla_data <- read.csv(file)
#   ceased_cla_data <- ceased_cla_data %>% mutate(geo_breakdown = case_when(
#     geographic_level == "National" ~ "National",#NA_character_,
#     geographic_level == "Regional" ~ region_name,
#     geographic_level == "Local authority" ~ la_name
#   )) %>%
#     mutate(number = case_when(
#       number == "z" ~ NA,
#       number == "x"  ~ NA,
#       number == "c"  ~ NA,
#       TRUE ~ as.numeric(number)
#     )) %>%
#     select("time_period", "geographic_level","geo_breakdown", "cla_group","characteristic", "number", "percentage")
# }
# read_outcome2 <- function(file = "data/la_children_who_ceased_during_the_year.csv"){
#   ceased_cla_data <- read.csv(file)
#   ceased_cla_data <- ceased_cla_data %>% mutate(geo_breakdown = case_when(
#     geographic_level == "National" ~ "National",#NA_character_,
#     geographic_level == "Regional" ~ region_name,
#     geographic_level == "Local authority" ~ la_name
#   )) %>%
#     mutate(number = case_when(
#       number == "z" ~ NA,
#       number == "x"  ~ NA,
#       number == "c"  ~ NA,
#       TRUE ~ as.numeric(number)
#     )) %>%
#     select("time_period", "geographic_level","geo_breakdown", "cla_group","characteristic", "number", "percentage")
#
#   totals <- ceased_cla_data %>% filter(characteristic == "Total" & cla_group == "Reason episode ceased") %>%
#     rename("Total" = "number") %>%
#     select(time_period, geographic_level, geo_breakdown, cla_group, Total)
#
#
#   test<- ceased_cla_data %>% filter(cla_group == "Reason episode ceased" & characteristic != "Total")
#
#   joined <- left_join(test, totals, by = c("time_period", "geographic_level","geo_breakdown", "cla_group"))
#   joined$perc <- round((joined$number/joined$Total)*100, digits = 1)
#   joined <- joined %>% mutate(perc = case_when(
#     percentage == "z" ~ "z",
#     percentage == "c" ~ "c",
#     percentage == "k" ~ "k",
#     percentage == "x" ~ "x",
#     TRUE ~ as.character(perc))) %>% mutate(`Percentage ceased %` = case_when(
#       percentage == "z" ~ NA,
#       percentage == "c" ~ NA,
#       percentage == "k" ~ NA,
#       percentage == "x" ~ NA,
#       TRUE ~ as.numeric(perc)
#     ))
#
#   return(joined)
# }

read_outcome2 <- function(file = "data/la_children_who_ceased_during_the_year.csv") {
  ceased_cla_data <- read.csv(file)
  old_dorset <- ceased_cla_data %>% filter(time_period <= 2019, new_la_code == "E10000009")
  new_dorset <- ceased_cla_data %>% filter(time_period > 2019, new_la_code == "E06000059")
  data_without_dorset <- ceased_cla_data %>% filter(la_name != "Dorset")

  dorset_data <- bind_rows(new_dorset, old_dorset)
  final_filtered_data <- bind_rows(data_without_dorset, dorset_data)

  ceased_cla_data <- final_filtered_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    mutate(number = case_when(
      number == "z" ~ NA,
      number == "x" ~ NA,
      number == "c" ~ NA,
      TRUE ~ as.numeric(number)
    )) %>%
    filter("new_la_code" != "E10000009") %>%
    select("time_period", "geographic_level", "geo_breakdown", "cla_group", "characteristic", "number", "percentage")

  totals <- ceased_cla_data %>%
    filter(characteristic == "Total" & cla_group == "Reason episode ceased") %>%
    rename("Total" = "number") %>%
    select(time_period, geographic_level, geo_breakdown, cla_group, Total)


  test <- ceased_cla_data %>% filter(cla_group == "Reason episode ceased" & characteristic != "Total")

  joined <- left_join(test, totals, by = c("time_period", "geographic_level", "geo_breakdown", "cla_group"))
  joined$perc <- round((joined$number / joined$Total) * 100, digits = 1)
  joined <- joined %>%
    mutate(perc = case_when(
      percentage == "z" ~ "z",
      percentage == "c" ~ "c",
      percentage == "k" ~ "k",
      percentage == "x" ~ "x",
      TRUE ~ as.character(perc)
    )) %>%
    mutate(`Ceased (%)` = case_when(
      percentage == "z" ~ NA,
      percentage == "c" ~ NA,
      percentage == "k" ~ NA,
      percentage == "x" ~ NA,
      TRUE ~ as.numeric(perc)
    ))

  return(joined)
}
