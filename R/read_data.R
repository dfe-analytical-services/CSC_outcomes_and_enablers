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

remove_old_la_data <- function(data) {
  ons_la_data <- read.csv("data/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(April_2023)_Lookup_in_England_and_Wales.csv")
  ons_la_data <- ons_la_data %>%
    select(UTLA23CD, UTLA23NM) %>%
    filter(!str_starts(UTLA23CD, "W")) %>%
    unique()

  removed_old_las <- data %>% filter(new_la_code == "" | new_la_code %in% (ons_la_data$UTLA23CD))

  return(removed_old_las)
}


# # sample data functions we dont need this~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read_revenue_data <- function(file = "data/la_maintained_schools_revenue_reserve_final.csv") {
#   # This reads in an example file. For the purposes of this demo, we're using the
#   # latest LA expenditure data downloaded from the EES release.
#   dfRevenue <- read.csv(file)
#   # The time period column name has some non-ascii characters so we're just going to rename it here.
#   colnames(dfRevenue)[1] <- "time_period"
#   dfRevenue <- dfRevenue %>% mutate(
#     year = as.numeric(paste0("20", substr(format(time_period), 5, 6))),
#     area_name = case_when(
#       geographic_level == "National" ~ country_name,
#       geographic_level == "Regional" ~ region_name,
#       TRUE ~ la_name
#     )
#   )
#   return(dfRevenue)
# }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Test not important
# read_definitions <- function(file = "data/definitions.csv") {
#   definitions <- read.csv(file)
#   # colnames(definitions) <- c("Outcome/Enabler", "Domain", "Indicator", "Rationale/Description")
#   #  definitions <- definitions[,1:4]
#   return(definitions)
# }


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
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, turnover_rate_fte, time_period, "time_period", "turnover_rate_fte", "absence_rate_fte",
      "agency_rate_fte", "agency_cover_rate_fte", "vacancy_rate_fte", "vacancy_agency_cover_rate_fte",
      "turnover_rate_headcount", "agency_rate_headcount", "caseload_fte"
    ) %>%
    # removing old Dorset
    filter(new_la_code != "E10000009") %>%
    distinct()

  workforce_data2 <- suppressWarnings(workforce_data %>%
    mutate(across(
      .cols = grep("fte", colnames(workforce_data)),
      .fns = ~ case_when(
        . == "c" ~ -100,
        . == "low" ~ -200,
        . == "k" ~ -200,
        . == "u" ~ -250,
        . == "x" ~ -300,
        . == "z" ~ -400,
        TRUE ~ as.numeric(.)
      ),
      .names = "{str_to_title(str_replace_all(.col, '_', ' '))}"
    )))

  workforce_data3 <- convert_perc_cols_to_numeric(workforce_data2)

  # colnames(workforce_data) <- c("Geographic Level","Geographic Breakdown", "Turnover Rate (FTE) %", "Time Period", "Absence Rate (FTE) %",
  #                             "Agency Worker Rate (FTE) %", "Agency Cover Rate (FTE) %", "Vacancy Rate (FTE) %", "Vacancy Agency Cover Rate (FTE) %",
  #                             "Turnover Rate Headcount %", "Agency Worker Rate Headcount %", "Caseload (FTE)")
  #
  #
  return(workforce_data3)
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
    ) %>%
    # removing old Dorset
    filter(new_la_code != "E10000009")

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
    filter(breakdown_topic == "Ethnicity major") %>%
    # removing old Dorset
    filter(new_la_code != "E10000009")

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
    mutate(`Rate Per 10000` = case_when(
      rate_per_10000 == "c" ~ -100,
      rate_per_10000 == "low" ~ -200,
      rate_per_10000 == "k" ~ -200,
      rate_per_10000 == "u" ~ -250,
      rate_per_10000 == "x" ~ -300,
      rate_per_10000 == "z" ~ -400,
      TRUE ~ as.numeric(rate_per_10000)
    )) %>%
    mutate(rate_per_10000 = case_when(
      rate_per_10000 == "z" ~ NA,
      rate_per_10000 == "x" ~ NA,
      TRUE ~ as.numeric(rate_per_10000)
    )) %>%
    # filter(!is.na(rate_per_10000)) %>%
    # removing old Dorset, Poole, Bournemouth, Northamptonshire
    filter(!(new_la_code %in% c("E10000009", "E10000021", "E06000028", "E06000029"))) %>%
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
    mutate(Percentage = case_when(
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      TRUE ~ as.numeric(percentage)
    )) %>%
    mutate(percentage = case_when(
      percentage == "z" ~ NA,
      percentage == "x" ~ NA,
      TRUE ~ as.numeric(percentage)
    )) %>%
    #  filter(!is.na(percentage)) %>%
    # removing old Dorset, Poole, Bournemouth, Northamptonshire
    filter(!(new_la_code %in% c("E10000009", "E10000021", "E06000028", "E06000029"))) %>%
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
    mutate(placement_per_10000 = round((as.numeric(placements_number) / as.numeric(population_estimate)) * 10000, 0)) %>%
    mutate(`Placement Rate Per 10000` = case_when(
      placements_number == "c" ~ -100,
      placements_number == "low" ~ -200,
      placements_number == "k" ~ -200,
      placements_number == "u" ~ -250,
      placements_number == "x" ~ -300,
      placements_number == "z" ~ -400,
      TRUE ~ as.numeric(placement_per_10000)
    ))


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
    mutate(CIN_number = case_when(
      At31_episodes == "Z" ~ NA,
      At31_episodes == "x" ~ NA,
      At31_episodes == "c" ~ NA,
      TRUE ~ as.numeric(At31_episodes)
    )) %>%
    mutate(CIN_rate = case_when(
      At31_episodes_rate == "Z" ~ NA,
      At31_episodes_rate == "x" ~ NA,
      At31_episodes_rate == "c" ~ NA,
      TRUE ~ as.numeric(At31_episodes_rate)
    )) %>%
    select(geographic_level, geo_breakdown, time_period, region_code, region_name, new_la_code, la_name, CIN_number, At31_episodes, CIN_rate, At31_episodes_rate) %>%
    distinct() %>%
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
    mutate(Referrals_num = case_when(
      Referrals == "Z" ~ NA,
      Referrals == "x" ~ NA,
      Referrals == "c" ~ NA,
      TRUE ~ as.numeric(Referrals)
    )) %>%
    mutate(Re_referrals_num = case_when(
      Re_referrals == "Z" ~ NA,
      Re_referrals == "x" ~ NA,
      Re_referrals == "c" ~ NA,
      TRUE ~ as.numeric(Re_referrals)
    )) %>%
    mutate(Re_referrals_percentage = case_when(
      Re_referrals_percent == "Z" ~ NA,
      Re_referrals_percent == "x" ~ NA,
      Re_referrals_percent == "c" ~ NA,
      TRUE ~ as.numeric(Re_referrals_percent)
    )) %>%
    mutate(`Re-referrals (%)` = case_when(
      Re_referrals_percent == "c" ~ -100,
      Re_referrals_percent == "low" ~ -200,
      Re_referrals_percent == "k" ~ -200,
      Re_referrals_percent == "u" ~ -250,
      Re_referrals_percent == "x" ~ -300,
      Re_referrals_percent == "z" ~ -400,
      TRUE ~ as.numeric(Re_referrals_percent)
    )) %>%
    select(
      time_period, geographic_level, geo_breakdown, region_code, region_name, new_la_code, la_name,
      Referrals, Re_referrals, Re_referrals_percent, Referrals_num, Re_referrals_num, Re_referrals_percentage
    ) %>%
    distinct()


  # Calculate the number of referrals not including re-referrals
  #  referrals <- cin_referral_data %>%
  #   group_by(time_period, geographic_level, geo_breakdown, region_code, region_name, new_la_code, la_name) %>%
  #  summarise(
  #  referrals_not_including_re_referrals_perc = round((Referrals - Re_referrals) / Referrals * 100, 1),
  # referrals_not_including_re_referrals = Referrals - Re_referrals,
  # )

  # Join the referall back to the original dataframe
  # cin_referral_data <- merge(referrals, cin_referral_data) %>%
  #  arrange(desc(time_period))


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
  read_data <- read.csv(file)
  # Call remove old la data function to remove the old
  # final_filtered_data <- remove_old_la_data(read_data)
  las_to_remove <- c("Poole", "Bournemouth", "Northamptonshire")

  final_filtered_data <- read_data %>% filter(new_la_code != "E10000009", !la_name %in% las_to_remove)
  ceased_cla_data <- final_filtered_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    mutate(number_num = case_when(
      number == "z" ~ NA,
      number == "x" ~ NA,
      number == "c" ~ NA,
      TRUE ~ as.numeric(number)
    )) %>%
    select("time_period", "geographic_level", "geo_breakdown", "old_la_code", "new_la_code", "cla_group", "characteristic", "number", "number_num", "percentage")

  totals <- ceased_cla_data %>%
    filter(characteristic == "Total") %>%
    rename("Total_num" = "number_num") %>%
    mutate("Total" = number) %>%
    select(time_period, geographic_level, geo_breakdown, cla_group, Total_num, Total)

  joined <- left_join(ceased_cla_data, totals, by = c("time_period", "geographic_level", "geo_breakdown", "cla_group"))
  joined$perc <- round((joined$number_num / joined$Total_num) * 100, digits = 1)

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
# Outcome 1 again ----
# Outcome 1 Outcomes absence data for child well being and development
read_outcomes_absence_data <- function(file = "data/absence_six_half_terms_la.csv") {
  outcomes_absence_data <- read.csv(file)
  # Select only columns we want
  outcomes_absence_data <- outcomes_absence_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, time_period,
      "time_period", "geographic_level", "region_name", year_breakdown, social_care_group,
      school_type, t_pupils, t_sess_possible, t_sess_overall, pt_overall, t_sess_authorised,
      pt_sess_authorised, t_sess_unauthorised, pt_sess_unauthorised, t_pupils_pa_10_exact, pt_pupils_pa_10_exact
    )

  # Make % columns numeric
  outcomes_absence_data <- outcomes_absence_data %>%
    mutate(`Overall absence (%)` = case_when(
      pt_overall == "z" ~ NA,
      pt_overall == "c" ~ NA,
      pt_overall == "k" ~ NA,
      pt_overall == "x" ~ NA,
      TRUE ~ as.numeric(pt_overall)
    )) %>%
    mutate(`Persistent absentees (%)` = case_when(
      pt_pupils_pa_10_exact == "z" ~ NA,
      pt_pupils_pa_10_exact == "c" ~ NA,
      pt_pupils_pa_10_exact == "k" ~ NA,
      pt_pupils_pa_10_exact == "x" ~ NA,
      TRUE ~ as.numeric(pt_pupils_pa_10_exact)
    )) %>%
    mutate(`Authorised absence (%)` = case_when(
      pt_sess_authorised == "z" ~ NA,
      pt_sess_authorised == "c" ~ NA,
      pt_sess_authorised == "k" ~ NA,
      pt_sess_authorised == "x" ~ NA,
      TRUE ~ as.numeric(pt_sess_authorised)
    )) %>%
    mutate(`Unauthorised absence (%)` = case_when(
      pt_sess_unauthorised == "z" ~ NA,
      pt_sess_unauthorised == "c" ~ NA,
      pt_sess_unauthorised == "k" ~ NA,
      pt_sess_unauthorised == "x" ~ NA,
      TRUE ~ as.numeric(pt_sess_unauthorised)
    ))


  return(outcomes_absence_data)
}


# Outcome 1 Outcomes KS2 data for education attainment
read_outcomes_ks2_data <- function(file = "data/ks2_la.csv") {
  outcomes_ks2_data <- read.csv(file)
  # Select only columns we want
  outcomes_ks2_data <- outcomes_ks2_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, time_period,
      "time_period", "geographic_level", "region_name", social_care_group,
      version, t_read_eligible_pupils, t_read_met_expected_standard, pt_read_met_expected_standard, t_writta_eligible_pupils,
      t_writta_met_expected_standard, pt_writta_met_expected_standard, t_mat_eligible_pupils, t_mat_met_expected_standard,
      pt_mat_met_expected_standard, t_rwm_eligible_pupils, t_rwm_met_expected_standard, pt_rwm_met_expected_standard,
      t_gps_eligible_pupils, t_gps_met_expected_standard, pt_gps_met_expected_standard, t_scita_eligible_pupils,
      t_scita_met_expected_standard, pt_scita_met_expected_standard, t_read_progress_eligible_pupils,
      t_read_progress_score, avg_read_progress_score, avg_read_progress_score_lower_CI, avg_read_progress_score_upper_CI,
      t_writta_progress_eligible_pupils, t_writta_progress_score, avg_writta_progress_score, avg_writta_progress_score_lower_CI,
      avg_writta_progress_score_upper_CI, t_mat_progress_eligible_pupils, t_mat_progress_score, avg_mat_progress_score,
      avg_mat_progress_score_lower_CI, avg_mat_progress_score_upper_CI
    )

  # Make % columns numeric
  outcomes_ks2_data <- outcomes_ks2_data %>%
    mutate(`Expected standard reading writing maths (%)` = case_when(
      pt_rwm_met_expected_standard == "z" ~ NA,
      pt_rwm_met_expected_standard == "c" ~ NA,
      pt_rwm_met_expected_standard == "k" ~ NA,
      pt_rwm_met_expected_standard == "x" ~ NA,
      TRUE ~ as.numeric(pt_rwm_met_expected_standard)
    ))


  return(outcomes_ks2_data)
}

# Outcome 1 Outcomes KS4 data for education attainment
read_outcomes_ks4_data <- function(file = "data/ks4_la.csv") {
  outcomes_ks4_data <- read.csv(file)
  # Select only columns we want
  outcomes_ks4_data <- outcomes_ks4_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, time_period,
      "time_period", "geographic_level", "region_name", social_care_group,
      version, t_pupils, t_att8, avg_att8, t_l2basics_95, pt_l2basics_95, t_l2basics_94, pt_l2basics_94,
      t_ebacc_e_ptq_ee, pt_ebacc_e_ptq_ee, t_ebaccaps, avg_ebaccaps, t_inp8calc,
      t_p8score, avg_p8score, p8score_CI_low, p8score_CI_upp
    )

  # Make number columns numeric
  outcomes_ks4_data <- outcomes_ks4_data %>%
    mutate(`Average Attainment 8` = case_when(
      avg_att8 == "z" ~ NA,
      avg_att8 == "c" ~ NA,
      avg_att8 == "k" ~ NA,
      avg_att8 == "x" ~ NA,
      TRUE ~ as.numeric(avg_att8)
    ))


  return(outcomes_ks4_data)
}

# Statistical Neighbours ------------
statistical_neighbours <- function(file = "data/New_Statistical_Neighbour_Groupings_April_2021.csv") {
  stats_neighbours <- read.csv(file)

  # Create a lookup table
  lookup <- stats_neighbours %>% select("LA.Name", "LA.number")

  df <- stats_neighbours %>% select("LA.Name", "LA.number", "SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10")


  for (col in c("SN1", "SN2", "SN3", "SN4", "SN5", "SN6", "SN7", "SN8", "SN9", "SN10")) {
    df[[col]] <- lookup$LA.Name[match(df[[col]], lookup$"LA.number")]
  }


  return(df)
}
