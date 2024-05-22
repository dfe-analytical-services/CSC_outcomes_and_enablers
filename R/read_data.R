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
# Enabler 2 -----------------
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
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, old_la_code, turnover_rate_fte, time_period, "time_period", "turnover_rate_fte", "absence_rate_fte",
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

  # workforce_data3 <- convert_perc_cols_to_numeric(workforce_data2)

  # colnames(workforce_data) <- c("Geographic Level","Geographic Breakdown", "Turnover Rate (FTE) %", "Time Period", "Absence Rate (FTE) %",
  #                             "Agency Worker Rate (FTE) %", "Agency Cover Rate (FTE) %", "Vacancy Rate (FTE) %", "Vacancy Agency Cover Rate (FTE) %",
  #                             "Turnover Rate Headcount %", "Agency Worker Rate Headcount %", "Caseload (FTE)")
  #
  #
  return(workforce_data2)
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
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, old_la_code, time_period,
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
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, old_la_code, time_period,
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
      inpost_headcount == "Z" ~ 0,
      inpost_headcount == "x" ~ 0,
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
# Outcome 1 -------------------
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
    mutate(Number = case_when(
      number == "c" ~ -100,
      number == "low" ~ -200,
      number == "k" ~ -200,
      number == "u" ~ -250,
      number == "x" ~ -300,
      number == "z" ~ -400,
      TRUE ~ as.numeric(number)
    )) %>%
    # )) %>%
    # filter(!is.na(rate_per_10000)) %>%
    # removing old Dorset, Poole, Bournemouth, Northamptonshire
    filter(!(new_la_code %in% c("E10000009", "E10000021", "E06000028", "E06000029"))) %>%
    select(geographic_level, geo_breakdown, time_period, region_code, region_name, new_la_code, old_la_code, la_name, population_count, population_estimate, number, Number, rate_per_10000, `Rate Per 10000`) %>%
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
    # mutate(percentage = case_when(
    #   percentage == "z" ~ NA,
    #   percentage == "x" ~ NA,
    #   TRUE ~ as.numeric(percentage)
    # )) %>%
    #  filter(!is.na(percentage)) %>%
    # removing old Dorset, Poole, Bournemouth, Northamptonshire
    filter(!(new_la_code %in% c("E10000009", "E10000021", "E06000028", "E06000029"))) %>%
    select(geographic_level, geo_breakdown, time_period, region_code, region_name, new_la_code, old_la_code, la_name, cla_group, characteristic, number, percentage) %>%
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
    by.x = c("geo_breakdown", "time_period", "geographic_level", "region_code", "region_name", "new_la_code", "old_la_code", "la_name"),
    by.y = c("geo_breakdown", "time_period", "geographic_level", "region_code", "region_name", "new_la_code", "old_la_code", "la_name")
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
    )) %>%
    mutate("placement_per_10000" = case_when(
      placements_number == "c" ~ "c",
      placements_number == "low" ~ "low",
      placements_number == "k" ~ "k",
      placements_number == "u" ~ "u",
      placements_number == "x" ~ "x",
      placements_number == "z" ~ "z",
      TRUE ~ as.character(placement_per_10000)
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
      # At31_episodes == "Z" ~ NA,
      # At31_episodes == "x" ~ NA,
      # At31_episodes == "c" ~ NA,
      At31_episodes == "c" ~ -100,
      At31_episodes == "low" ~ -200,
      At31_episodes == "k" ~ -200,
      At31_episodes == "u" ~ -250,
      At31_episodes == "x" ~ -300,
      At31_episodes == "z" ~ -400,
      TRUE ~ as.numeric(At31_episodes)
    )) %>%
    mutate(CIN_rate = case_when(
      # At31_episodes_rate == "Z" ~ NA,
      # At31_episodes_rate == "x" ~ NA,
      # At31_episodes_rate == "c" ~ NA,
      At31_episodes_rate == "c" ~ -100,
      At31_episodes_rate == "low" ~ -200,
      At31_episodes_rate == "k" ~ -200,
      At31_episodes_rate == "u" ~ -250,
      At31_episodes_rate == "x" ~ -300,
      At31_episodes_rate == "z" ~ -400,
      TRUE ~ as.numeric(At31_episodes_rate)
    )) %>%
    select(geographic_level, geo_breakdown, time_period, region_code, region_name, new_la_code, old_la_code, la_name, CIN_number, At31_episodes, CIN_rate, At31_episodes_rate) %>%
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
      # Referrals == "Z" ~ NA,
      # Referrals == "x" ~ NA,
      # Referrals == "c" ~ NA,
      Referrals == "c" ~ -100,
      Referrals == "low" ~ -200,
      Referrals == "k" ~ -200,
      Referrals == "u" ~ -250,
      Referrals == "x" ~ -300,
      Referrals == "z" ~ -400,
      TRUE ~ as.numeric(Referrals)
    )) %>%
    mutate(Re_referrals_num = case_when(
      # Re_referrals == "Z" ~ NA,
      # Re_referrals == "x" ~ NA,
      # Re_referrals == "c" ~ NA,
      Re_referrals == "c" ~ -100,
      Re_referrals == "low" ~ -200,
      Re_referrals == "k" ~ -200,
      Re_referrals == "u" ~ -250,
      Re_referrals == "x" ~ -300,
      Re_referrals == "z" ~ -400,
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
      time_period, geographic_level, geo_breakdown, region_code, region_name, new_la_code, old_la_code, la_name,
      Referrals, Re_referrals, Re_referrals_percent, Referrals_num, Re_referrals_num, Re_referrals_percentage, `Re-referrals (%)`
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
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, old_la_code, time_period,
      "time_period", "geographic_level", "region_name", year_breakdown, social_care_group,
      school_type, t_pupils, t_sess_possible, t_sess_overall, pt_overall, t_sess_authorised,
      pt_sess_authorised, t_sess_unauthorised, pt_sess_unauthorised, t_pupils_pa_10_exact, pt_pupils_pa_10_exact
    )

  # Make % columns numeric
  outcomes_absence_data <- outcomes_absence_data %>%
    mutate(`Overall absence (%)` = case_when(
      pt_overall == "c" ~ -100,
      pt_overall == "low" ~ -200,
      pt_overall == "k" ~ -200,
      pt_overall == "u" ~ -250,
      pt_overall == "x" ~ -300,
      pt_overall == "z" ~ -400,
      TRUE ~ as.numeric(pt_overall)
    )) %>%
    mutate(`Persistent absentees (%)` = case_when(
      pt_pupils_pa_10_exact == "c" ~ -100,
      pt_pupils_pa_10_exact == "low" ~ -200,
      pt_pupils_pa_10_exact == "k" ~ -200,
      pt_pupils_pa_10_exact == "u" ~ -250,
      pt_pupils_pa_10_exact == "x" ~ -300,
      pt_pupils_pa_10_exact == "z" ~ -400,
      TRUE ~ as.numeric(pt_pupils_pa_10_exact)
    )) %>%
    mutate(`Authorised absence (%)` = case_when(
      pt_sess_authorised == "c" ~ -100,
      pt_sess_authorised == "low" ~ -200,
      pt_sess_authorised == "k" ~ -200,
      pt_sess_authorised == "u" ~ -250,
      pt_sess_authorised == "x" ~ -300,
      pt_sess_authorised == "z" ~ -400,
      TRUE ~ as.numeric(pt_sess_authorised)
    )) %>%
    mutate(`Unauthorised absence (%)` = case_when(
      pt_sess_unauthorised == "c" ~ -100,
      pt_sess_unauthorised == "low" ~ -200,
      pt_sess_unauthorised == "k" ~ -200,
      pt_sess_unauthorised == "u" ~ -250,
      pt_sess_unauthorised == "x" ~ -300,
      pt_sess_unauthorised == "z" ~ -400,
      TRUE ~ as.numeric(pt_sess_unauthorised)
    )) %>%
    mutate(`Total pupils` = case_when(
      t_pupils == "c" ~ -100,
      t_pupils == "low" ~ -200,
      t_pupils == "k" ~ -200,
      t_pupils == "u" ~ -250,
      t_pupils == "x" ~ -300,
      t_pupils == "z" ~ -400,
      TRUE ~ as.numeric(t_pupils)
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
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, old_la_code, time_period,
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
      # pt_rwm_met_expected_standard == "z" ~ NA,
      # pt_rwm_met_expected_standard == "c" ~ NA,
      # pt_rwm_met_expected_standard == "k" ~ NA,
      # pt_rwm_met_expected_standard == "x" ~ NA,
      pt_rwm_met_expected_standard == "c" ~ -100,
      pt_rwm_met_expected_standard == "low" ~ -200,
      pt_rwm_met_expected_standard == "k" ~ -200,
      pt_rwm_met_expected_standard == "u" ~ -250,
      pt_rwm_met_expected_standard == "x" ~ -300,
      pt_rwm_met_expected_standard == "z" ~ -400,
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
      geographic_level, geo_breakdown, country_code, region_code, new_la_code, old_la_code, time_period,
      "time_period", "geographic_level", "region_name", social_care_group,
      version, t_pupils, t_att8, avg_att8, t_l2basics_95, pt_l2basics_95, t_l2basics_94, pt_l2basics_94,
      t_ebacc_e_ptq_ee, pt_ebacc_e_ptq_ee, t_ebaccaps, avg_ebaccaps, t_inp8calc,
      t_p8score, avg_p8score, p8score_CI_low, p8score_CI_upp
    )

  # Make number columns numeric
  outcomes_ks4_data <- outcomes_ks4_data %>%
    mutate(`Average Attainment 8` = case_when(
      #   avg_att8 == "z" ~ NA,
      #   avg_att8 == "c" ~ NA,
      #   avg_att8 == "k" ~ NA,
      #   avg_att8 == "x" ~ NA,
      avg_att8 == "c" ~ -100,
      avg_att8 == "low" ~ -200,
      avg_att8 == "k" ~ -200,
      avg_att8 == "u" ~ -250,
      avg_att8 == "x" ~ -300,
      avg_att8 == "z" ~ -400,
      TRUE ~ as.numeric(avg_att8)
    )) %>%
    mutate(`Total pupils` = case_when(
      t_pupils == "c" ~ -100,
      t_pupils == "low" ~ -200,
      t_pupils == "k" ~ -200,
      t_pupils == "u" ~ -250,
      t_pupils == "x" ~ -300,
      t_pupils == "z" ~ -400,
      TRUE ~ as.numeric(t_pupils)
    ))


  return(outcomes_ks4_data)
}

# Outcome 3 Child Protection Plans starting during year, which were second or subsequent plans
read_cpp_in_year_data <- function(file = "data/d3_cpps_subsequent_plan_2013_to_2023.csv") {
  cpp_in_year_data <- read.csv(file)

  # Select only columns we want
  cpp_in_year_data <- cpp_in_year_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(
      time_period, geographic_level, geo_breakdown, country_code, region_code, region_name, new_la_code, old_la_code, la_name, CPP_start, CPP_subsequent, CPP_subsequent_percent
    )

  # Make number columns
  #  cpp_in_year_data <- cpp_in_year_data %>%
  # mutate(`CPP_start` = case_when(
  #  CPP_start == "z" ~ NA,
  #  CPP_start == "c" ~ NA,
  # CPP_start == "k" ~ NA,
  #  CPP_start == "x" ~ NA,
  # TRUE ~ as.numeric(CPP_start)
  #  ))

  # cpp_in_year_data <- cpp_in_year_data %>%
  #  mutate(`CPP_subsequent` = case_when(
  #  CPP_subsequent == "z" ~ NA,
  #  CPP_subsequent == "c" ~ NA,
  #  CPP_subsequent == "k" ~ NA,
  #  CPP_subsequent == "x" ~ NA,
  #  TRUE ~ as.numeric(CPP_subsequent)
  # ))

  cpp_in_year_data <- cpp_in_year_data %>%
    mutate(`Repeat_CPP_percent` = case_when(
      CPP_subsequent_percent == "c" ~ -100,
      CPP_subsequent_percent == "low" ~ -200,
      CPP_subsequent_percent == "k" ~ -200,
      CPP_subsequent_percent == "u" ~ -250,
      CPP_subsequent_percent == "x" ~ -300,
      CPP_subsequent_percent == "z" ~ -400,
      TRUE ~ as.numeric(CPP_subsequent_percent)
    ))
}

read_cpp_by_duration_data <- function(file = "data/d5_cpps_at31march_by_duration_2013_to_2023.csv") {
  cpp_by_duration_data <- read.csv(file) %>%
    filter(geographic_level != "Local authority")

  cpp_by_duration_data <- cpp_by_duration_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",
      geographic_level == "Regional" ~ region_name
    )) %>%
    select(
      time_period, geographic_level, geo_breakdown, country_code, region_code, region_name,
      CPP_At31, `X3_months_or_less`, `X3_months_or_less_percent`, more_than_3_months_6_months, more_than_3_months_6_months_percent, more_than_6_months_less_than_1_year,
      more_than_6_months_less_than_1_year_percent, `X1_year_less_than_2_years`, `X1_year_less_than_2_years_percent`, `X2_years_or_more`, `X2_years_or_more_percent`
    ) %>%
    mutate(`X2_years_or_more_percent` = case_when(
      `X2_years_or_more_percent` == "c" ~ -100,
      `X2_years_or_more_percent` == "low" ~ -200,
      `X2_years_or_more_percent` == "k" ~ -200,
      `X2_years_or_more_percent` == "u" ~ -250,
      `X2_years_or_more_percent` == "x" ~ -300,
      `X2_years_or_more_percent` == "z" ~ -400,
      TRUE ~ as.numeric(X2_years_or_more_percent)
    ))
}




# Outcome 2 ----
# Used this before the new function below
# read_outcome2 <- function(file = "data/la_children_who_ceased_during_the_year.csv") {
#   read_data <- read.csv(file)
#   # Call remove old la data function to remove the old
#   # final_filtered_data <- remove_old_la_data(read_data)
#   las_to_remove <- c("Poole", "Bournemouth", "Northamptonshire")
#
#   final_filtered_data <- read_data %>% filter(new_la_code != "E10000009", !la_name %in% las_to_remove)
#   ceased_cla_data <- final_filtered_data %>%
#     mutate(geo_breakdown = case_when(
#       geographic_level == "National" ~ "National", # NA_character_,
#       geographic_level == "Regional" ~ region_name,
#       geographic_level == "Local authority" ~ la_name
#     )) %>%
#     mutate(number_num = case_when(
#       number == "z" ~ NA,
#       number == "x" ~ NA,
#       number == "c" ~ NA,
#       TRUE ~ as.numeric(number)
#     )) %>%
#     select("time_period", "geographic_level", "geo_breakdown", "old_la_code", "new_la_code", "cla_group", "characteristic", "number", "number_num", "percentage")
#
#   totals <- ceased_cla_data %>%
#     filter(characteristic == "Total") %>%
#     rename("Total_num" = "number_num") %>%
#     mutate("Total" = number) %>%
#     select(time_period, geographic_level, geo_breakdown, cla_group, Total_num, Total)
#
#   joined <- left_join(ceased_cla_data, totals, by = c("time_period", "geographic_level", "geo_breakdown", "cla_group"))
#   joined$perc <- round((joined$number_num / joined$Total_num) * 100, digits = 1)
#
#   joined <- joined %>%
#     mutate(perc = case_when(
#       percentage == "z" ~ "z",
#       percentage == "c" ~ "c",
#       percentage == "k" ~ "k",
#       percentage == "x" ~ "x",
#       TRUE ~ as.character(perc)
#     )) %>%
#     mutate(`Ceased (%)` = case_when(
#       percentage == "z" ~ NA,
#       percentage == "c" ~ NA,
#       percentage == "k" ~ NA,
#       percentage == "x" ~ NA,
#       TRUE ~ as.numeric(perc)
#     ))
#   return(joined)
# }


# read outcome 2 function but without manual calculation of the percentages.
read_outcome2 <- function(file = "data/la_children_who_ceased_during_the_year.csv") {
  # drop old LA's
  outcome2_raw <- read.csv("data/la_children_who_ceased_during_the_year.csv")
  las_to_remove <- c("Poole", "Bournemouth", "Northamptonshire")

  final_filtered_data <- outcome2_raw %>% filter(new_la_code != "E10000009", !la_name %in% las_to_remove)
  ceased_cla_data <- final_filtered_data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    # mutate(number_num = case_when(
    #   number == "z" ~ NA,
    #   number == "x" ~ NA,
    #   number == "c" ~ NA,
    #   TRUE ~ as.numeric(number)
    # )) %>%
    mutate(`Ceased (%)` = case_when(
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      TRUE ~ as.numeric(percentage)
    )) %>%
    mutate(`Number ceased` = case_when(
      number == "c" ~ -100,
      number == "low" ~ -200,
      number == "k" ~ -200,
      number == "u" ~ -250,
      number == "x" ~ -300,
      number == "z" ~ -400,
      TRUE ~ as.numeric(number)
    ))

  totals <- ceased_cla_data %>%
    filter(characteristic == "Total") %>%
    rename("Total_num" = "Number ceased") %>%
    mutate("Total" = number) %>%
    select(time_period, geographic_level, geo_breakdown, cla_group, Total_num, Total)

  joined <- left_join(ceased_cla_data, totals, by = c("time_period", "geographic_level", "geo_breakdown", "cla_group"))
  joined <- joined %>%
    select("time_period", "geographic_level", "geo_breakdown", "old_la_code", "new_la_code", "cla_group", "characteristic", "number", "Number ceased", "Total_num", "Total", "percentage", "Ceased (%)")
  # For tables, we want to show the suppressed letters so use columns "percentage" and "number".
  # For charts, char values do not work so use column "Ceased (%)"
  return(joined)
}

# Outcome 3 --------------------------------
## Hospital admissions ------
# Data for this indicator is from an API and only shows the latest data
# LA data from here: https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/pat/15/par/E92000001/ati/502/are/E09000002/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/tre-ao-0_car-do-0
# Region level data from here: https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/3/gid/1938133230/ati/6/iid/90284/age/26/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/tre-ao-0_car-do-0

read_a_and_e_data <- function(la_file = "data/la_hospital_admissions_2223.csv", region_file = "data/region_hospital_admissions_2223.csv") {
  la_admissions <- read.csv("data/la_hospital_admissions_2223.csv") # la_file)
  region_admissions <- read.csv("data/region_hospital_admissions_2223.csv") # region_file)

  la_admissions$AreaName <- sub(" UA$", "", la_admissions$AreaName)
  region_admissions$AreaName <- sub(" region \\(statistical\\)$", "", region_admissions$AreaName)

  admissions_data_joined <- rbind(la_admissions, region_admissions) %>%
    select("Time.period", "Area.Type", "AreaName", "Area.Code", "Value", "Count", "Denominator") %>%
    rename(`time_period` = `Time.period`, `geographic_level` = `Area.Type`, `geo_breakdown` = `AreaName`, `new_la_code` = `Area.Code`) %>%
    distinct()

  admissions_data_joined["geographic_level"][admissions_data_joined["geographic_level"] == "Government Office Region (E12)"] <- "Regional"
  admissions_data_joined["geographic_level"][admissions_data_joined["geographic_level"] == "Upper tier local authorities (post 4/23)"] <- "Local authority"
  admissions_data_joined["geographic_level"][admissions_data_joined["geographic_level"] == "England"] <- "National"
  admissions_data_joined["geo_breakdown"][admissions_data_joined["geo_breakdown"] == "England"] <- "National"

  admissions_data <- admissions_data_joined %>%
    mutate(Value = case_when(
      is.na(Value) ~ -300,
      TRUE ~ as.numeric(Value)
    ))
  admissions_data$Value <- round(admissions_data$Value, digits = 1)

  admissions_data2 <- admissions_data %>%
    mutate(rate_per_10000 = case_when(
      Value == -300 ~ "x",
      TRUE ~ as.character(Value)
    ))
  # For the stats neighbours charts we need to have old la codes, not available in this data so just get it from another dataset
  la_codes <- suppressWarnings(read_cin_rate_data()) %>%
    filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
    select(old_la_code, new_la_code) %>%
    distinct()

  admissions_data3 <- left_join(admissions_data2, la_codes, by = c("new_la_code"))

  return(admissions_data3)
}



## Assessment Factors ------
read_assessment_factors <- function(file = "data/c3_factors_identified_at_end_of_assessment_2018_to_2023.csv") {
  data <- read.csv(file)
  columns <- c(
    "Episodes_with_assessment_factor",
    "Alcohol_Misuse_child", "Alcohol_Misuse_parent", "Alcohol_Misuse_person", "Drug_Misuse_child",
    "Drug_Misuse_parent", "Drug_Misuse_person", "Domestic_Abuse_child", "Domestic_Abuse_parent",
    "Domestic_Abuse_person", "Mental_Health_child", "Mental_Health_parent", "Mental_Health_person", "Learning_Disability_child",
    "Learning_Disability_parent", "Learning_Disability_person", "Physical_Disability_child",
    "Physical_Disability_parent", "Physical_Disability_person", "Young_Carer", "Privately_fostered",
    "Unaccompanied_asylum_seeker", "Going_missing", "Child_sexual_exploitation", "Trafficking", "Gangs",
    "Socially_unacceptable_behaviour", "Self_harm", "Neglect", "Emotional_Abuse", "Physical_Abuse_unknown",
    "Physical_Abuse_child_on_child", "Physical_Abuse_adult_on_child", "Sexual_Abuse_unknown", "Sexual_Abuse_child_on_child",
    "Sexual_Abuse_adult_on_child", "Female_Genital_Mutilation", "Faith_linked_abuse", "Child_criminal_exploitation", "Other"
  )

  data2 <- data %>%
    pivot_longer(
      cols = columns,
      names_to = "assessment_factor",
      values_to = "value"
    ) %>%
    mutate(Number = case_when(
      value == "c" ~ -100,
      value == "low" ~ -200,
      value == "k" ~ -200,
      value == "u" ~ -250,
      value == "x" ~ -300,
      value == "z" ~ -400,
      TRUE ~ as.numeric(value)
    )) %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National", # NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    mutate(assessment_factor = gsub("_", " ", assessment_factor)) %>%
    select(time_period, geographic_level, geo_breakdown, old_la_code, new_la_code, category, assessment_factor, value, Number)

  # Data needs to be rates per 10,000
  # Using the population data from CLA rates data
  populations <- suppressWarnings(read_cla_rate_data()) %>%
    filter(time_period == max(time_period)) %>%
    select(geo_breakdown, new_la_code, old_la_code, population_estimate) %>%
    distinct()

  data3 <- left_join(data2, populations, by = c("geo_breakdown", "new_la_code", "old_la_code"), relationship = "many-to-many")
  data4 <- data3 %>%
    mutate(`rate_per_10000` = (data3$Number / as.numeric(data3$population_estimate)) * 10000)

  data4$rate_per_10000 <- round(data4$rate_per_10000, digits = 0)

  data5 <- data4 %>%
    mutate(rate_per_10000 = case_when(
      value == "c" ~ -100,
      value == "low" ~ -200,
      value == "k" ~ -200,
      value == "u" ~ -250,
      value == "x" ~ -300,
      value == "z" ~ -400,
      TRUE ~ as.numeric(rate_per_10000)
    ))

  return(data5)
}



# Outcome 4 -----
## Number of placements -----

read_number_placements_data <- function(file = "data/la_cla_placement_stability.csv") {
  data <- read.csv(file)

  data2 <- data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, cla_group, placement_stability, number, percentage) %>%
    mutate(Percent = case_when(
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      TRUE ~ as.numeric(percentage)
    )) %>%
    rename("Percentage" = "percentage", "Number" = "number") %>%
    filter(new_la_code != "E10000009")

  return(data2)
}

## Placement type and distance----
read_placement_info_data <- function(file = "data/la_cla_on_31_march_by_characteristics.csv") {
  data <- read.csv(file)

  data2 <- data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    filter(cla_group %in% c("Placement", "Distance between home and placement")) %>%
    # mutate(percentage = as.numeric(percentage)) %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, cla_group, characteristic, number, percentage) %>%
    mutate(Percent = case_when(
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      TRUE ~ as.numeric(percentage)
    )) %>%
    # replace_na(list(percentage = 0)) %>%
    # filter out old dorset code
    filter(new_la_code != "E10000009")
}

# Need to do some aggregation so that placement types is aggregated to these: "foster placements", "secure units, childrens's homes or semi-independent living", "other"



## Care leavers activity -----
read_care_leavers_activity_data <- function(file = "data/la_care_leavers_activity.csv") {
  data <- read.csv(file)

  data2 <- data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, age, activity, number, percentage)

  # Need to do some aggregation so that the activity type is In education/training/employment and NOT in education/training/employment
  # be careful with suppression

  data3 <- data2 %>%
    mutate(percent = case_when(
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      TRUE ~ as.numeric(percentage)
    )) %>%
    # filter out old dorset code
    filter(new_la_code != "E10000009")

  # Age column needs to be uniform with the accommodation data as they share the same age range filter
  # "17 to 18 years" sounds better than "aged 17 to 18" but this can be swapped around if needed
  data3["age"][data3["age"] == "Aged 17 to 18"] <- "17 to 18 years"
  data3["age"][data3["age"] == "Aged 19 to 21"] <- "19 to 21 years"

  return(data3)
}

## Care leavers accommodation -----
read_care_leavers_accommodation_suitability <- function(file = "data/la_care_leavers_accommodation_suitability.csv") {
  data <- read.csv(file)

  data2 <- data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, age, accommodation_suitability, number, percentage)

  data3 <- data2 %>%
    mutate(percent = case_when(
      # percent is numeric, percentage is character
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      TRUE ~ as.numeric(percentage)
    )) %>%
    # filter out old dorset code
    filter(new_la_code != "E10000009")

  return(data3)
}


## Wellbeing of child -----

read_wellbeing_child_data <- function(file = "data/la_conviction_health_outcome_cla.csv") {
  data <- read.csv(file)

  data2 <- data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    filter(cla_group == "Ages 5 to 16 years with SDQ score") %>%
    filter(new_la_code != "E10000009") %>%
    select(time_period, geographic_level, geo_breakdown, new_la_code, old_la_code, cla_group, characteristic, number, percentage)

  data3 <- data2 %>%
    mutate(number_num = case_when(
      number == "c" ~ -100,
      number == "low" ~ -200,
      number == "k" ~ -200,
      number == "u" ~ -250,
      number == "x" ~ -300,
      number == "z" ~ -400,
      TRUE ~ as.numeric(number)
    )) %>%
    mutate(percentage_num = case_when(
      percentage == "c" ~ -100,
      percentage == "low" ~ -200,
      percentage == "k" ~ -200,
      percentage == "u" ~ -250,
      percentage == "x" ~ -300,
      percentage == "z" ~ -400,
      TRUE ~ as.numeric(percentage)
    ))
  return(data3)
}

## Placement order and match data ----
read_placement_order_match_data <- function(file = "data/national_cla_adopted_average_time_between_adoption_process_stages.csv") {
  data <- read.csv(file)

  data <- data %>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National"
    )) %>%
    filter(stage_of_adoption_process == "6. Total average time between entry into care and adoption")

  data <- data %>%
    mutate(number_num = case_when(
      number == "c" ~ -100,
      number == "low" ~ -200,
      number == "k" ~ -200,
      number == "u" ~ -250,
      number == "x" ~ -300,
      number == "z" ~ -400,
      TRUE ~ as.numeric(number)
    ))

  data$months <- sapply(strsplit(data$number, ":"), function(x) {
    years <- as.numeric(x[1])
    months <- as.numeric(x[2])
    months <- years * 12 + months
    return(months)
  })


  return(data)
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
