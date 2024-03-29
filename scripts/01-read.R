# setup ####
## package libraries ####
library(here)
library(tidyverse)
library(pins)
library(janitor)

# mortality data #### 
pb_mortality <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data")

pb_mortality %>% pin_list()

mort_df <- pin_read(
  board = pb_mortality,
  name = "mortality-data-tidy-transformed-2010-2023"
)

glimpse(mort_df)

# hospital discharge data ####
pb_hdd <- board_folder("S:/HIPAA Compliance/Hospital Discharge Data/r-pin-board-rds-files/")


# SEER rate data #### 
pb_seer <- board_folder("S:/Unit-Program Folders/PHEP/Epidemiology/SEER-cancer")

## cause of death codes ####
seer_rate_cod_codes <- read_delim(
  file = "data-raw/seer-rate-cause-of-death-codes.txt",
  delim = "=",
  col_names = c("cause_of_death_recode", "cause_of_death"),
  trim_ws = TRUE,
  col_types = c("cc")
)

pin_write(
  board = pb_seer,
  x = seer_rate_cod_codes,
  name = "seer_rate_cause_of_death_codes",
  type = "rds",
  title = "Cause of death codes for SEER rate data",
  description = "Cause of death codes for SEER rate data, 1969-2020",
  metadata = list(
    user = "rherrera",
    department = "Epidemiology",
    source = "SEER"
  )
)

## cause of death codes RE alcohol ####
cod_codes_alcohol <- c(
  # c(tongue, salivary gland, floor of mouth, gum and other mouth,
  # nasopharynx, tonsil, oropharynx, hypopharynx, other oral cavity and pharynx)
  str_c(seq(4, 12, 1)), 
  "14", # esophagus
  "17", # colon and rectum
  "21", # liver and intrahepatic bile duct 
  "41" # breast
)

pin_write(
  board = pb_seer,
  x = cod_codes_alcohol,
  name = "seer_rate_cause_of_death_codes_re_alcohol",
  type = "rds",
  title = "Cause of death codes for SEER rate data, related to alcohol",
  description = "Cause of death codes for SEER rate data, related to alcohol, 1969-2020",
  metadata = list(
    user = "rherrera",
    department = "Epidemiology",
    source = "SEER"
  )
)

## year of death codes #### 
seer_rate_year_codes <- read_delim(
  file = "data-raw/seer-rate-year-of-death-codes.txt",
  delim = "=",
  col_names = c("year_of_death_recode", "year_of_death"),
  trim_ws = TRUE,
  col_types = c("cc")
)

pin_write(
  board = pb_seer,
  x = seer_rate_year_codes,
  name = "seer_rate_year_of_death_codes",
  type = "rds",
  title = "Year of death codes for SEER rate data",
  description = "Year of death codes for SEER rate data, 1969-2020",
  metadata = list(
    user = "rherrera",
    department = "Epidemiology",
    source = "SEER"
  )
)

## SEER rate data #### 
seer_rate <- read_csv(
  file = "data-raw/seer-rate.csv",
  col_types = "cccccccccccc"
) %>%
  clean_names()

glimpse(seer_rate)

pin_write(
  board = pb_seer,
  x = seer_rate,
  name = "seer_rate_data",
  type = "rds",
  title = "SEER rate data",
  description = "SEER rate data, Coconino County, 1969-2020",
  metadata = list(
    user = "rherrera",
    department = "Epidemiology",
    source = "SEER"
  )
)


# ADHS Cancer Registry data dashboard 
adhs_incidence_rate <- tibble(
  year = c("2016-2020", "2016-2020", "2016-2020", "2016-2020", "2016-2020", "2011-2015", "2011-2015", "2011-2015", "2011-2015", "2011-2015", "2008-2010", "2008-2010", "2008-2010", "2008-2010", "2008-2010"),
  cancer_site = c("Breast Invasive - Female", "Colorectal", "liver and intrahepatic bile duct", "Oral Cavity", "Esophagus", "Breast Invasive - Female", "Colorectal", "Oral Cavity", "liver and intrahepatic bile duct", "Esophagus", "Breast Invasive - Female", "Colorectal", "liver and intrahepatic bile duct", "Oral Cavity", "Esophagus"),
  incidence_rate = c(105.5, 25.8, 9.0, 8.8, 3.0, 112.2, 30.1, 8.9, 8.4, 4.3, 102.6, 24.1, 4.2, 6.6, 3.2)
) %>%
  arrange(cancer_site, year)

pin_write(
  board = pb_seer,
  x = adhs_incidence_rate,
  name = "adhs_incidence_rate",
  type = "rds",
  title = "ADHS Incidence rate data",
  description = "AZ Cancer Registry Incidence rate data, Coconino County, 2008-2020",
  metadata = list(
    user = "rherrera",
    department = "Epidemiology",
    source = "https://www.azdhs.gov/preparedness/public-health-statistics/cancer-registry/index.php#data-dashboard"
  )
)


 
