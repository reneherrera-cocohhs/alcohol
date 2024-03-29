library(here)
library(tidyverse)
library(pins)
library(janitor)


# mortality data #### 
pb_mortality <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data")

# pb_mortality %>% pin_list()

mort_df <- pin_read(
  board = pb_mortality,
  name = "mortality-data-tidy-transformed-2010-2023"
)

var_icd <- c(
  mort_df %>%
    select(contains("icd")) %>%
    names(),
  mort_df %>%
    select(contains("cod_")) %>%
    names(),
  mort_df %>%
    select(contains("_cause")) %>%
    names(),
  mort_df %>%
    select(contains("cdc")) %>%
    names()
)


mort_df %>%
  filter(d_date_of_death_year == "2021") %>%
  filter(d_alcohol_ind == TRUE) %>%
  filter(calc_age >= 20) %>%
  filter(d_county_resident == "Resident") %>%
  select(
    death_certificate_number,
    residence_county_name,
    residence_zip,
    c(contains(var_icd))
  ) %>% 
  write_csv(
    file = "data-tidy/2021-deaths-alcohol-induced.csv"
  )
