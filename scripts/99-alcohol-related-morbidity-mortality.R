# Request ####
# illustrate the current degree of alcohol related morbidity
# and mortality in Coconino County to our community partners.

# Deaths (Be specific):
# Intentional & Unintentional, Alcohol Related, Non-chronic disease
# Hospitalizations

# Race/Ethnicity
# Age
# Gender

# 2018-2022

# Please include all cited data sources for all data,
# please label all tables and data given,
# please include all counts (n=?),
# please cross tabulate each demographic variable with all data
# (alcohol related deaths: intentional and unintentional,
# include any death involving alcohol
# regardless of whether or not other substances were involved
# and be specific by age, gender, race/ethnicity).

# Setup ####
# package libraries
library(here)
library(tidyverse)
library(janitor)
library(pins)

# mortality data ####
## read mortality data ####
# create pin board folder
pb_mortality <- board_folder(
  "S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data"
)

# view available pins
pb_mortality %>% pin_list()

# read pin to data environment
mort_df <- pin_read(
  board = pb_mortality,
  name = "mortality-data-tidy-transformed-2010-2023"
)

glimpse(mort_df)

## tidy data ####
# icd10 codes via Rnssp
library(Rnssp)
icd10 <- webscrape_icd(year = 2022) %>%
  mutate(code = str_to_lower(code))

icd10 <- icd10 %>%
  mutate(code_clean = str_to_lower(code)) %>%
  mutate(code_cleaner = str_sub(code_clean, start = 1L, 4L))

glimpse(icd10)

icd10 %>%
  filter(str_detect(str_to_lower(description), "alcohol")) %>%
  filter(code != "k7581") %>%
  filter(!str_detect(code, "z6|z7|z8")) %>%
  as_tibble() %>%
  print(n = 127)

# code new dummy variables for project
### ICD 10 codes for Alcohol-induced deaths ####
# source: https://www.cdc.gov/alcohol/ardi/alcohol-related-icd-codes.html
# https://www.ncbi.nlm.nih.gov/books/NBK574248/
icd10_alcohol_induced <- c(
  "E244", # "Alcohol-induced pseudo-Cushing's syndrome"
  "F10", # Mental and behavioural disorders due to use of alcohol
  "G312", # "Degeneration of nervous system due to alcohol"
  "G621", # Alcoholic polyneuropathy
  "G721", # Alcoholic myopathy
  "I426", # Alcoholic cardiomyopathy
  "K292", # Alcoholic gastriti
  "K70", #  Alcoholic liver disease
  "K852", # Alcohol induced acute pancreatitis
  "K860", # Alcohol induced chronic pancreatitis
  "Q860", # Fetal induced alcohol syndrome (dysmorphic)
  "R780", # Excess alcohol blood levels
  "X45", # Accidental poisoning by and exposure to alcohol
  "X65", # Intentional self-poisoning by and exposure to alcohol
  "Y15" # Poisoning by and exposure to alcohol, undetermined intent
) %>%
  str_to_lower() %>%
  paste(collapse = "|")

### motor vehicle ####
# source: email from Yan Huang; yan.huang@azdhs.gov; 27 Jul 2022
icd10_mv <- c(
  "V90",
  "V91",
  "V92",
  "V12",
  "v13",
  "V14",
  "V190",
  "v191",
  "V192",
  "V194",
  "v195",
  "V196",
  str_c("v", as.character(seq(20, 79, 1))),
  "V803",
  "v804",
  "V805",
  "V810",
  "V811",
  "V820",
  "V821",
  str_c("v", as.character(seq(83, 86, 1))),
  str_c("v", as.character(seq(870, 878, 1))),
  str_c("v", as.character(seq(880, 888, 1))),
  str_c("v", as.character(seq(890, 892, 1)))
) %>%
  str_to_lower() %>%
  paste(collapse = "|")

# identify the variables for contributing cause of death
var_cod_contributing <- mort_df %>%
  select(contains("_icd")) %>%
  names()

# identify variables for e codes
var_ecode <- mort_df %>%
  select(contains("_e_code")) %>%
  names()

# identify variable for underlying cause of death
var_cod_underlying <- mort_df %>%
  select(contains("underlying_cause")) %>%
  names()

# any cause of death
var_cod_any <- mort_df %>%
  select(contains("underlying_cause"), contains("_icd")) %>%
  names()

# inspect data
glimpse(mort_df)

mort_df %>%
  tabyl(d_race_code)

## apply transformations to mortality data ####
mort_df_sample <- mort_df %>%
  filter(d_county_resident == "Resident") %>%
  filter(death_book_year %in% c(seq(2018, 2022, 1))) %>%
  mutate(
    # dummy count
    d_count = 1,
    # male
    d_sex_male = if_else(
      condition = d_sex == "Male",
      true = 1,
      false = 0
    ),
    # female
    d_sex_female = if_else(
      condition = d_sex == "Female",
      true = 1,
      false = 0
    ),
    # age groups
    # 20 - 64
    d_age_20_64 = if_else(
      condition = calc_age >= 20 & calc_age <= 64,
      true = 1,
      false = 0
    ),
    # 20-34
    d_age_20_34 = if_else(
      condition = calc_age >= 20 & calc_age <= 34,
      true = 1,
      false = 0
    ),
    # 35-49
    d_age_35_49 = if_else(
      condition = calc_age >= 35 & calc_age <= 49,
      true = 1,
      false = 0
    ),
    # 50-64
    d_age_50_64 = if_else(
      condition = calc_age >= 50 & calc_age <= 64,
      true = 1,
      false = 0
    ),
    # race
    # white, non-hispanic
    d_race_nhw = if_else(
      condition = d_race_code == "White Non-Hispanic",
      true = 1,
      false = 0
    ),
    # Native American Alaska Native, Non-Hispanic
    d_race_aian = if_else(
      condition = d_race_code == "American Indian and Alaska Native",
      true = 1,
      false = 0
    ),
    # Hispanic
    d_race_hisp = if_else(
      condition = d_race_code == "Hispanic or Latino (any race)",
      true = 1,
      false = 0
    ),
    # excess alcohol
    d_alcohol_excess = if_any(
      contains(var_cod_any),
      ~ str_detect(.x, "r780")
    ),
    # alcohol induced death contributing
    d_alcohol_ind_contributing = if_any(
      contains(var_cod_contributing),
      ~ str_detect(.x, icd10_alcohol_induced)
    ),
    # alcohol induced death underyling
    d_alcohol_ind_underlying = if_any(
      contains(var_cod_underlying),
      ~ str_detect(.x, icd10_alcohol_induced)
    ),
    # alcohol induced death unintentional
    d_alcohol_unintentional = if_any(
      contains(var_cod_contributing),
      ~ str_detect(.x, "x45")
    ),
    # alcohol induced death intentional
    d_alcohol_intentional = if_any(
      contains(var_cod_contributing),
      ~ str_detect(.x, "x65")
    ),
    # motor vehicle death, contributing
    d_mv_contributing = if_any(
      contains(var_cod_contributing),
      ~ str_detect(.x, icd10_mv)
    ),
    # motor vehicle death, underlying
    d_mv_underlying = if_any(
      contains(var_cod_underlying),
      ~ str_detect(.x, icd10_mv)
    ),
    # injury date time
    d_inj_datetime = ymd_hms(injury_date),
    # injury hour of the day
    d_inj_hour = hour(d_inj_datetime),
    # cause of death parsed for text analysis
    d_cod_parsed = str_to_lower(str_c(
      cdc_injurydesc, cod_a, cod_b, cod_c, cod_d,
      sep = " "
    ))
  )

# view data
mort_df_sample %>%
  glimpse()

# text analysis ####
library(tidytext)

# cause of death parsed
cod_words <- mort_df_sample %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    d_id,
    d_cod_parsed
  ) %>%
  unnest_tokens(word, d_cod_parsed) %>%
  anti_join(stop_words)

cod_words %>%
  count(word, sort = TRUE)

text_cod_all_2grams <- mort_df_sample %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    d_id,
    d_cod_parsed
  ) %>%
  unnest_tokens(bigram, d_cod_parsed, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

write_rds(
  x = text_cod_all_2grams,
  file = "data-tidy/text-cod-2grams.rds"
)

text_cod_all_3grams <- mort_df_sample %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    d_id,
    d_cod_parsed
  ) %>%
  unnest_tokens(bigram, d_cod_parsed, token = "ngrams", n = 3) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  unite(bigram, word1, word2, word3, sep = " ") %>%
  count(bigram, sort = TRUE)

write_rds(
  x = text_cod_all_3grams,
  file = "data-tidy/text-cod-3grams.rds"
)


## data table 1: population characteristics ####
### all deaths ####
(tab_1_all_deaths <- mort_df_sample %>%
  tabyl(d_alcohol_ind_underlying) %>%
  adorn_pct_formatting() %>%
  # adorn_totals() %>%
  as_tibble() %>%
  mutate(
    total = sum(n),
    n_alcohol = str_c(n, " (", percent, ")", sep = ""),
    characteristic = "All deaths"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all male deaths ####
(tab_1_all_mal_deaths <- mort_df_sample %>%
  tabyl(d_alcohol_ind_underlying, d_sex_male) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All male deaths"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all male deaths age 20-64 ####
(tab_1_all_mal_20_64 <- mort_df_sample %>%
  filter(d_sex_male == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_age_20_64) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All male deaths age 20-64"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all male deaths age 20-34 ####
(tab_1_all_mal_20_34 <- mort_df_sample %>%
  filter(d_sex_male == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_age_20_34) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All male deaths age 20-34"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all male deaths age 35-49 ####
(tab_1_all_mal_35_49 <- mort_df_sample %>%
  filter(d_sex_male == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_age_35_49) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All male deaths age 35-49"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all male deaths age 50-64 ####
(tab_1_all_mal_50_64 <- mort_df_sample %>%
  filter(d_sex_male == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_age_50_64) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All male deaths age 50-64"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all male deaths non hispanic white ####
(tab_1_all_mal_nhw <- mort_df_sample %>%
  filter(d_sex_male == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_race_nhw) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All male deaths White, Non-Hispanic"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all male deaths american indian alaska native  ####
(tab_1_all_mal_aian <- mort_df_sample %>%
  filter(d_sex_male == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_race_aian) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All male deaths Native American Alaska Native"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all male deaths hispanic ####
(tab_1_all_mal_hisp <- mort_df_sample %>%
  filter(d_sex_male == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_race_hisp) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All male deaths Hispanic or Latino"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all female deaths ####
(tab_1_all_fem_deaths <- mort_df_sample %>%
  tabyl(d_alcohol_ind_underlying, d_sex_female) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All female deaths"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all female deaths age 20-64 ####
(tab_1_all_fem_20_64 <- mort_df_sample %>%
  filter(d_sex_female == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_age_20_64) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All female deaths age 20-64"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all female deaths age 20-34 ####
(tab_1_all_fem_20_34 <- mort_df_sample %>%
  filter(d_sex_female == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_age_20_34) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All female deaths age 20-34"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all female deaths age 35-49 ####
(tab_1_all_fem_35_49 <- mort_df_sample %>%
  filter(d_sex_female == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_age_35_49) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All female deaths age 35-49"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all female deaths age 50-64 ####
(tab_1_all_fem_50_64 <- mort_df_sample %>%
  filter(d_sex_female == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_age_50_64) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All female deaths age 50-64"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all female deaths non hispanic white ####
(tab_1_all_fem_nhw <- mort_df_sample %>%
  filter(d_sex_female == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_race_nhw) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All female deaths White, Non-Hispanic"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all female deaths american indian alaska native ####
(tab_1_all_fem_aian <- mort_df_sample %>%
  filter(d_sex_female == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_race_aian) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All female deaths Native American Alaska Native"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

### all female deaths hispanic ####
(tab_1_all_fem_hisp <- mort_df_sample %>%
  filter(d_sex_female == 1) %>%
  tabyl(d_alcohol_ind_underlying, d_race_hisp) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(total = sum(`1`)) %>%
  mutate(
    `1` = if_else(
      condition = `1` >= 6,
      true = `1`,
      false = NA_integer_
    )
  ) %>%
  mutate(
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    characteristic = "All female deaths Hispanic or Latino"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

# save table to disk
(tab_1 <- bind_rows(
  tab_1_all_deaths,
  tab_1_all_mal_deaths,
  tab_1_all_mal_20_64,
  tab_1_all_mal_20_34,
  tab_1_all_mal_35_49,
  tab_1_all_mal_50_64,
  tab_1_all_mal_nhw,
  tab_1_all_mal_aian,
  tab_1_all_mal_hisp,
  tab_1_all_fem_deaths,
  tab_1_all_fem_20_64,
  tab_1_all_fem_20_34,
  tab_1_all_fem_35_49,
  tab_1_all_fem_50_64,
  tab_1_all_fem_nhw,
  tab_1_all_fem_aian,
  tab_1_all_fem_hisp
))

tab_1 %>%
  write_rds(file = "tables/tab-1-descriptive-stats.rds")

## data table 2: injury deaths ####
glimpse(mort_df_sample)

# inclusion criteria, manner of death is an accident, homicide, or suicide
#
mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  # filter(d_alcohol_ind_underlying == TRUE)
  tabyl(d_alcohol_ind_underlying) %>%
  adorn_totals()

mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  tabyl(d_alcohol_ind_contributing) %>%
  adorn_totals()

mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  tabyl(d_alcohol) %>%
  adorn_totals()

### all injury deaths ####
(tab_2_all_deaths <- mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  tabyl(d_alcohol) %>%
  # adorn_totals() %>%
  adorn_pct_formatting() %>%
  as_tibble() %>%
  mutate(
    total = sum(n),
    n_alcohol = str_c(n, " (", percent, ")", sep = ""),
    injury = "All injuries combined",
    group = "Total"
  ) %>%
  filter(d_alcohol == TRUE) %>%
  select(
    group,
    injury,
    total,
    n_alcohol
  ))

### all motor vehicle deaths ####
(tab_2_mv_deaths <- mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  tabyl(d_alcohol, d_mv) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`TRUE`),
    percent = round(100 * (`TRUE` / total), digits = 2),
    n_alcohol = str_c(`TRUE`, " (", percent, "%)", sep = ""),
    injury = "Motor vehicle accidents",
    group = "Total"
  ) %>%
  filter(d_alcohol == TRUE) %>%
  select(
    group,
    injury,
    total,
    n_alcohol
  ))

### all homicides ####
(tab_2_homicide_deaths <- mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  tabyl(d_alcohol, d_homicide) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`)
  ) %>%
  mutate(
    `1` = if_else(
      condition = `1` >= 6,
      true = `1`,
      false = NA_integer_
    )
  ) %>%
  mutate(
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    injury = "Homicide",
    group = "Total"
  ) %>%
  filter(d_alcohol == TRUE) %>%
  select(
    group,
    injury,
    total,
    n_alcohol
  ))

### all suicide ####
(tab_2_suicide_deaths <- mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  tabyl(d_alcohol, d_suicide) %>%
  # adorn_totals(where = c("row", "col")) %>%
  as_tibble() %>%
  mutate(
    total = sum(`1`),
    percent = round(100 * (`1` / total), digits = 2),
    n_alcohol = str_c(`1`, " (", percent, "%)", sep = ""),
    injury = "Suicide",
    group = "Total"
  ) %>%
  filter(d_alcohol == TRUE) %>%
  select(
    group,
    injury,
    total,
    n_alcohol
  ))

(tab2 <- bind_rows(
  tab_2_all_deaths,
  tab_2_mv_deaths,
  tab_2_homicide_deaths,
  tab_2_suicide_deaths
))

tab2 %>%
  write_rds(file = "tables/tab-2-injury-stats.rds")

unique(mort_df_sample$cdc_mannerofdeath_desc)

mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  filter(d_alcohol_ind_contributing == TRUE | d_alcohol_ind_underlying) %>%
  select(
    d_id,
    d_cod_parsed
  ) %>%
  unnest_tokens(word, d_cod_parsed) %>%
  anti_join(stop_words)

text_cod_violent_2grams <- mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  filter(d_alcohol_ind_contributing == TRUE | d_alcohol_ind_underlying) %>%
  select(
    d_id,
    d_cod_parsed
  ) %>%
  unnest_tokens(bigram, d_cod_parsed, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

text_cod_violent_2grams %>%
  write_rds(file = "data-tidy/text-cod-violent-2grams.rds")

text_cod_violent_3grams <- mort_df_sample %>%
  filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  filter(d_alcohol_ind_contributing == TRUE | d_alcohol_ind_underlying) %>%
  select(
    d_id,
    d_cod_parsed
  ) %>%
  unnest_tokens(bigram, d_cod_parsed, token = "ngrams", n = 3) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  unite(bigram, word1, word2, word3, sep = " ") %>%
  count(bigram, sort = TRUE)

text_cod_violent_3grams %>%
  write_rds(file = "data-tidy/text-cod-violent-3grams.rds")

mort_df_sample %>%
  filter(d_accident == 1) %>%
  tabyl(d_alcohol_ind_underlying)

mort_df_sample %>%
  filter(d_accident == 1) %>%
  tabyl(d_alcohol_ind_contributing)

mort_df_sample %>%
  filter(d_accident == 1) %>%
  tabyl(d_alcohol_excess)

mort_df_sample %>%
  filter(d_accident == 1) %>%
  tabyl(d_alcohol)

mort_df_sample %>%
  # filter(d_accident == 1 | d_suicide == 1 | d_homicide == 1) %>%
  filter(d_alcohol_ind_contributing == TRUE | d_alcohol_ind_underlying) %>%
  select(
    contains("cod"),
    contains("icd"),
    d_id,
    d_cod_parsed
  ) %>%
  slice_sample(n = 5) %>%
  glimpse()

## data table 3: chronic illness ####
# total
(tab3_chronic_illness <- mort_df_sample %>%
  filter(d_accident != 1) %>%
  filter(d_homicide != 1) %>%
  filter(d_suicide != 1) %>%
  tabyl(d_alcohol_ind_underlying) %>%
  adorn_pct_formatting() %>%
  # adorn_totals() %>%
  as_tibble() %>%
  mutate(
    total = sum(n),
    n_alcohol = str_c(n, " (", percent, ")", sep = ""),
    characteristic = "All non-injury deaths"
  ) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  select(
    characteristic,
    total,
    n_alcohol
  ))

# most common illness
tab3 <- mort_df_sample %>%
  filter(d_accident != 1) %>%
  filter(d_homicide != 1) %>%
  filter(d_suicide != 1) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  filter(acme_underlying_cause != "x45") %>%
  filter(acme_underlying_cause != "y15") %>%
  tabyl(acme_underlying_cause) %>%
  # adorn_pct_formatting() %>%
  # adorn_totals() %>%
  as_tibble() %>%
  arrange(desc(n)) %>%
  left_join(
    y = icd10,
    by = c("acme_underlying_cause" = "code_cleaner")
  ) %>%
  distinct(acme_underlying_cause, n, .keep_all = TRUE) %>%
  select(acme_underlying_cause, description, n, percent) %>%
  mutate(
    acme_underlying_cause = if_else(
      condition = n >= 6,
      true = acme_underlying_cause,
      false = "Other, Suppressed"
    ),
    description = if_else(
      condition = n >= 6,
      true = description,
      false = "Other, Suppressed"
    )
  ) %>%
  group_by(acme_underlying_cause) %>%
  reframe(
    description,
    n = sum(n), percent = sum(percent)
  ) %>%
  ungroup() %>%
  distinct(acme_underlying_cause, n, .keep_all = TRUE) %>%
  arrange(desc(n)) %>%
  mutate(description = str_replace(description, "(?<=, )\\w+", "")) %>%
  mutate(percent = str_c(round(100 * (percent), digits = 2), "%"))

write_rds(
  x = tab3,
  file = "tables/tab-3-chronic-illness.rds"
)

(tab3_text <- mort_df_sample %>%
  filter(d_accident != 1) %>%
  filter(d_homicide != 1) %>%
  filter(d_suicide != 1) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  filter(acme_underlying_cause != "x45") %>%
  filter(acme_underlying_cause != "y15") %>%
  select(
    d_id,
    d_cod_parsed
  ) %>%
  unnest_tokens(bigram, d_cod_parsed, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE))

write_rds(
  x = tab3_text,
  file = "tables/tab-3-text.rds"
)

mort_df_sample %>%
  filter(d_accident != 1) %>%
  filter(d_homicide != 1) %>%
  filter(d_suicide != 1) %>%
  filter(d_alcohol_ind_underlying == TRUE) %>%
  filter(acme_underlying_cause != "x45") %>%
  filter(acme_underlying_cause != "y15") %>%
  filter(acme_underlying_cause != "f102") %>%
  filter(acme_underlying_cause != "f101") %>%
  select(
    d_id,
    d_cod_parsed
  )
