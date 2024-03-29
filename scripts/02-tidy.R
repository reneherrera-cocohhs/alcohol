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

pin_list(pb_seer)

seer_rate <- pin_read(
  board = pb_seer,
  name = "seer_rate_data"
)

seer_rate %>%
  glimpse()

seer_rate <- seer_rate %>%
  mutate(
    age_adjusted_rate = str_remove(
      string = age_adjusted_rate,
      pattern = "#"
    )
  ) %>%
  mutate(across(
    .cols = c(-cause_of_death_recode, -year_of_death_recode),
    .fns = as.numeric
    #   c(
    #   age_adjusted_rate,
    #   standard_error,
    #   lower_confidence_interval,
    #   upper_confidence_interval,
    #   count,
    #   rate_ratio,
    #   ratio_lower_confidence_interval,
    #   ratio_upper_confidence_interval,
    #   ratio_p_value,
    #   
    # )
  ))
  
  

seer_rate_year_codes <- pin_read(
  board = pb_seer,
  name = "seer_rate_year_of_death_codes"
)

seer_rate_cod_codes <- pin_read(
  board = pb_seer,
  name = "seer_rate_cause_of_death_codes"
)

seer_rate_df <- left_join(
  x = seer_rate,
  y = seer_rate_year_codes,
  by = "year_of_death_recode"
) %>%
  left_join(
    y = seer_rate_cod_codes,
    by = "cause_of_death_recode"
  ) 

cod_codes_alcohol <- pin_read(
  board = pb_seer,
  name = "seer_rate_cause_of_death_codes_re_alcohol"
)

seer_rate_df <- seer_rate_df %>%
  mutate(
    d_alcohol_rel = if_else(
      condition = cause_of_death_recode %in% cod_codes_alcohol,
      true = TRUE,
      false = FALSE
    )
  ) 

pin_write(
  board = pb_seer,
  x = seer_rate_df,
  name = "seer_rate_data_tidy",
  type = "rds",
  title = "SEER rate data, tidy",
  description = "Tidy SEER rate data, Coconino County, 1969-2020",
  metadata = list(
    user = "rherrera",
    department = "Epidemiology",
    source = "SEER"
  )
)

seer_rate_df %>%
  glimpse()

seer_rate_df %>%
  filter(d_alcohol_rel == TRUE) %>%
  filter(year_of_death_recode  %in% c("14", "15", "16")) %>%
  distinct(year_of_death)
  drop_na(age_adjusted_rate) %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = cause_of_death,
      y = age_adjusted_rate,
      fill = year_of_death
    ), position = "dodge"
  ) +
  coord_flip()
