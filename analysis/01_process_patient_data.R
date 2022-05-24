
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_functions.R"))

# Create directory for processed data and diagnostics ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "diagnostics"), showWarnings = FALSE, recursive=TRUE)

# Load patient data from csv file ----
data_patient = here::here("output", "input.csv.gz") %>% 
  read_csv(col_types = read_column_type(.))

# Create factors and label variables -----
data_patient = data_patient %>%
  mutate(
    date_of_birth = if_else(is.na(date_of_birth),
                            NA_character_,
                            paste0(date_of_birth, "-15")) %>%
      as.Date(),

    age_2019 = as.numeric((ymd("2019-01-01") - date_of_birth)/365.25) %>%
      ff_label("Age on 1st Jan 2019 (years)"),

    age_2020 = as.numeric((ymd("2020-01-01") - date_of_birth)/365.25) %>%
      ff_label("Age on 1st Jan 2020 (years)"),

    age_2021 = as.numeric((ymd("2021-01-01") - date_of_birth)/365.25) %>%
      ff_label("Age on 1st Jan 2021 (years)"),

    age_2019_factor = cut(age_2019,
                     breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
                     labels = c("Under 4", "4-6", "7-10", "11-14", "15-17", "18+"))%>%
      ff_label("Age group on 1st Jan 2019 (years)"),

    age_2020_factor = cut(age_2020,
                          breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
                          labels = c("Under 4", "4-6", "7-10", "11-14", "15-17", "18+"))%>%
      ff_label("Age group on 1st Jan 2020 (years)"),

    age_2021_factor = cut(age_2021,
                          breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
                          labels = c("Under 4", "4-6", "7-10", "11-14", "15-17", "18+"))%>%
      ff_label("Age group on 1st Jan 2021 (years)"),

    sex = case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      TRUE ~ NA_character_
    ) %>%
      factor() %>%
      ff_label("Sex"),

    ethnicity = case_when(
      ethnicity == "1" ~ "White",
      ethnicity == "4" ~ "Black",
      ethnicity == "3" ~ "South Asian",
      ethnicity == "2" ~ "Mixed",
      ethnicity == "5" ~ "Other",
      TRUE ~ NA_character_
    ) %>%
      factor() %>%
      ff_label("Ethnicity (primary care)"),

    ethnicity_6_sus = case_when(
      ethnicity_6_sus == "1" ~ "White",
      ethnicity_6_sus == "4" ~ "Black",
      ethnicity_6_sus == "3" ~ "South Asian",
      ethnicity_6_sus == "2" ~ "Mixed",
      ethnicity_6_sus == "5" ~ "Other",
      TRUE ~ NA_character_
    ) %>%
      factor() %>%
      ff_label("Ethnicity (SUS)"),

    ethnicity_comb = coalesce(ethnicity, ethnicity_6_sus) %>%
      ff_label("Ethnicity"),

    region_2019 = region_2019 %>%
      factor() %>%
      ff_label("Region"),

    region_2020 = region_2020 %>%
      factor() %>%
      ff_label("Region"),

    region_2021 = region_2021 %>%
      factor() %>%
      ff_label("Region"),

    imd_Q5_2019 = case_when(
      (imd_2019 >=1)          & (imd_2019 < 32844*1/5) ~ "(most deprived) 1",
      (imd_2019 >= 32844*1/5) & (imd_2019 < 32844*2/5) ~ "2",
      (imd_2019 >= 32844*2/5) & (imd_2019 < 32844*3/5) ~ "3",
      (imd_2019 >= 32844*3/5) & (imd_2019 < 32844*4/5) ~ "4",
      (imd_2019 >= 32844*4/5)                          ~ "(least deprived) 5",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("(most deprived) 1", "2", "3", "4", "(least deprived) 5")) %>%
      ff_label("Multiple deprivation quintile"),

    imd_Q5_2020 = case_when(
      (imd_2020 >=1)          & (imd_2020 < 32844*1/5) ~ "(most deprived) 1",
      (imd_2020 >= 32844*1/5) & (imd_2020 < 32844*2/5) ~ "2",
      (imd_2020 >= 32844*2/5) & (imd_2020 < 32844*3/5) ~ "3",
      (imd_2020 >= 32844*3/5) & (imd_2020 < 32844*4/5) ~ "4",
      (imd_2020 >= 32844*4/5)                          ~ "(least deprived) 5",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("(most deprived) 1", "2", "3", "4", "(least deprived) 5")) %>%
      ff_label("Multiple deprivation quintile"),

    imd_Q5_2021 = case_when(
      (imd_2021 >=1)          & (imd_2021 < 32844*1/5) ~ "(most deprived) 1",
      (imd_2021 >= 32844*1/5) & (imd_2021 < 32844*2/5) ~ "2",
      (imd_2021 >= 32844*2/5) & (imd_2021 < 32844*3/5) ~ "3",
      (imd_2021 >= 32844*3/5) & (imd_2021 < 32844*4/5) ~ "4",
      (imd_2021 >= 32844*4/5)                          ~ "(least deprived) 5",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("(most deprived) 1", "2", "3", "4", "(least deprived) 5")) %>%
      ff_label("Multiple deprivation quintile"),

    rural_urban_2019 = case_when(
      rural_urban_2019 %in% c(1,2)     ~ "Urban conurbation",
      rural_urban_2019 %in% c(3,4)     ~ "Urban city or town",
      rural_urban_2019 %in% c(5,6,7,8) ~ "Rural town or village",
      TRUE                             ~ NA_character_
    ) %>%
      factor() %>%
      ff_label("Rural-urban classification"),

    rural_urban_2020 = case_when(
      rural_urban_2020 %in% c(1,2)     ~ "Urban conurbation",
      rural_urban_2020 %in% c(3,4)     ~ "Urban city or town",
      rural_urban_2020 %in% c(5,6,7,8) ~ "Rural town or village",
      TRUE                             ~ NA_character_
    ) %>%
      factor() %>%
      ff_label("Rural-urban classification"),

    rural_urban_2021 = case_when(
      rural_urban_2021 %in% c(1,2)     ~ "Urban conurbation",
      rural_urban_2021 %in% c(3,4)     ~ "Urban city or town",
      rural_urban_2021 %in% c(5,6,7,8) ~ "Rural town or village",
      TRUE                             ~ NA_character_
    ) %>%
      factor() %>%
      ff_label("Rural-urban classification"),

  )

# Create dataset with only patient IDs
data_id = data_patient %>% 
  select(patient_id)

# Save data as rds ----
write_rds(data_patient,
          here::here("output", "data", "data_patient.rds"),
          compress="gz")

write_rds(data_id,
          here::here("output", "data", "data_id.rds"),
          compress="gz")
