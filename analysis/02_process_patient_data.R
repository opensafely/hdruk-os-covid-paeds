
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Load patient data from csv file ----
data_patient = here::here("output", "input.csv.gz") %>% 
  read_csv(col_types = read_column_type(.))

# Load admission and testing datasets ----
data_admissions = readRDS(here::here("output", "data", "data_admissions.rds"))
data_testing    = readRDS(here::here("output", "data", "data_testing.rds"))

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

## Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Create directory for processed data and diagnostics ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)

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

    ethnicity_gp = case_when(
      ethnicity_gp == "1" ~ "White",
      ethnicity_gp == "4" ~ "Black",
      ethnicity_gp == "3" ~ "South Asian",
      ethnicity_gp == "2" ~ "Mixed",
      ethnicity_gp == "5" ~ "Other",
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

    ethnicity = coalesce(ethnicity_gp, ethnicity_6_sus) %>%
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
    
    asthma = if_else(is.na(asthma_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Asthma"),
    
    cancer = if_else(is.na(cancer_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Cancer"),
    
    diabetes = if_else(is.na(diabetes_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Diabetes"),
    
    epilepsy = if_else(is.na(epilepsy_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Epilepsy"),
    
    severe_mental_illness = if_else(is.na(severe_mental_illness_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Severe mental illness"),
    
    cerebral_palsy = if_else(is.na(cerebral_palsy_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Cerebral palsy"),
    
    chronic_infections = if_else(is.na(chronic_infections_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Chronic infections"),
    
    devices_and_stomas = if_else(is.na(devices_and_stomas_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Devices and stomas"),
    
    endocrine_disorders = if_else(is.na(endocrine_disorders_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Endocrine disorders"),
    
    gastrointestinal_disorders = if_else(is.na(gastrointestinal_disorders_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Gastrointestinal disorders"),
    
    haematological_disorders = if_else(is.na(haematological_disorders_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Haematological disorders"),
    
    immunological_disorders = if_else(is.na(immunological_disorders_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Immunological disorders"),
    
    learning_and_behaviour_difficulties = if_else(is.na(learning_and_behaviour_difficulties_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Learning and behavioural difficulties"),
    
    mental_illness = if_else(is.na(mental_illness_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Mental illness"),
    
    musculoskeletal_and_rheum = if_else(is.na(musculoskeletal_and_rheum_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Musculoskeletal and rheumatic diseases"),
    
    transplant = if_else(is.na(transplant_first_date), "No", "Yes") %>%
      factor() %>%
      ff_label("Transplant"),
  )

# Covid status, counts and test dates ----
## First positive test date ----
data_patient = data_patient %>% 
  left_join(
    data_testing %>% 
      filter(result == "Positive") %>% 
      group_by(patient_id) %>% 
      filter(row_number() == 1) %>% 
      select(patient_id, covid_pos_test_date_1 = test_date),
    by = "patient_id"
  )

## Count tests by result by period ----
data_patient = data_patient %>%
  left_join(
    data_testing %>% 
      mutate(
        result_abr = if_else(result == "Positive", "pos", "neg"),
        test_period = case_when(
          test_date < tp_start_date ~ NA_character_,
          test_date <= tp_end_date  ~ "tp", # Testing period
          test_date <= study_end_date ~ "fup", # Follow-up period
          TRUE ~ NA_character_
        ),
        result_period = paste0(result_abr, "_", test_period)
      ) %>% 
      group_by(patient_id) %>% 
      count(result_period) %>% 
      pivot_wider(
        names_from = result_period,
        names_glue = "covid_test_{result_period}_count",
        values_from = n,
      ),
    by = "patient_id"
  ) %>% 
  replace_na(
    list(covid_test_neg_tp_count = 0,
         covid_test_pos_tp_count = 0,
         covid_test_neg_fup_count = 0,
         covid_test_pos_fup_count = 0)
  ) %>% 
  mutate(
    covid_test_neg_tp_count = covid_test_neg_tp_count %>% 
      ff_label("Negative covid test count (testing period)"),
    covid_test_pos_tp_count = covid_test_pos_tp_count %>% 
      ff_label("Positive covid test count (testing period)"),
    covid_test_neg_fup_count = covid_test_neg_fup_count %>% 
      ff_label("Negative covid test count (follow-up period)"),
    covid_test_pos_fup_count = covid_test_pos_fup_count %>% 
      ff_label("Positive covid test count (follow-up period)")
  )

## Assign covid status ----
data_patient = data_patient %>% 
  mutate(
    covid_status_tp = case_when(
      covid_test_pos_tp_count > 0 ~ "Positive",
      covid_test_neg_tp_count > 0 ~ "Negative",
      TRUE ~ "Untested") %>%
      factor() %>%
      ff_label("SARS-CoV-2 status (testing period)"),
    covid_status_fup = case_when(
      covid_test_pos_fup_count > 0 ~ "Positive",
      covid_test_neg_fup_count > 0 ~ "Negative",
      TRUE ~ "Untested") %>%
      factor() %>%
      ff_label("SARS-CoV-2 status (follow-up period)"),
  )

# Exclusion criteria variables ----
## Potential nosocomial infection ----
## Defined as a positive covid test after day 7 in hospital and on or before 7th
##  day following discharge
data_patient = data_patient %>%
  left_join(
    data_admissions %>%
      left_join(
        data_patient %>%
          select(patient_id, covid_pos_test_date_1),
        by = "patient_id") %>%
      mutate(
        covid_nosocomial = case_when(
          # Length of stay less than 7 days: Not nosocomial 
          (discharge_date - admission_date) < 7 ~ NA_character_,
          # Length of stay 7+ days: Nosocomial if positive after day 7 in hospital
          # and on or before day 7 following discharge, otherwise not nosocomial
          (admission_date + days(7) < covid_pos_test_date_1) &
            (discharge_date + days(7) >= covid_pos_test_date_1) ~ "Yes",
          TRUE ~ NA_character_
        ) %>%
          ff_label("Nosocomial infection")) %>%
      filter(covid_nosocomial == "Yes") %>%
      select(patient_id, covid_nosocomial) %>%
      group_by(patient_id) %>%
      slice(1) %>%
      ungroup(),
    by = "patient_id"
  ) %>% 
  replace_na(list(covid_nosocomial = "No"))

## Discrepant test result ----
data_patient = data_patient %>% 
  left_join(
    data_testing %>%
      group_by(patient_id) %>%
      mutate(
        covid_test_pos_1 = first(test_date[result == "Positive"])
      ) %>%
      ungroup() %>% 
      filter(test_date == covid_test_pos_1 & result == "Negative") %>% 
      mutate(covid_discrepant_test = "Yes") %>% 
      select(patient_id, covid_discrepant_test),
    by = "patient_id"
  ) %>% 
  replace_na(list(covid_discrepant_test = "No"))

# Save data as rds ----
write_rds(data_patient,
          here::here("output", "data", "data_patient.rds"),
          compress="gz")