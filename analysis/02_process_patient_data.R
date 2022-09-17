
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))
source(here::here("analysis", "00_load_codelists.R"))

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

# Update first and last comorbidity dates with info from admissions data ----
data_patient = icd_10_codelist %>% 
  map2(as.list(names(icd_10_codelist)), function(.icd_10_codelist, .comorb_vars){

    # Filter for admissions with primary diagnosis in codelist ----
    icd_10_first_last_dates = data_admissions %>%
      filter(primary_diagnosis %in% .icd_10_codelist) %>%
      group_by(patient_id) %>%
      # Extract first/last ----
      summarise(
        icd10_first_date = first(admission_date),
        icd10_last_date = last(admission_date)
      )
    
    # Comorbidity first and last date variable names
    snomed_first_date = paste0(.comorb_vars, "_first_date")
    snomed_last_date  = paste0(.comorb_vars, "_last_date")
    
    data_patient %>%
      select(patient_id, snomed_first_date = all_of(snomed_first_date),
             snomed_last_date = all_of(snomed_last_date)) %>% 
      left_join(icd_10_first_last_dates, by = "patient_id") %>% 
      mutate(
        # Select minimum first date and max for last date
        new_first_date = pmin(snomed_first_date, icd10_first_date, na.rm = TRUE),
        new_last_date  = pmax(snomed_last_date,  icd10_last_date,  na.rm = TRUE),
       ) %>%
      select(-c(snomed_first_date, snomed_last_date, icd10_first_date, icd10_last_date)) %>%
      rename({{snomed_first_date}} := new_first_date,
             {{snomed_last_date}} := new_last_date)
   }) %>% 
  reduce(full_join, by = "patient_id") %>% 
  right_join(
    data_patient %>% 
      select(-contains(names(icd_10_codelist))),
    by = "patient_id"
  )

# Create factors and label variables -----
data_patient = data_patient %>%
  mutate(
    date_of_birth = if_else(is.na(date_of_birth),
                            NA_character_,
                            paste0(date_of_birth, "-15")) %>%
      ymd(),

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
      ethnicity_gp == "3" ~ "Asian",
      ethnicity_gp == "2" ~ "Mixed",
      ethnicity_gp == "5" ~ "Other",
      TRUE ~ NA_character_
    ) %>%
      factor() %>%
      fct_relevel("White") %>% 
      ff_label("Ethnicity (primary care)"),

    ethnicity_6_sus = case_when(
      ethnicity_6_sus == "1" ~ "White",
      ethnicity_6_sus == "4" ~ "Black",
      ethnicity_6_sus == "3" ~ "Asian",
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
      fct_relevel("East Midlands") %>% 
      ff_label("Region"),

    imd_Q5_2019 = case_when(
      (imd_2019 >=1)          & (imd_2019 < 32844*1/5) ~ "1 (most deprived)",
      (imd_2019 >= 32844*1/5) & (imd_2019 < 32844*2/5) ~ "2",
      (imd_2019 >= 32844*2/5) & (imd_2019 < 32844*3/5) ~ "3",
      (imd_2019 >= 32844*3/5) & (imd_2019 < 32844*4/5) ~ "4",
      (imd_2019 >= 32844*4/5)                          ~ "5 (least deprived)",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)")) %>%
      ff_label("Multiple deprivation quintile"),

    rural_urban_2019 = case_when(
      rural_urban_2019 %in% c(1,2)     ~ "Urban conurbation",
      rural_urban_2019 %in% c(3,4)     ~ "Urban city or town",
      rural_urban_2019 %in% c(5,6,7,8) ~ "Rural town or village",
      TRUE                             ~ NA_character_
    ) %>%
      factor() %>%
      fct_relevel("Urban city or town", "Urban conurbation") %>% 
      ff_label("Rural-urban classification"),
    
     shielding = if_else(is.na(shielding_first_date), "No", "Yes") %>% 
       factor() %>% 
       ff_label("COVID-19 shielding")
  ) %>% 
  calc_indexed_variables(ymd("2019-01-01"))

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
      filter(!is.na(test_period)) %>% 
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
      fct_relevel("Negative", "Positive") %>% 
      ff_label("SARS-CoV-2 status (testing period)"),
    covid_status_fup = case_when(
      covid_test_pos_fup_count > 0 ~ "Positive",
      covid_test_neg_fup_count > 0 ~ "Negative",
      TRUE ~ "Untested") %>%
      factor() %>%
      fct_relevel("Negative", "Positive") %>% 
      ff_label("SARS-CoV-2 status (follow-up period)"),
  )

# 1st covid test dates (testing period and follow up) ----
data_patient = data_patient %>% 
  left_join(
    data_testing %>%
      filter(test_date >= tp_start_date, test_date <= tp_end_date) %>% 
      filter(result == "Positive") %>%
      group_by(patient_id) %>% 
      slice(1) %>%
      ungroup() %>% 
      select(patient_id, covid_test_date_pos_tp = test_date),
    by = "patient_id"
  ) %>% 
  left_join(
    data_testing %>%
      filter(test_date >= tp_start_date, test_date <= tp_end_date) %>% 
      filter(result == "Negative") %>% 
      group_by(patient_id) %>% 
      slice(1) %>%
      ungroup() %>% 
      select(patient_id, covid_test_date_neg_tp = test_date),
    by = "patient_id"
  ) %>% 
  left_join(
    data_testing %>%
      filter(test_date >= fup_start_date, test_date <= study_end_date) %>% 
      filter(result == "Positive") %>% 
      group_by(patient_id) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(patient_id, covid_test_date_pos_fup = test_date),
    by = "patient_id"
  ) %>% 
  left_join(
    data_testing %>%
      filter(test_date >= fup_start_date, test_date <= study_end_date) %>% 
      filter(result == "Negative") %>% 
      group_by(patient_id) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(patient_id, covid_test_date_neg_fup = test_date),
    by = "patient_id"
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
          select(patient_id, covid_test_date_pos_tp),
        by = "patient_id") %>%
      mutate(
        covid_nosocomial = case_when(
          # Length of stay less than 7 days: Not nosocomial 
          (discharge_date - admission_date) < 7 ~ NA_character_,
          # Length of stay 7+ days: Nosocomial if positive after day 7 in hospital
          # and on or before day 7 following discharge, otherwise not nosocomial
          (admission_date + days(7) < covid_test_date_pos_tp) &
            (discharge_date + days(7) >= covid_test_date_pos_tp) ~ "Yes",
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