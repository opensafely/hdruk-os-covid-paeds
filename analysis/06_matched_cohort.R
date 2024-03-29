# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# Create matched cohort
# 06_matched_cohort.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
#
# This script matches COVID-positive patients with COVID-negative and 
# untested patients during the testing period of interest. Positive-negative
# matching is performed iteratively between the 1st positive test date
# and negative test dates with patients being removed from the pool of
# potential matches once matched. Untested patients are randomly assigned
# a test date based on the distribution of matched test dates and matched
# with a positive patient. The final matched dataset consists of sets of
# matched patients with a positive:negative:untested ratio of 1:5:5.

# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions and lookup tables ----
source(here::here("analysis", "00_utility_functions.R"))
source(here::here("analysis", "00_lookup_tables.R"))

# Output directories ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "matched_cohort"), showWarnings = FALSE, recursive=TRUE)

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round = global_var$disclosure_count_round

# Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Matching parameters ----
match_ratio = 5
match_window = 0
min_followup_months = 6

# Load datasets ----
data_patient    = read_rds(here::here("output", "data", "data_patient.rds"))
data_testing    = read_rds(here::here("output", "data", "data_testing.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))

# Variable labels ----
var_label = data_patient %>% extract_variable_label()

# Create dataset to log inclusion for inclusion flowchart ----
data_inclusion = data_patient %>% 
  transmute(
    patient_id,
    covid_status_tp,
    not_nosocomial = covid_nosocomial == "No",
    no_discrepant_results = covid_discrepant_test == "No",
    no_missing_demographics = case_when(
      !is.na(sex) ~ TRUE,
      !is.na(ethnicity) ~ TRUE,
      !is.na(imd_Q5_2019) ~ TRUE,
      !is.na(region_2019) ~ TRUE,
      !is.na(rural_urban_2019) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Only consider test data within testing period ----
data_testing_tp = data_testing %>% 
  filter(test_date >= tp_start_date,
         test_date < tp_end_date)

# Filter out nosocomial covid and discrepant covid test results ----
data_patient = data_patient %>% 
  filter(covid_nosocomial == "No", covid_discrepant_test == "No")

# Filter out missing demographics ----
data_patient = data_patient %>% 
  filter(!is.na(sex), !is.na(imd_Q5_2019),
         !is.na(region_2019), !is.na(rural_urban_2019),
         !is.na(ethnicity))

data_testing_tp = data_testing_tp %>%
  filter(patient_id %in% data_patient$patient_id) %>% 
  left_join(data_patient %>% 
              select(patient_id, covid_status_tp, date_of_birth, death_date,
                     covid_test_date_pos_fup),
            by = "patient_id") %>% 
  drop_na(patient_id, covid_status_tp, date_of_birth)

# Filter test dates ----
# Include only:
#  - 1st positive test date from positive patients, 
#  - negatives test dates from patients who only tested negative
data_testing_tp = data_testing_tp %>%
  group_by(patient_id, result) %>% 
  filter(covid_status_tp == "Positive" & result == "Positive" & row_number() == 1 |
           covid_status_tp == "Negative" & result == "Negative") %>% 
  ungroup()

## Calculate age on test date ----
data_testing_tp = data_testing_tp %>% 
  mutate(
    age_test_date = time_length(difftime(test_date, date_of_birth), "years")
  ) %>%
  filter(age_test_date >= 4, age_test_date < 18)

### Log number of patients satisfying age criteria ----
data_inclusion = data_inclusion %>% 
  left_join(data_testing_tp %>%
              group_by(patient_id) %>% 
              slice(1) %>% 
              ungroup() %>% 
              mutate(age_criteria_test_date = TRUE) %>% 
              select(patient_id, age_criteria_test_date),
            by = c("patient_id"))

## Has at least 2 months follow-up ---
data_testing_tp = data_testing_tp %>% 
  filter(is.na(death_date) | test_date + days(14) + months(min_followup_months) < death_date) %>% 
  filter(covid_status_tp == "Positive" |
           is.na(covid_test_date_pos_fup) |
           (test_date + days(14) + months(min_followup_months) < covid_test_date_pos_fup))

### Log with minimum follow-up ----
data_inclusion = data_inclusion %>% 
  left_join(data_testing_tp %>%
              group_by(patient_id) %>% 
              slice(1) %>% 
              ungroup() %>% 
              mutate(min_months_followup_pos_neg = TRUE) %>% 
              select(patient_id, min_months_followup_pos_neg),
            by = c("patient_id"))

## Filter out in-hospital test-dates for negative patients ----
data_testing_tp = data_testing_tp %>% 
  left_join(
    data_testing_tp %>%
      filter(covid_status_tp == "Negative") %>% 
      left_join(
        data_admissions %>% 
          select(patient_id, admission_date, discharge_date),
        by = "patient_id"
      ) %>% 
      filter((test_date >= admission_date) &
               (test_date <= discharge_date)) %>% 
      mutate(in_hospital_neg_test = TRUE) %>%
      distinct(patient_id, test_date, result, in_hospital_neg_test),
    by = c("patient_id", "test_date", "result")
  ) %>% 
  filter(is.na(in_hospital_neg_test))

### Log number of patients not hospitalised during negative test ----
data_inclusion = data_inclusion %>% 
  left_join(data_testing_tp %>%
              group_by(patient_id) %>% 
              slice(1) %>% 
              ungroup() %>% 
              mutate(not_in_hospital_neg_test = TRUE) %>% 
              select(patient_id, not_in_hospital_neg_test),
            by = c("patient_id"))

# Remove unnecessary columns ----
data_testing_tp = data_testing_tp %>% 
  select(patient_id, result, test_date, covid_status_tp)

# Split testing data into positives and negatives ----
data_testing_pos = data_testing_tp %>% 
  filter(covid_status_tp == "Positive")

data_testing_neg = data_testing_tp %>% 
  filter(covid_status_tp == "Negative")

# Match positives with negatives ----
## Matching set up ----
matched = vector()

match_pos_neg = data_testing_pos %>%
  group_by(test_date) %>% 
  group_map(function(.x, .y){
    
    # Positive test date ----
    pos_test_date = .y %>%
      pull(test_date) %>% 
      ymd()
    
    # Positive pool ----
    df_pool_pos = .x
    
    # Negative pool ----
    # Restrict to match window, remove matched negatives from last iteration ----
    df_pool_neg = data_testing_neg %>%
      filter(test_date <= pos_test_date + days(match_window),
             test_date >= pos_test_date - days(match_window)) %>% 
      filter(!patient_id %in% matched) %>%
      group_by(patient_id) %>%
      slice(1) %>%
      ungroup()
    
    # Calculate number of potential matches in each pool ----
    n_pos = df_pool_pos %>% nrow()
    n_neg = df_pool_neg %>% nrow()
    n_match = min(n_pos, floor(n_neg/match_ratio))
    
    # If potential matches exist, otherwise skip ----
    if(n_match > 0){
      
      # Filter pools based on maximum possible matches ----
      df_pool_pos = df_pool_pos %>% 
        slice_sample(n = n_match) %>% 
        mutate(match_id = paste0(.y, 1:n_match))
      
      df_pool_neg = df_pool_neg %>% 
        slice_sample(n = n_match*match_ratio) %>% 
        mutate(match_id = paste0(.y, rep(1:n_match, each = match_ratio)))
      
      # Bind pools ----
      df_out = df_pool_pos %>% 
        bind_rows(df_pool_neg)
      
      # Update remove vector
      matched_iter = df_out %>%
        pull(patient_id)
      matched <<- c(matched, matched_iter)
      
    } else{
      df_out = NULL
    }
    
    return(df_out)
  },
  .keep = TRUE
  ) %>% 
  map_df(bind_rows)

# Match positive cases with untested ----
## Create dataset of untested ----
data_untested = data_patient %>% 
  filter(covid_status_tp == "Untested") %>% 
  select(patient_id, covid_status_tp, date_of_birth, death_date, covid_test_date_pos_fup) %>% 
  mutate(
    test_date = sample(
      match_pos_neg %>% 
        filter(result == "Positive") %>%
        pull(test_date),
      size = n(),
      replace = TRUE)
  )

## Filter out patients with less than 2 months followup ----
data_untested = data_untested %>% 
  filter(is.na(death_date) | test_date + days(14) + months(min_followup_months) < death_date) %>% 
  filter(is.na(covid_test_date_pos_fup) | test_date + days(14) + months(min_followup_months) < covid_test_date_pos_fup)


### Log number of patients minimum 2 months followup ----
data_inclusion = data_inclusion %>% 
  left_join(data_untested %>% 
              mutate(min_months_followup_untested = TRUE) %>% 
              select(patient_id, min_months_followup_untested),
            by = "patient_id")

## Filter out patients not aged 4-17 ----
### Calculate age on matched test date ----
data_untested = data_untested %>% 
  mutate(
    age_test_date = time_length(difftime(test_date, date_of_birth), "years"),
    result = "Untested"
  ) %>% 
  filter(age_test_date >= 4, age_test_date < 18)

### Log number of patients satisfying age criteria ----
data_inclusion = data_inclusion %>% 
  left_join(data_untested %>% 
              mutate(age_criteria_matched_date = TRUE) %>% 
              select(patient_id, age_criteria_matched_date),
            by = "patient_id")

## Filter out in-hospital test-dates for negative patients ----
data_untested = data_untested %>% 
  left_join(
    data_untested %>%
      left_join(
        data_admissions %>% 
          select(patient_id, admission_date, discharge_date),
        by = "patient_id"
      ) %>% 
      filter((test_date >= admission_date) &
               (test_date <= discharge_date)) %>% 
      mutate(in_hospital_untested = TRUE) %>%
      distinct(patient_id, test_date, result, in_hospital_untested),
    by = c("patient_id", "test_date", "result")
  ) %>% 
  filter(is.na(in_hospital_untested))

### Log number of untested patients not in-hospital on match date ----
data_inclusion = data_inclusion %>% 
  left_join(data_untested %>% 
              mutate(is_not_inhospital_untested = TRUE) %>% 
              select(patient_id, is_not_inhospital_untested),
            by = "patient_id")

## Filter out unnecessary columns ----
data_untested = data_untested %>% 
  select(patient_id, result, test_date, covid_status_tp)

# Create untested data matched to positives ----
match_untested = match_pos_neg %>% 
  filter(covid_status_tp == "Positive") %>% 
  pull(test_date) %>% unique() %>% sort() %>% 
  map(function(pos_test_date){
    
    # Define positive and untested pools ----
    df_pool_pos = match_pos_neg %>% 
      filter(covid_status_tp == "Positive", test_date == pos_test_date)
    
    df_pool_unt = data_untested %>% 
      filter(test_date == pos_test_date)
    
    # Calculate number of potential matches in each pool ----
    n_positive = df_pool_pos %>% nrow()
    n_untested = df_pool_unt %>% nrow()
    n_match    = min(n_positive, floor(n_untested/match_ratio))
    
    # Restrict number to number of matches, assign match_id ----
    if(n_match > 0){
      df_out = df_pool_unt %>%
        slice_sample(n = n_match*match_ratio) %>% 
        mutate(match_id = rep(df_pool_pos$match_id[1:n_match], each = match_ratio))
    } else {
      df_out = NULL
    }
    return(df_out)
  }) %>% 
  bind_rows()

# Combine the two matches ----
data_matched = match_pos_neg %>% 
  bind_rows(match_untested) %>% 
  select(match_id, patient_id, result, covid_status_tp, test_date) %>% 
  arrange(match_id, result) %>% 
  group_by(match_id) %>% 
  mutate(n_matches = n()) %>% 
  ungroup()

## Filter out incomplete match sets ----
data_matched = data_matched %>% 
  filter(n_matches == (match_ratio*2 +1)) %>% 
  select(-n_matches)

## Relabel variables ----
data_matched = data_matched %>% 
  ff_relabel(var_label)

## Save data as rds ----
write_rds(data_matched,
          here::here("output", "data", "data_matched.rds"),
          compress="gz")

## Log sucessful matches ----
data_inclusion = data_inclusion %>%
  left_join(
    data_matched %>%
      transmute(
        patient_id, matched = TRUE
      ),
    by = c("patient_id")
  )

# Clean-up inclusion data ----
data_inclusion = data_inclusion %>% 
  mutate(
    meets_age_criteria = case_when(
      age_criteria_test_date == TRUE ~ TRUE,
      age_criteria_matched_date == TRUE ~ TRUE, 
      TRUE ~ FALSE
    ),
    min_months_followup = case_when(
      covid_status_tp == "Untested" & is.na(min_months_followup_untested) ~ FALSE,
      (covid_status_tp == "Negative" | covid_status_tp == "Positive") &
        is.na(min_months_followup_pos_neg) ~ FALSE,
      TRUE ~ TRUE
    ),
    not_in_hospital_test_match_date = case_when(
      covid_status_tp == "Positive" ~ TRUE,
      not_in_hospital_neg_test == TRUE ~ TRUE,
      is_not_inhospital_untested == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    matched = case_when(
      matched == TRUE ~ TRUE,
      TRUE ~ FALSE)
  )

# Create inclusion flowchart ----
flowchart = data_inclusion %>% 
  transmute(
    patient_id,
    covid_status_tp,
    c0 = TRUE,
    c1 = c0 & no_missing_demographics,
    c2 = c1 & no_discrepant_results,
    c3 = c2 & not_nosocomial,
    c4 = c3 & meets_age_criteria,
    c5 = c4 & min_months_followup,
    c6 = c5 & not_in_hospital_test_match_date,
    c7 = c6 & matched
  ) %>%
  select(-patient_id) %>%
  group_by(covid_status_tp) %>%
  summarise(
    across(.fns=sum)
  ) %>%
  pivot_longer(
    cols=-covid_status_tp,
    names_to="criteria",
    values_to="n"
  ) %>%
  group_by(covid_status_tp) %>%
  mutate(
    n = n %>% plyr::round_any(count_round)
  ) %>% 
  mutate(
    n_exclude = lag(n) - n,
    pct_all = (n/first(n)) %>% scales::percent(0.1),
    pct_exclude_step = (n_exclude/lag(n)) %>% scales::percent(0.1),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "OpenSAFELY extract: Registered with GP, alive, with age >0 and <18 years on 01 January 2019",
      crit == "c1" ~ "-  with no missing demographic data (sex, ethnicity, IMD, region, rural-urban classification)",
      crit == "c2" ~ "-  with no same-day discrepant RT-PCR test result",
      crit == "c3" ~ "-  with no probable nosocomial infection",
      crit == "c4" ~ "-  with age between 4 and 17 years inclusive on test/matched date",
      crit == "c5" ~ "-  minimum 6 months follow-up",
      crit == "c6" ~ "-  not hospitalised on day of matching/negative RT-PCR test result",
      crit == "c7" ~ "-  successfully matched with negative:untested:positive of 5:5:1",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(n_exclude = n_exclude %>% as.character()) %>% 
  replace_na(
    list(n_exclude = "-", pct_exclude_step = "-")
  )


## Format flowchart table ----
tbl_flowchart = flowchart %>% 
  select(covid_status_tp, criteria, n, n_exclude, pct_all, pct_exclude_step) %>%
  group_by(covid_status_tp) %>% 
  mutate(
    covid_status_tp = covid_status_tp %>% as.character(),
    covid_status_tp = if_else(row_number() == 1,covid_status_tp, "")
  )

## Save flowchart table ----
write_csv(tbl_flowchart, 
          here::here("output", "descriptives", "matched_cohort",
                     "tbl_flowchart.csv"))

