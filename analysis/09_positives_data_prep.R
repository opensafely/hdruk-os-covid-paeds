
# Load packages ----
library(tidyverse)
library(lubridate)

# Load custom functions and lookup tables ----
source(here::here("analysis", "00_utility_functions.R"))

# Output directories ----
dir.create(here::here("output", "data"),
           showWarnings = FALSE, recursive=TRUE)

dir.create(here::here("output", "descriptives", "machine_learning"),
           showWarnings = FALSE, recursive=TRUE)

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Load patient data ----
data_patient    = read_rds(here::here("output", "data", "data_patient.rds"))

# Create inclusion flowchart data ----
data_inclusion = data_patient %>%
  transmute(
    patient_id,
    tested_positive = covid_status_tp == "Positive",
    not_nosocomial = covid_nosocomial == "No",
    no_discrepant_results = covid_discrepant_test == "No"
  )

# Filter out non-positives and not aged 4 to 17 inclusive ----
data_positives = data_patient %>% 
  filter(covid_status_tp == "Positive",
         covid_nosocomial == "No",
         covid_discrepant_test == "No") 

data_positives = data_positives %>% 
  calc_indexed_variables(data_positives %>% pull(covid_test_date_pos_tp)) %>% 
  apply_exclusion_criteria()

## Update inclusion data ----
data_inclusion = data_inclusion %>% 
  left_join(
    data_positives %>% 
      transmute(
        patient_id,
        meets_age_criteria = TRUE
      ),
    by = "patient_id"
  ) %>% 
  replace_na(list(meets_age_criteria = FALSE))

# Create inclusion flowchart ----
flowchart = data_inclusion %>% 
  transmute(
    patient_id,
    c0 = TRUE,
    c1 = c0 & tested_positive,
    c2 = c1 & not_nosocomial,
    c3 = c2 & no_discrepant_results,
    c4 = c3 & meets_age_criteria,
  ) %>%
  select(-patient_id) %>%
  summarise(
    across(.fns=sum)
  ) %>% 
  mutate(
    pivot_col = "pivot"
  ) %>% 
  pivot_longer(
    cols=-pivot_col,
    names_to="criteria",
    values_to="n"
  ) %>% 
  select(-pivot_col) %>% 
  mutate(
    n = n #%>% plyr::round_any(count_round)
  ) %>% 
  mutate(
    n_exclude = lag(n) - n,
    pct_all = (n/first(n)) %>% scales::percent(0.1),
    pct_exclude_step = (n_exclude/lag(n)) %>% scales::percent(0.1),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "OpenSAFELY extract: Registered with GP, alive, with age >0 and <18 years on 01 January 2019",
      crit == "c1" ~ "-  Positive SARS-CoV-2 RT-PCR test during testing period",
      crit == "c2" ~ "-  with no probable nosocomial infection",
      crit == "c3" ~ "-  with no same-day discrepant RT-PCR test result",
      crit == "c4" ~ "-  with age between 4 and 17 years inclusive on test/matched date",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(n_exclude = n_exclude %>% as.character()) %>% 
  replace_na(
    list(n_exclude = "-", pct_exclude_step = "-")
  )

## Format flowchart table ----
tbl_flowchart = flowchart %>% 
  select(criteria, n, n_exclude, pct_all, pct_exclude_step)

## Save flowchart table ----
write_csv(tbl_flowchart, 
          here::here("output", "descriptives", "machine_learning",
                     "tbl_flowchart.csv"))



# Load resource data and filter patient id ----
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds")) %>% 
  filter(patient_id %in% data_positives$patient_id)

data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds")) %>% 
  filter(patient_id %in% data_positives$patient_id)

data_gp = read_rds(here::here("output", "data", "data_gp.rds"))  %>% 
  filter(patient_id %in% data_positives$patient_id)

# Filter out specialty specific/non-contact code types ----
## Outpatient ----
data_outpatient = data_outpatient %>% 
  filter(is.na(specialty))

## GP ----
data_gp = data_gp %>%
  filter(str_starts(code_type, "KM_") | str_starts(code_type, "mapped_1_"))

# Calculate start and end of follow-up
data_positives = data_positives %>% 
  mutate(
    follow_up_start_date = covid_test_date_pos_tp + days(15),
    follow_up_end_date   = follow_up_start_date   + days(364) 
  )

# Resource dataset ----
## Create template ----
data_resource = data_positives %>% 
  group_by(patient_id) %>% 
  summarise(
    date = seq(follow_up_start_date, follow_up_end_date, "day"),
    date_indexed = 1:365,
    alive = date < death_date
  ) %>% 
  ungroup() %>% 
  replace_na(list(alive = TRUE))

## Admissions -----
### Filter for dates during 1-year follow-up (starts 14 days after test date) ----
data_admissions = data_admissions %>% 
  left_join(
    data_positives %>% 
      select(patient_id, follow_up_start_date, follow_up_end_date),
    by = "patient_id"
  ) %>% 
  filter(admission_date >= follow_up_start_date,
         admission_date <= follow_up_end_date)

### Bed-days ----
data_resource = data_resource %>% 
  left_join(
    data_admissions %>%
      select(patient_id, admission_date, discharge_date, follow_up_end_date) %>%
      rowwise() %>% 
      mutate(date = list(seq(admission_date, discharge_date, by = "day"))) %>% 
      unnest(date) %>%
      ungroup() %>% 
      mutate(n_beddays = case_when(
        date == admission_date ~ 0.5,
        date == discharge_date ~ 0.5,
        TRUE ~ 1
      )) %>% 
      select(patient_id, date, n_beddays),
    by = c("patient_id", "date")
  )

### Critical-care ----
data_resource = data_resource %>% 
  left_join(
    data_admissions %>%
      filter(critical_care_days > 0) %>% 
      select(patient_id, index, admission_date, discharge_date, follow_up_end_date, critical_care_days) %>%
      rowwise() %>% 
      mutate(date = list(seq(admission_date,
                             min(admission_date + days((critical_care_days)), discharge_date),
                             by = "day"))) %>% 
      unnest(date) %>%
      ungroup() %>% 
      group_by(patient_id, index) %>% 
      mutate(n_critical_care = case_when(
        row_number() == 1 ~ 0.5,
        row_number() == n() ~ 0.5,
        TRUE ~ 1
      )) %>% 
      ungroup() %>% 
      select(patient_id, date, n_critical_care),
    by = c("patient_id", "date")
  )

## Outpatient ----
data_resource = data_resource %>% 
  left_join(
    data_outpatient %>% 
      left_join(
        data_positives %>% 
          select(patient_id, follow_up_start_date, follow_up_end_date),
        by = "patient_id"
      ) %>% 
      filter(outpatient_date >= follow_up_start_date,
             outpatient_date <= follow_up_end_date) %>%
      rename(date = outpatient_date, n_outpatient = outpatient_count) %>% 
      select(patient_id, date, n_outpatient),
    by = c("patient_id", "date")
  )

## GP ----
data_resource = data_resource %>% 
  left_join(
    data_gp %>% 
      left_join(
        data_positives %>% 
          select(patient_id, follow_up_start_date, follow_up_end_date),
        by = "patient_id"
      ) %>% 
      filter(gp_date >= follow_up_start_date,
             gp_date <= follow_up_end_date) %>% 
      distinct(patient_id, gp_date, .keep_all = TRUE) %>% 
      mutate(n_gp = 1) %>% 
      select(patient_id, date = gp_date, n_gp),
    by = c("patient_id", "date")
  )

# Fix N/A values ----
data_resource = data_resource %>% 
  replace_na(
    list(alive = TRUE,
         n_beddays = 0,
         n_critical_care = 0,
         n_outpatient = 0,
         n_gp = 0))

# Save resource data ----
write_rds(data_resource,
          here::here("output", "data", "data_resource.rds"),
          compress="gz")

# Save positive cohort data ----
write_rds(data_positives,
          here::here("output", "data", "data_positives.rds"),
          compress="gz")