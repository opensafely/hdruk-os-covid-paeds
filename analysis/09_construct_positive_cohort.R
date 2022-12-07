
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions and lookup tables ----
source(here::here("analysis", "00_utility_functions.R"))

# Output directories ----
dir.create(here::here("output", "data"),
           showWarnings = FALSE, recursive=TRUE)

dir.create(here::here("output", "descriptives", "positive_cohort"),
           showWarnings = FALSE, recursive=TRUE)

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Random sample ----
set.seed(4192875)
n_positive_sample = 50000

# Load patient data ----
data_patient    = read_rds(here::here("output", "data", "data_patient.rds"))

# Calculate time dependendt variables on test date
# (age, comorbidity, vaccination, death)
data_patient = data_patient %>% 
  calc_indexed_variables(data_patient %>% pull(covid_test_date_pos_tp))

# Calculate follow-up/censor dates, duration and events -----
data_patient = data_patient %>% 
  mutate(
    follow_up_start_date = covid_test_date_pos_tp + days(15),
    max_follow_up_end_date = follow_up_start_date + days(364),
    censor_date = pmin(study_end_date,
                       max_follow_up_end_date,
                       death_date,
                       na.rm = TRUE),
    censor_event = case_when(
      censor_date == max_follow_up_end_date ~ "Maximum follow-up duration",
      censor_date == study_end_date ~ "Study end",
      censor_date == death_date ~ "Died",
    ),
    follow_up_days = (censor_date - follow_up_start_date) %>% 
      as.numeric()
  )

# Create inclusion flowchart data ----
data_inclusion = data_patient %>%
  transmute(
    patient_id,
    tested_positive = covid_status_tp == "Positive",
    not_nosocomial = covid_nosocomial == "No",
    no_discrepant_results = covid_discrepant_test == "No",
    age_between_4_and_17 = (age >= 4) & (age < 18),
    alive_on_test_date = death == "No",
    minimum_90_days_follow_up = follow_up_days >= 90
  ) %>% 
  replace_na(
    list(age_between_4_and_17 = FALSE,
         minimum_90_days_follow_up = FALSE))

# Randomly sample from patients satisfying inclusion ---- 
random_sample = data_inclusion %>%
  pivot_longer(-patient_id) %>%
  group_by(patient_id) %>%
  summarise(elegible_for_sample = all(value)) %>% 
  filter(elegible_for_sample) %>% 
  slice_sample(n = min(n_positive_sample, nrow(.)), replace = FALSE) %>% 
  mutate(randomly_sampled = TRUE)


data_inclusion = data_inclusion %>% 
  left_join(random_sample %>% 
              select(patient_id, randomly_sampled), by = "patient_id") %>% 
  replace_na(list(randomly_sampled = FALSE))

# Create inclusion flowchart ----
flowchart = data_inclusion %>% 
  transmute(
    patient_id,
    c0 = TRUE,
    c1 = c0 & tested_positive,
    c2 = c1 & not_nosocomial,
    c3 = c2 & no_discrepant_results,
    c4 = c3 & alive_on_test_date,
    c5 = c4 & age_between_4_and_17,
    c6 = c5 & minimum_90_days_follow_up,
    c7 = c6 & randomly_sampled
  ) %>%
  select(-patient_id) %>%
  summarise(across(.fns=sum)) %>% 
  mutate(pivot_col = "pivot") %>% 
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
      crit == "c4" ~ "-  alive on test date",
      crit == "c5" ~ "-  with age between 4 and 17 years inclusive on test date",
      crit == "c6" ~ "-  with minimum 90 days follow-up",
      crit == "c7" ~ "-  randomly sampled",
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
          here::here("output", "descriptives", "positive_cohort",
                     "tbl_flowchart.csv"))

# Create inclusion flag and append to patient data ----
data_patient = data_patient %>% 
  left_join(data_inclusion %>%
              pivot_longer(-patient_id) %>%
              group_by(patient_id) %>%
              summarise(include_flag = all(value)),
            by = "patient_id")

# Create positive cohort based on inclusion flag ----
data_positives = data_patient %>% 
  filter(include_flag)

# Load resource data and filter patient id ----
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds")) %>% 
  filter(patient_id %in% data_positives$patient_id)

data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds")) %>% 
  filter(patient_id %in% data_positives$patient_id)

data_gp = read_rds(here::here("output", "data", "data_gp.rds"))  %>% 
  filter(patient_id %in% data_positives$patient_id)

# Filter out specialty specific/non-contact code types ----
data_outpatient = data_outpatient %>% 
  filter(is.na(specialty))

data_gp = data_gp %>%
  filter(str_starts(code_type, "KM_") |
           str_starts(code_type, "mapped_1_") |
           str_starts(code_type, "mapped_2"))

# Resource dataset ----
## Create template spanning 1 year prior to positive test to censor date ----
data_resource = data_positives %>% 
  group_by(patient_id) %>% 
  summarise(
    date = seq(covid_test_date_pos_tp - days(365), censor_date, "day"),
    date_indexed = (date - covid_test_date_pos_tp) %>% 
      as.numeric() %>% 
      ff_label("Day relative to index positive test date")
  ) %>% 
  ungroup()

## Bed-days ---- 
data_resource = data_resource %>% 
  left_join(
    data_admissions %>%
      select(patient_id, admission_date, discharge_date) %>% 
      rowwise() %>% 
      mutate(date = list(seq(admission_date, discharge_date, by = "day"))) %>% 
      unnest(date) %>%
      ungroup() %>%
      mutate(
        n_beddays = case_when(
          date == admission_date ~ 0.5,
          date == discharge_date ~ 0.5,
          TRUE ~ 1)
      ) %>% 
      group_by(patient_id, date) %>% 
      summarise(n_beddays = min(sum(n_beddays), 1)) %>% 
      ungroup(),
    by = c("patient_id", "date")
  ) %>% 
  replace_na(list(n_beddays = 0))

## Critical-care ----

data_resource = data_resource %>% 
  left_join(
    data_admissions %>% 
      filter(critical_care_days > 0) %>% 
      select(patient_id, index, admission_date, discharge_date, critical_care_days) %>%
      rowwise() %>% 
      mutate(date = list(seq(admission_date,
                             min(admission_date + days(critical_care_days), discharge_date),
                             by = "day"))) %>% 
      unnest(date) %>%
      group_by(patient_id, index) %>% 
      mutate(
        critical_care_days = case_when(
          row_number() == 1 ~ 0.5,
          row_number() == n() ~ 0.5,
          TRUE ~ 1
        )
      ) %>% 
      group_by(patient_id, date) %>% 
      summarise(
        n_critical_care = min(sum(critical_care_days), 1)
      ),
    by = c("patient_id", "date")
  ) %>% 
  replace_na(list(n_critical_care = 0))

## Outpatient ----
data_resource = data_resource %>% 
  left_join(
    data_outpatient %>%
      group_by(patient_id, outpatient_date) %>% 
      summarise(
        n_outpatient = sum(outpatient_count)
      ) %>% 
      rename(date = outpatient_date),
    by = c("patient_id", "date")
  ) %>% 
  replace_na(list(n_outpatient = 0))

## GP ----
data_resource = data_resource %>% 
  left_join(
    data_gp %>%
      group_by(patient_id, gp_date) %>% 
      summarise(
        n_gp = 1
      ) %>% 
      rename(date = gp_date),
    by = c("patient_id", "date")
  ) %>% 
  replace_na(list(n_gp = 0))



# Calculate resource related covariates ----
## Healthcare use 1-year prior to positive test ----
data_positives = data_positives %>% 
  left_join(
    data_resource %>% 
      filter(date_indexed < 0) %>% 
      group_by(patient_id) %>% 
      summarise(
        n_beddays_pre_covid_1yr = sum(n_beddays),
        n_outpatient_pre_covid_1yr = sum(n_outpatient),
        n_gp_pre_covid_1yr = sum(n_gp)
      ),
    by = "patient_id"
  ) %>% 
  mutate(
    n_beddays_pre_covid_1yr = n_beddays_pre_covid_1yr %>% 
      na_if(0) %>% 
      ff_label("Bed-days in year prior to positive test"),
    n_outpatient_pre_covid_1yr = n_outpatient_pre_covid_1yr %>% 
      na_if(0) %>% 
      ff_label("Outpatient appointments (in year prior to positive test)"),
    n_gp_pre_covid_1yr = n_gp_pre_covid_1yr %>% 
      na_if(0) %>% 
      ff_label("Contact days (in year prior to positive test)"),
    ntile_beddays_pre_covid_1yr = n_beddays_pre_covid_1yr %>% 
      ntile(3) %>% 
      factor() %>%
      fct_explicit_na("None") %>% 
      fct_relevel("None") %>% 
      fct_recode("1 (lowest)" = "1", "3 (highest)" = "3") %>% 
      ff_label("Prior inpatient bed-days (tertile)"),
    ntile_outpatient_pre_covid_1yr = n_outpatient_pre_covid_1yr %>% 
      ntile(3) %>% 
      factor() %>%
      fct_explicit_na("None") %>% 
      fct_relevel("None") %>% 
      fct_recode("1 (lowest)" = "1", "3 (highest)" = "3") %>% 
      ff_label("Prior outpatient appointments (tertile)"),
    ntile_gp_pre_covid_1yr = n_gp_pre_covid_1yr %>% 
      ntile(3) %>% 
      factor() %>%
      fct_explicit_na("None") %>% 
      fct_relevel("None") %>% 
      fct_recode("1 (lowest)" = "1", "3 (highest)" = "3") %>% 
      ff_label("Prior healthcare episodes (tertile)"),
  )

## Healthcare use 2 weeks following positive test -----
data_positives = data_positives %>% 
  left_join(
    data_resource %>% 
      filter(date_indexed > 0, date_indexed < 15) %>% 
      group_by(patient_id) %>% 
      summarise(
        n_critical_care_2wks_post_covid = sum(n_critical_care),
        n_beddays_2wks_post_covid = sum(n_beddays),
        n_outpatient_2wks_post_covid = sum(n_outpatient),
        n_gp_2wks_post_covid = sum(n_gp)
      ),
    by = "patient_id"
  ) %>% 
  mutate(
    n_critical_care_2wks_post_covid = n_critical_care_2wks_post_covid %>% 
      na_if(0) %>% 
      ff_label("Critical care days (2 weeks after positive test)"),
    n_beddays_2wks_post_covid = n_beddays_2wks_post_covid %>% 
      na_if(0) %>% 
      ff_label("Bed-days (2 weeks after positive test)"),
    n_outpatient_2wks_post_covid = n_outpatient_2wks_post_covid %>% 
      na_if(0) %>% 
      ff_label("Outpatient appointments (2 weeks after positive test)"),
    n_gp_2wks_post_covid = n_gp_2wks_post_covid %>% 
      na_if(0) %>% 
      ff_label("Contact days (2 weeks after positive test)"),
    illness_severity_2wks = case_when(
      !is.na(n_critical_care_2wks_post_covid) ~ "Critical care",
      !is.na(n_beddays_2wks_post_covid) ~ "Inpatient",
      !is.na(n_outpatient_2wks_post_covid) ~ "Outpatient",
      !is.na(n_gp_2wks_post_covid) ~ "Healthcare episode",
      TRUE ~ "No contact"
    ) %>%
      factor() %>% 
      fct_relevel("No contact", "Healthcare episode",
                  "Outpatient", "Inpatient", "Critical care") %>% 
      ff_label("Illness severity within 2 weeks")
  )

## PIMS-TS status ----
data_positives = data_positives %>% 
  left_join(
    data_admissions %>% 
      left_join(data_positives %>% 
                  select(patient_id, covid_test_date_pos_tp),
                by = "patient_id") %>% 
      filter(admission_date > covid_test_date_pos_tp,
             admission_date <= covid_test_date_pos_tp + days(14)) %>%
      filter(
        (str_sub(primary_diagnosis, 1, 4) == "M303" &
           admission_date < ymd("2020-11-01")) |
          (str_sub(primary_diagnosis, 1, 3) == "R65" &
             admission_date < ymd("2020-11-01")) |
          (str_sub(primary_diagnosis, 1, 4) == "U075" &
             admission_date >= ymd("2020-11-01"))
      ) %>%
      group_by(patient_id) %>% 
      summarise(pims_ts = "Yes"),
    by = "patient_id"
  ) %>% 
  replace_na(list(pims_ts = "No")) %>% 
  mutate(pims_ts = pims_ts %>% 
           factor() %>% 
           ff_label("PIMS-TS")
  )

# Save resource data ----
write_rds(data_resource,
          here::here("output", "data", "data_resource.rds"),
          compress="gz")

# Save positive cohort data ----
write_rds(data_positives,
          here::here("output", "data", "data_positives.rds"),
          compress="gz")