

# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)
library(scales)
library(zoo)

# Load custom functions and lookup tables ----
source(here::here("analysis", "00_utility_functions.R"))
source(here::here("analysis", "00_lookup_tables.R"))

# Output directories
dir.create(here::here("output", "descriptives", "healthcare_use_2019_2022"),
           showWarnings = FALSE, recursive=TRUE)

# Plot theme
theme_set(theme_bw())

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

# Load datasets ----
data_patient    = read_rds(here::here("output", "data", "data_patient.rds"))

# Yearly cohort construction ----
## Index dates ----
index_date = c("2019-01-01",
               "2020-01-01",
               "2021-01-01",
               "2022-01-01") %>% 
  ymd() %>% 
  as.list()

## Create cohort data for each year ----
data_cohort = index_date %>% 
  map(function(.index_date, .data_patient){
    data_cohort = .data_patient %>%
      mutate(cohort = year(.index_date)) %>% 
      calc_indexed_variables(.index_date)
  },
  .data_patient = data_patient
  )

# Inclusion flowchart ----
## Calculate inclusion flowchart ----
tbl_flowchart = data_cohort %>% 
  map2(
    index_date,
    function(.data_cohort, .index_date){
      data_criteria = .data_cohort %>% 
        count_exclusion_criteria() %>% 
        transmute(
          c0 = TRUE,
          c1 = c0 & is_not_covid_nosocomial,
          c2 = c1 & is_not_covid_discrepant_test,
          c3 = c2 & is_alive,
          c4 = c3 & is_aged_between_4_17,
        ) %>%
        summarise(
          across(.fns=sum)
        ) %>%
        mutate(pivot_col = NA) %>% 
        pivot_longer(
          cols=-pivot_col,
          names_to="criteria",
          values_to="n"
        ) %>% 
        select(-pivot_col) %>% 
        mutate(
          n = n %>% plyr::round_any(count_round)
        ) %>% 
        mutate(
          cohort = year(.index_date),
          n_exclude = lag(n) - n,
          pct_all = (n/first(n)) %>% percent(0.1),
          pct_exclude_step = (n_exclude/lag(n)) %>% percent(0.1),
          crit = str_extract(criteria, "^c\\d+"),
          criteria = fct_case_when(
            crit == "c0" ~ "OpenSAFELY extract: Registered with GP, alive, with age >0 and <18 years on 01 January 2019",
            crit == "c1" ~ "  with no probable nosocomial infection",
            crit == "c2" ~ "  with no same-day discrepant RT-PCR test result",
            crit == "c3" ~ "  is alive on 1st January",
            crit == "c4" ~ "  with age between 4 and 17 years inclusive",
            TRUE ~ NA_character_
          )
        ) %>%
        mutate(n_exclude = n_exclude %>% as.character()) %>% 
        replace_na(
          list(n_exclude = "-", pct_exclude_step = "-")
        )
    }) %>% 
  bind_rows() %>% 
  select(cohort, criteria, n, n_exclude, pct_all, pct_exclude_step) %>%
  group_by(cohort) %>% 
  mutate(cohort = if_else(row_number() == 1, cohort %>% as.character(), ""))

## Save inclusion flowchart ----
write_csv(tbl_flowchart, 
          here::here("output", "descriptives", "healthcare_use_2019_2022",
                     "tbl_flowchart.csv"))

# Filter cohort based on exclusion criteria ----
data_cohort = data_cohort %>% 
  map(function(.data_cohort){
    .data_cohort %>%
      apply_exclusion_criteria()
  })

# Cohort summary table ----
explanatory_var = c(
  # Covid status
  "covid_status_tp",
  
  # Demographics
  "age", "age_group", "sex", "ethnicity", "imd_Q5_2019", "region_2019",
  "rural_urban_2019",
  
  # Shielding
  "shielding",
  
  # Comorbidities
  "comorbidity_count.factor",
  "asthma", "cancer", "cerebral_palsy", "chronic_infections", "cystic_fibrosis",
  "devices_and_stomas", "diabetes", "endocrine_disorders",
  "epilepsy", "gastrointestinal_disorders", "haematological_disorders",
  "immunological_disorders", "learning_and_behaviour_difficulties",
  "mental_illness", "musculoskeletal_and_rheum",
  "severe_mental_illness", "transplant"

)

## Create cohort summary table ----
tbl_cohort_summary = data_cohort %>% 
  map(function(.data_cohort){
    .data_cohort %>%
      mutate(cohort = cohort %>% factor()) %>% 
      summary_factorlist(
        dependent = "cohort",
        explanatory = explanatory_var,
        cont = "median",
        total_col = FALSE,
        add_col_totals = TRUE,
        na_include = TRUE
        
      ) %>% 
      mutate(row_num = row_number())
  }) %>% 
  reduce(left_join, by = c("row_num", "label", "levels")) %>% 
  select(-row_num) %>% 
  ff_round_counts(count_round)

## Save cohort summary table ----
write_csv(tbl_cohort_summary, 
          here::here("output", "descriptives", "healthcare_use_2019_2022",
                     "tbl_cohort_summary.csv"))

## Save cohort data ----
write_rds(data_cohort,
          here::here("output", "data", "data_cohort.rds"),
          compress="gz")
