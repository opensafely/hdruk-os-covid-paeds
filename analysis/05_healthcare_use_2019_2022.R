

# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)
library(scales)
library(zoo)
library(knitr)

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

## Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Load datasets
data_patient = read_rds(here::here("output", "data", "data_patient.rds"))
data_testing = read_rds(here::here("output", "data", "data_testing.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))
data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds"))
data_gp = read_rds(here::here("output", "data", "data_gp.rds"))

# Cohort construction ----
index_date = c("2019-01-01",
               "2020-01-01",
               "2021-01-01",
               "2022-01-01") %>% 
  ymd() %>% 
  as.list()

data_criteria = map(.x = index_date,
                    .data_patient = data_patient,
                    .f = function(.index_date, .data_patient){
                      data_criteria = .data_patient %>% 
                        transmute(
                          patient_id,
                          index_date = .index_date,
                          cohort = year(index_date),
                          age_indexed = time_length(
                            interval(date_of_birth, .index_date),
                            unit = "years"
                          ) %>% 
                            ff_label("Age on 1st January (years)"),
                          age_group_indexed = age_indexed %>%
                            cut(
                              breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
                              labels = c("Under 4", "4-6", "7-10", "11-14", "15-17", "18+")
                            )%>%
                            ff_label("Age group on 1st January (years)"),
                          is_alive = is.na(death_date) | death_date > index_date, 
                          isnot_nosocomial = covid_nosocomial == "No",
                          isnot_descrepant_result = covid_discrepant_test == "No",
                          is_age_4_or_more = age_indexed >= 4,
                          is_age_under_18 = age_indexed < 18,
                          include = is_alive & 
                            isnot_nosocomial & 
                            isnot_descrepant_result &
                            is_age_4_or_more & is_age_under_18
                        )
                    }
)

data_cohort = data_criteria %>% 
  map(
    function(.data_criteria){
      data_cohort = .data_criteria %>%
        filter(include) %>%
        select(patient_id, cohort, index_date, age_indexed, age_group_indexed) %>% 
        left_join(
          data_patient, by = "patient_id"
        )
    }
  )


# Inclusion flowchart ----
flowchart = data_criteria %>% 
  map(
    function(.data_criteria){
      ## Create inclusion flowchart ----
      flowchart = .data_criteria %>%
        transmute(
          patient_id,
          cohort,
          c0 = TRUE,
          c1 = c0 & isnot_nosocomial,
          c2 = c1 & isnot_descrepant_result,
          c3 = c2 & is_alive,
          c4 = c3 & (is_age_4_or_more & is_age_under_18),
        ) %>%
        select(-patient_id) %>%
        group_by(cohort) %>%
        summarise(
          across(.fns=sum)
        ) %>%
        pivot_longer(
          cols=-cohort,
          names_to="criteria",
          values_to="n"
        ) %>%
        group_by(cohort) %>%
        mutate(
          n = n %>% plyr::round_any(count_round)
        ) %>% 
        mutate(
          n_exclude = lag(n) - n,
          pct_all = (n/first(n)) %>% percent(0.1),
          pct_exclude_step = (n_exclude/lag(n)) %>% percent(0.1),
          crit = str_extract(criteria, "^c\\d+"),
          criteria = fct_case_when(
            crit == "c0" ~ "OpenSAFELY extract: Registered with GP, alive, with age >0 and <18 years on 01 January 2019",
            crit == "c1" ~ "-  with no probable nosocomial infection",
            crit == "c2" ~ "-  with no same-day discrepent RT-PCR test result",
            crit == "c3" ~ "-  is alive on 1st January",
            crit == "c4" ~ "-  with age between 4 and 17 years inclusive",
            TRUE ~ NA_character_
          )
        ) %>%
        mutate(n_exclude = n_exclude %>% as.character()) %>% 
        replace_na(
          list(n_exclude = "-", pct_exclude_step = "-")
        )
    }
  ) %>% 
  bind_rows()

tbl_flowchart = flowchart %>% 
  select(cohort, criteria, n, n_exclude, pct_all, pct_exclude_step) %>%
  group_by(cohort) %>% 
  mutate(cohort = if_else(row_number() == 1, cohort %>% as.character(), ""))

write_csv(tbl_flowchart, 
          here::here("output", "descriptives", "healthcare_use_2019_2022",
                     "tbl_flowchart.csv"))


# Cohort summary ----

explanatory_var = c(
  "age_indexed",
  "age_group_indexed",
  "sex",
  "ethnicity",
  "imd_Q5_2019",
  "region_2019",
  "rural_urban_2019",
  
  "covid_status_tp",
  "covid_status_fup",
  "covid_test_neg_tp_count",
  "covid_test_pos_tp_count",
  "covid_test_neg_fup_count",
  "covid_test_pos_fup_count",
  
  "asthma",
  "cancer",
  "diabetes",
  "epilepsy",
  "severe_mental_illness",
  
  "cerebral_palsy",
  "chronic_infections",
  "devices_and_stomas",
  "endocrine_disorders",
  "gastrointestinal_disorders",
  "haematological_disorders",
  "immunological_disorders", 
  "learning_and_behaviour_difficulties", 
  "mental_illness",
  "musculoskeletal_and_rheum",
  "transplant"
)

tbl_cohort_summary = data_cohort %>% 
  map(
    .explanatory_var = explanatory_var,
    .f = function(.data_cohort, .explanatory_var){
      .data_cohort %>%
        mutate(cohort = cohort %>% factor()) %>% 
        summary_factorlist(
          dependent = "cohort",
          explanatory = .explanatory_var,
          total_col = FALSE,
          add_col_totals = TRUE,
          na_include = TRUE
          
        ) %>% 
        mutate(row_num = row_number())
    }) %>% 
  reduce(left_join, by = c("row_num", "label", "levels")) %>% 
  select(-row_num) %>% 
  ff_round_counts(count_round) 

write_csv(tbl_cohort_summary, 
          here::here("output", "descriptives", "healthcare_use_2019_2022",
                     "tbl_cohort_summary.csv"))


# Calculate resource use ----
time_seq = seq(study_start_date, study_end_date - months(1), by = "month")
group_variable_list = c("overall", "asthma")

## Bed-days ----
### Create bed-days dataset ----
admission_counts = data_admissions %>%
  rowwise() %>% 
  mutate(dates = list(seq(admission_date, discharge_date, by = "day"))) %>% 
  unnest(dates) %>% 
  mutate(date_period = floor_date(dates, unit = "month")) %>%
  group_by(patient_id, index) %>% 
  mutate(
    daily_count = case_when(
      (admission_date == discharge_date) ~ 0.5, # Day-case
      row_number() == 1 ~ 0, 
      TRUE ~ 1
    )
  ) %>% 
  group_by(patient_id, date_period) %>% 
  summarise(
    bed_days = sum(daily_count)
  ) %>% 
  ungroup()
           
### Calculate bed-day incidence rate ----
bedday_rate = time_seq %>% 
  map(
    function(index_date, .data_patient, .data_bed_days,
             group_variable = "overall"){
      
      .data_patient = .data_patient %>% 
        calc_indexed_variables(index_date) %>% 
        apply_exclusion_criteria() %>% 
        mutate(
          overall = "Overall"
        )
      
      map(
        group_variable,
        function(group_variable, index_date,
                 .data_patient, .data_bed_days){
          mean_bed_days = .data_patient %>% 
            select(patient_id, death_date, group = all_of(group_variable)) %>%
            mutate(
              date_period = index_date,
              patient_years = (pmin(death_date, index_date + months(1), na.rm = TRUE) - 
                                 pmin(death_date, index_date, na.rm = TRUE)) %>%
                as.numeric() / 365.25
            ) %>% 
            left_join(
              admission_counts,
              by = c("patient_id", "date_period")
            ) %>% 
            replace_na(list(bed_days = 0)) %>% 
            group_by(group) %>%
            summarise(
              index_date,
              group_variable,
              n_patient = n(),
              bed_days_total = round(sum(bed_days)),
              patient_years_total = sum(patient_years),
              bedday_rate = bed_days_total/patient_years_total,
              bedday_LL = poisson.test(bed_days_total, patient_years_total)$conf.int[1],
              bedday_UL = poisson.test(bed_days_total, patient_years_total)$conf.int[2]
            )
        },
        .data_patient = .data_patient,
        .data_bed_days = .data_bed_days,
        index_date = index_date
      ) %>% 
        bind_rows()
    },
    .data_patient = data_patient,
    .data_bed_days = data_bed_days,
    group_variable = group_variable_list
  ) %>% 
  bind_rows()

### Format table ----
bedday_rate %>% 
  relocate(group_variable, index_date, group) %>% 
  mutate(group_variable = group_variable %>%
           factor(levels = group_variable_list)) %>% 
  arrange(group_variable, index_date)

