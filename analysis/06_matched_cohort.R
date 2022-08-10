
library(tidyverse)
library(lubridate)
library(MatchIt)
library(finalfit)

# Load custom functions and lookup tables ----
source(here::here("analysis", "00_utility_functions.R"))
source(here::here("analysis", "00_lookup_tables.R"))

# Output directories ----
dir.create(here::here("output", "descriptives", "matched_cohort"), showWarnings = FALSE, recursive=TRUE)

# Plot theme ----
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
data_patient = read_rds(here::here("output", "data", "data_patient.rds"))
data_testing = read_rds(here::here("output", "data", "data_testing.rds"))

# Define groups ----
data_inclusion = data_patient %>% 
  transmute(
    patient_id,
    covid_status = case_when(
      is.na(covid_test_date_neg_tp) & !is.na(covid_test_date_pos_tp) ~ "Positive",
      is.na(covid_test_date_pos_tp) & !is.na(covid_test_date_neg_tp) ~ "Negative",
      covid_test_date_pos_tp < covid_test_date_neg_tp ~ "Positive",
      covid_test_date_neg_tp < covid_test_date_pos_tp ~ "Negative (will test Positive)",
      is.na(covid_test_date_neg_tp) & is.na(covid_test_date_pos_tp) ~ "Untested",
      TRUE ~ "Error"
    ),
    not_nosocomial = covid_nosocomial == "No",
    no_discrepant_results = covid_discrepant_test == "No"
  )

# Create duplicate rows for patients testing positive after negative ----
data_inclusion = data_inclusion %>% 
  bind_rows(data_inclusion %>% 
              filter(covid_status == "Negative (will test Positive)") %>%
              mutate(covid_status = "Positive (prior Negative)")
              ) %>%
  mutate(
    result = case_when(
      covid_status == "Positive" ~ "Positive",
      covid_status == "Negative" ~ "Negative",
      covid_status == "Negative (will test Positive)" ~ "Negative",
      covid_status == "Positive (prior Negative)" ~ "Positive",
      covid_status == "Untested" ~ "Untested",
      TRUE ~ "Error"
    ))

# Only consider test data within testing period ----
data_testing_tp = data_testing %>% 
  filter(test_date >= tp_start_date,
         test_date < tp_end_date)

# Filter out nosocomial covid and descrepant covid test results ----
data_patient = data_patient %>% 
  filter(covid_nosocomial == "No", covid_discrepant_test == "No")

data_testing_tp = data_testing_tp %>%
  filter(patient_id %in% data_patient$patient_id) 

## Include only 1st positive or negatives before 1st positive ----
data_testing_tp = data_testing_tp %>%
  left_join(
    data_testing_tp %>%
      group_by(patient_id, result) %>% 
      filter(result == "Positive" & row_number() == 1) %>%
      ungroup() %>% 
      select(patient_id, test_date_1st_pos_tp = test_date) ,
    by = "patient_id"
  ) %>% 
  filter((result == "Positive" & test_date == test_date_1st_pos_tp) | 
           (result == "Negative" & is.na(test_date_1st_pos_tp)) |
           (result == "Negative" & (test_date < test_date_1st_pos_tp))
         )

## Append covid_status to dataset ----
data_testing_tp = data_testing_tp %>% 
  left_join(
    data_inclusion %>% 
      select(patient_id, covid_status, result),
    by = c("patient_id", "result")
  )

## Calculate age on test date ----
data_testing_tp = data_testing_tp %>% 
  left_join(
    data_patient %>%
      select(patient_id, date_of_birth),
    by = "patient_id"
  ) %>% 
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
              select(patient_id, age_criteria_test_date, covid_status),
            by = c("patient_id", "covid_status"))

## Make results factor, remove columns ----
data_testing_tp = data_testing_tp %>% 
  mutate(result = result %>%
           factor() %>% 
           fct_relevel("Negative", "Positive")) %>% 
  select(patient_id, result, covid_status, test_date)

# Match positives with negatvies ----
## Matching set up ----
match_ratio = 10

## Perform matching ----
match_pos_neg = matchit(result ~ test_date,
                        data = data_testing_tp,
                        exact = ~ test_date,
                        ratio = match_ratio,
                        replace = FALSE) %>% 
  match.data() %>% 
  rename(match_id = subclass)

## Filter out incomplete sets ----
match_pos_neg = match_pos_neg %>%
  arrange(match_id, desc(result)) %>% 
  group_by(match_id) %>% 
  mutate(n_match = n()) %>% 
  ungroup() %>% 
  filter(n_match == match_ratio + 1)

# Match positive cases with untested ----
## Create dataset of untested ----
data_untested = data_inclusion %>% 
  filter(result == "Untested") %>% 
  select(patient_id, result, covid_status) %>%
  left_join(
    data_patient %>% 
      select(patient_id, date_of_birth, death_date),
    by = "patient_id"
  ) %>% 
  mutate(test_date = sample(
    match_pos_neg %>% 
      filter(result == "Positive") %>%
      pull(test_date),
    size = n(),
    replace = TRUE))

## Filter out dead patients on matched test date ----
data_untested = data_untested %>% 
  filter(test_date < death_date | is.na(death_date))

### Log number of patients alive on matched test date ----
data_inclusion = data_inclusion %>% 
  left_join(data_untested %>% 
              mutate(alive_matched_date = TRUE) %>% 
              select(patient_id, alive_matched_date),
            by = "patient_id")

## Filter out patients not aged 4-17 ----
### Calculate age on matched test date ----
data_untested = data_untested %>% 
  mutate(
    age_test_date = time_length(difftime(test_date, date_of_birth), "years")
) %>% 
  filter(age_test_date >= 4, age_test_date < 18)

### Log number of patients satisfying age criteria ----
data_inclusion = data_inclusion %>% 
  left_join(data_untested %>% 
              mutate(age_criteria_matched_date = TRUE) %>% 
              select(patient_id, age_criteria_matched_date),
            by = "patient_id")

## Bind dataset of untested with positive ----
data_untested = data_untested %>% 
  bind_rows(
    match_pos_neg %>% 
      filter(result == "Positive") %>% 
      select(patient_id, result, test_date, match_id)
  ) %>% 
  mutate(
    result = result %>% 
      factor() %>% 
      fct_relevel("Untested", "Positive")
  ) 

## Perform matching, fill in match_id ----
match_pos_untested = matchit(result ~ test_date,
                             data = data_untested,
                             exact = ~ test_date,
                             ratio = match_ratio,
                             replace = FALSE) %>% 
  match.data() %>% 
  group_by(subclass) %>% 
  fill(match_id, .direction = "downup") %>% 
  ungroup()

# Combine the two matches (pos-neg, pos-untested) ----
matched_cohort = match_pos_neg %>% 
  bind_rows(match_pos_untested %>%
              filter(!result == "Positive")) %>% 
  select(match_id, patient_id, result, covid_status, test_date) %>% 
  arrange(match_id, result) %>% 
  group_by(match_id) %>% 
  mutate(n_matches = n()) %>% 
  ungroup()

# Filter out incomplete match sets ----
matched_cohort = matched_cohort %>% 
  filter(n_matches == (match_ratio*2 +1))

## Log sucessful matches ----
data_inclusion = data_inclusion %>%
  left_join(
    matched_cohort %>%
      transmute(
        patient_id, covid_status, matched = TRUE
      ) %>% 
      group_by(patient_id, covid_status) %>% 
      slice(1) %>% 
      ungroup(),
    by = c("patient_id", "covid_status")
  )
  
# Clean-up inclusion data ----
data_inclusion = data_inclusion %>% 
  mutate(
    meets_age_criteria = case_when(
      age_criteria_test_date == TRUE ~ TRUE,
      age_criteria_matched_date == TRUE ~ TRUE, 
      TRUE ~ FALSE
    ),
    alive_matched_date = case_when(
      covid_status == "Untested" & is.na(alive_matched_date) ~ FALSE,
      TRUE ~ TRUE
    ),
    matched = if_else(matched == TRUE, TRUE, FALSE)
  )

flowchart = data_inclusion %>% 
  transmute(
    patient_id,
    covid_status,
    c0 = TRUE,
    c1 = c0 & not_nosocomial,
    c2 = c1 & no_discrepant_results,
    c3 = c2 & alive_matched_date,
    c4 = c3 & meets_age_criteria,
    c5 = c4 & matched
  ) %>%
  select(-patient_id) %>%
  group_by(covid_status) %>%
  summarise(
    across(.fns=sum)
  ) %>%
  pivot_longer(
    cols=-covid_status,
    names_to="criteria",
    values_to="n"
  ) %>%
  group_by(covid_status) %>%
  mutate(
    n = n #%>% plyr::round_any(count_round)
  ) %>% 
  mutate(
    n_exclude = lag(n) - n,
    pct_all = (n/first(n)) %>% scales::percent(0.1),
    pct_exclude_step = (n_exclude/lag(n)) %>% scales::percent(0.1),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "OpenSAFELY extract: Registered with GP, alive, with age >1 and <18 years on 01 January 2019",
      crit == "c1" ~ "-  with no probable nosocomial infection",
      crit == "c2" ~ "-  with no same-day discrepent RT-PCR test result",
      crit == "c3" ~ "-  is alive on test/matched date",
      crit == "c4" ~ "-  with age between 4 and 17 years inclusive on test/matched date",
      crit == "c5" ~ "-  sucessfully matched with negative:untested:positive of 10:10:1",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(n_exclude = n_exclude %>% as.character()) %>% 
  replace_na(
    list(n_exclude = "-", pct_exclude_step = "-")
  )

tbl_flowchart = flowchart %>% 
  select(covid_status, criteria, n, n_exclude, pct_all, pct_exclude_step) %>%
  group_by(covid_status) %>% 
  mutate(covid_status = if_else(row_number() == 1, covid_status, ""))

write_csv(tbl_flowchart, 
          here::here("output", "descriptives", "matched_cohort",
                     "tbl_flowchart.csv"))
    
