
library(tidyverse)
library(lubridate)
library(MatchIt)
library(finalfit)

# Load custom functions and lookup tables ----
source(here::here("analysis", "00_utility_functions.R"))
source(here::here("analysis", "00_lookup_tables.R"))

# Output directories ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)
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

# Matching parameters ----
match_ratio = 10
test_period_span = "1 week"

# Load datasets ----
data_patient = read_rds(here::here("output", "data", "data_patient.rds"))
data_testing = read_rds(here::here("output", "data", "data_testing.rds"))

# Define groups ----
data_inclusion = data_patient %>% 
  transmute(
    patient_id,
    covid_status_tp,
    not_nosocomial = covid_nosocomial == "No",
    no_discrepant_results = covid_discrepant_test == "No"
  )

# Only consider test data within testing period ----
data_testing_tp = data_testing %>% 
  filter(test_date >= tp_start_date,
         test_date < tp_end_date)

# Filter out nosocomial covid and descrepant covid test results ----
data_patient = data_patient %>% 
  filter(covid_nosocomial == "No", covid_discrepant_test == "No")

data_testing_tp = data_testing_tp %>%
  filter(patient_id %in% data_patient$patient_id) %>% 
  left_join(data_patient %>% 
              select(patient_id, covid_status_tp, date_of_birth),
            by = "patient_id") %>% 
  drop_na()

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

## Make results factor, remove columns ----
data_testing_tp = data_testing_tp %>% 
  mutate(result = result %>%
           factor() %>% 
           fct_relevel("Negative", "Positive"),
         test_period = floor_date(test_date, test_period_span)) %>% 
  select(patient_id, result, covid_status_tp, test_date, test_period)

# Match positives with negatives ----
## Matching set up ----
matched = vector()

match_pos_neg = data_testing_tp %>% 
  group_by(test_period) %>% 
  group_map(
    function(.x, .y){
      
      # Remove matched controls from last iteration
      df_in = .x %>%
        filter(!patient_id %in% matched)
      
      # Count number of tests ----
      n_positive = sum(df_in$result == "Positive")
      n_negative = sum(df_in$result == "Negative")
      
      # If insufficient numbers, skip matching.
      if(n_positive == 0 | n_negative < match_ratio){
        df_out = NULL
      } else{
        
        # Reduce positives if insufficient negative potential matches ----
        if(n_negative < n_positive*match_ratio){
          df_in = df_in %>%
            group_by(result) %>% 
            filter(!(result == "Positive" & 
                       (row_number() > floor(n_negative/match_ratio)))) %>% 
            ungroup()
        }
        
        # Match
        df_out = matchit(result ~ test_period,
                         data = df_in,
                         exact = ~ test_period,
                         ratio = match_ratio,
                         replace = FALSE)
        
        # Unique ID for subclasses (date pasted with subclass)
        df_out = match.data(df_out) %>% 
          mutate(subclass = paste0(.y, subclass))
        
        # Update remove vector
        matched_iter = df_out %>%
          pull(patient_id)
        matched <<- c(matched, matched_iter)
      }
      return(df_out)
    },
    .keep = TRUE
  ) %>% 
  map_df(bind_rows) %>% 
  rename(match_id = subclass)

# Match positive cases with untested ----
## Create dataset of untested ----
data_untested = data_patient %>% 
  filter(covid_status_tp == "Untested") %>% 
  select(patient_id, covid_status_tp, date_of_birth, death_date) %>% 
  mutate(
    test_date = sample(
      match_pos_neg %>% 
        filter(result == "Positive") %>%
        pull(test_date),
      size = n(),
      replace = TRUE),
    test_period = floor_date(test_date, test_period_span)
    )

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

## Bind dataset of untested with positive ----
data_untested = data_untested %>% 
  bind_rows(
    match_pos_neg %>% 
      filter(result == "Positive") %>% 
      select(patient_id, result, covid_status_tp, test_date, test_period, match_id)
  ) %>% 
  mutate(
    result = result %>% 
      factor() %>% 
      fct_relevel("Untested", "Positive")
  ) 


data_untested = data_untested %>% 
  left_join(
    data_untested %>% 
      count(result, test_period) %>% 
      pivot_wider(names_from = "result",
                  values_from = "n",
                  names_prefix = "n_"),
    by = "test_period"
  ) %>%
  group_by(result, test_period) %>% 
  filter(
    ((result == "Positive") & (row_number() <= n_Untested/match_ratio)) |
      ((result == "Untested") & (row_number() <= n_Positive*match_ratio))
  )

  
## Perform matching, fill in match_id ----
match_pos_untested = matchit(result ~ test_period,
                             data = data_untested,
                             exact = ~ test_period,
                             ratio = match_ratio,
                             replace = FALSE) %>% 
  match.data() %>% 
  group_by(subclass) %>% 
  fill(match_id, .direction = "downup") %>% 
  ungroup()


# Combine the two matches (pos-neg, pos-untested) ----
data_matched = match_pos_neg %>% 
  bind_rows(match_pos_untested %>%
              filter(!result == "Positive")) %>% 
  select(match_id, patient_id, result, covid_status_tp, test_date, test_period) %>% 
  arrange(match_id, result) %>% 
  group_by(match_id) %>% 
  mutate(n_matches = n()) %>% 
  ungroup()

# Filter out incomplete match sets ----
data_matched = data_matched %>% 
  filter(n_matches == (match_ratio*2 +1)) %>% 
  select(-n_matches)

# Save data as rds ----
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
    alive_matched_date = case_when(
      covid_status_tp == "Untested" & is.na(alive_matched_date) ~ FALSE,
      TRUE ~ TRUE
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
    c1 = c0 & not_nosocomial,
    c2 = c1 & no_discrepant_results,
    c3 = c2 & alive_matched_date,
    c4 = c3 & meets_age_criteria,
    c5 = c4 & matched
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
    n = n #%>% plyr::round_any(count_round)
  ) %>% 
  mutate(
    n_exclude = lag(n) - n,
    pct_all = (n/first(n)) %>% scales::percent(0.1),
    pct_exclude_step = (n_exclude/lag(n)) %>% scales::percent(0.1),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "OpenSAFELY extract: Registered with GP, alive, with age >0 and <18 years on 01 January 2019",
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
    
