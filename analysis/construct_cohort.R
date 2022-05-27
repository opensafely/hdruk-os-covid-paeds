
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)
library(MatchIt)

# Load custom functions ----
source(here::here("analysis", "00_functions.R"))

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

## Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Create directory for outputs ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "cohort_flowchart"), showWarnings = FALSE, recursive=TRUE)

# Load datasets ----
data_patient = read_rds(here::here("output", "data", "data_patient.rds"))
data_testing = read_rds(here::here("output", "data", "data_testing.rds"))


# Objective 1: Yearly cohorts ----

ymd(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01")) %>% 
  map(function(index_date){
    
    ## Criteria for cohort inclusion ----
    data_criteria = data_patient %>% 
      transmute(
        patient_id = patient_id,
        index_date = index_date,
        covid_status = covid_status,
        is_alive = is.na(death_date) | death_date > index_date, 
        isnot_nosocomial = covid_nosocomial == "No",
        isnot_descrepant_result = covid_discrepant_test == "No",
        is_age_4_or_more = (index_date - date_of_birth)/365.25 >= 4,
        is_age_under_18 = (index_date - date_of_birth)/365.25 < 18,
        include = is_alive & 
          isnot_nosocomial & 
          isnot_descrepant_result &
          is_age_4_or_more & is_age_under_18
      )
    
    ## Cohort dataset ----
    data_cohort = data_criteria %>%
      filter(include) %>%
      select(patient_id, index_date) %>%
      left_join(data_patient, by="patient_id") %>%
      droplevels()
    
    ## Create inclusion flowchart ----
    flowchart = data_criteria %>%
      transmute(
        patient_id,
        covid_status,
        c0 = TRUE,
        c1 = c0 & is_alive,
        c2 = c1 & isnot_nosocomial,
        c3 = c2 & isnot_descrepant_result,
        c4 = c3 & (is_age_4_or_more & is_age_under_18),
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
        n_exclude = lag(n) - n,
        pct_exclude = n_exclude/lag(n),
        pct_all = n / first(n),
        pct_step = n / lag(n),
        crit = str_extract(criteria, "^c\\d+"),
        criteria = fct_case_when(
          crit == "c0" ~ "OpenSAFELY extract: Registered with GP between 1st January 2019 and 1st May 2022, and alive with age between 1 and 18 years on 01 January 2019",
          crit == "c1" ~ "  is alive on 1st January 2019",
          crit == "c2" ~ "  with no probable nosocomial infection",
          crit == "c3" ~ "  with no same-day discrepent RT-PCR test result",
          crit == "c4" ~ "  with age between 4 and 17 years",
          TRUE ~ NA_character_
        )
      ) 
    
    # Save data as rds ----
    write_rds(data_criteria,
              here::here("output", "data", paste0("data_criteria_", year(index_date), ".rds")),
              compress="gz")
    
    write_rds(data_cohort,
              here::here("output", "data", paste0("data_cohort_", year(index_date), ".rds")),
              compress="gz")
    
    # Save flowchart as csv ----
    write_csv(flowchart,
              here::here("output", "descriptives", "cohort_flowchart", paste0("cohort_flowchart_", year(index_date), ".csv")))
    
    return(NULL)
  }) %>% 
  invisible()


# Objective 2: Matched cohort  ----

## Define matching parameters ----
match_ratio = 1

## Create pool of patients eligible for matching ----
## Remove those with potential nosocomial and discrepant test results
## Filter out test dates where patient is aged <4 or 18+
pool_pos_neg = data_testing %>%
  left_join(
    data_patient %>% 
      select(patient_id, covid_nosocomial, covid_discrepant_test, date_of_birth),
    by = "patient_id"
  ) %>%
  mutate(
    age_on_test_date = ((test_date - date_of_birth) / 365.25) %>% 
      as.numeric(),
    result = result %>% factor()
  ) %>% 
  filter(test_date >= tp_start_date, test_date <= tp_end_date) %>%
  filter(covid_nosocomial == "No",
         covid_discrepant_test == "No") %>% 
  filter(age_on_test_date > 4, age_on_test_date < 18) %>% 
  select(-c(date_of_birth, age_on_test_date, covid_nosocomial, covid_discrepant_test))

## Only consider first positive or all negative  ----
pool_pos_neg = pool_pos_neg %>%
  arrange(patient_id, test_date) %>% 
  group_by(patient_id, result) %>% 
  filter((result == "positive" & row_number() == 1) |
           result == "negative")

patient_id_positive = pool_pos_neg %>% 
  filter(result == "positive") %>% 
  pull(patient_id)

pool_pos_neg = pool_pos_neg %>%
  filter(
    result == "positive" |
      (result == "negative" & 
         !patient_id %in% patient_id_positive)
  )

## Perform matching ----
matched = vector()
match_out = pool_pos_neg %>% 
  group_by(test_date) %>% 
  group_map(
    function(.x, .y){
      
      # Remove matched controls from last iteration
      df_in = .x %>%
        filter(!patient_id %in% matched)
      
      if(df_in$result %>% unique() %>% length() < 2){
        df_out = NULL
      } else{
        # Match
        df_out = matchit(result ~ test_date,
                         data = df_in,
                         exact = ~ test_date,
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
  map_df(bind_rows)

## Remove subclasses without full match count ----
match_out = match_out %>%
  group_by(subclass) %>% 
  mutate(n_subclass = n()) %>% 
  ungroup() %>% 
  filter(n_subclass == (match_ratio + 1)) %>% 
  select(patient_id, result, test_date, subclass)


pool_pos_untested = match_out %>% 
  filter(result == "positive") %>% 
  bind_rows(
    data_patient %>% 
      filter(covid_status == "Untested") %>% 
      select(patient_id, result = covid_status, death_date)
  ) %>%
  droplevels() %>% 
  mutate(result = result %>% fct_relevel("Untested"))
  
matched_2 = vector()
match_out = pool_pos_untested %>% 
  group_by(test_date) %>% 
  group_map(
    function(.x, .y){
      
      # Remove matched controls from last iteration
      df_in = .x %>%
        filter(!patient_id %in% matched_2)
      
      if(df_in$result %>% unique() %>% length() < 2){
        df_out = NULL
      } else{
        # Match
        df_out = matchit(result ~ test_date,
                         data = df_in,
                         exact = ~ test_date,
                         ratio = match_ratio,
                         replace = FALSE)
        
        # Unique ID for subclasses (date pasted with subclass)
        df_out = match.data(df_out) %>% 
          mutate(subclass = paste0(.y, subclass))
        
        # Update remove vector
        matched_iter = df_out %>%
          pull(patient_id)
        matched_2 <<- c(matched_2, matched_iter)
      }
      return(df_out)
    },
    .keep = TRUE
  ) %>% 
  map_df(bind_rows)
  
  
  
  data_patient %>% 
  select(patient_id, covid_status, death_date) %>% 
  filter(covid_status == "Untested" | 
           (covid_status == "Positive" & patient_id %in% match_out$patient_id))



## Add matched index to data_patient 
data_patient = data_patient %>% 
  left_join(
    match_out %>% 
      select(patient_id, 
             matched_subclass = subclass,
             matched_test_date = test_date),
    by = "patient_id"
  )

## Matched cohort inclusion criteria
data_matched_criteria = data_patient %>% 
  transmute(
    patient_id = patient_id,
    covid_status = covid_status,
    isnot_nosocomial = covid_nosocomial == "No",
    isnot_descrepant_result = covid_discrepant_test == "No",
    is_matched = !is.na(matched_subclass),
    include = isnot_nosocomial & 
      isnot_descrepant_result &
      is_matched
  )

## Matched cohort dataset ----
data_matched_cohort = data_matched_criteria %>%
  filter(include) %>%
  select(patient_id) %>%
  left_join(data_patient, by="patient_id") %>%
  droplevels()

## Create inclusion flowchart ----
flowchart_matched = data_matched_criteria %>%
  transmute(
    patient_id,
    covid_status,
    c0 = TRUE,
    c1 = c0 & isnot_nosocomial,
    c2 = c1 & isnot_descrepant_result,
    c3 = c2 & is_matched,
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
    n_exclude = lag(n) - n,
    pct_exclude = n_exclude/lag(n),
    pct_all = n / first(n),
    pct_step = n / lag(n),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "OpenSAFELY extract: Registered with GP between 1st January 2019 and 1st May 2022, and alive with age between 1 and 18 years on 01 January 2019",
      crit == "c1" ~ "  with no probable nosocomial infection",
      crit == "c2" ~ "  with no same-day discrepent RT-PCR test result",
      crit == "c3" ~ "  matched on test-date",
      TRUE ~ NA_character_
    )
  ) 

