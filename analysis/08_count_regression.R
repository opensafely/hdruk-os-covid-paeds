

library(tidyverse)
library(lubridate)
library(finalfit)
library(broom)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

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

# Create output directory folders ----
dir.create(here::here("output", "descriptives", "matched_cohort", "nb_model", "tables"),
           showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "matched_cohort", "nb_model", "plots"),
           showWarnings = FALSE, recursive=TRUE)

#Plot theme
theme_set(theme_bw())

# Command arguments to set resource type ----
args = commandArgs(trailingOnly=TRUE)

if(length(args) == 0){
  resource_type  = "gp"
  condition      = "all"
} else{
  resource_type  = args[[1]]
  condition      = args[[2]]
}

# Load weighted matched cohort  ----
data_weighted = read_rds(here::here("output", "data", "data_weighted.rds"))

# Load resource type ----
if(resource_type == "gp"){
  
  data_resource = read_rds(here::here("output", "data", "data_gp.rds"))
  
  if(condition == "all"){
    
    data_resource = data_resource %>%
      filter(str_starts(code_type, "KM_") |
               str_starts(code_type, "mapped_1") |
               str_starts(code_type, "mapped_2"))%>%
      left_join(data_weighted %>% 
                  select(patient_id, followup_start_date, followup_end_date_grouped),
                by = "patient_id") %>% 
      filter(gp_date >= followup_start_date,
             gp_date <= followup_end_date_grouped) %>% 
      distinct(patient_id, gp_date) %>%
      count(patient_id) %>% 
      rename(health_contact = n)
    
  } else{
    
    data_resource = data_resource %>% 
      filter(code_type == paste0("KM_", condition)) %>%
      left_join(data_weighted %>% 
                  select(patient_id, followup_start_date, followup_end_date_grouped),
                by = "patient_id") %>% 
      filter(gp_date >= followup_start_date,
             gp_date <= followup_end_date_grouped) %>% 
      distinct(patient_id, gp_date) %>%
      count(patient_id) %>% 
      rename(health_contact = n)
    
  }
  
} else if(resource_type == "outpatient"){
  
  data_resource = read_rds(here::here("output", "data", "data_outpatient.rds"))
  
  if(condition == "all"){
    
    data_resource = data_resource %>% 
      filter(is.na(specialty)) %>%
      left_join(data_weighted %>% 
                  select(patient_id, followup_start_date, followup_end_date_grouped),
                by = "patient_id") %>% 
      filter(outpatient_date >= followup_start_date,
             outpatient_date <= followup_end_date_grouped) %>%
      group_by(patient_id) %>% 
      summarise(health_contact = sum(outpatient_count)) %>% 
      ungroup()
    
  } else{
    
    data_resource = data_resource %>% 
      filter(specialty == paste0("TF_", condition)) %>%
      left_join(data_weighted %>% 
                  select(patient_id, followup_start_date, followup_end_date_grouped),
                by = "patient_id") %>% 
      filter(outpatient_date >= followup_start_date,
             outpatient_date <= followup_end_date_grouped) %>%
      group_by(patient_id) %>% 
      summarise(health_contact = sum(outpatient_count)) %>% 
      ungroup()
    
  }
  
} else if(resource_type == "admissions" | resource_type == "beddays"){
  
  data_resource = read_rds(here::here("output", "data", "data_admissions.rds"))
  
  if(condition != "all"){
    data_resource = data_resource %>% 
      mutate(primary_diagnosis.chapter = primary_diagnosis %>% 
               icd10_code_to_chapter() %>%
               str_to_lower() %>% 
               str_replace_all(":", "") %>% 
               str_replace_all(",", "") %>%
               str_replace_all("\\(", "") %>%
               str_replace_all("\\)", "") %>%
               str_replace_all("-", "_") %>%
               str_replace_all(" ", "_") %>% 
               factor()) %>% 
      filter(primary_diagnosis.chapter == condition)
  }
  
  if (resource_type == "admissions"){
    
    # Admission count
    
    data_resource = data_resource %>%
      left_join(data_weighted %>% 
                  select(patient_id, followup_start_date, followup_end_date_grouped),
                by = "patient_id") %>% 
      filter(admission_date >= followup_start_date,
             admission_date <= followup_end_date_grouped) %>% 
      count(patient_id) %>% 
      rename(health_contact = n)
      
    
  } else {
    
    # Bed-days
    
    data_resource = data_resource %>%
      left_join(data_weighted %>% 
                  select(patient_id, followup_start_date, followup_end_date_grouped),
                by = "patient_id") %>%
      filter(!is.na(followup_start_date), !is.na(followup_end_date_grouped)) %>%
      filter(admission_date <= followup_end_date_grouped,
             discharge_date >= followup_start_date) %>%
      mutate(
        admission_half_day = if_else(
          admission_date >= followup_start_date &
            admission_date <= followup_end_date_grouped, 0.5, 0),
        discharge_half_day = if_else(
          discharge_date >= followup_start_date &
            discharge_date <= followup_end_date_grouped, 0.5, 0),
        inbetween_day = (pmin(discharge_date, followup_end_date_grouped) -
                           pmax(admission_date, followup_start_date)) %>% as.numeric(),
        length_of_stay = inbetween_day + 1 - admission_half_day -
          discharge_half_day
      ) %>% 
      group_by(patient_id) %>% 
      summarise(
        health_contact = sum(length_of_stay)
      )
  }
  
} else {
  stop("Unrecognised resource_type")
}

# Add health contact data ---- 
data_weighted = data_weighted %>% 
  left_join(
    data_resource %>% 
      select(patient_id, health_contact),
    by = "patient_id"
  ) %>% 
  replace_na(list(health_contact = 0)) %>% 
  mutate(health_contact = round(health_contact))

# Negative binomial regression ----
## Predictors
predictors = c(
  # Covid status
  "covid_status_tp",
  
  # Demographics
  "age_group", "sex", "ethnicity", "imd_Q5_2019",
  "region_2019", "rural_urban_2019",
  
  # Comorbidities
  "comorbidity_count.factor",
  "mental_health_disorders", "neurodevelopmental_and_behavioural",
  "asthma", "cystic_fibrosis", "other_respiratory",
  "cardiovascular", "epilepsy", "headaches", "other_neurological",
  "gastrointestinal_conditions", "genitourinary", "cancer",
  "non_malignant_haematological", "immunological", "chronic_infections",
  "rheumatology", "congenital_malformation", "diabetes", "other_endocrine",
  "metabolic", "obesity", "transplant", "palliative_care",
  
  # Resource use and covid testing
  #"n_covid_tests",
  #"n_beddays", "n_outpatient", "n_gp"
)

## Model forumla ----
model_formula = paste0("health_contact ~ ",
                       paste(predictors, collapse = " + ")) %>%
  paste0(" + offset(log(person_time_grouped))") %>% 
  as.formula()


if(fit_type == "poisson"){
  
  ## Model healthcare contacts using Poisson regression
  model_fit = glm(model_formula,
                  weights = data_weighted$weights,
                  family = poisson,
                  data = data_weighted)
  
} else if(fit_type == "negative_binomial"){
  
  model_fit = MASS::glm.nb(model_formula,
                        weights = data_weighted$weights,
                        data = data_weighted,
                        maxit = 1000)
  
} else {
  stop("Unrecognised fit_type")
}


model_coeff = model_fit %>%
  tidy(conf.int = TRUE, exponentiate = TRUE)

model_stats = model_fit %>%
  glance()