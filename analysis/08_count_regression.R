


library(broom)
library(broom.helpers)
library(tidyverse)
library(lubridate)
library(survey)
library(finalfit)

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

#Plot theme
theme_set(theme_bw())

# Command arguments to set resource type ----
args = commandArgs(trailingOnly=TRUE)

if(length(args) == 0){
  resource_type  = "beddays"
  condition      = "all"
  model_type     = "poisson"
  pred_type      = "uni_var"
} else{
  resource_type  = args[[1]]
  condition      = args[[2]]
  model_type     = args[[3]]
  pred_type      = args[[4]]
}

# Create output directory folders ----
dir.create(here::here("output", "descriptives", "matched_cohort", model_type, pred_type, "tables"),
           showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "matched_cohort", model_type, pred_type, "plots"),
           showWarnings = FALSE, recursive=TRUE)

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
        length_of_stay = case_when(
          admission_date == discharge_date ~ 0.5, # Day-case
          TRUE ~ (pmin(discharge_date, followup_end_date_grouped) -
                    pmax(admission_date, followup_start_date)) %>% as.numeric()
        )
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

# Summarise count and person-time data ----
data_crude_rates = data_weighted %>% 
  group_by(covid_status_tp) %>% 
  summarise(
    # Sample size
    n = if_else(n() <= 7,
                NA_real_,
                n() %>% plyr::round_any(count_round)),
    # Event count 
    health_contact = case_when(
      is.na(n) ~ NA_real_,
      sum(health_contact) <= 7 ~ NA_real_,
      TRUE ~ sum(health_contact)
    ),
    # Person-time
    person_years_000 = if_else(is.na(n), NA_real_, sum(person_time_grouped)/365.25/1000),
    # Crude rate (events per person-year)
    estimate = ifelse(is.na(health_contact) | is.na(person_years_000), NA_real_,
                      poisson.test(health_contact, person_years_000)$estimate),
    ci_lower = ifelse(is.na(health_contact) | is.na(person_years_000), NA_real_,
                      poisson.test(health_contact, person_years_000)$conf.int[1]),
    ci_upper = ifelse(is.na(health_contact) | is.na(person_years_000), NA_real_,
                      poisson.test(health_contact, person_years_000)$conf.int[2])
  )

# Redact low counts ----
data_crude_rates = data_crude_rates %>% 
  mutate(
    n = n %>% as.character(),
    health_contact = health_contact %>% as.character(),
  ) %>% 
  replace_na(list(n = "[REDACTED]", health_contact = "[REDACTED]"))

# Save crude rates ----
write_csv(data_crude_rates,
          here::here("output", "descriptives", "matched_cohort", model_type, pred_type, "tables",
                     paste0("crude_rates_", resource_type, "_", condition, ".csv")))

# Predictors ----
if (pred_type == "uni_var"){
  
  predictors = "covid_status_tp"
  
} else if (pred_type == "multi_var"){
  
  predictors = c(
    # Covid status
    "covid_status_tp",
    
    # Demographics
    "age_group", "sex", "ethnicity", "imd_Q5_2019",
    "region_2019", "rural_urban_2019",
    
    # Comorbidities
    "comorbidity_count_factor",
    "mental_health_disorders", "neurodevelopmental_and_behavioural",
    "asthma", "cystic_fibrosis", "other_respiratory",
    "cardiovascular", "epilepsy", "headaches", "other_neurological",
    "gastrointestinal_conditions", "genitourinary", "cancer",
    "non_malignant_haematological", "immunological", "chronic_infections",
    "rheumatology", "congenital_malformation", "diabetes", "other_endocrine",
    "metabolic", "obesity", "transplant", "palliative_care",
    
    # Vaccination status
    "vaccination_status",
    
    # Resource use and covid testing
    "n_covid_tests_Q",
    "n_beddays_Q", "n_outpatient_Q", "n_gp_Q"
  )
  
} else {
  stop("Unrecognised pred_type")
}



## Model forumla ----
model_formula = paste0("health_contact ~ ",
                       paste(predictors, collapse = " + ")) %>%
  paste0(" + offset(log(person_time_grouped))") %>% 
  as.formula()



if(model_type == "poisson"){
  
  # Survey deisgn ----
  d.w = svydesign(~1,
                  weights = data_weighted$weights,
                  data = data_weighted)
  
  # Fit poisson model ----
  model_fit = svyglm(model_formula,
                   family = poisson,
                   design = d.w)
  
} else if(model_type == "negative_binomial"){
  
  # Survey deisgn ----
  d.w = svydesign(~1,
                  weights = data_weighted$weights,
                  data = data_weighted)
  
  model_fit = sjstats::svyglm.nb(model_formula,
                                 design = d.w)
  
} else {
  stop("Unrecognised fit_type")
}


# Model coefficients ----
model_coeff = model_fit %>%
  tidy_and_attach(exponentiate = TRUE, conf.int = TRUE) %>%
  tidy_add_reference_rows() %>%
  tidy_add_estimate_to_reference_rows() %>% 
  tidy_add_term_labels()

## Save model coefficients ----
write_csv(model_coeff,
          here::here("output", "descriptives", "matched_cohort", model_type, pred_type, "tables",
                     paste0("coeff_", resource_type, "_", condition, ".csv")))

## Plot incidence rate ratios ----
plot_rr = model_coeff %>%
  tidy_remove_intercept() %>%
  filter(reference_row == FALSE) %>% 
  mutate(plot_label = paste0(var_label, ": ", label) %>% 
           factor() %>% 
           fct_inorder() %>% 
           fct_rev()) %>%
  ggplot(aes(x = plot_label, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(colour = "blue", size = 1.5) + 
  geom_errorbar(colour = "blue", width=.2) +
  geom_hline(yintercept=1, lty=2) +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  labs(x = NULL) +
  ylab("Incidence rate ratio (95% CI)")

## Save plot ----
ggsave(filename = paste0("rrplot_", resource_type, "_", condition, ".jpeg"),
       plot = plot_rr,
       path = here::here("output", "descriptives", "matched_cohort", model_type, pred_type, "plots"),
       width = 8, height = 8, units = "in")

# Model summary stats ----
model_stats = model_fit %>%
  glance()

## Save model summary stats ----
write_csv(model_stats,
          here::here("output", "descriptives", "matched_cohort", model_type, pred_type, "tables",
                     paste0("stats_", resource_type, "_", condition, ".csv")))

