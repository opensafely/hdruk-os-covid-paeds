library(tidyverse)
library(lubridate)
library(finalfit)
library(furrr)

# Source custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Command arguments ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  resource_type  = "outpatient"
} else{
  resource_type  = args[[1]]
}

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round  = global_var$disclosure_count_round
count_redact = global_var$disclosure_redact

# Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Create output directory folders ----
dir.create(here::here("output", "comorbidity_yearly"),
           showWarnings = FALSE, recursive=TRUE)

dir.create(here::here("output", "comorbidity_yearly", resource_type, "model_coef"),
           showWarnings = FALSE, recursive=TRUE)

dir.create(here::here("output", "comorbidity_yearly", resource_type, "model_metrics"),
           showWarnings = FALSE, recursive=TRUE)

dir.create(here::here("output", "comorbidity_yearly", resource_type, "incidence_rate_ratio"),
           showWarnings = FALSE, recursive=TRUE)

# Load cohort data ----
data_cohort = read_rds(here::here("output", "data", "data_cohort.rds"))

# Extract variable labels ----
var_labs = extract_variable_label(data_cohort[[1]])

# Bind yearly extracts, drop unused levels, and relabel ----
data_cohort = data_cohort%>% 
  bind_rows() %>%
  mutate(across(where(is.factor), fct_drop)) %>% 
  ff_relabel(var_labs)

# Calculate days under observation ----
# Earliest of calander year-end, death date or study end date less 1st Jan
# (except for outpatient data where records begin from 1st April 2019)
data_cohort = data_cohort %>% 
  mutate(
    days = (pmin(ymd(paste0(cohort, "-12-31")), death_date, study_end_date,
                na.rm = TRUE) - 
              if_else(resource_type == "outpatient" & cohort == 2019,
                      ymd("2019-04-01"), ymd(paste0(cohort, "-01-01")))) %>% 
      as.numeric() + 1
  )

# Comorbidity interactions ----
data_cohort = data_cohort %>% 
  mutate(
    asthma_with_cardiovascular = if_else(asthma == "Yes" & cardiovascular == "Yes",
                                         "Yes", "No") %>% 
      factor() %>% 
      ff_label("Asthma & cardiovascular conditions"),
    
    cystic_fibrosis_with_cardiovascular = if_else(cystic_fibrosis == "Yes" & cardiovascular == "Yes",
                                                  "Yes", "No") %>% 
      factor() %>% 
      ff_label("Cystic fibrosis & cardiovascular conditions"),
    
    other_respiratory_with_cardiovascular = if_else(other_respiratory == "Yes" & cardiovascular == "Yes",
                                                    "Yes", "No") %>% 
      factor() %>% 
      ff_label("Other respiratory conditions & cardiovascular conditions"),
    
    epilepsy_with_cardiovascular = if_else(epilepsy == "Yes" & cardiovascular == "Yes",
                                                    "Yes", "No") %>% 
      factor() %>% 
      ff_label("Epilepsy & cardiovascular conditions"),
    
    headaches_with_cardiovascular = if_else(headaches == "Yes" & cardiovascular == "Yes",
                                           "Yes", "No") %>% 
      factor() %>% 
      ff_label("Headaches & cardiovascular conditions"),
    
    other_neurological_with_cardiovascular = if_else(other_neurological == "Yes" & cardiovascular == "Yes",
                                            "Yes", "No") %>% 
      factor() %>% 
      ff_label("Other neurological conditions & cardiovascular conditions"),
    
    asthma_with_epilepsy = if_else(asthma == "Yes" & epilepsy == "Yes",
                                                     "Yes", "No") %>% 
      factor() %>% 
      ff_label("Asthma & epilepsy"),
    
    cystic_fibrosis_with_epilepsy = if_else(cystic_fibrosis == "Yes" & epilepsy == "Yes",
                                   "Yes", "No") %>% 
      factor() %>% 
      ff_label("Cystic fibrosis & epilepsy"),
    
    other_respiratory_with_epilepsy = if_else(other_respiratory == "Yes" & epilepsy == "Yes",
                                            "Yes", "No") %>% 
      factor() %>% 
      ff_label("Other respiratory conditions & epilepsy"),
    
    asthma_with_headaches = if_else(asthma == "Yes" & headaches == "Yes",
                                   "Yes", "No") %>% 
      factor() %>% 
      ff_label("Asthma & headaches"),
    
    cystic_fibrosis_with_headaches = if_else(cystic_fibrosis == "Yes" & headaches == "Yes",
                                            "Yes", "No") %>% 
      factor() %>% 
      ff_label("Cystic fibrosis & headaches"),
    
    other_respiratory_with_headaches = if_else(other_respiratory == "Yes" & headaches == "Yes",
                                              "Yes", "No") %>% 
      factor() %>% 
      ff_label("Other respiratory conditions & headaches"),
    
    asthma_with_other_neurological = if_else(asthma == "Yes" & other_neurological == "Yes",
                                    "Yes", "No") %>% 
      factor() %>% 
      ff_label("Asthma & other neurological conditions"),
    
    cystic_fibrosis_with_other_neurological = if_else(cystic_fibrosis == "Yes" & other_neurological == "Yes",
                                             "Yes", "No") %>% 
      factor() %>% 
      ff_label("Cystic fibrosis & other neurological conditions"),
    
    other_respiratory_with_other_neurological = if_else(other_respiratory == "Yes" & other_neurological == "Yes",
                                               "Yes", "No") %>% 
      factor() %>% 
      ff_label("Other respiratory conditions & other neurological conditions"),
    
  )

var_labs = extract_variable_label(data_cohort)

# List of adjusting variables ----
var_adjusting = c("age_group", "sex", "ethnicity", "imd_Q5_2019", "region_2019",
                  "rural_urban_2019")

# List of comorbidity variables----
var_comorbidity = c(
  "comorbidity_count.factor",
  
  "mental_health_disorders", "neurodevelopmental_and_behavioural",
  "asthma", "cystic_fibrosis", "other_respiratory",
  "cardiovascular", "epilepsy", "headaches", "other_neurological",
  "gastrointestinal_conditions", "genitourinary", "cancer",
  "non_malignant_haematological", "immunological", "chronic_infections",
  "rheumatology", "congenital_malformation", "diabetes", "other_endocrine",
  "metabolic", "transplant", "palliative_care",

  # Comorbidity interactions
  "asthma_with_cardiovascular", "cystic_fibrosis_with_cardiovascular",
  "other_respiratory_with_cardiovascular",
  "epilepsy_with_cardiovascular", "headaches_with_cardiovascular",
  "other_neurological_with_cardiovascular",
  "asthma_with_epilepsy", "cystic_fibrosis_with_epilepsy",
  "other_respiratory_with_epilepsy",
  "asthma_with_headaches", "cystic_fibrosis_with_headaches",
  "other_respiratory_with_headaches",
  "asthma_with_other_neurological", "cystic_fibrosis_with_other_neurological",
  "other_respiratory_with_other_neurological"
  
)

# Retain regression variables in dataset ----
data_cohort = data_cohort %>% 
  select(all_of(c("patient_id", "days",  
                  var_adjusting, var_comorbidity)), year = cohort) %>%
  mutate(year = year %>% factor()) %>% 
  drop_na() %>% 
  filter(days > 0)

# Summary table
tbl_cohort_summary = data_cohort %>% 
  summary_factorlist(
    dependent = "year",
    explanatory = c(var_adjusting, var_comorbidity),
    cont = "median",
    total_col = FALSE,
    add_col_totals = TRUE,
    na_include = TRUE
  ) %>% 
  ff_round_counts(count_round) %>% 
  ff_redact_counts(count_redact)

## Save Summary table -----
write_csv(tbl_cohort_summary, 
          here::here("output", "comorbidity_yearly",
                     paste0("tbl_cohort_summary_", resource_type, ".csv")))

# Calculate yearly resource use ----
if(resource_type == "gp"){
  
  data_resource = read_rds(here::here("output", "data", "data_gp.rds"))
  
  data_resource = data_resource %>%
    filter(patient_id %in% data_cohort$patient_id) %>% 
    filter(str_starts(code_type, "KM_") |
             str_starts(code_type, "mapped_1") |
             str_starts(code_type, "mapped_2"))%>% 
    distinct(patient_id, gp_date) %>% 
    mutate(year = year(gp_date)) %>% 
    count(patient_id, year)
  
} else if (resource_type == "outpatient"){
  
  data_resource = read_rds(here::here("output", "data", "data_outpatient.rds"))
  
  data_resource = data_resource %>%
    filter(patient_id %in% data_cohort$patient_id) %>% 
    filter(is.na(specialty)) %>% 
    mutate(year = year(outpatient_date)) %>%
    group_by(patient_id, year) %>% 
    summarise(n = sum(outpatient_count)) %>% 
    ungroup()
  
} else if (resource_type == "admissions" | resource_type == "beddays"){
  
  data_resource = read_rds(here::here("output", "data", "data_admissions.rds"))
  
  if (resource_type == "admissions"){
    
    data_resource = data_resource %>%
      filter(patient_id %in% data_cohort$patient_id) %>% 
      mutate(year = year(admission_date)) %>% 
      count(patient_id, year)
    
  } else {
    
    data_resource = seq(year(study_start_date),
                        year(study_end_date - days(1))) %>%
      as.list() %>% 
      map(function(year){
        data_resource %>%
          filter(patient_id %in% data_cohort$patient_id) %>% 
          select(patient_id, admission_date, discharge_date) %>%
          mutate(
            year = year,
            start_date = ymd(paste0(year, "-01-01")),
            end_date = if_else(year == 2022,
                               ymd("2022-04-30"),
                               (start_date + years(1) - days(1)))
          ) %>%
          filter(admission_date <= end_date,
                 discharge_date >= start_date) %>% 
          mutate(
            length_of_stay = case_when(
              admission_date == discharge_date ~ 0.5, # day-case
              TRUE ~ (pmin(discharge_date, end_date) -
                        pmax(admission_date, start_date)) %>% as.numeric()
            )
          )%>%
          group_by(patient_id, year) %>% 
          summarise(
            n = sum(length_of_stay) %>% round()
          ) %>% 
          ungroup()
      })%>% 
      bind_rows()
  }
}

# Add resource data to patient data, replace NAs with 0 count ----
data_cohort = data_cohort %>% 
  left_join(
    data_resource %>%
      mutate(year = year %>% as.character()) %>% 
      select(patient_id, year, n),
    by = c("patient_id", "year")
  ) %>% 
  replace_na(list(n = 0)) %>% 
  rename(health_contact = n) %>% 
  mutate(year = year %>% factor())

# Label lookup table ----
lookup_label = tibble(
  var = names(var_labs),
  var_label = var_labs
)

# Set up parallel sessions ----
plan(multisession, workers = 4)
options(future.globals.maxSize = 5000*1024^2) # Set process limit to 5GB

# For each comorbidity variable, perform regression to calculate IRR ----
var_comorbidity %>%
  future_map(function(var_comorb){
    
    # Model formula ----
    model_formula = as.formula(
      paste0("health_contact ~ ", paste0(var_adjusting, collapse = " + "), " + ",
             paste0("year*", var_comorb, " + offset(log(days))")))
    
    # Restrict comparison to patients with no comorbidities vs specific comorbidity
    if(var_comorb != "comorbidity_count.factor"){
      data_model = data_cohort %>% 
        filter(comorbidity_count.factor == "0" | (!!rlang::sym(var_comorb) == "Yes"))
    } else{
      data_model = data_cohort
    }
    
    # Fit model ----
    model_fit = MASS::glm.nb(formula = model_formula, data = data_model)
    #model_fit = glm(formula = model_formula, family = "poisson", data = data_model)
    
    # Model coefficient ----
    tbl_model_coef = model_fit %>%
      broom::tidy()
    
    write_csv(tbl_model_coef,
              here::here("output", "comorbidity_yearly", resource_type, "model_coef",
                         paste0("tbl_model_coef_", var_comorb, ".csv")))
    
    # Model metrics ----
    tbl_model_metrics = model_fit %>%
      broom::glance()
    
    write_csv(tbl_model_metrics,
              here::here("output", "comorbidity_yearly", resource_type, "model_metrics",
                         paste0("tbl_model_metrics_", var_comorb, ".csv")))
    
    # IRR - linear combination ----
    level_1 = data_cohort[,"year"] %>% levels()
    level_2 = data_cohort[,var_comorb] %>% levels()
    
    tbl_irr = expand_grid(level_1, level_2) %>% 
      mutate(var_1 = "year",
             var_2 = var_comorb) %>% 
      relocate(var_1, var_2) %>% 
      group_by(level_2) %>% 
      mutate(ref_1 = if_else(row_number() == 1, TRUE, FALSE)) %>%
      group_by(level_1) %>% 
      mutate(ref_2 = if_else(row_number() == 1, TRUE, FALSE)) %>% 
      ungroup() %>% 
      filter(!(ref_1 == TRUE & ref_2 == TRUE)) %>% # Remove both reference
      rowwise() %>% 
      mutate(
        lincom_term = case_when(
          ref_1 == FALSE & ref_2 == TRUE  ~ paste0(var_1, level_1),
          ref_1 == TRUE  & ref_2 == FALSE ~ paste0(var_2, level_2),
          ref_1 == FALSE & ref_2 == FALSE ~ paste0(c(paste0(var_1, level_1),
                                                     paste0(var_2, level_2),
                                                     paste0(var_1, level_1, ":", var_2, level_2)),
                                                   collapse = "+"),
          TRUE ~ NA_character_
        )
      )
    
    # If model estimates contain NA, return empty IRR table 
    if(any(is.na(tbl_model_coef))){
      tbl_irr = tbl_irr %>% 
        mutate(
          estimate = NA_real_,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          chisq = NA_real_,
          pr_chisq = NA_real_
        )
    } else {
      tbl_irr = tbl_irr %>% 
        left_join(
          lincom(model_fit, tbl_irr$lincom_term) %>% 
            as_tibble(rownames = "lincom_term"),
          by = "lincom_term"
        ) %>% 
        janitor::clean_names() %>% 
        rename(ci_lower = x2_5_percent, ci_upper = x97_5_percent) %>% 
        unnest(c(estimate, ci_lower, ci_upper, chisq, pr_chisq))
    }
    
    # Join variable label ----
    tbl_irr = tbl_irr %>% 
      left_join(
        lookup_label %>% 
          rename(var_2 = var),
        by = "var_2"
      )
    
    write_csv(tbl_irr,
              here::here("output", "comorbidity_yearly", resource_type, "incidence_rate_ratio",
                         paste0("tbl_irr_", var_comorb, ".csv")))
    
    return(NULL)
  })

