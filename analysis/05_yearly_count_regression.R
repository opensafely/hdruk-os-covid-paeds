library(tidyverse)
library(lubridate)
library(finalfit)
library(glmnet)
library(SparseM)
library(doParallel)

# Source custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Set seed
set.seed(1573088)

# Plot theme ----
theme_set(theme_bw())

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
dir.create(here::here("output", "descriptives", "lasso_model"),
           showWarnings = FALSE, recursive=TRUE)

# Plot theme ----
theme_set(theme_bw())

# Command arguments ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  resource_type  = "outpatient"
} else{
  resource_type  = args[[1]]
}

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

# List of predictor variables ----
predictor_vars = c(
  # Demographics
  "age_group",
  "sex", "ethnicity", "imd_Q5_2019",
  "region_2019", "rural_urban_2019",
  
  # Comorbidities
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
  select(all_of(c("patient_id", "days", predictor_vars)), year = cohort) %>%
  mutate(year = year %>% factor()) %>% 
  drop_na() %>% 
  filter(days > 0)

# Summary table
tbl_cohort_summary = data_cohort %>% 
  summary_factorlist(
    dependent = "year",
    explanatory = predictor_vars,
    cont = "median",
    total_col = FALSE,
    add_col_totals = TRUE,
    na_include = TRUE
  ) %>% 
  ff_round_counts(count_round) %>% 
  ff_redact_counts(count_redact)

## Save Summary table -----
write_csv(tbl_cohort_summary, 
          here::here("output", "descriptives", "lasso_model",
                     paste0("tbl_cohort_summary_", resource_type, ".csv")))

# Create lookup table for labels ----
label_lookup = tibble(
  var = predictor_vars) %>% 
  rowwise() %>% 
  mutate(
    level_labels = levels(data_cohort[,var]) %>% list()
  ) %>%
  ungroup() %>% 
  unnest(level_labels) %>% 
  mutate(var_level_combined = paste0(var, level_labels)) %>% 
  left_join(
    tibble(var = names(var_labs), var_labels = var_labs)
  ) %>% 
  mutate(
    var_level_label = paste0(var_labels, ": ", level_labels)
  )


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
  rename(health_contact = n)

# Model formula with year-interaction ---- 
predictor_formula = as.formula(
  paste0("~ year*(", paste0(predictor_vars, collapse = " + "), ")"))

# Model matrices and offset ----
X = sparse.model.matrix(predictor_formula, data = data_cohort)
y = sparse.model.matrix(~ health_contact - 1, data = data_cohort) # "-1" removes intercept
offset = log(data_cohort$days)

# Set up parallel ----
n_workers = 5        
registerDoParallel(n_workers)

# Perform LASSO regression ----
lasso_model_est = cv.glmnet(x = X[,-1], # "-1" removes additional intercept term
                            y = y,
                            offset = offset,
                            standardize = FALSE,
                            family = "poisson",
                            type.measure = "deviance",
                            nfolds = 10,
                            parallel = TRUE
)

# Extract coefficient estimates ----
lasso_coef_est = lasso_model_est %>%
  coef.glmnet(s = "lambda.min") %>%
  as.matrix() %>%
  as_tibble(rownames = "coeff_name") %>%
  rename("estimate" = "lambda.min")

# Bootstrap set up ----
n_rows = nrow(X)   # number of rows in dataset
n_bootstrap = 1  # number of bootstrap samples
alpha = 0.05       # significance level 

# Set custom limit of 5GB to be passed to future proceses ---- 
options(future.globals.maxSize = 5000*1024^2)

# Set up parallelisation ----
plan(multisession, workers = min(parallel::detectCores(), n_cores))

# Perform bootstrap ----
lasso_bootstrap = 1:n_bootstrap %>%
  map(function(boot_index){

    # Sample row index
    i_boot = sample(1:n_rows, n_rows, replace = TRUE)

    # Perform Lasso regression with bootstrap sample ----
    lasso_model_boot = cv.glmnet(x = X[i_boot, -1],
                                 y = y[i_boot],
                                 offset = offset[i_boot],
                                 standardize = FALSE,
                                 family = "poisson",
                                 type.measure = "deviance",
                                 nfolds = 10,
                                 parallel = TRUE)
    
    # Output Lasso model ----
    return(lasso_model_boot)
  }
  )

# Extract poisson deviance from bootstrap ----
lasso_pois_dev = lasso_bootstrap %>% 
  map(function(lasso_model){
    tibble(
      pois_dev = lasso_model$cvm[which(lasso_model$lambda.min == lasso_model$lambda)]
    )
  }) %>%
  bind_rows(.id = "boot_id") %>% 
  summarise(
    estimate = median(pois_dev),
    lower    = quantile(pois_dev, probs = alpha/2),
    upper    = quantile(pois_dev, probs = 1 - alpha/2),
  )

# Save lasso coefficients ----
write_csv(lasso_pois_dev,
          here::here("output", "descriptives", "lasso_model",
                     paste0("tbl_lasso_pois_dev_", resource_type, ".csv")))


# Extract coefficients from bootstrap ----
lasso_coef_boot = lasso_bootstrap %>%
  map(function(lasso_model){
    lasso_model %>%
      coef.glmnet(s = "lambda.min") %>%
      as.matrix() %>%
      as_tibble(rownames = "coeff_name") %>%
      rename("coeff_value" = "lambda.min")
  }) %>%
  bind_rows(.id = "boot_id")

# Summarise bootstrap coefficients ----
lasso_coef_boot = lasso_coef_boot %>%
  group_by(coeff_name) %>%
  summarise(
    mean     = mean(coeff_value),
    median   = median(coeff_value),
    lower    = quantile(coeff_value, probs = alpha/2),
    upper    = quantile(coeff_value, probs = 1 - alpha/2),
    minimum  = min(coeff_value),
    maximum  = max(coeff_value)
  )

# Format variable labels for plot ----
lasso_coef = lasso_coef_est %>%
  left_join(lasso_coef_boot) %>% 
  mutate(year = str_extract(coeff_name, "year\\d{4}") %>%
           str_remove("year"),
         var  = str_remove(coeff_name, "year\\d{4}[:]?"),
         var  = if_else(var == "", "Year", var)) %>%
  replace_na(list(year = "2019")) %>%
  mutate(var_type = case_when(
    year == "2019" & str_detect(var, "_with_") ~ "Baseline comorbidity interaction",
    year == "2019" ~ "Baseline covariate effect",
    var == "Year" ~ "Headline year effect",
    TRUE ~ "Year-covariate interaction"
  ) %>% factor() %>% 
    fct_relevel("Baseline covariate effect")) %>%
  left_join(
    label_lookup %>% select(var = var_level_combined, var_level_label)
  ) %>%
  mutate(var_level_label = if_else(is.na(var_level_label),
                                   var, var_level_label)) %>%
  select(-var) %>%
  mutate(
    var_level_label = var_level_label %>%
      factor() %>%
      fct_relevel(unique(var_level_label)) %>%
      fct_rev()
  )

# Save lasso coefficients ----
write_csv(lasso_coef,
          here::here("output", "descriptives", "lasso_model",
                     paste0("tbl_lasso_coef_", resource_type, ".csv")))

# Plot coefficients ----
plot_lasso_coef = lasso_coef %>%
  filter(var_level_label != "(Intercept)") %>%
  ggplot(aes(x = var_level_label, y = estimate, ymin = lower, ymax = upper,
             colour = var_type)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
  facet_wrap(~ year, ncol = 4) +
  labs(y = "Regression coefficient",
       x = NULL, colour = NULL) +
  theme(
    legend.position = "bottom") +
  coord_flip()

# Save coefficient plot ----
ggsave(paste0("plot_lasso_coef_", resource_type, ".jpeg"),
       plot_lasso_coef,
       path = here::here("output", "descriptives", "lasso_model"),
       height = 8, width = 12)
