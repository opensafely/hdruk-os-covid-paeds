library(tidyverse)
library(lubridate)
library(finalfit)
library(glmnet)
library(SparseM)
library(furrr)

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
  resource_type  = "gp"
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

# List of predictor varaibles ----
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
  "metabolic", "transplant", "palliative_care"

)

# Retain regression variables in dataset ----
data_cohort = data_cohort %>% 
  select(all_of(c("patient_id", "days", predictor_vars)), year = cohort) %>%
  mutate(year = year %>% factor()) %>% 
  drop_na()

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
    filter(str_starts(code_type, "KM_") |
             str_starts(code_type, "mapped_1") |
             str_starts(code_type, "mapped_2"))%>% 
    distinct(patient_id, gp_date) %>% 
    mutate(year = year(gp_date)) %>% 
    count(patient_id, year)
  
} else if (resource_type == "outpatient"){
  
  data_resource = read_rds(here::here("output", "data", "data_outpatient.rds"))
  
  data_resource = data_resource %>% 
    filter(is.na(specialty)) %>% 
    mutate(year = year(outpatient_date)) %>%
    group_by(patient_id, year) %>% 
    summarise(n = sum(outpatient_count)) %>% 
    ungroup()
  
} else if (resource_type == "admissions" | resource_type == "beddays"){
  
  data_resource = read_rds(here::here("output", "data", "data_admissions.rds"))
  
  if (resource_type == "admissions"){
    
    data_resource = data_resource %>% 
      mutate(year = year(admission_date)) %>% 
      count(patient_id, year)
    
  } else {
    
    data_resource = seq(year(study_start_date),
                        year(study_end_date - days(1))) %>%
      as.list() %>% 
      map(function(year){
        data_resource %>%
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
            n = sum(length_of_stay)
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

# Perform LASSO regression ----
lasso_model_est = cv.glmnet(x = X[,-1], # "-1" removes additional intercept term
                            y = y,
                            offset = offset,
                            family = "poisson",
                            type.measure = "deviance",
                            nfolds = 10
                            )

# Extract coefficient estimates ----
lasso_coef_est = lasso_model_est %>%
  coef.glmnet(s = "lambda.min") %>%
  as.matrix() %>%
  as_tibble(rownames = "coeff_name") %>%
  rename("estimate" = "lambda.min")
# 
# # Bootstrap set up ----
# n_rows = nrow(X)    # number of rows in dataset
# n_bootstrap = 5     # number of bootstrap samples
# n_cores = 4         # number of cores to use
# alpha = 0.05        # significance
# 
# 
# # Set up parallelisation ----
# plan(multisession, workers = min(parallel::detectCores(), n_cores))
# 
# # Perform bootstrap ----
# lasso_bootstrap = 1:n_bootstrap %>%
#   future_map(function(boot_index){
# 
#     # Calculate row weight for bootstrap ----
#     row_weight = tibble(row_id = 1:n_rows) %>%
#       left_join(
#         tibble(index_sample = sample(1:n_rows, replace = TRUE)) %>%
#           count(index_sample) %>%
#           select(row_id = index_sample, weight = n),
#         by = "row_id"
#       ) %>%
#       replace_na(list(weight = 0))
# 
#     # Perform Lasso regression with bootstrap sample (via weights) ----
#     lasso_model_boot = cv.glmnet(x = X[,-1],
#                                  y = y,
#                                  offset = offset,
#                                  weights = row_weight$weight,
#                                  family = "poisson")
#     # Output Lasso model ----
#     return(lasso_model_boot)
#   }#,
#   #.options = furrr_options(seed = TRUE)
#   )
# 
# # Extract coefficients from bootstrap ----
# lasso_coeff_bootstrap = lasso_bootstrap %>%
#   map(function(lasso_model){
#     lasso_model %>%
#       coef.glmnet(s = "lambda.min") %>%
#       as.matrix() %>%
#       as_tibble(rownames = "coeff_name") %>%
#       rename("coeff_value" = "lambda.min")
#   }) %>%
#   bind_rows(.id = "boot_id")
# 
# # Summarise bootstrap coefficients ----
# lasso_coeff_bootstrap = lasso_coeff_bootstrap %>%
#   group_by(coeff_name) %>%
#   summarise(
#     lower    = quantile(coeff_value, probs = alpha/2),
#     upper    = quantile(coeff_value, probs = 1 - alpha/2),
#     minimum  = min(coeff_value),
#     maximum  = max(coeff_value)
#   )
# 
# # Combine Lasso estimate and with bootstrap ----
# lasso_coef = lasso_coef_est %>%
#   left_join(lasso_coeff_bootstrap, by = "coeff_name")
# 
# # Format variable labels for plot ----
# lasso_coef = lasso_coef %>%
#   mutate(year = str_extract(coeff_name, "year\\d{4}") %>%
#            str_remove("year"),
#          var  = str_remove(coeff_name, "year\\d{4}[:]?"),
#          var  = if_else(var == "", "Year", var)) %>%
#   replace_na(list(year = "2019")) %>%
#   mutate(var_type = case_when(
#     year == "2019" ~ "Baseline covariate effect",
#     var == "Year" ~ "Headline year effect",
#     TRUE ~ "Year-covariate interaction"
#   )) %>%
#   left_join(
#     label_lookup %>% select(var = var_level_combined, var_level_label)
#   ) %>%
#   mutate(var_level_label = if_else(is.na(var_level_label),
#                                    var, var_level_label)) %>%
#   select(-var) %>%
#   mutate(
#     var_level_label = var_level_label %>%
#       factor() %>%
#       fct_relevel(unique(var_level_label)) %>%
#       fct_rev()
#   )
# 
# # Save lasso coefficients ----
# write_csv(lasso_coef,
#           here::here("output", "descriptives", "lasso_model",
#                      paste0("tbl_lasso_coef_", resource_type, ".csv")))
# 
# # Plot coefficients ----
# plot_lasso_coef = lasso_coef %>%
#   filter(var_level_label != "(Intercept)") %>%
#   ggplot(aes(x = var_level_label, y = estimate, ymin = lower, ymax = upper,
#              colour = var_type)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
#   geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
#   facet_wrap(~ year, ncol = 4) +
#   labs(y = "Regression coefficient",
#        x = NULL, colour = NULL) +
#   theme(
#     legend.position = "bottom") +
#   coord_flip()
# 
# # Save coefficient plot ----
# ggsave(paste0("plot_lasso_coef_", resource_type, ".jpeg"),
#        plot_lasso_coef,
#        path = here::here("output", "descriptives", "lasso_model"),
#        height = 8, width = 12)
