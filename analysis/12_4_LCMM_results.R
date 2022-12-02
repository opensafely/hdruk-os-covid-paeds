# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 12_LCMM_results.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#
# This script calculates summary patient characteristics stratified by latent 
# class from the LCLMM model. Mean observed secondary care healthcare use during
# the 12 month follow-up period is plotted with bootstrapped confidence intervals. 

# Load packages ----
library(tidyverse)
library(finalfit)
library(lcmm)
library(splines2)

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  ng = 1
  resource_type = "beddays"
} else{
  ng = args[[1]] %>% as.integer()
  resource_type = args[[2]]
}

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round  = global_var$disclosure_count_round
count_redact = global_var$disclosure_redact

# Directories ----
dir_lcmm_summary_tbl     = here::here("output", "lcmm", resource_type, "summary_tbl")
dir_lcmm_obs_trajectory  = here::here("output", "lcmm", resource_type, "obs_trajectory")
dir_lcmm_pred_trajectory = here::here("output", "lcmm", resource_type, "pred_trajectory")
dir_lcmm_model_summary   = here::here("output", "lcmm", resource_type, "model_summary")

## Create new output directories ----
dir.create(dir_lcmm_summary_tbl,     showWarnings = FALSE, recursive=TRUE)
dir.create(dir_lcmm_obs_trajectory,  showWarnings = FALSE, recursive=TRUE)
dir.create(dir_lcmm_pred_trajectory, showWarnings = FALSE, recursive=TRUE)
dir.create(dir_lcmm_model_summary,   showWarnings = FALSE, recursive=TRUE)

# Plot theme ----
theme_set(theme_bw())

# Load data ----
data_resource_lcmm  = read_rds(here::here("output", "data", "data_resource_lcmm.rds"))
data_positives_lcmm = read_rds(here::here("output", "data", "data_positives_lcmm.rds"))
lcmm_model          = read_rds(here::here("output", "lcmm", resource_type, "models", 
                                         paste0("lcmm_model_", ng, ".rds")))

# Join class to patient data ----
data_positives_lcmm = data_positives_lcmm %>% 
  left_join(
    lcmm_model$pprob %>% 
      as_tibble() %>% 
      select(patient_id, class),
    by = "patient_id"
  ) %>% 
  replace_na(list(class = 0)) %>%
  mutate(
    class = class %>%
      factor() %>% 
      ff_label("Class")
  )

# Create patient summary table by class ----
dependent_var = "class"
explanatory_var = c(
  
  # Follow-up
  "follow_up_days",
  
  # Demographics
  "age", "age_group", "sex", "ethnicity", "imd_Q5_2019", "region_2019",
  "rural_urban_2019",
  
  # Shielding
  "shielding",
  
  # Comorbidities
  "comorbidity_count_factor",
  "mental_health_disorders", "neurodevelopmental_and_behavioural",
  "asthma", "cystic_fibrosis", "other_respiratory",
  "cardiovascular", "epilepsy", "headaches", "other_neurological",
  "gastrointestinal_conditions", "genitourinary", "cancer",
  "non_malignant_haematological", "immunological", "chronic_infections",
  "rheumatology", "congenital_malformation", "diabetes", "other_endocrine",
  "metabolic", "transplant", "palliative_care",
  
  # Vaccination status
  "vaccination_status",
  
  # Illness severity 2 weeks after positive test
  "illness_severity_2wks", "pims_ts", 
  "n_gp_2wks_post_covid", "n_outpatient_2wks_post_covid",
  "n_beddays_2wks_post_covid", "n_critical_care_2wks_post_covid",
  
  # Previous healthcare use
  "ntile_gp_pre_covid_1yr", "n_gp_pre_covid_1yr",
  "ntile_outpatient_pre_covid_1yr", "n_outpatient_pre_covid_1yr",
  "ntile_beddays_pre_covid_1yr", "n_beddays_pre_covid_1yr"
)

## Summary factorlist ----
tbl_summary = data_positives_lcmm %>% 
  summary_factorlist(
    dependent = dependent_var,
    explanatory = explanatory_var,
    cont = "median",
    total_col = FALSE,
    add_col_totals = TRUE,
    na_include = TRUE
  ) %>% 
  ff_round_counts(count_round) %>% 
  ff_redact_counts(count_redact)

## Save summary table ----
write_csv(tbl_summary,
          here::here("output", "lcmm", resource_type, "summary_tbl",
                     paste0("tbl_summary_", ng, ".csv")))

# Create observed trajectories by class ----
## Join class to resource data ----
data_resource_lcmm = data_resource_lcmm %>% 
  left_join(
    data_positives_lcmm %>% 
      select(patient_id, class),
    by = "patient_id"
  )

## Calculate observed trajectories by class ----
## Bootstrapped 95% confidence intervals over 1000 realisations
tbl_obs_trajectory = data_resource_lcmm %>% 
  select(patient_id, followup_month, class,
         resource_use = all_of(paste0("n_", resource_type))) %>% 
  group_by(class, followup_month) %>% 
  summarise(
    n_patient = n(),
    resource_use = list(Hmisc::smean.cl.boot(resource_use,
                                             conf.int = 0.95,
                                             B = 1000))
  ) %>% 
  unnest_wider(resource_use) %>% 
  ungroup()

## Save observed trajectory table ----
write_csv(tbl_obs_trajectory,
          here::here("output", "lcmm", resource_type, "obs_trajectory",
                     paste0("tbl_obs_trajectory_", ng, ".csv")))

## Plot observed trajectory ----
y_label = case_when(
  resource_type == "beddays" ~ "Bed-days per 30 days",
  resource_type == "outpatient" ~ "Outpatient appointments per 30 days",
  resource_type == "gp" ~ "Healthcare contact days per 30 days",
  TRUE ~ "Error"
)
  
plot_obs_trajectory = tbl_obs_trajectory %>%
  ggplot(aes(x = followup_month, y = Mean, ymin = Lower, ymax = Upper,
             colour = class, fill = class)) +
  geom_line() + geom_point() +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  labs(x = "Follow-up period (months)",
       y = y_label,
       fill = "Class", colour = "Class") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(limits = c(0, NA))

ggsave(filename = here::here("output", "lcmm", resource_type, "obs_trajectory",
                             paste0("plot_observed_trajectory_",
                                    lcmm_model$ng, ".jpeg")),
       plot = plot_obs_trajectory,
       height = 6, width = 6, units = "in")

# Predicted trajectories ----
## New time data ----
data_time = data.frame(followup_month  = seq(1, 12, length = 100))

## Predict resource trajectories ----
predict_resource = predictY(lcmm_model, data_time, var.time = "followup_month",
                            draws = TRUE)

## Save model summary ----
sink(here::here("output", "lcmm", resource_type, "model_summary",
                paste0("model_summary_", lcmm_model$ng, ".txt")))
#print(summary(lcmm_model))
print(predict_resource)
sink()

tbl_predicted_trajectory = predict_resource$pred %>% 
  as_tibble() %>% 
  bind_cols(predict_resource$times)

# Save predicted trajectory
write_csv(tbl_predicted_trajectory,
          here::here("output", "lcmm", resource_type,"pred_trajectory",
                     paste0("tbl_predicted_trajectory_", lcmm_model$ng, ".csv")))

## Table of predicted trajectories by class ----
tbl_predicted_trajectory = tbl_predicted_trajectory %>% 
  pivot_longer(cols = -followup_month) %>% 
  mutate(
    class = case_when(
      str_ends(name, "_class\\d+$") ~ str_extract(name, "\\d+$"),
      TRUE ~ "1"
    ),
    statistic = case_when(
      str_starts(name, "Ypred_50")   ~ "y",
      str_starts(name, "Ypred_2.5")  ~ "y_lower",
      str_starts(name, "Ypred_97.5") ~ "y_upper"
    )
  ) %>%
  select(-name) %>% 
  pivot_wider(names_from = statistic)


# Plot predicted trajectory by class ----
y_label_pred = case_when(
  resource_type == "beddays" ~ "Predicted bed-days per 30 days",
  resource_type == "outpatient" ~ "Predicted outpatient appointments per 30 days",
  resource_type == "gp" ~ "Predicted healthcare contact days per 30 days",
  TRUE ~ "Error"
)

plot_predicted_trajectory = tbl_predicted_trajectory %>% 
  ggplot(aes(x = followup_month, y = y,
             ymin = y_lower, ymax = y_upper,
             colour = class, fill = class)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  labs(x = "Follow-up period (months)",
       y = y_label_pred,
       fill = "Class", colour = "Class") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(limits = c(0, NA))

ggsave(filename = here::here("output", "lcmm", resource_type, "pred_trajectory",
                             paste0("plot_predicted_trajectory_",
                                    lcmm_model$ng, ".jpeg")),
       plot = plot_predicted_trajectory,
       height = 6, width = 6, units = "in")


