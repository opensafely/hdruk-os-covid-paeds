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
library(nnet)
library(broom)
library(broom.helpers)

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
dir_lcmm_multinomial     = here::here("output", "lcmm", resource_type, "multinomial")

## Create new output directories ----
dir.create(dir_lcmm_summary_tbl,     showWarnings = FALSE, recursive=TRUE)
dir.create(dir_lcmm_obs_trajectory,  showWarnings = FALSE, recursive=TRUE)
dir.create(dir_lcmm_pred_trajectory, showWarnings = FALSE, recursive=TRUE)
dir.create(dir_lcmm_model_summary,   showWarnings = FALSE, recursive=TRUE)
dir.create(dir_lcmm_multinomial,     showWarnings = FALSE, recursive=TRUE)

# Plot theme ----
theme_set(theme_bw())

# Bootstrap samples ----
B = 500

# Load data ----
data_resource       = read_rds(here::here("output", "data", "data_resource.rds"))
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

# Table of patient characteristics by class ----
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
  
  # Previous healthcare use
  "n_beddays_pre_covid_1yr", "beddays_pre_covid_1yr",
  "n_outpatient_pre_covid_1yr", "outpatient_pre_covid_1yr",
  "n_gp_pre_covid_1yr", "gp_pre_covid_1yr"
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
## Calculate observed trajectories by class ----
data_resource = data_resource %>% 
  select(patient_id, date_indexed,
         resource_use = all_of(paste0("n_", resource_type))) %>%
  mutate(
    week_indexed = case_when(
      date_indexed == 0 ~ 0,
      date_indexed < 0 ~ floor(date_indexed/7),
      date_indexed > 0 ~ ceiling(date_indexed/7)),
    period = case_when(
      week_indexed < 0 ~ "prior",
      week_indexed == 0 ~ "index",
      week_indexed < 3 ~ "immediate",
      week_indexed >= 3 ~ "follow_up"
    )) %>% 
  group_by(patient_id, week_indexed, period) %>% 
  summarise(
    resource_use = sum(resource_use),
    days = n()
  )

## Join class to resource data ----
data_resource = data_resource %>%
  left_join(
    data_positives_lcmm %>% 
      select(patient_id, class),
    by = "patient_id"
  )

## Bootstrapped 95% confidence intervals over B realisations ----
tbl_obs_trajectory = data_resource %>% 
  filter(days == 7, week_indexed > -53, week_indexed < 55) %>% 
  group_by(class, period, week_indexed) %>% 
  summarise(
    n_patient = n(),
    n_events = sum(resource_use)
  ) %>% 
  ungroup()

## Apply disclosure controls ----
tbl_obs_trajectory = tbl_obs_trajectory %>% 
  mutate(
    n_patient_midpoint6 = roundmid_any(n_patient),
    n_events_midpoint6  = roundmid_any(n_events)
    ) %>% 
  rowwise() %>% 
  mutate(
    mean_midpoint6_derived  = poisson.test(n_events_midpoint6, n_patient_midpoint6)$estimate,
    lower_midpoint6_derived = poisson.test(n_events_midpoint6, n_patient_midpoint6)$conf.int[1],
    upper_midpoint6_derived = poisson.test(n_events_midpoint6, n_patient_midpoint6)$conf.int[2]
  ) %>%
  ungroup() %>% 
  select(-n_patient, -n_events)

## Plot observed trajectory ----
y_label = case_when(
  resource_type == "beddays" ~ "Weekly bed-days per 1,000 CYP",
  resource_type == "outpatient" ~ "Weekly outpatient appointments per 1,000 CYP",
  resource_type == "gp" ~ "Weekly healthcare episodes days per 1,000 CYP",
  TRUE ~ "Error"
)
  
plot_obs_trajectory = tbl_obs_trajectory %>%
  ggplot(aes(x = week_indexed, y = mean_midpoint6_derived*1000,
             ymin = lower_midpoint6_derived*1000, ymax = upper_midpoint6_derived*1000,
             group = period)) +
  geom_line()+
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  facet_wrap(~class) +
  geom_vline(xintercept = 2, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Weeks from positive SARS-CoV-2 test",
       y = y_label)

ggsave(filename = here::here("output", "lcmm", resource_type, "obs_trajectory",
                             paste0("plot_observed_trajectory_",
                                    lcmm_model$ng, ".jpeg")),
       plot = plot_obs_trajectory,
       height = 7, width = 10, units = "in")

## Save observed trajectory table ----
write_csv(tbl_obs_trajectory,
          here::here("output", "lcmm", resource_type, "obs_trajectory",
                     paste0("tbl_obs_trajectory_", ng, ".csv")))


# Save model summary ----
sink(here::here("output", "lcmm", resource_type, "model_summary",
                paste0("model_summary_", lcmm_model$ng, ".txt")))
print(summary(lcmm_model))
sink()

# Predicted trajectories ----
## New time data ----
data_time = data.frame(followup_month  = seq(1, 12, length = 100))

## Predict resource trajectories ----
predict_resource = predictY(lcmm_model, data_time, var.time = "followup_month",
                            draws = TRUE)

## Table of predicted trajectories by class ----
tbl_predicted_trajectory = predict_resource$pred %>% 
  as_tibble() %>% 
  bind_cols(predict_resource$times) %>% 
  pivot_longer(cols = -followup_month) %>% 
  mutate(
    class = case_when(
      str_ends(name, "_class\\d+$") ~ str_extract(name, "\\d+$"),
      TRUE ~ "1"
    ),
    statistic = case_when(
      str_starts(name, "Ypred_50") ~ "y",
      str_starts(name, "Ypred_2.5") ~ "y_lower",
      str_starts(name, "Ypred_97.5") ~ "y_upper"
    )
  ) %>%
  select(-name) %>% 
  pivot_wider(names_from = statistic)

## Save predicted trajectory
write_csv(tbl_predicted_trajectory,
          here::here("output", "lcmm", resource_type,"pred_trajectory",
                     paste0("tbl_predicted_trajectory_", lcmm_model$ng, ".csv")))

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



# Multinomial logistic regression ----------------
## Prep ---------
data_positives_lcmm = data_positives_lcmm %>% 
  mutate(illness_severity_2wks = illness_severity_2wks %>% 
           fct_collapse("Inpatient" = "Critical care"))



## Predictor variables -----------------------
predictor_var = c(
  # Demographics
  "age_group", "sex", "imd_Q5_2019", "region_2019",
  "rural_urban_2019",
  
  # Shielding and comorbidity count
  "shielding", "comorbidity_count_factor",
  
  # Illness severity 2 weeks after positive test
  "illness_severity_2wks",
  
  # Previous healthcare use
  "beddays_pre_covid_1yr",
  "outpatient_pre_covid_1yr",
  "gp_pre_covid_1yr"
)

## Model forumla --------------
model_formula = paste0("class ~ ",
                       paste(predictor_var, collapse = " + ")) %>% 
  as.formula()

## Fit logistic/multinomial model ------------------------
if(ng == 1){
  model_out = glm(formula = model_formula, family = binomial,
                  data = data_positives_lcmm)
} else{
  model_out = multinom(formula = model_formula, data = data_positives_lcmm)
}


## Model coefficients ------
tbl_multinom_coef = model_out %>%
  tidy_and_attach(exponentiate = TRUE, conf.int = TRUE) %>%
  tidy_add_reference_rows() %>%
  tidy_add_estimate_to_reference_rows() %>% 
  tidy_add_term_labels()

### Add class column if only one group -----
if(ng == 1){
  tbl_multinom_coef = tbl_multinom_coef %>%
    mutate(y.level = 1)
}

## Save model coefficients table ----
write_csv(tbl_multinom_coef,
          here::here("output", "lcmm", resource_type, "multinomial",
                     paste0("tbl_multinom_coef_", ng, ".csv")))

## Save model metrics -----
tbl_multinom_metrics = model_out %>% 
  glance()

## Apply disclosure controls
tbl_multinom_metrics = tbl_multinom_metrics %>% 
  mutate(nobs = if_else(nobs <= count_redact, "[REDACTED]",
                        plyr::round_any(nobs, count_round) %>% 
                          as.character()))

## Save model metrics -----
write_csv(tbl_multinom_metrics,
          here::here("output", "lcmm", resource_type, "multinomial",
                     paste0("tbl_multinom_metrics_", ng, ".csv")))

## Plot odds ratios ----
plot_or = tbl_multinom_coef %>%
  tidy_remove_intercept() %>%
  filter(reference_row == FALSE) %>% 
  mutate(plot_label = paste0(var_label, ": ", label) %>% 
           factor() %>% 
           fct_inorder() %>% 
           fct_rev()) %>%
  ggplot(aes(y = plot_label, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_point(colour = "blue", size = 1.5) + 
  geom_errorbar(colour = "blue", width=.2) +
  geom_vline(xintercept = 1, lty = 2) +
  facet_wrap(~ y.level) +
  scale_x_continuous(trans='log10') +
  labs(y = NULL) +
  xlab("Odds ratio (95% CI)")

## Save plot --------
ggsave(filename = paste0("plot_multinom_coef_", ng, ".jpeg"),
       plot = plot_or,
       path = here::here("output", "lcmm", resource_type, "multinomial"),
       width = 10, height = 10, units = "in")

