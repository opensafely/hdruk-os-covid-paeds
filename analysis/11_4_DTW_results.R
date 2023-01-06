# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 11_DTW_results.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#
# This script takes the allocated clusters determined by time series clustering
# and calculates patient summary tables and resource-use summaries stratified by
# cluster.

# Load packages ----
library(tidyverse)
library(finalfit)
library(nnet)
library(broom)
library(broom.helpers)

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  n_clusters = 5 # will hard code n_clusters = 3 to 8 in .yaml
} else{
  n_clusters = args[[1]]
}

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round  = global_var$disclosure_count_round
count_redact = global_var$disclosure_redact

# Create output directories ----
dir.create(here::here("output", "dtw", "results"), showWarnings = FALSE, recursive=TRUE)

# Bootstrap samples ----
B = 500

# Plot theme ----
theme_set(theme_bw())

# Load data ----
data_resource_dtw  = read_rds(here::here("output", "data", "data_resource_dtw.rds"))
data_positives_dtw = read_rds(here::here("output", "data", "data_positives_dtw.rds"))
data_cluster   = read_rds(
  here::here("output", "dtw", "data_cluster",
             paste0("data_cluster_", n_clusters, ".rds")))

# Add clustering assignment to patient and resource data ----
data_resource_dtw = data_resource_dtw %>% 
  left_join(data_cluster, by = "patient_id") %>% 
  replace_na(list(cluster = 0)) %>% 
  mutate(cluster = cluster %>% 
           factor() %>% 
           ff_label("Cluster"))

data_positives_dtw = data_positives_dtw %>% 
  left_join(data_cluster, by = "patient_id")  %>% 
  replace_na(list(cluster = 0)) %>% 
  mutate(cluster = cluster %>% 
           factor() %>% 
           ff_label("Cluster"))

# Resource use by cluster ----
tbl_resource_use_cluster = data_resource_dtw %>%
  filter(period != "index") %>%
  filter(week_indexed > -53, week_indexed < 55) %>% 
  pivot_longer(cols = c(starts_with("n_")),
               names_pattern = "n_([[:alnum:]_]+)", names_to = "resource_type") %>% 
  group_by(week_indexed, period, cluster, resource_type) %>% 
  summarise(
    n_patient = n(),
    n_events = sum(value),
    bootstats = list(Hmisc::smean.cl.boot(value, conf.int = 0.95, B = B))
  ) %>% 
  ungroup() %>%
  unnest_wider(bootstats) %>% 
  mutate(resource_type = resource_type %>%
           fct_relevel("gp", "outpatient", "beddays") %>% 
           fct_recode("Healthcare episode" = "gp",
                      "Outpatient appointment" = "outpatient",
                      "Inpatient bed-days" = "beddays"))
  

## Save table ----
write_csv(tbl_resource_use_cluster,
          here::here("output", "dtw", "results",
                     paste0("tbl_resource_use_cluster_", n_clusters, ".csv")))

# Plot resource use by type and cluster ----
plot_resource_use_cluster = tbl_resource_use_cluster %>%
  ggplot(aes(x = week_indexed, y = Mean*1000, 
             ymin = Lower*1000, ymax = Upper*1000,
             group = period)) +
  geom_line() +
  geom_vline(xintercept = 2, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  facet_grid(resource_type ~ cluster, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Weeks from positive SARS-CoV-2 test",
       y = "Incidence (count per 1,000 CYP)")

## Save plot ----
ggsave(here::here("output", "dtw", "results", paste0("plot_resource_use_cluster_",
                                          n_clusters, ".jpeg")),
       plot = plot_resource_use_cluster,
       height = 8, width = 17, units = "in")

# Table of patient characteristics by cluster ----
dependent_var = "cluster"
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
tbl_summary = data_positives_dtw %>% 
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

## Save patient summary table ----
write_csv(tbl_summary,
          here::here("output", "dtw", "results",
                     paste0("tbl_summary_", n_clusters, ".csv")))


# Multinomial logistic regression ----------------
## Prep ---------
data_positives_dtw = data_positives_dtw %>% 
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
model_formula = paste0("cluster ~ ",
                       paste(predictor_var, collapse = " + ")) %>% 
  as.formula()

## Fit multinomial model ------------------------
model_multinom = multinom(formula = model_formula, data = data_positives_dtw)

## Model coefficients ------
tbl_multinom_coef = model_multinom %>%
  tidy_and_attach(exponentiate = TRUE, conf.int = TRUE) %>%
  tidy_add_reference_rows() %>%
  tidy_add_estimate_to_reference_rows() %>% 
  tidy_add_term_labels()

## Save model coefficients table ----
write_csv(tbl_multinom_coef,
          here::here("output", "dtw", "results",
                     paste0("tbl_multinom_coef_", n_clusters, ".csv")))

## Save model metrics -----
tbl_multinom_metrics = model_multinom %>% 
  glance()

## Save model metrics -----
write_csv(tbl_multinom_metrics,
          here::here("output", "dtw", "results",
                     paste0("tbl_multinom_metrics_", n_clusters, ".csv")))

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
ggsave(filename = paste0("plot_multinom_coef_", n_clusters, ".jpeg"),
       plot = plot_or,
       path = here::here("output", "dtw", "results"),
       width = 10, height = 10, units = "in")