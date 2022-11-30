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

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  n_clusters = 8 # will hard code n_clusters = 3 to 8 in .yaml
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

# Bootstrap samples
B = 10

# Load data ----
data_resource_dtw  = read_rds(here::here("output", "data", "data_resource_dtw.rds"))
data_positives = read_rds(here::here("output", "data", "data_positives.rds"))
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

data_positives = data_positives %>% 
  left_join(data_cluster, by = "patient_id")  %>% 
  replace_na(list(cluster = 0)) %>% 
  mutate(cluster = cluster %>% 
           factor() %>% 
           ff_label("Cluster"))

# Resource use by cluster ----
tbl_resource_use_cluster = data_resource_dtw %>%
  group_by(date_indexed, cluster) %>% 
  summarise(
    n_patient = n(),
    critical_care = list(Hmisc::smean.cl.boot(n_critical_care,
                                              conf.int = 0.95,
                                              B = B)),
    beddays = list(Hmisc::smean.cl.boot(n_beddays,
                                        conf.int = 0.95,
                                        B = B)),
    outpatient = list(Hmisc::smean.cl.boot(n_outpatient,
                                           conf.int = 0.95,
                                           B = B)),
    gp = list(Hmisc::smean.cl.boot(n_gp,
                                   conf.int = 0.95,
                                   B = B)),
  )

## Tidy up table and resource factor levels ----
tbl_resource_use_cluster = tbl_resource_use_cluster %>%
  unnest_wider(c(critical_care, beddays, outpatient, gp),
               names_sep = "_") %>%
  pivot_longer(-c(date_indexed, n_patient, cluster),
               names_to = c("resource_type", "statistic"),
               names_pattern = "(.*)_([[:alpha:]]+)$") %>% 
  pivot_wider(names_from = statistic, values_from = value) %>% 
  mutate(resource_type = resource_type %>%
           fct_relevel("gp", "outpatient", "beddays", "critical_care") %>% 
           fct_recode("Healthcare episode" = "gp",
                      "Outpatient appointment" = "outpatient",
                      "Inpatient admission" = "beddays",
                      "Critical care" = "critical_care"))

## Save table ----
write_csv(tbl_resource_use_cluster,
          here::here("output", "dtw", "results",
                     paste0("tbl_resource_use_cluster_", n_clusters, ".csv")))

# Plot resource use by type and cluster ----
plot_resource_use_cluster = tbl_resource_use_cluster %>%
  ggplot(aes(x = date_indexed, y = Mean, ymin = Lower, ymax = Upper)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  facet_grid(resource_type ~ cluster, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Follow-up period (days)", y = "Counts per person-day")

## Save plot ----
ggsave(here::here("output", "dtw", "results", paste0("plot_resource_use_cluster_",
                                          n_clusters, ".jpeg")),
       plot = plot_resource_use_cluster,
       height = 8, width = 15, units = "in")

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
  "n_gp_2wks_post_covid", "n_outpatient_2wks_post_covid",
  "n_beddays_2wks_post_covid", "n_critical_care_2wks_post_covid",
  
  # Previous healthcare use
  "ntile_gp_pre_covid_1yr", "n_gp_pre_covid_1yr",
  "ntile_outpatient_pre_covid_1yr", "n_outpatient_pre_covid_1yr",
  "ntile_beddays_pre_covid_1yr", "n_beddays_pre_covid_1yr"
)

## Summary factorlist ----
tbl_summary = data_positives %>% 
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
