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

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  ng = 1
} else{
  ng = args[[1]] %>% as.integer()
}

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round  = global_var$disclosure_count_round
count_redact = global_var$disclosure_redact

# Directories ----
dir_lcmm_summary_tbl    = here::here("output", "lcmm", "summary_tbl")
dir_lcmm_obs_trajectory = here::here("output", "lcmm", "obs_trajectory")

## Create new output directories ----
dir.create(dir_lcmm_summary_tbl,    showWarnings = FALSE, recursive=TRUE)
dir.create(dir_lcmm_obs_trajectory, showWarnings = FALSE, recursive=TRUE)

# Plot theme ----
theme_set(theme_bw())

# Load data ----
data_resource_lcmm = read_rds(here::here("output", "data", "data_resource_lcmm.rds"))
data_positives     = read_rds(here::here("output", "data", "data_positives.rds"))
lcmm_model         = read_rds(here::here("output", "lcmm", "models", 
                                         paste0("lcmm_model_", ng, ".rds")))

# Join class to patient data ----
data_positives = data_positives %>% 
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
  "vaccination_status"
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

## Save summary table ----
write_csv(tbl_summary,
          here::here("output", "lcmm", "summary_tbl",
                     paste0("tbl_summary_", ng, ".csv")))

# Create observed trajectories by class ----
## Join class to resource data ----
data_resource_lcmm = data_resource_lcmm %>% 
  left_join(
    data_positives %>% 
      select(patient_id, class),
    by = "patient_id"
  )

## Calculate observed trajectories by class ----
## Bootstrapped 95% confidence intervals over 1000 realisations
tbl_obs_trajectory = data_resource_lcmm %>%
  group_by(class, indexed_month) %>% 
  summarise(
    n_patient = n(),
    hospital_use = list(Hmisc::smean.cl.boot(hospital_use,
                                             conf.int = 0.95,
                                             B = 1000))
  ) %>% 
  unnest_wider(hospital_use) %>% 
  ungroup()

## Save observed trajectory table ----
write_csv(tbl_summary,
          here::here("output", "lcmm", "obs_trajectory",
                     paste0("tbl_obs_trajectory_", ng, ".csv")))

## Plot observed trajectory ----
plot_obs_trajectory = tbl_obs_trajectory %>%
  ggplot(aes(x = indexed_month, y = Mean, ymin = Lower, ymax = Upper,
             colour = class, fill = class)) +
  geom_line() + geom_point() +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  labs(x = "Follow-up period (months)",
       y = "Secondary care contact-days per 30 days",
       fill = "Class", colour = "Class") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(limits = c(0, NA))

ggsave(filename = here::here("output", "lcmm", "obs_trajectory",
                             paste0("plot_observed_trajectory_",
                                    lcmm_model$ng, ".jpeg")),
       plot = plot_obs_trajectory,
       height = 6, width = 6, units = "in")

