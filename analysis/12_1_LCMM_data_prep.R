# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 12_LCMM_model.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#
# This script processes the healthcare resource-use dataset to be ready for the
# latent class linear mixed model. 

# Load packages ----
library(tidyverse)

# Create output directories  ----
dir_lcmm = here::here("output", "lcmm")
dir.create(dir_lcmm, showWarnings = FALSE, recursive=TRUE)

# Load resource data ----
data_positives_lcmm = read_rds(here::here("output", "data", "data_positives.rds"))
data_resource_lcmm = read_rds(here::here("output", "data", "data_resource.rds"))

# Group days into 12 x 30-day periods from follow-up start date ----
data_resource_lcmm = data_resource_lcmm %>%
  mutate(date_followup = date_indexed - 14,
          followup_month = ceiling(date_followup/30)) %>% 
  filter(followup_month > 0, followup_month < 13)

# Aggregate by period and filter out incomplete periods ----
data_resource_lcmm = data_resource_lcmm %>% 
  group_by(patient_id, followup_month) %>% 
  summarise(n_beddays = sum(n_beddays),
            n_critical_care = sum(n_critical_care),
            n_outpatient = sum(n_outpatient),
            n_gp = sum(n_gp),
            days = n()) %>% 
  filter(days == 30)

# Create summary of resource-use for diagnositics
summary_data_resource_lcmm = data_resource_lcmm %>% 
  pivot_longer(cols = -c(patient_id, followup_month)) %>% 
  group_by(name, followup_month) %>% 
  count(value)

## Save summary
write_csv(summary_data_resource_lcmm,
          here::here("output", "lcmm", "summary_data_resource_lcmm.csv"))

# Save resource data ----
write_rds(data_resource_lcmm,
          here::here("output", "data", "data_resource_lcmm.rds"))

# Save patient data ----
write_rds(data_positives_lcmm,
          here::here("output", "data", "data_positives_lcmm.rds"))