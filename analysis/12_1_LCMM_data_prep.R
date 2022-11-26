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

# Load resource data ----
data_positives = read_rds(here::here("output", "data", "data_positives.rds"))
data_resource = read_rds(here::here("output", "data", "data_resource.rds"))

# Random sample
data_positives_lcmm = data_positives 
data_resource_lcmm = data_resource %>% 
  filter(patient_id %in% data_positives_lcmm$patient_id)

# Group days into 12 x 30-day periods from follow-up start date ----
data_resource_lcmm = data_resource_lcmm %>% 
  mutate(indexed_month = ceiling(date_indexed/30)) %>% 
  filter(indexed_month < 13)

# Aggregate by period and filter out incomplete periods ----
data_resource_lcmm = data_resource_lcmm %>% 
  group_by(patient_id, indexed_month) %>% 
  summarise(n_beddays = sum(n_beddays),
            n_beddays = sum(n_critical_care),
            n_outpatient = sum(n_outpatient),
            n_gp = sum(n_gp),
            days = n()) %>% 
  filter(days == 30)

# Save resource data ----
write_rds(data_resource_lcmm,
          here::here("output", "data", "data_resource_lcmm.rds"))

# Save patient data ----
write_rds(data_positives_lcmm,
          here::here("output", "data", "data_positives_lcmm.rds"))