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
data_resource = read_rds(here::here("output", "data", "data_resource.rds"))

# Some preprocessing for resource RDS ----
## Create used service column ----
data_resource = data_resource %>%
  mutate(service = case_when(
    n_critical_care > 0 ~ "CC",
    n_beddays > 0 & n_critical_care == 0 ~ "BD",
    n_outpatient > 0 & (n_beddays == 0 & n_critical_care == 0) ~ "OP",
    n_gp > 0 & (n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0) ~ "Contact",
    n_gp == 0 & n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0 ~ "None")
  )

# Flag secondary care contact-day ----
data_resource = data_resource %>% 
  mutate(hospital = case_when(service %in% c("CC", "BD", "OP") ~ 1,
                              service %in% c("Contact", "None") ~ 0))

# Only include alive status ----
data_resource = data_resource %>% 
  filter(alive == TRUE)

# Group days into 12 x 30-day periods from follow-up start date ----
data_resource = data_resource %>% 
  mutate(indexed_month = ceiling(date_indexed/30)) %>% 
  filter(indexed_month < 13)

# Aggregate by period ----
data_resource_lcmm = data_resource %>% 
  group_by(patient_id, indexed_month) %>% 
  summarise(hospital_use = sum(hospital))

# Only include patients with at least 1 episode of healthcare use ----
## Identify patient ids ----
patient_id_non_zero = data_resource_lcmm %>% 
  group_by(patient_id) %>% 
  summarise(total = sum(hospital_use)) %>% 
  filter(total > 0) %>% 
  pull(patient_id)

# Filter for patient id ----
data_resource_lcmm = data_resource_lcmm %>% 
  filter(patient_id %in% patient_id_non_zero)

# Save resource data ----
write_rds(data_resource_lcmm,
          here::here("output", "data", "data_resource_lcmm.rds"))