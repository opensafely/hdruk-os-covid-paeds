# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 11_DTW_prep_resource.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#

# Load packages ----
library(tidyverse)

# Load data ----
data_resource = read_rds(here::here("output", "data", "data_resource.rds"))

## Create used service column ----
data_resource_dtw = data_resource %>%
  mutate(
    day_followup = date_indexed - 14,
    service = case_when(
      n_critical_care > 0 ~ "Critial care",
      n_beddays > 0 ~ "Inpatient admission",
      n_outpatient > 0 ~ "Outpatient appointment",
      n_gp > 0 ~ "Healthcare episode",
      TRUE ~ "None") %>% 
      factor() %>% 
      fct_relevel("None", "Healthcare episode",
                  "Outpatient appointment", "Inpatient admission",
                  "Critial care")
  ) %>% 
  filter(day_followup > 0)

# Create timeseries list of resource use ----
data_timeseries_dtw = data_resource_dtw %>% 
  group_by(patient_id) %>% 
  summarise(service = list(service %>% as.integer())) %>%
  ungroup() %>% 
  mutate(service = service %>% set_names(patient_id)) %>% 
  pull(service)

# Save resource and time series data ----
write_rds(data_resource_dtw,
          here::here("output", "data", "data_resource_dtw.rds"))

write_rds(data_timeseries_dtw,
          here::here("output", "data", "data_timeseries_dtw.rds"))