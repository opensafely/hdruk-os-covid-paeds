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
data_resource_dtw = read_rds(here::here("output", "data", "data_resource.rds"))
data_positives_dtw = read_rds(here::here("output", "data", "data_positives.rds"))

## Create used service column by week ----
data_resource_dtw = data_resource_dtw %>%
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
    n_beddays = sum(n_beddays),
    n_outpatient = sum(n_outpatient),
    n_gp = sum(n_gp),
    days = n()
  ) %>%
  ungroup() %>% 
  mutate(
    service = case_when(
      n_beddays > 0 ~ "Inpatient bed-days",
      n_outpatient > 0 ~ "Outpatient appointment",
      n_gp > 0 ~ "Healthcare episode",
      TRUE ~ "None") %>% 
      factor() %>% 
      fct_relevel("None", "Healthcare episode",
                  "Outpatient appointment", "Inpatient bed-days")
  )

# Patient IDs with no healthcare contacts in follow-up period ----
patient_id_no_service = data_resource_dtw %>%
  filter(period == "follow_up") %>% 
  group_by(patient_id) %>% 
  summarise(no_service = all(service == "None")) %>% 
  filter(no_service) %>% 
  pull(patient_id)

# Create timeseries list of resource use during follow-up period ----
data_timeseries_dtw = data_resource_dtw %>%
  filter(period == "follow_up") %>% 
  filter(!patient_id %in% patient_id_no_service) %>% 
  group_by(patient_id) %>% 
  summarise(service = list(service %>% as.integer())) %>%
  ungroup() %>% 
  mutate(service = service %>% set_names(patient_id)) %>% 
  pull(service)

# Save patient, resource and time series data ----
write_rds(data_resource_dtw,
          here::here("output", "data", "data_resource_dtw.rds"))

write_rds(data_timeseries_dtw,
          here::here("output", "data", "data_timeseries_dtw.rds"))

write_rds(data_positives_dtw,
          here::here("output", "data", "data_positives_dtw.rds"))