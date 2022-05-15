
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_functions.R"))

# Read processed data  ----
data_patient    = read_rds(here::here("output", "datasets", "data_patient.rds"))
data_admissions = read_rds(here::here("output", "datasets", "data_admissions.rds"))
data_testing    = read_rds(here::here("output", "datasets", "data_testing.rds"))


data_criteria = data_patient %>% 
  transmute(
    patient_id
    ) %>%
  # Nosocomial infection
  left_join(
    data_admissions %>% 
      left_join(data_testing %>% 
                  filter(index == 1, result == "positive") %>% 
                  select(patient_id, test_date),
                by = "patient_id") %>%
      filter((discharge_date - admission_date) %>% as.numeric() > 7,
             ((admission_date + days(7)) < test_date) &
               ((discharge_date + days(7)) >= test_date)) %>% 
      mutate(covid_nosocomial = "Yes" %>% 
               ff_label("Nosocomial infection")) %>% 
      select(patient_id, covid_nosocomial)
  ) %>% 
  left_join(
    
  )

# First positive covid test date 
data_patient = data_patient %>% 
  left_join(
    data_testing %>%
      filter(result == "positive") %>% 
      group_by(patient_id) %>% 
      filter(index == min(index)) %>% 
      ungroup() %>% 
      select(patient_id, covid_positive_test_date_1 = test_date),
    by = "patient_id"
  )

# Nosocomial infection

