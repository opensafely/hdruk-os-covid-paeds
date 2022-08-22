
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Create directory for outputs ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "diagnostics"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "data_admissions"), showWarnings = FALSE, recursive=TRUE)

# Load patient IDs
data_id = read_rds(here::here("output", "data", "data_id.rds"))

# Data Files ----
files_admissions = list.files(path = here::here("output", "data_weekly"),
                              pattern = "input_admissions_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

# Admission data ----
data_admissions = here::here("output", "data_weekly", files_admissions) %>%
  map(function(file){
    file %>%
      read_csv(col_types = read_column_type(.)) %>%
      as_tibble()
  })

# Extract diagnostics data  ----
diagnostics_admissions = data_admissions %>%
  map(function(data){
    n_row = nrow(data)
    n_row_bad_id = data %>%
      filter(!patient_id %in% data_id$patient_id) %>%
      nrow()
    n_col = ncol(data)
    n_col_empty = data %>%
      select_if(~(all(is.na(.)))) %>%
      ncol()
    n_empty_admission_1 = data %>% 
      select(admission_date_1) %>% 
      pull() %>% is.na() %>% sum()
    n_empty_discharge_1 = data %>% 
      select(discharge_date_1) %>% 
      pull() %>% is.na() %>% sum()
    n_empty_method_1 = data %>% 
      select(admission_method_1) %>% 
      pull() %>% is.na() %>% sum()
    n_empty_primary_diagnosis_1 = data %>% 
      select(admission_method_1) %>% 
      pull() %>% is.na() %>% sum()
    n_empty_treatment_function_1 = data %>% 
      select(admission_method_1) %>% 
      pull() %>% is.na() %>% sum()
    max_count = data %>%
      select(ends_with("_count")) %>%
      pull() %>% max()
    tibble(n_row, n_row_bad_id, n_col, n_col_empty,
           n_empty_admission_1, n_empty_discharge_1, n_empty_method_1,
           n_empty_primary_diagnosis_1, n_empty_treatment_function_1, max_count)
  }) %>%
  bind_rows() %>%
  mutate(file = files_admissions) %>%
  relocate(file)

# Filter out bad patient IDs, pivot longer ----
data_admissions = data_admissions %>%
  map(function(data){
    data %>%
      filter(patient_id %in% data_id$patient_id) %>%
      select(-ends_with("_count")) %>%
      mutate_at(vars(starts_with(c("admission_date", "discharge_date"))),
                as.character) %>%
      pivot_longer(
        cols = -patient_id,
        names_to = c("variable", "index"),
        names_pattern = "^(.*)_(\\d+)",
        values_to = "data",
        values_drop_na = TRUE
      ) %>%
      pivot_wider(
        names_from = variable,
        values_from = data
      )
  })

## Filter out rows with missing or bad dates ----
data_admissions = data_admissions %>%
  bind_rows() %>%
  mutate_at(vars(contains("_date")), as.Date, format = "%Y-%m-%d") %>%
  filter(admission_date <= discharge_date,
         !is.na(admission_date),
         !is.na(discharge_date)) %>%
  arrange(patient_id, admission_date)

## Fix overlapping admission spells ----
data_admissions = data_admissions %>%
  group_by(patient_id) %>%
  mutate(index = row_number(),
         overlap_with_prior =
           case_when(admission_date < lag(discharge_date)~ 1,
                     TRUE ~ 0)) %>%
  mutate(index = row_number() - cumsum(overlap_with_prior))%>%
  select(-overlap_with_prior) %>%
  group_by(patient_id, index) %>%
  mutate(admission_date = min(admission_date),
         discharge_date = max(discharge_date)) %>%
  ungroup() %>% 
  distinct(patient_id, admission_date, discharge_date,
           .keep_all = TRUE) %>%
  group_by(patient_id) %>% 
  mutate(index = row_number()) %>% 
  ungroup() %>% 
  mutate(critical_care_days = critical_care_days %>% 
           as.numeric())

# Save data as rds ----
write_rds(data_admissions,
          here::here("output", "data", "data_admissions.rds"),
          compress="gz")

# Save diagnostics as csv ----
write_csv(diagnostics_admissions,
          here::here("output", "diagnostics", "diagnostics_admissions.csv"))