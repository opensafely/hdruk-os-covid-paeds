
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_functions.R"))

# Create directory for processed data, descriptive tables and plots ----
dir.create(here::here("output", "datasets"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "extract_descriptives", "tables"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "extract_descriptives", "figures"), showWarnings = FALSE, recursive=TRUE)

# Monthly data files ----
path_data_weekly = here::here("output", "data_weekly")
files_admissions = list.files(path_data_weekly, "input_admissions_\\d{4}-\\d{2}-\\d{2}.csv.gz")
files_outpatient = list.files(path_data_weekly, "input_outpatient_\\d{4}-\\d{2}-\\d{2}.csv.gz")
files_gp         = list.files(path_data_weekly, "input_gp_\\d{4}-\\d{2}-\\d{2}.csv.gz")
files_testing    = list.files(path_data_weekly, "input_covid_tests_[[:lower:]]+_\\d{4}-\\d{2}-\\d{2}.csv.gz")

# Patient data ----
data_patient = here::here("output", "input.csv.gz") %>% 
  read_csv(col_types = read_column_type(.))

# Admission data ----
data_admissions = here::here("output", "data_weekly", files_admissions) %>%
  map(function(file){
    file %>%
      read_csv(col_types = read_column_type(.)) %>%
      as_tibble()
  })

## Record extract characteristics ----
extract_summary_admissions = data_admissions %>%
  map(function(data){
    n_row = nrow(data)
    n_row_bad_id = data %>%
      filter(!patient_id %in% data_patient$patient_id) %>%
      nrow()
    n_col = ncol(data)
    n_col_empty = data %>%
      select_if(~(all(is.na(.)))) %>%
      ncol()
    n_max_count = max(data %>% pull(admission_count))
    tibble(n_row, n_row_bad_id, n_col, n_col_empty, n_max_count)
  }) %>%
  bind_rows() %>%
  mutate(file = files_admissions) %>%
  relocate(file)

## Filter out patients not in cohort, pivot longer to one row per admission spell ----
data_admissions = data_admissions %>%
  map(function(data){
    data %>%
      filter(patient_id %in% data_patient$patient_id) %>%
      select(-admission_count) %>% 
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
  }) %>%
  bind_rows() %>%
  mutate_at(vars(contains("_date")), as.Date, format = "%Y-%m-%d") 

## Record filtering out bad admission spells ----
extract_summary_admissions = extract_summary_admissions %>% 
  mutate(spells_missing_admission_date = sum(is.na(data_admissions$admission_date)),
         spells_missing_discharge_date = sum(is.na(data_admissions$discharge_date)),
         spells_discharge_before_admission =
           sum(data_admissions$discharge_date < data_admissions$admission_date, na.rm = TRUE))

## Filter out admission spells with missing or bad dates ----
data_admissions = data_admissions %>%
  filter(admission_date <= discharge_date,
         !is.na(admission_date),
         !is.na(discharge_date)) %>%
  arrange(patient_id, admission_date)

## Record rows to calculate number of overlapping spells ----
extract_summary_admissions = extract_summary_admissions %>% 
  mutate(spells_overlap = nrow(data_admissions))

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
  select(-index) %>%
  distinct(patient_id, admission_date, discharge_date,
           .keep_all = TRUE) %>% 
  group_by(patient_id) %>% 
  mutate(index = row_number()) %>% 
  ungroup() %>% 
  relocate(index, .after = patient_id)

## Calculate number of overlapping spells ----
extract_summary_admissions = extract_summary_admissions %>% 
  mutate(spells_overlap = spells_overlap - nrow(data_admissions))

# Outpatient data ----
data_outpatient = here::here("output", "data_weekly", files_outpatient) %>%
  map(function(file){
    file %>%
      read_csv(col_types = read_column_type(.)) %>%
      as_tibble()
  })

## Record extract characteristics ----
extract_summary_outpatient = data_outpatient %>%
  map(function(data){
    n_row = nrow(data)
    n_row_bad_id = data %>%
      filter(!patient_id %in% data_patient$patient_id) %>%
      nrow()
    n_col = ncol(data)
    n_col_empty = data %>%
      select_if(~(all(is.na(.)))) %>%
      ncol()
    n_max_count = max(data %>% pull(outpatient_count))
    tibble(n_row, n_row_bad_id, n_col, n_col_empty, n_max_count)
  }) %>%
  bind_rows() %>%
  mutate(file = files_outpatient) %>%
  relocate(file)

## Filter out patients not in cohort, pivot longer to one row per outpatient appointment ----
data_outpatient = data_outpatient %>%
  map(function(data){
    data %>%
      filter(patient_id %in% data_patient$patient_id) %>%
      select(-outpatient_count) %>%
      pivot_longer(
        cols = -patient_id,
        names_to = c("variable", "index"),
        names_pattern = "^(.*)_(\\d+)",
        values_to = "outpatient_date",
        values_drop_na = TRUE
      ) %>%
      select(-variable, -index)
  }) %>%
  bind_rows() %>%
  arrange(patient_id, outpatient_date) %>%
  distinct(patient_id, outpatient_date) %>% 
  group_by(patient_id) %>% 
  mutate(index = row_number()) %>% 
  ungroup() %>% 
  relocate(index, .after = patient_id)


# GP contact data ----
data_gp = here::here("output", "data_weekly", files_gp) %>%
  map(function(file){
    file %>%
      read_csv(col_types = read_column_type(.)) %>%
      as_tibble()
  })

## Record extract characteristics ----
extract_summary_gp = data_gp %>%
  map(function(data){
    n_row = nrow(data)
    n_row_bad_id = data %>%
      filter(!patient_id %in% data_patient$patient_id) %>%
      nrow()
    n_col = ncol(data)
    n_col_empty = data %>%
      select_if(~(all(is.na(.)))) %>%
      ncol()
    n_max_count = max(data %>% pull(gp_contact_count))
    tibble(n_row, n_row_bad_id, n_col, n_col_empty, n_max_count)
  }) %>%
  bind_rows() %>%
  mutate(file = files_gp) %>%
  relocate(file)

## Filter out patients not in cohort, pivot longer to one row per GP contact ----
data_gp = data_gp %>%
  map(function(data){
    data %>%
      filter(patient_id %in% data_patient$patient_id) %>%
      select(-gp_contact_count) %>% 
      pivot_longer(
        cols = -patient_id,
        names_to = c("variable", "index"),
        names_pattern = "^(.*)_(\\d+)",
        values_to = "gp_date",
        values_drop_na = TRUE
      ) %>%
      select(-variable, -index)
  }) %>%
  bind_rows() %>%
  arrange(patient_id, gp_date) %>%
  distinct(patient_id, gp_date) %>% 
  group_by(patient_id) %>% 
  mutate(index = row_number()) %>% 
  ungroup() %>% 
  relocate(index, .after = patient_id)

# Testing data ----
data_testing = here::here("output", "data_weekly", files_testing) %>%
  map(function(file){
    file %>% 
      read_csv(col_types = read_column_type(.)) %>% 
      as_tibble()
  })

extract_summary_testing = data_testing %>%
  map(function(data){
    n_row = nrow(data)
    n_row_bad_id = data %>%
      filter(!patient_id %in% data_patient$patient_id) %>%
      nrow()
    n_col = ncol(data)
    n_col_empty = data %>%
      select_if(~(all(is.na(.)))) %>%
      ncol()
    n_max_count = max(data %>% select(ends_with("_count")) %>% pull())
    tibble(n_row, n_row_bad_id, n_col, n_col_empty, n_max_count)
  }) %>%
  bind_rows() %>%
  mutate(file = files_testing) %>%
  relocate(file)

data_testing = data_testing %>%
  map(function(data){
    data %>%
      filter(patient_id %in% data_patient$patient_id) %>%
      select(-ends_with("_count")) %>% 
      pivot_longer(
        cols = -patient_id,
        names_to = c("result", "index"),
        names_pattern = "covid_(.*)_test_date_(\\d+)",
        values_to = "test_date",
        values_drop_na = TRUE
      ) %>%
      select(-index)
  }) %>%
  bind_rows() %>%
  arrange(patient_id, test_date, result) %>%
  distinct(patient_id, test_date, result) %>% 
  group_by(patient_id) %>% 
  mutate(index = row_number()) %>% 
  ungroup() %>% 
  relocate(index, .after = patient_id)

# Plot histograms ----
plot_hist(data_admissions, "admission_date",  here::here("output", "extract_descriptives", "figures"))
plot_hist(data_admissions, "discharge_date",  here::here("output", "extract_descriptives", "figures"))
plot_hist(data_outpatient, "outpatient_date", here::here("output", "extract_descriptives", "figures"))
plot_hist(data_gp,         "gp_date",         here::here("output", "extract_descriptives", "figures"))
plot_hist(data_testing,    "test_date",       here::here("output", "extract_descriptives", "figures"), fill = "result")


# Save extract summary files ----
write_csv(extract_summary_admissions,
          here::here("output", "extract_descriptives", "tables", "extract_summary_admissions.csv"))
write_csv(extract_summary_outpatient,
          here::here("output", "extract_descriptives", "tables", "extract_summary_outpatient.csv"))
write_csv(extract_summary_gp,
          here::here("output", "extract_descriptives", "tables", "extract_summary_gp.csv"))
write_csv(extract_summary_testing,
          here::here("output", "extract_descriptives", "tables", "extract_summary_testing.csv"))

# Save processed data as rds files ----
write_rds(data_patient,
          here::here("output", "datasets", "data_patient.rds"),
          compress="gz")
write_rds(data_admissions,
          here::here("output", "datasets", "data_admissions.rds"),
          compress="gz")

write_rds(data_outpatient,
          here::here("output", "datasets", "data_outpatient.rds"),
          compress="gz")

write_rds(data_gp,
          here::here("output", "datasets", "data_gp.rds"),
          compress="gz")

write_rds(data_testing,
          here::here("output", "datasets", "data_testing.rds"),
          compress="gz")

