
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Create directory for processed data and diagnostics ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "diagnostics"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "data_gp"), showWarnings = FALSE, recursive=TRUE)

# Load patient IDs
data_id = read_rds(here::here("output", "data", "data_id.rds"))

# Data Files ----
files_gp_disorder = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_gp_disorder_20\\d{2}-\\d{2}-\\d{2}.csv.gz"
)

files_gp_finding = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_gp_finding_20\\d{2}-\\d{2}-\\d{2}.csv.gz"
)

files_gp_procedure = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_gp_procedure_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

files_gp_regime_therapy = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_gp_regime_therapy_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

files_gp_observable_entity = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_gp_observable_entity_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

files_gp_specimen = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_gp_specimen_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

files_gp = c(files_gp_disorder, files_gp_finding, files_gp_procedure, 
             files_gp_regime_therapy, files_gp_observable_entity, 
             files_gp_specimen)

# Read GP data from csv ----
data_gp = here::here("output", "data_weekly", files_gp) %>%
  map(function(file){
    file %>%
      read_csv(col_types = read_column_type(.)) %>%
      as_tibble()
  })

# Extract diagnostics data  ----
diagnostics_gp = data_gp %>%
  map(function(data){
    n_row = nrow(data)
    n_row_bad_id = data %>%
      filter(!patient_id %in% data_id$patient_id) %>%
      nrow()
    n_col = ncol(data)
    n_col_empty = data %>%
      select_if(~(all(is.na(.)))) %>%
      ncol()
    
    tibble(n_row, n_row_bad_id, n_col, n_col_empty)
    
  }) %>%
  bind_rows() %>%
  mutate(file = files_gp) %>%
  relocate(file)

# Filter out bad patient IDs, pivot longer ----
data_gp = map2(
    .x = data_gp,
    .y = files_gp,
    .f = function(.data, .file_list){
      .data %>%
        filter(patient_id %in% data_id$patient_id) %>% 
        select(-gp_contact_count) %>% 
        pivot_longer(
          cols = -patient_id,
          names_to = c("index"),
          names_pattern = "gp_contact_date_(\\d+)",
          values_to = "date",
          values_drop_na = TRUE
        ) %>%
        select(-index) %>% 
        mutate(
          date = date %>% ymd()
        )
  }) %>%
  bind_rows() %>% 
  distinct(patient_id, date)

# Save data as rds ----
write_rds(data_gp,
          here::here("output", "data", "data_gp.rds"),
          compress="gz")

# Save diagnostics as csv ----
write_csv(diagnostics_gp,
          here::here("output", "diagnostics", "diagnostics_gp.csv"))