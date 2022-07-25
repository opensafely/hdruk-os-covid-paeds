
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Create directory for outputs ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "diagnostics"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "data_outpatient"), showWarnings = FALSE, recursive=TRUE)

# Load patient IDs
data_id = read_rds(here::here("output", "data", "data_id.rds"))

# Data Files ----
files_outpatient = list.files(path = here::here("output", "data_weekly"),
                      pattern = "input_outpatient_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

# Read outpatient data from csv ----
data_outpatient = here::here("output", "data_weekly", files_outpatient) %>%
  map(function(file){
    file %>% 
      read_csv(col_types = read_column_type(.)) %>% 
      as_tibble()
  })

# Extract diagnostics data  ----
diagnostics_outpatient = data_outpatient %>%
  map(function(data){
    n_row = nrow(data)
    n_row_bad_id = data %>%
      filter(!patient_id %in% data_id$patient_id) %>%
      nrow()
    n_col = ncol(data)
    n_col_empty = data %>%
      select_if(~(all(is.na(.)))) %>%
      ncol()
    
    nonzero_counts = data %>%
      summarise(
        nonzero_count_1 = (outpatient_count_1 > 0) %>% sum(),
        nonzero_count_2 = (outpatient_count_2 > 0) %>% sum(),
        nonzero_count_3 = (outpatient_count_3 > 0) %>% sum(),
        nonzero_count_4 = (outpatient_count_4 > 0) %>% sum(),
        nonzero_count_5 = (outpatient_count_5 > 0) %>% sum(),
        nonzero_count_6 = (outpatient_count_6 > 0) %>% sum(),
        nonzero_count_7 = (outpatient_count_7 > 0) %>% sum(),
        nonzero_count_week = (outpatient_count_week > 0) %>% sum()
      )
    
    tibble(n_row, n_row_bad_id, n_col, n_col_empty) %>% 
      bind_cols(nonzero_counts)
  }) %>%
  bind_rows() %>%
  mutate(file = files_outpatient) %>%
  relocate(file)

# Filter out bad patient IDs, pivot longer ----
data_outpatient = map2(
  .x = data_outpatient,
  .y = files_outpatient,
  .f = function(.data, .file_list){
    .data %>%
      filter(patient_id %in% data_id$patient_id) %>%
      select(-outpatient_count_week) %>% 
      pivot_longer(
        cols = -patient_id,
        names_to = c("index"),
        names_pattern = "outpatient_count_(\\d+)",
        values_to = "value",
        values_drop_na = FALSE
      ) %>% 
      filter(value > 0) %>% 
      mutate(
        index = index %>% as.numeric(),
        date = .file_list %>%
          str_extract(
            pattern = "20\\d{2}-\\d{2}-\\d{2}(?=\\.csv\\.gz)") %>% 
          ymd() + days(index - 1)
      )
  }) %>%
  bind_rows() %>% 
  select(-index)

# Save data as rds ----
write_rds(data_outpatient,
          here::here("output", "data", "data_outpatient.rds"),
          compress="gz")

# Save diagnostics as csv ----
write_csv(diagnostics_outpatient,
          here::here("output", "diagnostics", "diagnostics_outpatient.csv"))
