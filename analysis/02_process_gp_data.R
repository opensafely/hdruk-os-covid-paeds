
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
files_gp_KM = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_gp_KM_[[:lower:]_]+_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

files_gp_mapped = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_gp_mapped_\\d{1}_[[:lower:]_]+_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

files_gp = c(files_gp_KM, files_gp_mapped)

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
    
    nonzero_counts = data %>%
      summarise(
        nonzero_count_1 = (gp_count_1 > 0) %>% sum(),
        nonzero_count_2 = (gp_count_2 > 0) %>% sum(),
        nonzero_count_3 = (gp_count_3 > 0) %>% sum(),
        nonzero_count_4 = (gp_count_4 > 0) %>% sum(),
        nonzero_count_5 = (gp_count_5 > 0) %>% sum(),
        nonzero_count_6 = (gp_count_6 > 0) %>% sum(),
        nonzero_count_7 = (gp_count_7 > 0) %>% sum(),
        nonzero_count_week = (gp_count_week > 0) %>% sum()
      )
    
    tibble(n_row, n_row_bad_id, n_col, n_col_empty) %>% 
      bind_cols(nonzero_counts)
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
      select(-gp_count_week) %>% 
      pivot_longer(
        cols = -patient_id,
        names_to = c("index"),
        names_pattern = "gp_count_(\\d+)",
        values_to = "gp_count",
        values_drop_na = FALSE
      ) %>% 
      filter(gp_count > 0) %>% 
      mutate(
        index = index %>% as.numeric(),
        gp_date = .file_list %>%
          str_extract(
            pattern = "20\\d{2}-\\d{2}-\\d{2}(?=\\.csv\\.gz)") %>% 
          ymd() + (index - 1),
        code_type = .file_list %>%
          str_extract(
            pattern = "(?<=input_gp_)[[:alnum:]_]+(?=_20\\d{2}-\\d{2}-\\d{2}\\.csv\\.gz)")
      ) %>% 
      select(-index)
  }) %>% 
  bind_rows()

# Save data as rds ----
write_rds(data_gp,
          here::here("output", "data", "data_gp.rds"),
          compress="gz")

# Save diagnostics as csv ----
write_csv(diagnostics_gp,
          here::here("output", "diagnostics", "diagnostics_gp.csv"))