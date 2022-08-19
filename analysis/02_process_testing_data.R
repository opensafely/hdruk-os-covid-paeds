
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Create directory for processed data and diagnostics ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "diagnostics"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "data_testing"), showWarnings = FALSE, recursive=TRUE)

# Load patient IDs
data_id = read_rds(here::here("output", "data", "data_id.rds"))

# Data Files ----
files_negative_tests = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_covid_tests_negative_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

files_positive_tests = list.files(
  path = here::here("output", "data_weekly"),
  pattern = "input_covid_tests_positive_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

# Read in testing data ----
data_testing_negative = here::here("output", "data_weekly", files_negative_tests) %>%
  map(function(file){
    file %>% 
      read_csv(col_types = read_column_type(.)) %>% 
      as_tibble()
  })

data_testing_positive = here::here("output", "data_weekly", files_positive_tests) %>%
  map(function(file){
    file %>% 
      read_csv(col_types = read_column_type(.)) %>% 
      as_tibble()
  })

# Extract diagnostics data  ----
## Negative tests ----
diagnostics_testing_negative = data_testing_negative %>%
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
        nonzero_count_1 = (covid_negative_test_count_1 > 0) %>% sum(),
        nonzero_count_2 = (covid_negative_test_count_2 > 0) %>% sum(),
        nonzero_count_3 = (covid_negative_test_count_3 > 0) %>% sum(),
        nonzero_count_4 = (covid_negative_test_count_4 > 0) %>% sum(),
        nonzero_count_5 = (covid_negative_test_count_5 > 0) %>% sum(),
        nonzero_count_6 = (covid_negative_test_count_6 > 0) %>% sum(),
        nonzero_count_7 = (covid_negative_test_count_7 > 0) %>% sum(),
        nonzero_count_week = (covid_negative_test_week_count  > 0) %>% sum()
      )
    
    tibble(n_row, n_row_bad_id, n_col, n_col_empty) %>% 
      bind_cols(nonzero_counts)
  }) %>%
  bind_rows() %>%
  mutate(file = files_negative_tests) %>%
  relocate(file)

## Positive tests ----
diagnostics_testing_positive = data_testing_positive %>%
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
        nonzero_count_1 = (covid_positive_test_count_1 > 0) %>% sum(),
        nonzero_count_2 = (covid_positive_test_count_2 > 0) %>% sum(),
        nonzero_count_3 = (covid_positive_test_count_3 > 0) %>% sum(),
        nonzero_count_4 = (covid_positive_test_count_4 > 0) %>% sum(),
        nonzero_count_5 = (covid_positive_test_count_5 > 0) %>% sum(),
        nonzero_count_6 = (covid_positive_test_count_6 > 0) %>% sum(),
        nonzero_count_7 = (covid_positive_test_count_7 > 0) %>% sum(),
        nonzero_count_week = (covid_positive_test_week_count  > 0) %>% sum()
      )
    
    tibble(n_row, n_row_bad_id, n_col, n_col_empty) %>% 
      bind_cols(nonzero_counts)
  }) %>%
  bind_rows() %>%
  mutate(file = files_positive_tests) %>%
  relocate(file)

## Combine diagnositics ----
diagnostics_testing = diagnostics_testing_negative %>% 
  bind_rows(diagnostics_testing_positive)


# Filter out bad patient IDs, pivot longer ----
## Negative ----
data_testing_negative = map2(
  .x = data_testing_negative,
  .y = files_negative_tests,
  .f = function(.data, .file_list){
    .data %>%
      filter(patient_id %in% data_id$patient_id) %>%
      select(-covid_negative_test_week_count) %>%
      pivot_longer(
        cols = -patient_id,
        names_to = c("result", "index"),
        names_pattern = "covid_(.*)_test_count_(\\d+)",
        values_to = "test_count",
        values_drop_na = TRUE
      ) %>% 
      filter(test_count > 0) %>% 
      mutate(
        index = index %>% as.numeric(),
        test_date = .file_list %>%
          str_extract(
            pattern = "20\\d{2}-\\d{2}-\\d{2}(?=\\.csv\\.gz)") %>% 
          ymd() + days(index - 1)
      ) %>% 
      select(-c(index, test_count))
  }) %>%
  bind_rows() 

## Positive ----
data_testing_positive = map2(
  .x = data_testing_positive,
  .y = files_positive_tests,
  .f = function(.data, .file_list){
    .data %>%
      filter(patient_id %in% data_id$patient_id) %>%
      select(-covid_positive_test_week_count) %>%
      pivot_longer(
        cols = -patient_id,
        names_to = c("result", "index"),
        names_pattern = "covid_(.*)_test_count_(\\d+)",
        values_to = "test_count",
        values_drop_na = TRUE
      ) %>% 
      filter(test_count > 0) %>% 
      mutate(
        index = index %>% as.numeric(),
        test_date = .file_list %>%
          str_extract(
            pattern = "20\\d{2}-\\d{2}-\\d{2}(?=\\.csv\\.gz)") %>% 
          ymd() + days(index - 1)
      ) %>% 
      select(-c(index, test_count))
  }) %>%
  bind_rows() 

## Combine ----
data_testing = data_testing_negative %>% 
  bind_rows(data_testing_positive)%>%
  arrange(patient_id, test_date, result) %>%
  distinct(patient_id, test_date, result)

# Recode test results ----
data_testing = data_testing %>% 
  mutate(
    result = result %>% 
      fct_recode(Positive = "positive",
                 Negative = "negative")
  )

# Save data as rds ----
write_rds(data_testing,
          here::here("output", "data", "data_testing.rds"),
          compress="gz")

# Save diagnostics as csv ----
write_csv(diagnostics_testing,
          here::here("output", "diagnostics", "diagnostics_testing.csv"))