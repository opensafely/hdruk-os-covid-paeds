
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
files_testing = list.files(path = here::here("output", "data_weekly"),
                           pattern = "input_covid_tests_[[:lower:]]+_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

# Read in testing data ----
data_testing = here::here("output", "data_weekly", files_testing) %>%
  map(function(file){
    file %>% 
      read_csv(col_types = read_column_type(.)) %>% 
      as_tibble()
  })

# Extract diagnostics data  ----
diagnostics_testing = data_testing %>%
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
  mutate(file = files_testing) %>%
  relocate(file)

# Filter out bad patient IDs, pivot longer ----
data_testing = data_testing %>%
  map(function(data){
    data %>%
      filter(patient_id %in% data_id$patient_id) %>%
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
  distinct(patient_id, test_date, result)

# Save data as rds ----
write_rds(data_testing,
          here::here("output", "data", "data_testing.rds"),
          compress="gz")

# Save diagnostics as csv ----
write_csv(diagnostics_testing,
          here::here("output", "diagnostics", "diagnostics_testing.csv"))

# Create plots ----
c("test_date") %>% 
  map(function(var){
    # Plot weekly ----
    plot_weekly = data_testing %>% 
      filter(result == "positive") %>% 
      count_dates_by_period(var, period = "week") %>% 
      ggplot(aes(x = date, y = n)) +
      geom_line() +
      theme_bw()
    
    ggsave(filename = paste0("weekly_positive_", var, ".jpeg"),
           plot = plot_weekly,
           path = here::here("output", "descriptives", "data_testing"))
    
    plot_weekly = data_testing %>% 
      filter(result == "negative") %>% 
      count_dates_by_period(var, period = "week") %>% 
      ggplot(aes(x = date, y = n)) +
      geom_line() +
      theme_bw()
    
    ggsave(filename = paste0("weekly_negative_", var, ".jpeg"),
           plot = plot_weekly,
           path = here::here("output", "descriptives", "data_testing"))
    
    # Plot monthly ----
    plot_monthly = data_testing %>% 
      filter(result == "positive") %>% 
      count_dates_by_period(var, period = "month") %>% 
      ggplot(aes(x = date, y = n)) +
      geom_line() +
      theme_bw()
    
    ggsave(filename = paste0("monthly_positive_", var, ".jpeg"),
           plot = plot_monthly,
           path = here::here("output", "descriptives", "data_testing"))
    
    plot_monthly = data_testing %>% 
      filter(result == "negative") %>% 
      count_dates_by_period(var, period = "month") %>% 
      ggplot(aes(x = date, y = n)) +
      geom_line() +
      theme_bw()
    
    ggsave(filename = paste0("monthly_negative_", var, ".jpeg"),
           plot = plot_monthly,
           path = here::here("output", "descriptives", "data_testing"))
  })
