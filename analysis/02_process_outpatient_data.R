
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
    n_empty_outpatient_1 = data %>% 
      select(outpatient_date_1) %>% 
      pull() %>% is.na() %>% sum()
    max_count = data %>%
      select(ends_with("_count")) %>%
      pull() %>% max()
    tibble(n_row, n_row_bad_id, n_col, n_col_empty, n_empty_outpatient_1,
           max_count)
  }) %>%
  bind_rows() %>%
  mutate(file = files_outpatient) %>%
  relocate(file)

# Filter out bad patient IDs, pivot longer ----
data_outpatient = data_outpatient %>%
  map(function(data){
    data %>%
      filter(patient_id %in% data_id$patient_id) %>%
      select(-ends_with("_count")) %>%
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
  distinct(patient_id, outpatient_date)

# Save data as rds ----
write_rds(data_outpatient,
          here::here("output", "data", "data_outpatient.rds"),
          compress="gz")

# Save diagnostics as csv ----
write_csv(diagnostics_outpatient,
          here::here("output", "diagnostics", "diagnostics_outpatient.csv"))

# Create plots ----
c("outpatient_date") %>% 
  map(function(var){
    # Plot weekly ----
    plot_weekly = data_outpatient %>% 
      count_dates_by_period(var, period = "week") %>% 
      ggplot(aes(x = date, y = n)) +
      geom_line() +
      theme_bw()
    
    ggsave(filename = paste0("weekly_", var, ".jpeg"),
           plot = plot_weekly,
           path = here::here("output", "descriptives", "data_outpatient"))
    
    # Plot monthly ----
    plot_monthly = data_outpatient %>% 
      count_dates_by_period(var, period = "month") %>% 
      ggplot(aes(x = date, y = n)) +
      geom_line() +
      theme_bw()
    
    ggsave(filename = paste0("monthly_", var, ".jpeg"),
           plot = plot_monthly,
           path = here::here("output", "descriptives", "data_outpatient"))
  })