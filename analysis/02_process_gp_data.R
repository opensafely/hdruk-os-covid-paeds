
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

# Load patient IDs, admissions and outpatient data
data_id = read_rds(here::here("output", "data", "data_id.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))
data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds"))

# Data Files ----
files_gp = list.files(path = here::here("output", "data_weekly"),
                           pattern = "input_gp_20\\d{2}-\\d{2}-\\d{2}.csv.gz")

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
    n_empty_gp_1 = data %>% 
      select(gp_contact_date_1) %>% 
      pull() %>% is.na() %>% sum()
    max_count = data %>%
      select(ends_with("_count")) %>%
      pull() %>% max()
    tibble(n_row, n_row_bad_id, n_col, n_col_empty, n_empty_gp_1, max_count)
  }) %>%
  bind_rows() %>%
  mutate(file = files_gp) %>%
  relocate(file)

# Filter out bad patient IDs, pivot longer ----
data_gp = data_gp %>%
  map(function(data){
    data %>%
      filter(patient_id %in% data_id$patient_id) %>%
      select(-ends_with("_count")) %>%
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
  distinct(patient_id, gp_date)

# Flag dates coinciding with secondary care ----
data_gp = data_gp %>% 
  left_join(
    data_outpatient %>%
      mutate(outpatient_flag = 1) %>% 
      select(patient_id, gp_date = outpatient_date, outpatient_flag),
    by = c("patient_id", "gp_date")
  ) %>% 
  left_join(
    data_admissions %>%
      select(patient_id, admission_date, discharge_date) %>% 
      rowwise() %>% 
      mutate(gp_date = list(seq(admission_date, discharge_date, by = "day"))) %>% 
      unnest(gp_date) %>% 
      mutate(admission_flag = 1) %>% 
      select(patient_id, gp_date, admission_flag),
    by = c("patient_id", "gp_date")
  ) %>%
  replace_na(list(outpatient_flag = 0, admission_flag = 0))

# Save data as rds ----
write_rds(data_gp,
          here::here("output", "data", "data_gp.rds"),
          compress="gz")

# Save diagnostics as csv ----
write_csv(diagnostics_gp,
          here::here("output", "diagnostics", "diagnostics_gp.csv"))