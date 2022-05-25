
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_functions.R"))

# Create directory for processed data and diagnostics ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)

# Load patient data from csv file ----
data_id = here::here("output", "input.csv.gz") %>% 
  read_csv(col_types = read_column_type(.))

# Create dataset with only patient IDs
data_id = data_id %>% 
  select(patient_id)

# Save data as rds ----
write_rds(data_id,
          here::here("output", "data", "data_id.rds"),
          compress="gz")