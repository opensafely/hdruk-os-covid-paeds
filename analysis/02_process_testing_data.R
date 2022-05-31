
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
files_testing = list.files(
  path = here::here("output", "data_weekly"),
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
    n_empty_col_1 = data %>% 
      select(contains("_date_1")) %>% 
      pull() %>% is.na() %>% sum()
    max_count = data %>%
      select(ends_with("_count")) %>%
      pull() %>% max()
    tibble(n_row, n_row_bad_id, n_col, n_col_empty, n_empty_col_1, max_count)
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

# Create plots ----

## Plot weekly ----
### Positive ----
plot_pos_weekly = data_testing %>%
  filter(result == "Positive") %>% 
  ggplot(aes(test_date)) +
  geom_histogram(breaks = date_seq(data_testing$test_date, by = "week")) +
  scale_x_date(labels = scales::date_format("%b %Y"),
               breaks = date_seq(data_testing$test_date, by = "month")) + 
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))

ggsave(filename = paste0("weekly_positive_test_date.jpeg"),
       plot = plot_pos_weekly,
       path = here::here("output", "descriptives", "data_testing"))

### Negative ----
plot_neg_weekly = data_testing %>%
  filter(result == "Negative") %>% 
  ggplot(aes(test_date)) +
  geom_histogram(breaks = date_seq(data_testing$test_date, by = "week")) +
  scale_x_date(labels = scales::date_format("%b %Y"),
               breaks = date_seq(data_testing$test_date, by = "month")) + 
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))

ggsave(filename = paste0("weekly_negative_test_date.jpeg"),
       plot = plot_neg_weekly,
       path = here::here("output", "descriptives", "data_testing"))
    
## Plot monthly ----
### Positive ----
plot_pos_weekly = data_testing %>%
  filter(result == "Positive") %>% 
  ggplot(aes(test_date)) +
  geom_histogram(breaks = date_seq(data_testing$test_date, by = "month")) +
  scale_x_date(labels = scales::date_format("%b %Y"),
               breaks = date_seq(data_testing$test_date, by = "month")) + 
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))

ggsave(filename = paste0("monthly_positive_test_date.jpeg"),
       plot = plot_pos_weekly,
       path = here::here("output", "descriptives", "data_testing"))

### Negative ----
plot_neg_weekly = data_testing %>%
  filter(result == "Negative") %>% 
  ggplot(aes(test_date)) +
  geom_histogram(breaks = date_seq(data_testing$test_date, by = "month")) +
  scale_x_date(labels = scales::date_format("%b %Y"),
               breaks = date_seq(data_testing$test_date, by = "month")) + 
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))

ggsave(filename = paste0("monthly_negative_test_date.jpeg"),
       plot = plot_neg_weekly,
       path = here::here("output", "descriptives", "data_testing"))
