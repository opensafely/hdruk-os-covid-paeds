
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Functions ----
# read_column_type: read column name to determine column data type
read_column_type = function(file){
  
  first_row = read_csv(
    file,
    n_max = 1,
    col_names = FALSE,
    col_types = cols(.default = "c")
  )
  
  type_string = tibble(
    column_names = c(first_row)) %>% 
    mutate(column_type = case_when(
      column_names == "patient_id" ~ "i",
      str_detect(column_names, "_date") ~ "D",
      str_detect(column_names, "imd_") ~ "i",
      str_detect(column_names, "age") ~ "d",
      str_detect(column_names, "_count") ~ "i",
      TRUE ~ "c"
    )) %>%
    pull(column_type) %>%
    paste(collapse = "")
  
  return(type_string)
}

# Create directory for processed data and diagnostics ----
dir.create(here::here("output", "admissions_table"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "admissions_plots"), showWarnings = FALSE, recursive=TRUE)

# Data Files ----
files_admissions = list.files(path = "output",
                              pattern = "input_admissions_\\d{4}-\\d{2}-\\d{2}.csv.gz")

# Patient data ----
data_patient = here::here("output", "input.csv.gz") %>% 
  read_csv(col_types = read_column_type(.))

# Admission data ----
data_admissions = here::here("output", files_admissions) %>%
  map(function(file){
    file %>%
      read_csv(col_types = read_column_type(.)) %>%
      as_tibble()
  })

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
  })

## Filter out rows with missing or bad dates ----
data_admissions = data_admissions %>%
  bind_rows() %>%
  mutate_at(vars(contains("_date")), as.Date, format = "%Y-%m-%d") %>%
  filter(admission_date <= discharge_date,
         !is.na(admission_date),
         !is.na(discharge_date)) %>%
  arrange(patient_id, admission_date)

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
  distinct(patient_id, admission_date, discharge_date,
           .keep_all = TRUE) %>%
  ungroup()


plot_admission_date = data_admissions %>% 
  ggplot(aes(admission_date)) +
  geom_histogram()

ggsave(filename = "admission_date.jpeg",
       plot = plot_admission_date,
       path = here::here("output", "admissions_plots"))

plot_discharge_date = data_admissions %>% 
  ggplot(aes(discharge_date)) +
  geom_histogram()

ggsave(filename = "discharge_date.jpeg",
       plot = plot_discharge_date,
       path = here::here("output", "admissions_plots"))


write_csv(extract_summary_admissions,
          here::here("output", "admissions_table", "extract_summary_admissions.csv"))
