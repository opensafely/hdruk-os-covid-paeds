##### Description ----
# This script:
# - imports data extracted by the cohort extractor
# - tidies missing values
# - removes bad dummy admission dates and reorders dates so no negative time 
#   differences (only alters data for dummy data)
# - fills in unknown ethnicity from GP records with ethnicity from SUS (secondary care)

# standardises some variables (eg convert to factor) and derives some new ones
# - saves: - processed one-row-per-patient dataset
#          - processed one-row-per-hospital admission dataset
#          - processed one-row-per-GP contact dataset

# Import libraries
library("tidyverse")
library("lubridate")
library("finalfit")

# Import globally defined variables
gbl_vars = jsonlite::fromJSON(
  txt="./analysis/global_variables.json"
)

# Create directory for processed data ----
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)

# Set column type based on column name ----
data_format = tibble(
  column_names = c(read_csv(
    here::here("output", "input.csv.gz"),
    n_max = 1,
    col_names = FALSE
  ))) %>% 
  mutate(column_type = case_when(
    column_names == "patient_id" ~ "i",
    column_names == "imd" ~ "i",
    str_detect(column_names, "age") ~ "d",
    str_detect(column_names, "_date") ~ "D",
    str_detect(column_names, "_count") ~ "i",
    TRUE ~ "c"
  ))

# Read data from csv ----
data_patient = read_csv(
  here::here("output", "input.csv.gz"),
  col_types = data_format %>% pull(column_type) %>% paste(collapse = "")
)

# Parse character NAs ----
data_patient = data_patient %>%
  mutate(across(
    .cols = where(is.character),
    .fns = ~na_if(.x, "")
  )) %>%
  arrange(patient_id) %>%
  select(patient_id, everything())

# Hospital admissions dataset ----
data_admissions = data_patient %>% 
  select(patient_id,
         starts_with(c("admission_date", "discharge_date", "admission_method"))) %>%
  mutate_at(vars(starts_with(c("admission_date", "discharge_date"))), as.character) %>% 
  pivot_longer(
    cols = -patient_id,
    names_to = c("variable", "index"),
    names_pattern = "^(.*)_(\\d+)",
    values_to = "data",
    values_drop_na = FALSE
  ) %>% 
  pivot_wider(
    names_from = variable,
    values_from = data
  ) %>% 
  mutate_at(vars(contains("_date")), as.Date, format = "%Y-%m-%d")

# Log number of bad admission rows ----
# Expect to be 0 for real data
log_admissions_filter = data_admissions %>% 
  summarise(n_discharge_before_admission = 
              sum(discharge_date < admission_date, na.rm=TRUE),
            n_missing_admission_date = sum(is.na(admission_date)),
            n_missing_discharge_date = sum(is.na(discharge_date)),
            n_missing_both_date = sum(is.na(admission_date) & is.na(discharge_date)))

# Filter out rows with bad admission dates ----
data_admissions = data_admissions %>% 
  filter(admission_date <= discharge_date,
         !is.na(admission_date),
         !is.na(discharge_date)) %>% 
  group_by(patient_id) %>% 
  arrange(patient_id, admission_date) %>% 
  mutate(index = row_number()) %>% 
  ungroup()

# Check and log if admission spells overlap ----
data_admissions = data_admissions %>%
  group_by(patient_id) %>% 
  mutate(overlap_with_prior =  
           case_when(admission_date < lag(discharge_date)~ 1,
                     TRUE ~ 0)) %>% 
  ungroup()

data_admissions_overlap = data_admissions %>% 
  filter(patient_id %in% 
           (data_admissions %>% 
            filter(overlap_with_prior == 1) %>% 
            pull(patient_id)))
  

log_admissions_filter = log_admissions_filter %>% 
  mutate(overlapping_spell = sum(data_admissions %>% 
                                      pull(overlap_with_prior)))

# Fix overlapping admission spells ----
data_admissions = data_admissions %>%
  mutate(index = row_number() - cumsum(overlap_with_prior)) %>%
  select(-overlap_with_prior) %>% 
  group_by(patient_id, index) %>% 
  mutate(admission_date = min(admission_date),
         discharge_date = max(discharge_date)) %>%
  slice(1) %>% 
  ungroup()

# Outpatient dataset ----
data_outpatient = data_patient %>% 
  select(patient_id, starts_with("outpatient_date_")) %>%
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
  ) %>% 
  mutate_at(vars(contains("_date")), as.Date, format = "%Y-%m-%d") %>% 
  group_by(patient_id) %>% 
  arrange(patient_id, outpatient_date) %>% 
  mutate(index = row_number()) %>% 
  ungroup()

# GP contact dataset ----
data_gp = data_patient %>% 
  select(patient_id, starts_with("gp_contact_date_")) %>%
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
  ) %>% 
  mutate_at(vars(contains("_date")), as.Date, format = "%Y-%m-%d") %>% 
  group_by(patient_id) %>% 
  arrange(patient_id, gp_contact_date) %>% 
  mutate(index = row_number()) %>% 
  ungroup()

# Remove admission, outpatient and gp columns from data_patient ----
data_patient = data_patient %>% 
  select(-starts_with(c("admission_date", "discharge_date", "admission_method"))) %>% 
  select(-starts_with("outpatient_date_")) %>% 
  select(-starts_with("gp_contact_date_"))

# Create factors and label variables -----
data_patient = data_patient %>% 
  mutate(
    
    date_of_birth = if_else(is.na(date_of_birth),
                            NA_character_,
                            paste0(date_of_birth, "-15")) %>% 
      as.Date(),
    
    age = age %>% 
      ff_label("Age (years)"),
    
    age_factor = cut(age, 
                     breaks = c(4, 10, 15, Inf),
                     labels = c("4-9", "10-14", "15+"))%>%
      factor(levels = c("4-9", "10-14", "15+")) %>% 
      ff_label("Age group (years)"),
    
    sex = case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      TRUE ~ NA_character_
    ) %>% 
      factor() %>% 
      ff_label("Sex"),
    
    ethnicity = case_when(
      ethnicity == "1" ~ "White",
      ethnicity == "4" ~ "Black",
      ethnicity == "3" ~ "South Asian",
      ethnicity == "2" ~ "Mixed",
      ethnicity == "5" ~ "Other",
      TRUE ~ NA_character_
    ) %>% 
      factor() %>% 
      ff_label("Ethnicity (primary care)"),
    
    ethnicity_6_sus = case_when(
      ethnicity_6_sus == "1" ~ "White",
      ethnicity_6_sus == "4" ~ "Black",
      ethnicity_6_sus == "3" ~ "South Asian",
      ethnicity_6_sus == "2" ~ "Mixed",
      ethnicity_6_sus == "5" ~ "Other",
      TRUE ~ NA_character_
    ) %>% 
      factor() %>% 
      ff_label("Ethnicity (SUS)"),
    
    ethnicity_comb = coalesce(ethnicity, ethnicity_6_sus) %>% 
      ff_label("Ethnicity"),
    
    region = region %>%
      factor() %>% 
      ff_label("Region"),
    
    imd_Q5 = case_when(
      (imd >=1)          & (imd < 32844*1/5) ~ "(most deprived) 1",
      (imd >= 32844*1/5) & (imd < 32844*2/5) ~ "2",
      (imd >= 32844*2/5) & (imd < 32844*3/5) ~ "3",
      (imd >= 32844*3/5) & (imd < 32844*4/5) ~ "4",
      (imd >= 32844*4/5)                     ~ "(least deprived) 5",
      TRUE ~ NA_character_
      ) %>% 
      factor(levels = c("(most deprived) 1", "2", "3", "4", "(least deprived) 5")) %>% 
      ff_label("Multiple deprivation quintile"),
    
    rural_urban_group = case_when(
      rural_urban %in% c(1,2)     ~ "Urban conurbation",
      rural_urban %in% c(3,4)     ~ "Urban city or town",
      rural_urban %in% c(5,6,7,8) ~ "Rural town or village",
      TRUE                        ~ NA_character_
    ) %>%
      factor() %>% 
      ff_label("Rural-urban classification"),
    
    admission_count_factor = case_when(
      admission_count == 0 ~ "0",
      admission_count == 1 ~ "1",
      admission_count > 1  ~ "2+",
      TRUE                 ~ NA_character_
    ) %>%
      factor() %>% 
      ff_label("Hospital admissions"),
    
    gp_contact_count_factor = case_when(
      gp_contact_count == 0        ~ "0",
      gp_contact_count %in% c(1,2) ~ "1-2",
      gp_contact_count > 2         ~ "3+",
      TRUE                         ~ NA_character_
    ) %>%
      factor() %>% 
      ff_label("GP interactions"),
    
    covid_status = case_when(
      !is.na(covid_positive_test_date_1) ~ "SARS-CoV-2 positive",
      !is.na(covid_negative_test_date_1) ~ "SARS-CoV-2 negative",
      TRUE ~ "Untested"
    ) %>%
      factor() %>% 
      ff_label("COVID status"),
    
    death_factor = case_when(
      !is.na(death_date) ~ "Dead",
      TRUE               ~ "Alive"
    ) %>% 
      factor() %>% 
      ff_label("Death"),
    
    # diabetes = diabetes %>% 
    #   factor() %>% 
    #   ff_label("Diabetes"),
    # 
    # asthma = asthma %>% 
    #   factor() %>% 
    #   ff_label("Asthma")
    )

# Define potential nosocomial infection ----
# Defined as a positive covid test on or after day 7 and on or before day of 
# discharge
data_patient = data_patient %>% 
  left_join(
    data_admissions %>% 
      left_join(data_patient %>% select(patient_id,
                                        covid_positive_test_date_1),
                by = "patient_id") %>% 
      mutate(covid_nosocomial = if_else(
        (admission_date + days(7) <= covid_positive_test_date_1) &
          (discharge_date >= covid_positive_test_date_1),
        "Yes", NA_character_) %>% 
          ff_label("Nosocomial infection")) %>%
      filter(covid_nosocomial == "Yes") %>% 
      select(patient_id, covid_nosocomial),
    by = "patient_id"
  )

# Discrepant test result ----
data_patient = data_patient %>% 
  mutate(covid_discrepant_test = if_else(
    covid_positive_test_date_1 == covid_negative_test_date_before_positive,
    "Yes",
    NA_character_
  ))

# Save rds ----
write_rds(data_patient,
          here::here("output", "data", "data_patient.rds"),
          compress="gz")

write_rds(data_admissions,
          here::here("output", "data", "data_admissions.rds"),
          compress="gz")

write_rds(data_outpatient,
          here::here("output", "data", "data_outpatient.rds"),
          compress="gz")

write_rds(data_gp,
          here::here("output", "data", "data_gp.rds"),
          compress="gz")

# Save csv ----
write_csv(log_admissions_filter,
          here::here("output", "data", "log_admissions_filter.csv"))

write_csv(data_admissions_overlap,
          here::here("output", "data", "overlap_admissions.csv"))

