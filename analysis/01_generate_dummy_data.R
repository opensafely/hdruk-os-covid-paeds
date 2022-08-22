# Load packages ----
library("tidyverse")
library("lubridate")
library("finalfit")

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

## Study dates ----
start_date     = ymd(global_var$start_date)
end_date       = ymd(global_var$end_date)
tp_start_date  = ymd(global_var$tp_start_date)
tp_end_date    = ymd(global_var$tp_end_date)
fup_start_date = ymd(global_var$fup_start_date)

## Max counts ----
n_admission = global_var$n_admission
n_outpatient = global_var$n_outpatient
n_gp = global_var$n_gp
n_positive_test = global_var$n_positive_test
n_negative_test = global_var$n_negative_test


# Load patient data and sample ----
data_patient = here::here("output", "input.csv.gz") %>% 
  read_csv(col_types = read_column_type(.)) 

# Create directory for dummy data ----
dir.create(here::here("output", "dummy_data"), showWarnings = FALSE, recursive=TRUE)

# Dummy data variables ----
n_max = 10000
incidence = 0.2
date_range = seq(start_date, end_date, by="day")
date_range_testing = seq(tp_start_date, end_date, by="day")
admission_method = c("11", "12", "13", "21", "22", "23", "24", "25", "2A", 
                     "2B", "2C", "2D", "28", "31", "32", "82", "83", "81")
primary_diagnosis = paste0(
  sample(LETTERS, 1000, replace = TRUE),
  sample(0:9, 1000, replace = TRUE),
  sample(0:9, 1000, replace = TRUE),
  sample(0:9, 1000, replace = TRUE)
)
treatment_function = c("100", "101", "102", "110", "242", "260", "280", "321",
                       "180", "190", "192", "300", "301", "302", "361", "711")

# Small sample from patient_data ----
data_patient = data_patient %>% 
  filter(patient_id %in% sample(data_patient$patient_id, 
                                min(n_max, nrow(.))))
n_patient = nrow(data_patient)

# Admission data ----
dummy_data_admissions = data_patient %>% 
  transmute(
    admission_date_1 = sample(date_range, n_patient, replace = TRUE),
    discharge_date_1 = sample(date_range, n_patient, replace = TRUE),
    admission_method_1 = sample(admission_method, n_patient, replace = TRUE),
    primary_diagnosis_1 = sample(primary_diagnosis, n_patient, replace = TRUE),
    treatment_function_1 = sample(treatment_function, n_patient, replace = TRUE),
    critical_care_days_1 = rpois(n_patient, 0.1),
    admission_date_2 = sample(date_range, n_patient, replace = TRUE),
    discharge_date_2 = sample(date_range, n_patient, replace = TRUE),
    admission_method_2 = sample(admission_method, n_patient, replace = TRUE),
    primary_diagnosis_2 = sample(primary_diagnosis, n_patient, replace = TRUE),
    treatment_function_2 = sample(treatment_function, n_patient, replace = TRUE),
    critical_care_days_2 = rpois(n_patient, 0.1),
    admission_date_3 = sample(date_range, n_patient, replace = TRUE),
    discharge_date_3 = sample(date_range, n_patient, replace = TRUE),
    admission_method_3 = sample(admission_method, n_patient, replace = TRUE),
    primary_diagnosis_3 = sample(primary_diagnosis, n_patient, replace = TRUE),
    treatment_function_3 = sample(treatment_function, n_patient, replace = TRUE),
    critical_care_days_3 = rpois(n_patient, 0.1),
    admission_date_4 = sample(date_range, n_patient, replace = TRUE),
    discharge_date_4 = sample(date_range, n_patient, replace = TRUE),
    admission_method_4 = sample(admission_method, n_patient, replace = TRUE),
    primary_diagnosis_4 = sample(primary_diagnosis, n_patient, replace = TRUE),
    treatment_function_4 = sample(treatment_function, n_patient, replace = TRUE),
    critical_care_days_4 = rpois(n_patient, 0.1),
    admission_date_5 = sample(date_range, n_patient, replace = TRUE),
    discharge_date_5 = sample(date_range, n_patient, replace = TRUE),
    admission_method_5 = sample(admission_method, n_patient, replace = TRUE),
    primary_diagnosis_5 = sample(primary_diagnosis, n_patient, replace = TRUE),
    treatment_function_5 = sample(treatment_function, n_patient, replace = TRUE),
    critical_care_days_5 = rpois(n_patient, 0.1),
    admission_date_6 = sample(date_range, n_patient, replace = TRUE),
    discharge_date_6 = sample(date_range, n_patient, replace = TRUE),
    admission_method_6 = sample(admission_method, n_patient, replace = TRUE),
    primary_diagnosis_6 = sample(primary_diagnosis, n_patient, replace = TRUE),
    treatment_function_6 = sample(treatment_function, n_patient, replace = TRUE),
    critical_care_days_6 = rpois(n_patient, 0.1),
    admission_date_7 = sample(date_range, n_patient, replace = TRUE),
    discharge_date_7 = sample(date_range, n_patient, replace = TRUE),
    admission_method_7 = sample(admission_method, n_patient, replace = TRUE),
    primary_diagnosis_7 = sample(primary_diagnosis, n_patient, replace = TRUE),
    treatment_function_7 = sample(treatment_function, n_patient, replace = TRUE),
    critical_care_days_7 = rpois(n_patient, 0.1),
    patient_id = patient_id,
  )

## Make dates consecutive, create missing values based on incidence ----
dummy_data_admissions = dummy_data_admissions %>%
  mutate_at(vars(starts_with(c("admission_date", "discharge_date", "critical_care_days"))),
            as.character) %>%
  pivot_longer(-patient_id,
               names_to = c("variable", "index"),
               names_pattern = "^(.*)_(\\d+)") %>% 
  pivot_wider(names_from = variable) %>% 
  mutate(admission_date_temp = admission_date %>% ymd(),
         discharge_date_temp = discharge_date %>% ymd()) %>% 
  group_by(patient_id) %>% 
  mutate(
    admission_date = sort(c(admission_date_temp, discharge_date_temp)
                          )[seq(1,2*n(), by = 2)] %>% 
      as.character(),
    discharge_date = sort(c(admission_date_temp, discharge_date_temp)
                          )[seq(2,2*n(), by = 2)] %>% 
      as.character(),
    ) %>% 
  ungroup() %>% 
  mutate(prob = runif(n())) %>% 
  filter(prob <= incidence) %>%
  select(-c(prob, admission_date_temp, discharge_date_temp)) %>% 
  group_by(patient_id) %>% 
  mutate(index = 1:n()) %>% 
  pivot_longer(-c(patient_id, index)) %>% 
  mutate(var_name = paste0(name, "_", index)) %>% 
  select(-c(name, index))

## Ensure valid number of rows and columns ----
dummy_data_admissions = tibble(
  patient_id = unique(data_patient$patient_id) %>% rep(each = 6*n_admission),
  var_name = rep(paste0(rep(c("admission_date_", "discharge_date_", "admission_method_",
                              "primary_diagnosis_", "treatment_function_", "critical_care_days_"), 7),
                    rep(c(1:n_admission), each = 6)),
                 length(unique(data_patient$patient_id)))
) %>% 
  left_join(dummy_data_admissions) %>% 
  pivot_wider(names_from = var_name, values_from = value) 

## Add admission count, relocate patient_id to last row ----
dummy_data_admissions = dummy_data_admissions %>% 
  mutate(admission_count = dummy_data_admissions %>%
           transmute(across(starts_with("admission_date"), ~ !is.na(.x))) %>%
           mutate(admission_count = reduce(select(., starts_with("admission_date")), `+`)) %>% 
           pull(admission_count)) %>% 
  relocate(patient_id, .after = last_col())

# Outpatient data ----
lambda_outpatient = 0.15
dummy_data_outpatient = data_patient %>% 
  transmute(
    outpatient_count_1 = rpois(n_patient, lambda = lambda_outpatient),
    outpatient_count_2 = rpois(n_patient, lambda = lambda_outpatient),
    outpatient_count_3 = rpois(n_patient, lambda = lambda_outpatient),
    outpatient_count_4 = rpois(n_patient, lambda = lambda_outpatient),
    outpatient_count_5 = rpois(n_patient, lambda = lambda_outpatient),
    outpatient_count_6 = rpois(n_patient, lambda = lambda_outpatient),
    outpatient_count_7 = rpois(n_patient, lambda = lambda_outpatient),
    outpatient_count_week = outpatient_count_1 + outpatient_count_2 +
      outpatient_count_3 + outpatient_count_4 + outpatient_count_5 +
      outpatient_count_6 + outpatient_count_7,
    patient_id = patient_id,
  )

# GP data ----
lambda_gp = 0.25
dummy_data_gp = data_patient %>% 
  transmute(
    gp_count_1 = rpois(n_patient, lambda = lambda_gp),
    gp_count_2 = rpois(n_patient, lambda = lambda_gp),
    gp_count_3 = rpois(n_patient, lambda = lambda_gp),
    gp_count_4 = rpois(n_patient, lambda = lambda_gp),
    gp_count_5 = rpois(n_patient, lambda = lambda_gp),
    gp_count_6 = rpois(n_patient, lambda = lambda_gp),
    gp_count_7 = rpois(n_patient, lambda = lambda_gp),
    gp_count_week = gp_count_1 + gp_count_2 + gp_count_3 + gp_count_4 + 
      gp_count_5 + gp_count_6 + gp_count_7,
    patient_id = patient_id,
  )

# Negative covid testing data ----
lambda_covid_negative = 0.15
dummy_data_testing_negative = data_patient %>% 
  transmute(
    covid_negative_test_count_1 = rpois(n_patient, lambda = lambda_covid_negative),
    covid_negative_test_count_2 = rpois(n_patient, lambda = lambda_covid_negative),
    covid_negative_test_count_3 = rpois(n_patient, lambda = lambda_covid_negative),
    covid_negative_test_count_4 = rpois(n_patient, lambda = lambda_covid_negative),
    covid_negative_test_count_5 = rpois(n_patient, lambda = lambda_covid_negative),
    covid_negative_test_count_6 = rpois(n_patient, lambda = lambda_covid_negative),
    covid_negative_test_count_7 = rpois(n_patient, lambda = lambda_covid_negative),
    covid_negative_test_week_count = covid_negative_test_count_1 + covid_negative_test_count_2 +
      covid_negative_test_count_3 + covid_negative_test_count_4 + covid_negative_test_count_5 +
      covid_negative_test_count_6 + covid_negative_test_count_7,
    patient_id = patient_id,
  )

# Positive covid testing data ----
lambda_covid_positive = 0.01
dummy_data_testing_positive = data_patient %>% 
  transmute(
    covid_positive_test_count_1 = rpois(n_patient, lambda = lambda_covid_positive),
    covid_positive_test_count_2 = rpois(n_patient, lambda = lambda_covid_positive),
    covid_positive_test_count_3 = rpois(n_patient, lambda = lambda_covid_positive),
    covid_positive_test_count_4 = rpois(n_patient, lambda = lambda_covid_positive),
    covid_positive_test_count_5 = rpois(n_patient, lambda = lambda_covid_positive),
    covid_positive_test_count_6 = rpois(n_patient, lambda = lambda_covid_positive),
    covid_positive_test_count_7 = rpois(n_patient, lambda = lambda_covid_positive),
    covid_positive_test_week_count = covid_positive_test_count_1 + covid_positive_test_count_2 +
      covid_positive_test_count_3 + covid_positive_test_count_4 + covid_positive_test_count_5 +
      covid_positive_test_count_6 + covid_positive_test_count_7,
    patient_id = patient_id,
  )
  
# Save dummy data ----
write_csv(dummy_data_admissions,       gzfile(here::here("output", "dummy_data", "dummy_data_admissions.csv.gz")))
write_csv(dummy_data_outpatient,       gzfile(here::here("output", "dummy_data", "dummy_data_outpatient.csv.gz")))
write_csv(dummy_data_gp,               gzfile(here::here("output", "dummy_data", "dummy_data_gp.csv.gz")))
write_csv(dummy_data_testing_negative, gzfile(here::here("output", "dummy_data", "dummy_data_testing_negative.csv.gz")))
write_csv(dummy_data_testing_positive, gzfile(here::here("output", "dummy_data", "dummy_data_testing_positive.csv.gz")))
