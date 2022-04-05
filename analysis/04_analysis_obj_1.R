



# Load library
library("tidyverse")
library("lubridate")
library("finalfit")

# Read processed data  ----
data_patient_2019 = read_rds(here::here("output", "data", "data_patient_2019.rds"))
data_patient_2020 = read_rds(here::here("output", "data", "data_patient_2020.rds"))
data_patient_2021 = read_rds(here::here("output", "data", "data_patient_2021.rds"))
data_admissions   = read_rds(here::here("output", "data", "data_admissions.rds"))
data_outpatient   = read_rds(here::here("output", "data", "data_outpatient.rds"))
data_gp           = read_rds(here::here("output", "data", "data_gp.rds"))

# Create summary table stratified by cohort year ----
dependent = "cohort_year"
explanatory = c("covid_status", "age", "age_factor", "sex", 
                "ethnicity", "ethnicity_6_sus", "ethnicity_comb",
                "region", "imd_Q5", "rural_urban",
                "asthma", "diabetes",
                "death_factor",
                "admission_count_factor", 
                "gp_contact_count_factor")

tbl_summary_cohort_year = data_patient_2019 %>% 
  select(all_of(c(dependent, explanatory))) %>% 
  bind_rows(data_patient_2020 %>% 
              select(all_of(c(dependent, explanatory)))) %>% 
  bind_rows(data_patient_2021 %>% 
              select(all_of(c(dependent, explanatory)))) %>% 
  summary_factorlist(dependent, explanatory, p = TRUE, 
                     add_col_totals = TRUE,
                     add_row_total = TRUE)

x = data_outpatient %>% 
  filter(patient_id %in% data_patient_2019$patient_id) %>% 
  filter(outpatient_date >= "2019-01-01") %>% 
  filter(outpatient_date <= "2019-12-31") %>% 
  mutate(week_index = week(outpatient_date),
         week_date  = as.Date("2019-01-01") + 7*(week_index - 1) + 3)

x %>% 
  count(week_date)
