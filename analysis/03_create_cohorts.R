# This script takes the processed data and filters patients based on the 
# exclusion and objective-specific inclusion criteria as set out in the 
# protocol
#
# - input:

# Load library
library("tidyverse")
library("lubridate")
library("consort")

# Read processed data  ----
data_patient =    read_rds(here::here("output", "data", "data_patient.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))


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
        1, 
        NA_real_
      )) %>% 
      select(patient_id, covid_nosocomial),
    by = "patient_id"
  )

# Discrepant test result
data_patient = data_patient %>% 
  mutate(covid_discrepant_test = if_else(
    covid_positive_test_date_1 == covid_negative_test_date_before_positive,
    1,
    NA_real_
  ))



