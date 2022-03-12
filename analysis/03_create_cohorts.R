# This script takes the processed data and filters patients based on the 
# exclusion and objective-specific inclusion criteria as set out in the 
# protocol
#
# - input:

# Load library
library("tidyverse")
library("lubridate")
library("finalfit")
library("consort")

# Read processed data  ----
data_patient =    read_rds(here::here("output", "data", "data_patient.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))

# Exclusions for each cohort ----
data_patient = data_patient %>% 
  mutate(
    excl_general = case_when(
      covid_nosocomial == "Yes" ~ "Probable nosocomial infection",
      covid_discrepant_test == "Yes" ~ "Discrepant same-day RT-PCR result",
      TRUE ~ NA_character_),
    
    cohort_general = if_else(is.na(excl_general), patient_id, NA_integer_))

# Define index date ----
list_index_dates = c("2019-01-01", "2020-01-01")

# Create new data with age and comorbidities indexed to index date
list_data = list_index_dates %>% 
  map(function(index_date){
    data_index = data_patient %>% 
      mutate(
        
        cohort_year = year(index_date) %>% 
          ff_label("Cohort year"),
        
        age = ((as_date(index_date) - date_of_birth)/365.25) %>%
          as.numeric() %>% 
          ff_label("Age (years)"),
        
        age_factor = cut(age2019, 
                         breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
                         labels = c("Below 4", "4-7", "8-11", "12-15", "15-17", "18+"))%>%
          factor(levels = c("Below 4", "4-7", "8-11", "12-15", "15-17", "18+")) %>% 
          ff_label("Age group (years)"),
        
        
        
        excl_index = case_when(
          as_date(index_date) > death_date ~ paste0("Died prior to ", index_date),
          age <=4 ~ paste0("Age 4 years or below\non ", index_date),
          age >=18 ~ paste0("Age 18 years or above on ", index_date),
          TRUE ~ NA_character_
        ),
        
        cohort_index = if_else(is.na(excl_general) & is.na(excl_index),
                               patient_id, NA_integer_)
        
      )
  })




# Exclusions for each cohort ----
data_patient = data_patient %>% 
  mutate(
    
    excl_2019 = case_when(
      as_date("2019-01-01") > death_date ~ "Died prior to 01/01/2019",
      age2019 <=4 ~ "Age 4 years or below\non 01/01/2019",
      age2019 >=18 ~ "Age 18 years or above on 01/01/2019",
      TRUE ~ NA_character_
    ),
    
    cohort_2019 = if_else(is.na(excl_general) & is.na(excl_2019),
                          patient_id, NA_integer_),
    
    excl_2020 = case_when(
      as_date("2020-01-01") > death_date ~ "Died prior to 01/01/2020",
      age2020 <=4 ~ "Age 4 years or below on 01/01/2020",
      age2020 >=18 ~ "Age 18 years or above on 01/01/2020",
      TRUE ~ NA_character_
    ),
    
    cohort_2020 = if_else(is.na(excl_general) & is.na(excl_2020),
                          patient_id, NA_integer_),
    
    excl_2021 = case_when(
      as_date("2021-01-01") > death_date ~ "Died prior to 01/01/2021",
      age2021 <=4 ~ "Age 4 years or below on 01/01/2021",
      age2021 >=18 ~ "Age 18 years or above on 01/01/2021",
      TRUE ~ NA_character_
    ),
    
    cohort_2021 = if_else(is.na(excl_general) & is.na(excl_2021),
                          patient_id, NA_integer_)
  )

# Create data for consort diagrams ----
consort2019 = consort_plot(data = data_patient,
                   orders = c(patient_id       = "OpenSAFELY extract population: alive, registered with GP\n and age between 1 and 18 years on 1/1/2019",
                              excl_general     = "Excluded",
                              cohort_general   = "Extract population excluding probable nosocomial infection\n and discrepant same-day RT-PCR test results",
                              excl_2019         = "Excluded",
                              cohort_2019       = "2019 cohort: Age between 4 and 18 years on 01/01/2019"),
                   side_box = c("excl_general", "excl_2019"),
                   cex = 0.9)

consort2020 = consort_plot(data = data_patient,
                           orders = c(patient_id       = "OpenSAFELY extract population: alive, registered with GP\n and age between 1 and 18 years on 1/1/2019",
                                      excl_general     = "Excluded",
                                      cohort_general   = "Extract population excluding probable nosocomial infection\n and discrepant same-day RT-PCR test results",
                                      excl_2020         = "Excluded",
                                      cohort_2020       = "2020 cohort: Age between 4 and 18 years on 01/01/2020"),
                           side_box = c("excl_general", "excl_2020"),
                           cex = 0.9)

consort2021 = consort_plot(data = data_patient,
                           orders = c(patient_id       = "OpenSAFELY extract population: alive, registered with GP\n and age between 1 and 18 years on 1/1/2019",
                                      excl_general     = "Excluded",
                                      cohort_general   = "Extract population excluding probable nosocomial infection\n and discrepant same-day RT-PCR test results",
                                      excl_2021         = "Excluded",
                                      cohort_2021       = "2021 cohort: Age between 4 and 18 years on 01/01/2021"),
                           side_box = c("excl_general", "excl_2021"),
                           cex = 0.9)

# Save consort diagrams
ggsave(paste0("obj_1_consort_2019.jpeg"),
       plot = consort2019,
       width = 10, height = 4, units = "in",
       device = "jpeg",
       path = here::here("output", "descriptive", "plots"))

ggsave(paste0("obj_1_consort_2020.jpeg"),
       plot = consort2020,
       width = 10, height = 4, units = "in",
       device = "jpeg",
       path = here::here("output", "descriptive", "plots"))

ggsave(paste0("obj_1_consort_2021.jpeg"),
       plot = consort2021,
       width = 10, height = 4, units = "in",
       device = "jpeg",
       path = here::here("output", "descriptive", "plots"))


