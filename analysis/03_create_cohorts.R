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

# General exclusions for all cohorts ----
data_patient = data_patient %>% 
  mutate(
    excl_general = case_when(
      covid_nosocomial == "Yes" ~ "Probable nosocomial infection",
      covid_discrepant_test == "Yes" ~ "Discrepant same-day RT-PCR result",
      TRUE ~ NA_character_),
    
    cohort_general = if_else(is.na(excl_general), patient_id, NA_integer_))

# Define index dates ----
list_index_dates = c("2019-01-01", "2020-01-01", "2021-01-01") %>% as_date()

# Create new data with age, demographics and comorbidities indexed ----
list_indexed_data = list_index_dates %>% 
  map(function(index_date){
    data_index = data_patient %>% 
      mutate(
        
        cohort_year = year(index_date) %>%
          as.character() %>% 
          ff_label("Cohort"),
        
        age = ((index_date - date_of_birth)/365.25) %>%
          as.numeric() %>% 
          ff_label("Age (years)"),
        
        age_factor = cut(age, 
                         breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
                         labels = c("Below 4", "4-6", "7-10", "11-14", "15-17", "18+"))%>%
          factor(levels = c("Below 4", "4-6", "7-10", "11-14", "15-17", "18+")) %>% 
          ff_label("Age group (years)"),
        
        # Comorbidities
        asthma = if_else(!is.na(asthma_date) & (asthma_date < index_date),
                         "Yes", "No") %>% 
          ff_label("Asthma"),
        
        diabetes = if_else(!is.na(diabetes_date) & (diabetes_date < index_date),
                           "Yes", "No") %>% 
          ff_label("Diabetes"),
        
        # Exclusions
        excl_index = case_when(
          index_date > death_date ~ paste0("Died prior to\n ", format(index_date, "%d %B %Y")),
          age <=4 ~ paste0("Age 4 years or below\n on ", format(index_date, "%d %B %Y")),
          age >=18 ~ paste0("Age 18 years or above\n on ", format(index_date, "%d %B %Y")),
          TRUE ~ NA_character_
        ),
        
        # Cohort
        cohort_index = if_else(is.na(excl_general) & is.na(excl_index),
                               patient_id, NA_integer_)
        
      ) %>% 
      rename(imd = paste0("imd_", year(index_date)),
             imd_Q5 = paste0("imd_Q5_", year(index_date)),
             rural_urban = paste0("rural_urban_", year(index_date)),
             region = paste0("region_", year(index_date))
             ) %>% 
      select(-contains("_2019"), -contains("_2020"), -contains("_2021"))
    
    # Create consort diagrams ----
    consort_diagram = consort_plot(
      data = data_index,
      orders = c(patient_id       = "OpenSAFELY extract population: alive, registered with GP\n and age between 1 and 18 years on 01 January 2019",
                 excl_general     = "Excluded",
                 cohort_general   = "Extract population excluding probable nosocomial infection\n and discrepant same-day RT-PCR test results",
                 excl_index       = "Excluded",
                 cohort_index     = paste0(year(index_date), " cohort: Age between 4 and 18 years\n on ", format(index_date, "%d %B %Y"))),
      side_box = c("excl_general", "excl_index"),
      cex = 0.9)
    
    # Save consort diagrams ----
    ggsave(paste0("obj_1_consort_", year(index_date), ".jpeg"),
           plot = consort_diagram,
           width = 10, height = 4, units = "in",
           device = "jpeg",
           path = here::here("output", "descriptive", "plots"))
    
    # Filter out excluded patients ----
    data_index = data_index %>% 
      filter(!is.na(cohort_index))
    
    # Save indexed patient data as rds ----
    write_rds(data_index,
              here::here("output", "data", paste0("data_patient_", year(index_date), ".rds")),
              compress="gz")
    
    return(data_index)
  })
