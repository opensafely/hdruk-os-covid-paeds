# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 10_BayesMCC_prep_resource.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#

# Load packages ----
library(tidyverse)

# Load data ----
data_resource = read_rds(here::here("output", "data", "data_resource.rds"))

## Create used service column ----
data_resource = data_resource %>%
  mutate(service = case_when(
    n_critical_care > 0 ~ "CC",
    n_beddays > 0 & n_critical_care == 0 ~ "BD",
    n_outpatient > 0 & (n_beddays == 0 & n_critical_care == 0) ~ "OP",
    n_gp > 0 & (n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0) ~ "Contact",
    n_gp == 0 & n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0 ~ "None") %>% 
      factor() %>% 
      fct_relevel("None", "Contact", "OP", "BD", "CC")
  )

## Filter out rows with dead status ----
data_resource = data_resource %>% 
  filter(alive == TRUE)

# Save resource data for time series clustering ----
write_rds(data_resource,
          here::here("output", "data", "data_resource_mcc.rds"))