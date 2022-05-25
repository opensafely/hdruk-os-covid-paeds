
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_functions.R"))

# Load datasets ----
data_patient = readRDS(here::here("output", "data", "data_patient.rds"))


# Cohort criteria ----
criteria_2019 = data_patient %>% 
  transmute(
    patient_id = patient_id,
    covid_status = covid_status,
    is_alive = is.na(death_date) | death_date > ymd("2019-01-01"), 
    isnot_nosocomial = covid_nosocomial == "No",
    isnot_descrepant_result = covid_discrepant_test == "No",
    is_age_4_or_more = age_2019 >= 4,
    is_age_under_18 = age_2019 < 18,
    include = isnot_nosocomial & isnot_descrepant_result &
      is_age_4_or_more & is_age_under_18
  )

data_cohort_2019 = criteria_2019 %>%
  filter(include) %>%
  select(patient_id) %>%
  left_join(data_patient, by="patient_id") %>%
  droplevels()


data_inclusioncriteria = criteria_2019 %>%
  transmute(
    patient_id,
    covid_status,
    c0 = TRUE,
    c1 = c0 & is_alive,
    c2 = c1 & isnot_nosocomial,
    c3 = c2 & (isnot_descrepant_result),
    c4 = c3 & (is_age_4_or_more & is_age_under_18),
  )

data_flowchart <-
  data_inclusioncriteria %>%
  select(-patient_id) %>%
  group_by(covid_status) %>%
  summarise(
    across(.fns=sum)
  ) %>%
  pivot_longer(
    cols=-covid_status,
    names_to="criteria",
    values_to="n"
  ) %>%
  group_by(covid_status) %>%
  mutate(
    n_exclude = lag(n) - n,
    pct_exclude = n_exclude/lag(n),
    pct_all = n / first(n),
    pct_step = n / lag(n),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "OpenSAFELY extract: Registered with GP between 1st January 2019 and 1st May 2022, and alive with age between 1 and 18 years on 01 January 2019",
      crit == "c1" ~ "  is alive on 1st January 2019",
      crit == "c2" ~ "  with no probable nosocomial infection",
      crit == "c3" ~ "  with no same-day discrepent RT-PCR test result",
      crit == "c4" ~ "  with age between 4 and 17 years",
      TRUE ~ NA_character_
    )
  ) #




data_inclusioncriteria <- criteria_2019 %>%
  transmute(
    patient_id,
    vax3_type,
    c0 = vax1_afterfirstvaxdate & vax3_afterstartdate & vax3_beforeenddate & has_expectedvax3type,
    c1 = c0 & (has_age & has_sex & has_imd & has_ethnicity & has_region),
    c2 = c1 & (has_vaxgap12 & has_vaxgap23 & has_knownvax1 & has_knownvax2 & vax12_homologous),
    c3 = c2 & (isnot_hscworker ),
    c4 = c3 & (isnot_carehomeresident & isnot_endoflife & isnot_housebound),
    c5 = c4 & (has_norecentcovid),
    c6 = c5 & (isnot_inhospitalunplanned),
  ) %>%
  filter(c0)
