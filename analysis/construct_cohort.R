# Define potential nosocomial infection ----
# Defined as a positive covid test after day 7 in hospital and on or before 7th
# day following discharge
data_patient = data_patient %>%
  left_join(
    data_admissions %>%
      left_join(data_patient %>% select(patient_id,
                                        covid_positive_test_date_1),
                by = "patient_id") %>%
      mutate(
        covid_nosocomial = case_when(
          # Length of stay less than 7 days: Not nosocomial 
          (discharge_date - admission_date) < 7 ~ NA_character_,
          # Length of stay 7+ days: Nosocomial if positive after day 7 in hospital
          # and on or before day 7 following discharge, otherwise not nosocomial
          (admission_date + days(7) < covid_positive_test_date_1) &
            (discharge_date + days(7) >= covid_positive_test_date_1) ~ "Yes",
          TRUE ~ NA_character_
        ) %>%
          ff_label("Nosocomial infection")) %>%
      filter(covid_nosocomial == "Yes") %>%
      select(patient_id, covid_nosocomial) %>%
      group_by(patient_id) %>%
      slice(1) %>%
      ungroup(),
    by = "patient_id"
  )

# Discrepant test result ----
data_patient = data_patient %>%
  mutate(covid_discrepant_test = if_else(
    covid_positive_test_date_1 == covid_negative_test_date_before_positive,
    "Yes",
    NA_character_
  ))

# Construct cohorts ----
data_patient = data_patient %>%
  mutate(
    excl_cohort_2019 = case_when(
      covid_nosocomial == "Yes" ~ "Probable nosocomial infection",
      covid_discrepant_test == "Yes" ~ "Discrepant same-day RT-PCR result",
      age_2019_factor == "Under 4" ~ "Age 4 years or below on 1st Jan 2019",
      age_2019_factor == "18+" ~ "Age 18 years or above on 1st Jan 2019",
      TRUE ~ NA_character_
    ),
    excl_cohort_2020 = case_when(
      covid_nosocomial == "Yes" ~ "Probable nosocomial infection",
      covid_discrepant_test == "Yes" ~ "Discrepant same-day RT-PCR result",
      age_2020_factor == "Under 4" ~ "Age 4 years or below on 1st Jan 2020",
      age_2020_factor == "18+" ~ "Age 18 years or above on 1st Jan 2020",
      TRUE ~ NA_character_
    ),
    excl_cohort_2021 = case_when(
      covid_nosocomial == "Yes" ~ "Probable nosocomial infection",
      covid_discrepant_test == "Yes" ~ "Discrepant same-day RT-PCR result",
      age_2021_factor == "Under 4" ~ "Age 4 years or below on 1st Jan 2021",
      age_2021_factor == "18+" ~ "Age 18 years or above on 1st Jan 2021",
      TRUE ~ NA_character_
    ),
  )