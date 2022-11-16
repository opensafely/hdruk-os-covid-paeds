# Functions ----

# Applies general exclusion criteria to cohort
apply_exclusion_criteria = function(.data_patient){
  .data_patient  %>% 
    filter(
      (age >= 4) & (age < 18),
      covid_nosocomial == "No",
      covid_discrepant_test == "No",
      death == "No"
    )
}

# Calculates age and age group at index date
calc_age = function(.data_patient, index_date){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    mutate(
      age = time_length(
        interval(date_of_birth, index_date),
        unit = "years"
      ) %>% 
        ff_label("Age (years)"),
      age_group = age %>%
        cut(
          breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
          labels = c("Under 4", "4-6", "7-10", "11-14", "15-17", "18+"),
          right = FALSE
        ) %>% 
        ff_label("Age group (years)")
    )
}

# Calculates comorbidity status given index date
calc_comorbidity_status = function(.data_patient, index_date,
                                   years_from_last = 5){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    mutate(
      
      # 1. Mental health disorders ----
      ## Mental illness ----
      mental_illness = case_when(
        (mental_illness_first_date < index_date) &
          ((mental_illness_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Mental illness"),
      
      mental_illness_icd10 = case_when(
        (mental_illness_first_date_icd10 < index_date) &
          ((mental_illness_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Mental illness (ICD-10)"),
      
      ## Severe mental illness ----
      severe_mental_illness = case_when(
        (severe_mental_illness_first_date < index_date) &
          ((severe_mental_illness_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Severe mental illness"),
      
      severe_mental_illness_icd10 = case_when(
        (severe_mental_illness_first_date_icd10 < index_date) &
          ((severe_mental_illness_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Severe mental illness (ICD-10)"),
      
      ## Mental health disorders ----
      mental_health_disorders = case_when(
        mental_illness == "Yes" ~ "Yes",
        severe_mental_illness == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Mental health disorders"),
      
      mental_health_disorders_icd10 = case_when(
        mental_illness == "Yes" ~ "Yes",
        severe_mental_illness == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Mental health disorders (ICD-10)"),
      
      # 2. Neurodevelopmental and behavioural conditions ----
      neurodevelopmental_and_behavioural = case_when(
        (behavioural_and_developmental_including_autism_first_date < index_date) &
          ((behavioural_and_developmental_including_autism_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Neurodevelopmental and behavioural conditions"),
      
      neurodevelopmental_and_behavioural_icd10 = case_when(
        (behavioural_and_developmental_including_autism_first_date_icd10 < index_date) &
          ((behavioural_and_developmental_including_autism_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Neurodevelopmental and behavioural conditions (ICD-10)"),
      
      # 3. Asthma ----
      asthma = case_when(
        (asthma_first_date < index_date) &
          ((asthma_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Asthma"),
      
      asthma_icd10 = case_when(
        (asthma_first_date_icd10 < index_date) &
          ((asthma_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Asthma (ICD-10)"),
      
      # 4. Cystic fibrosis ----
      cystic_fibrosis = case_when(
        (cystic_fibrosis_first_date < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cystic fibrosis"),
      
      cystic_fibrosis_icd10 = case_when(
        (cystic_fibrosis_first_date_icd10 < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cystic fibrosis (ICD-10)"),
      
      # 5. Other respiratory ----
      ## Congenital respiratory conditions ----
      resp_congenital = case_when(
        (resp_congenital_first_date < index_date) &
          ((resp_congenital_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital respiratory conditions"),
      
      resp_congenital_icd10 = case_when(
        (resp_congenital_first_date_icd10 < index_date) &
          ((resp_congenital_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital respiratory conditions (ICD-10)"),
      
      ## Respiratory devices ----
      resp_devices = case_when(
        (resp_devices_first_date < index_date) &
          ((resp_devices_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Respiratory devices"),
      
      resp_devices_icd10 = case_when(
        (resp_devices_first_date_icd10 < index_date) &
          ((resp_devices_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Respiratory devices (ICD-10)"),
      
      ## Respiratory (not asthma or cystic fibrosis) ----
      respiratory_not_asthma_or_cf = case_when(
        (respiratory_not_asthma_or_cf_first_date < index_date) &
          ((respiratory_not_asthma_or_cf_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Respiratory conditions (not asthma or cystic fibrosis)"),
      
      respiratory_not_asthma_or_cf_icd10 = case_when(
        (respiratory_not_asthma_or_cf_first_date_icd10 < index_date) &
          ((respiratory_not_asthma_or_cf_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Respiratory conditions (not asthma or cystic fibrosis) (ICD-10)"),
      
      ## Other respiratory ----
      other_respiratory = case_when(
        resp_congenital == "Yes" ~ "Yes",
        resp_devices == "Yes" ~ "Yes",
        respiratory_not_asthma_or_cf == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Other respiratory conditions"),
      
      other_respiratory_icd10 = case_when(
        resp_congenital_icd10 == "Yes" ~ "Yes",
        resp_devices_icd10 == "Yes" ~ "Yes",
        respiratory_not_asthma_or_cf_icd10 == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Other respiratory conditions (ICD-10)"),
      
      # 6. Cardiovascular conditions ----
      ## Cardiovascular congenital ----
      cardiovascular_congenital = case_when(
        (cardiovascular_congenital_first_date < index_date) &
          ((cardiovascular_congenital_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital cardiovascular conditions"),
      
      cardiovascular_congenital_icd10 = case_when(
        (cardiovascular_congenital_first_date_icd10 < index_date) &
          ((cardiovascular_congenital_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital cardiovascular conditions (ICD-10)"),
      
      ## Cardiovascular devices ----
      cardiovascular_devices = case_when(
        (cardiovascular_devices_first_date < index_date) &
          ((cardiovascular_devices_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cardiovascular devices"),
      
      cardiovascular_devices_icd10 = case_when(
        (cardiovascular_devices_first_date_icd10 < index_date) &
          ((cardiovascular_devices_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cardiovascular devices (ICD-10)"),
      
      ## Cardiovascular (non-congenital) ----
      cardiovascular_non_congenital = case_when(
        (cardiovascular_non_congenital_first_date < index_date) &
          ((cardiovascular_non_congenital_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Non-congenital cardiovascular conditions"),
      
      cardiovascular_non_congenital_icd10 = case_when(
        (cardiovascular_non_congenital_first_date_icd10 < index_date) &
          ((cardiovascular_non_congenital_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Non-congenital cardiovascular conditions (ICD-10)"),
      
      ## Cardiovascular conditions ----
      cardiovascular = case_when(
        cardiovascular_congenital == "Yes" ~ "Yes",
        cardiovascular_devices == "Yes" ~ "Yes",
        cardiovascular_non_congenital == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cardiovascular conditions"),
      
      cardiovascular_icd10 = case_when(
        cardiovascular_congenital_icd10 == "Yes" ~ "Yes",
        cardiovascular_devices_icd10 == "Yes" ~ "Yes",
        cardiovascular_non_congenital_icd10 == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cardiovascular conditions (ICD-10)"),
      
      # 7. Epilepsy ----
      epilepsy = case_when(
        (epilepsy_first_date < index_date) &
          ((epilepsy_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Epilepsy"),
      
      epilepsy_icd10 = case_when(
        (epilepsy_first_date_icd10 < index_date) &
          ((epilepsy_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Epilepsy (ICD-10)"),
      
      # 8. Headaches ----
      headaches = case_when(
        (headaches_first_date < index_date) &
          ((headaches_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Headaches"),
      
      headaches_icd10 = case_when(
        (headaches_first_date_icd10 < index_date) &
          ((headaches_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Headaches (ICD-10)"),
      
      # 9. Other neurological ---- 
      ## Cerebral palsy or paralysis ----
      cerebral_palsy_paralysis = case_when(
        (cerebral_palsy_paralysis_first_date < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cerebral palsy and paralysis"),
      
      cerebral_palsy_paralysis_icd10 = case_when(
        (cerebral_palsy_paralysis_first_date_icd10 < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cerebral palsy and paralysis (ICD-10)"),
      
      ## Congenital neuro ----
      congenital_neuro = case_when(
        (congenital_neuro_first_date < index_date) &
          ((congenital_neuro_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital neurological conditions"),
      
      congenital_neuro_icd10 = case_when(
        (congenital_neuro_first_date_icd10 < index_date) &
          ((congenital_neuro_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital neurological conditions (ICD-10)"),
      
      ## Neuro devices ----
      neuro_devices = case_when(
        (neuro_devices_first_date < index_date) &
          ((neuro_devices_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Neurological devices"),
      
      neuro_devices_icd10 = case_when(
        (neuro_devices_first_date_icd10 < index_date) &
          ((neuro_devices_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Neurological devices (ICD-10)"),
      
      ## Neurological (no epilepsy, cp or headaches) ----
      neurological_no_epilepsy_or_cp_headaches = case_when(
        (neurological_no_epilepsy_or_cp_headaches_first_date < index_date) &
          ((neurological_no_epilepsy_or_cp_headaches_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Neurological conditions excluding epilepsy, cerebral palsy and headaches"),
      
      
      neurological_no_epilepsy_or_cp_headaches_icd10 = case_when(
        (neurological_no_epilepsy_or_cp_headaches_first_date_icd10 < index_date) &
          ((neurological_no_epilepsy_or_cp_headaches_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Neurological conditions excluding epilepsy, cerebral palsy and headaches (ICD-10)"),
      
      ## Other neurological ----
      other_neurological = case_when(
        cerebral_palsy_paralysis == "Yes" ~ "Yes",
        congenital_neuro == "Yes" ~ "Yes",
        neuro_devices == "Yes" ~ "Yes",
        neurological_no_epilepsy_or_cp_headaches == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Other neurological conditions"),
      
      other_neurological_icd10 = case_when(
        cerebral_palsy_paralysis_icd10 == "Yes" ~ "Yes",
        congenital_neuro_icd10 == "Yes" ~ "Yes",
        neuro_devices_icd10 == "Yes" ~ "Yes",
        neurological_no_epilepsy_or_cp_headaches_icd10 == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Other neurological conditions (ICD-10)"),
      
      # 10. Gastrointestinal conditions ----
      ## Gastrointestinal (non-device) ----
      gastrointestinal_non_device = case_when(
        (gastrointestinal_first_date < index_date) &
          ((gastrointestinal_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Gastrointestinal (non-device)"),
      
      gastrointestinal_non_device_icd10 = case_when(
        (gastrointestinal_first_date_icd10 < index_date) &
          ((gastrointestinal_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Gastrointestinal (non-device) (ICD-10)"),
      
      ## Gastrointestinal (devices) ----
      gastrointestinal_devices = case_when(
        (gastrointestinal_devices_first_date < index_date) &
          ((gastrointestinal_devices_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Gastrointestinal (device)"),
      
      gastrointestinal_devices_icd10 = case_when(
        (gastrointestinal_devices_first_date_icd10 < index_date) &
          ((gastrointestinal_devices_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Gastrointestinal (device) (ICD-10)"),
      
      ## Gastrointestinal conditions ----
      gastrointestinal_conditions = case_when(
        gastrointestinal_non_device == "Yes" ~ "Yes",
        gastrointestinal_devices == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Gastrointestinal conditions"),
      
      gastrointestinal_conditions_icd10 = case_when(
        gastrointestinal_non_device_icd10 == "Yes" ~ "Yes",
        gastrointestinal_devices_icd10 == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Gastrointestinal conditions (ICD-10)"),
      
      # 11. Genitourinary conditions ----
      ## Congenital renal ----
      congenital_renal = case_when(
        (congenital_renal_first_date < index_date) &
          ((congenital_renal_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital renal conditions"),
      
      congenital_renal_icd10 = case_when(
        (congenital_renal_first_date_icd10 < index_date) &
          ((congenital_renal_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital renal conditions (ICD-10)"),
      
      ## Congenital urogenital ----
      congenital_urogenital = case_when(
        (congenital_urogenital_first_date < index_date) &
          ((congenital_urogenital_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital urogenital conditions"),
      
      congenital_urogenital_icd10 = case_when(
        (congenital_urogenital_first_date_icd10 < index_date) &
          ((congenital_urogenital_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital urogenital conditions (ICD-10)"),
      
      ## Genitourinary non congenital ----
      genitourinary_non_congenital = case_when(
        (genitourinary_non_congenital_first_date < index_date) &
          ((genitourinary_non_congenital_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Non-congenital genitourinary conditions"),
      
      genitourinary_non_congenital_icd10 = case_when(
        (genitourinary_non_congenital_first_date_icd10 < index_date) &
          ((genitourinary_non_congenital_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Non-congenital genitourinary conditions (ICD-10)"),
      
      ## Renal devices ----
      renal_devices = case_when(
        (renal_devices_first_date < index_date) &
          ((renal_devices_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Renal devices"),
      
      renal_devices_icd10 = case_when(
        (renal_devices_first_date_icd10 < index_date) &
          ((renal_devices_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Renal devices (ICD-10)"),
      
      ## Genitourinary conditions ----
      genitourinary = case_when(
        congenital_renal == "Yes" ~ "Yes",
        congenital_urogenital == "Yes" ~ "Yes",
        genitourinary_non_congenital == "Yes" ~ "Yes",
        renal_devices == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Genitourinary conditions"),
      
      genitourinary_icd10 = case_when(
        congenital_renal_icd10 == "Yes" ~ "Yes",
        congenital_urogenital_icd10 == "Yes" ~ "Yes",
        genitourinary_non_congenital_icd10 == "Yes" ~ "Yes",
        renal_devices_icd10 == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Genitourinary conditions (ICD-10)"),
      
      # 12. Cancer ----
      cancer = case_when(
        (cancer_first_date < index_date) &
          ((cancer_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cancer"),
      
      cancer_icd10 = case_when(
        (cancer_first_date_icd10 < index_date) &
          ((cancer_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cancer (ICD-10)"),
      
      # 13. Non-malignant haematological conditions ----
      non_malignant_haematological = case_when(
        (haematology_first_date < index_date) &
          ((haematology_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Non-malignant haematological conditions"),
      
      non_malignant_haematological_icd10 = case_when(
        (haematology_first_date_icd10 < index_date) &
          ((haematology_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Non-malignant haematological conditions (ICD-10)"),
      
      # 14. Immunological conditions ----
      immunological = case_when(
        (immunological_first_date < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Immunological conditions"),
      
      immunological_icd10 = case_when(
        (immunological_first_date_icd10 < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Immunological conditions (ICD-10)"),
      
      # 15. Chronic infections ----
      chronic_infections = case_when(
        (chronic_infections_first_date < index_date) &
          ((chronic_infections_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Chronic infections"),
      
      chronic_infections_icd10 = case_when(
        (chronic_infections_first_date_icd10 < index_date) &
          ((chronic_infections_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Chronic infections (ICD-10)"),
      
      # 16. Rheumatology ----
      rheumatology = case_when(
        (rheumatology_first_date < index_date) &
          ((rheumatology_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Rheumatological conditions"),
      
      rheumatology_icd10 = case_when(
        (rheumatology_first_date_icd10 < index_date) &
          ((rheumatology_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Rheumatological conditions (ICD-10)"),
      
      # 17. Other congenital multisystem syndromes and chromosomal abnormalities ----
      congenital_malformation = case_when(
        (congenital_malformation_syndromes_and_chromosomal_first_date < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Other malformation syndromes and abnormalities"),
      
      congenital_malformation_icd10 = case_when(
        (congenital_malformation_syndromes_and_chromosomal_first_date_icd10 < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Other malformation syndromes and abnormalities (ICD-10)"),
      
      # 18. Diabetes ----
      diabetes = case_when(
        (diabetes_first_date < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Diabetes"),
      
      diabetes_icd10 = case_when(
        (diabetes_first_date_icd10 < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Diabetes (ICD-10)"),
      
      # 19. Other endocrine ----
      ## Congenital endocrine ----
      congenital_endocrine = case_when(
        (congenital_endocrine_first_date < index_date) &
          ((congenital_endocrine_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital endocrine"),
      
      congenital_endocrine_icd10 = case_when(
        (congenital_endocrine_first_date_icd10 < index_date) &
          ((congenital_endocrine_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Congenital endocrine (ICD-10)"),
      
      ## Endocrine (no dm) ----
      endocrine_no_dm = case_when(
        (endocrine_no_dm_first_date < index_date) &
          ((endocrine_no_dm_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Endocrine (no dm)"),
      
      endocrine_no_dm_icd10 = case_when(
        (endocrine_no_dm_first_date_icd10 < index_date) &
          ((endocrine_no_dm_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Endocrine (no dm) (ICD-10)"),
      
      ## Other endocrine ----
      other_endocrine = case_when(
        congenital_endocrine_icd10 == "Yes" ~ "Yes",
        endocrine_no_dm_icd10 == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Other endocrine conditions"),
      
      other_endocrine_icd10 = case_when(
        congenital_endocrine_icd10 == "Yes" ~ "Yes",
        endocrine_no_dm_icd10 == "Yes" ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Other endocrine conditions (ICD-10)"),
      
      # 20. Metabolic conditions ----
      metabolic = case_when(
        (metabolic_first_date < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Metabolic conditions"),
      
      metabolic_icd10 = case_when(
        (metabolic_first_date_icd10 < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Metabolic conditions (ICD-10)"),
      
      # 21. Obesity ----
      obesity = case_when(
        (obesity_first_date < index_date) &
          ((obesity_last_date + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Obesity"),
      
      obesity_icd10 = case_when(
        (obesity_first_date_icd10 < index_date) &
          ((obesity_last_date_icd10 + years(years_from_last)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Obesity (ICD-10)"),
      
      # 22. Transplant ----
      transplant = case_when(
        (transplant_first_date < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Transplant"),
      
      transplant_icd10 = case_when(
        (transplant_first_date_icd10 < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Transplant (ICD-10)"),
      
      # 23. Palliative care ----
      palliative_care = case_when(
        (palliative_care_first_date < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Receiving palliative care"),
      
      palliative_care_icd10 = case_when(
        (palliative_care_first_date_icd10 < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Receiving palliative care (ICD-10)"),
      
      # Other neuro with other respiratory ----
      other_neurological_with_other_respiratory =
        if_else(other_neurological == "Yes" & other_respiratory == "Yes",
                "Yes", "No") %>% 
        factor() %>% 
        ff_label("Other neurological with other respiratory conditions"),
      
      # Other neuro with cardiovascular ----
      other_neurological_with_cardiovascular =
        if_else(other_neurological == "Yes" & cardiovascular == "Yes",
                "Yes", "No") %>% 
        factor() %>% 
        ff_label("Other neurological with other cardiovascular conditions"),
      
      # Other respiratory with cardiovascular ----
      other_respiratory_with_cardiovascular =
        if_else(other_respiratory == "Yes" & cardiovascular == "Yes",
                "Yes", "No") %>% 
        factor() %>% 
        ff_label("Other respiratory with cardiovascular conditions")
      
    ) %>% 
    mutate(
      comorbidity_count = rowSums(
        select(.,
               mental_health_disorders, neurodevelopmental_and_behavioural,
               asthma, cystic_fibrosis, other_respiratory,
               cardiovascular, epilepsy, headaches, other_neurological,
               gastrointestinal_conditions, genitourinary, cancer,
               non_malignant_haematological, immunological, chronic_infections,
               rheumatology, congenital_malformation, diabetes, other_endocrine,
               metabolic, transplant, palliative_care
               ) == "Yes") %>% 
        ff_label("Number of comorbidities"),
      
      comorbidity_count_icd10 = rowSums(
        select(.,
               mental_health_disorders_icd10, neurodevelopmental_and_behavioural_icd10,
               asthma_icd10, cystic_fibrosis_icd10, other_respiratory_icd10,
               cardiovascular_icd10, epilepsy_icd10, headaches_icd10, other_neurological_icd10,
               gastrointestinal_conditions_icd10, genitourinary_icd10, cancer_icd10,
               non_malignant_haematological_icd10, immunological_icd10, chronic_infections_icd10,
               rheumatology_icd10, congenital_malformation_icd10, diabetes_icd10, other_endocrine_icd10,
               metabolic_icd10, transplant_icd10, palliative_care_icd10
        ) == "Yes") %>% 
        ff_label("Number of comorbidities (ICD-10)"),
      
      comorbidity_count_factor = case_when(
        comorbidity_count == 0 ~ "0",
        comorbidity_count == 1 ~ "1",
        comorbidity_count == 2 ~ "2",
        comorbidity_count >= 3 ~ "3+",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("0", "1", "2", "3+")) %>% 
        ff_label("Comorbidity count"),
      
      comorbidity_count_icd10_factor = case_when(
        comorbidity_count_icd10 == 0 ~ "0",
        comorbidity_count_icd10 == 1 ~ "1",
        comorbidity_count_icd10 == 2 ~ "2",
        comorbidity_count_icd10 >= 3 ~ "3+",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("0", "1", "2", "3+")) %>% 
        ff_label("Comorbidity count (ICD-10)")
    )
}

# Calculate death status at index date
calc_death_status = function(.data_patient, index_date){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    mutate(
      death = case_when(
        death_date < index_date ~ "Yes",
        is.na(death_date) ~ "No",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Death")
    )
}

# count exclusion criteria to cohort
count_exclusion_criteria = function(.data_patient){
  data_exclusion = .data_patient  %>% 
    transmute(
      is_aged_between_4_17 = (age >= 4) & (age < 18),
      is_not_covid_nosocomial = covid_nosocomial == "No",
      is_not_covid_discrepant_test = covid_discrepant_test == "No",
      is_alive = death == "No",
      include = is_aged_between_4_17 & is_not_covid_nosocomial &
        is_not_covid_discrepant_test & is_alive
    )
  if(any(is.na(data_exclusion$include))) warning("Inclusion column contains NAs")
  return(data_exclusion)
}

# Wrapper function for calculating time-varying variables
calc_indexed_variables = function(.data_patient, index_date){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    calc_age(index_date) %>%
    calc_comorbidity_status(index_date) %>%
    calc_vaccination_status(index_date) %>% 
    calc_death_status(index_date)
}

# Calculates vaccination status at index date
calc_vaccination_status = function(.data_patient, index_date){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    mutate(
      vaccination_status = case_when(
        (vax_covid_date_1 + days(21) < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Vaccination status"),
    )
}

fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# ff_round_counts: Round counts from finalfit::summary_factorlist output
ff_round_counts = function (.data, accuracy = 15, ignore = c("label", "levels", "p")){ 
  if (!any(names(.data) == "label")) 
    stop("summary_factorlist() must include: add_dependent_label = FALSE")
  df.out = .data %>%
    dplyr::mutate(label = dplyr::if_else(label == "", NA_character_, label)) %>% 
    tidyr::fill(label) %>%
    dplyr::group_by(label) %>% 
    dplyr::mutate(across(-dplyr::any_of(ignore), 
                         function(.){
                           value_count = as.numeric(stringr::str_extract(., "[:digit:]+")) %>% 
                             plyr::round_any(accuracy)
                           value_perc = value_count/sum(value_count)*100
                           
                           dplyr::case_when(
                             !levels %in% c("Mean (SD)", "Median (IQR)") ~ 
                               format_n_percent(value_count, value_perc, 1), 
                             TRUE ~ .)
                         })) %>%
    dplyr::mutate(label = dplyr::if_else(dplyr::row_number()==1, label, "")) %>% 
    dplyr::ungroup()
  class(df.out) = c("data.frame.ff", class(df.out))
  return(df.out)
}

# ff_round_counts: Round counts from finalfit::summary_factorlist output
ff_redact_counts = function (.data, n_redact = 7, ignore = c("label", "levels", "p")){ 
  if (!any(names(.data) == "label")) 
    stop("summary_factorlist() must include: add_dependent_label = FALSE")
  df.out = .data %>%
    dplyr::mutate(label = dplyr::if_else(label == "", NA_character_, label)) %>% 
    tidyr::fill(label) %>%
    dplyr::group_by(label) %>% 
    dplyr::mutate(across(-dplyr::any_of(ignore), 
                         function(.){
                           value_count = as.numeric(stringr::str_extract(., "[:digit:]+"))
                           value_readact = if_else(value_count <= n_redact, NA_real_, value_count)
                           value_perc = value_readact/sum(value_readact, na.rm = TRUE)*100
                           
                           dplyr::case_when(
                             is.na(value_readact) ~ "[REDACTED]",
                             !levels %in% c("Mean (SD)", "Median (IQR)") ~ 
                               format_n_percent(value_readact, value_perc, 1), 
                             TRUE ~ .)
                           
                         })) %>%
    dplyr::mutate(label = dplyr::if_else(dplyr::row_number()==1, label, "")) %>% 
    dplyr::ungroup()
  class(df.out) = c("data.frame.ff", class(df.out))
  return(df.out)
}

# icd10_code_to_chapter: 
icd10_code_to_chapter = function(icd10_code){
  
  case_when(
    str_sub(icd10_code, 1,3) %in% c(paste0("T", 36:50), paste0("X", 60:84))  ~ "23: Self-harm and poisoning by drugs",
    str_sub(icd10_code, 1,1) == "A" ~ "01: Infectious and parasitic diseases",
    str_sub(icd10_code, 1,1) == "B" ~ "01: Infectious and parasitic diseases",
    str_sub(icd10_code, 1,1) == "C" ~ "02: Neoplasms",
    str_sub(icd10_code, 1,3) %in% sprintf("%s%02d", "D",  0:49) ~ "02: Neoplasms",
    str_sub(icd10_code, 1,3) %in% sprintf("%s%02d", "D",  50:89) ~ "03: Diseases of the blood",
    str_sub(icd10_code, 1,1) == "E" ~ "04: Endocrine, nutritional and metabolic",
    str_sub(icd10_code, 1,1) == "F" ~ "05: Mental and behavioural disorders",
    str_sub(icd10_code, 1,1) == "G" ~ "06: Nervous system",
    str_sub(icd10_code, 1,3) %in% sprintf("%s%02d", "H",  0:59) ~ "07: Eye and adnexa",
    str_sub(icd10_code, 1,3) %in% sprintf("%s%02d", "H",  60:95) ~ "08: Ear and mastoid process",
    str_sub(icd10_code, 1,1) == "I" ~ "09: Circulatory system",
    str_sub(icd10_code, 1,1) == "J" ~ "10: Respiratory system",
    str_sub(icd10_code, 1,1) == "K" ~ "11: Digestive system",
    str_sub(icd10_code, 1,1) == "L" ~ "12: Skin and subcutaneous tissue",
    str_sub(icd10_code, 1,1) == "M" ~ "13: Musculoskeletal and connective tissue",
    str_sub(icd10_code, 1,1) == "N" ~ "14: Genitourinary system",
    str_sub(icd10_code, 1,1) == "O" ~ "15: Pregnancy, childbirth and the puerperium",
    str_sub(icd10_code, 1,1) == "P" ~ "16: Conditions in the perinatal period",
    str_sub(icd10_code, 1,1) == "Q" ~ "17: Congenital malformations",
    str_sub(icd10_code, 1,1) == "R" ~ "18: Abnormal clinical and laboratory findings",
    str_sub(icd10_code, 1,1) == "S" ~ "19: Injury and poisoning (excluding poisoning by drugs)",
    str_sub(icd10_code, 1,1) == "T" ~ "19: Injury and poisoning (excluding poisoning by drugs)",
    str_sub(icd10_code, 1,1) == "V" ~ "20: External causes (excluding self-harm)",
    str_sub(icd10_code, 1,1) == "W" ~ "20: External causes (excluding self-harm)",
    str_sub(icd10_code, 1,1) == "X" ~ "20: External causes (excluding self-harm)",
    str_sub(icd10_code, 1,1) == "Y" ~ "20: External causes (excluding self-harm)",
    str_sub(icd10_code, 1,1) == "Z" ~ "21: Factors influencing health status",
    str_sub(icd10_code, 1,1) == "U" ~ "22: Codes for special purposes",
    TRUE ~ NA_character_
  )
}

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

# Returns vector of mean and confidence intervals from t.test()
mean.cl.ttest = function(x){
  t_test = t.test(x)
  x_ci = c(t_test$estimate, t_test$conf.int[1], t_test$conf.int[2])
  names(x_ci) = c("Mean", "Lower", "Upper")
  x_ci
}

# plot_hist: plot histogram given data and variable name
plot_hist = function(data, x, path = here::here("output"),
                     fill = NULL, bins = 50, my_theme = theme_bw()){
  if(is.null(fill)){
    ggplot(data = data, aes_string(x = x)) + 
      geom_histogram(bins = bins) +
      my_theme
  } else {
    ggplot(data = data, aes_string(x = x, fill = fill)) + 
      geom_histogram(bins = bins) +
      my_theme
  }
  ggsave(paste0("plot_hist_", x, ".jpeg"),
         plot = last_plot(),
         device = "jpeg",
         path = path)
}

# count_dates_by_period: count of date variables in data by period 
count_dates_by_period = function(data, var, start_date = NULL, end_date = NULL, 
                        period = "week"){
  
  if(is.null(start_date)) {
    start_date = data %>% pull(all_of(var)) %>% min(na.rm = FALSE)
  } else if (is.character(start_date)){
    start_date = ymd(start_date)
  }
  
  if(is.null(end_date)) {
    end_date = data %>% pull(all_of(var)) %>% max(na.rm = FALSE)
  } else if (is.character(end_date)){
    end_date = ymd(end_date)
  }
  
  tibble(
    date = seq(start_date, end_date, by=period) %>%
      floor_date(period) %>% 
      rep(length(var)),
    name = var) %>% 
    left_join(
      data %>%
        select(all_of(var)) %>% 
        rownames_to_column() %>% 
        pivot_longer(-rowname) %>% 
        mutate(date = floor_date(value, period)) %>% 
        group_by(name) %>% 
        count(date),
      by = c("date", "name")
    ) %>% 
    replace_na(list(n = 0))
} 


date_seq = function(dates, by = "month", week_start = 1){
  seq(min(floor_date(dates, unit = by, week_start = week_start), na.rm = TRUE), 
      max(floor_date(dates, unit = by, week_start = week_start), na.rm = TRUE),
      by = by)
}


# lincom (from biostat3 V0.1.6) - calculate a linear combination of regression parameters.
lincom = function (model, specification, level = 0.95, eform = FALSE, 
                   ...) 
{
  stopifnot(requireNamespace("car"))
  if (length(specification) > 1) 
    return(t(sapply(specification, function(spec) lincom(model, 
                                                         spec, level, eform, ...))))
  x <- car::linearHypothesis(model, specification, ...)
  cf <- as.vector(attr(x, "value"))
  ses <- sqrt(as.vector(attr(x, "vcov")))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- format_perc(a, 3)
  fac <- qnorm(a)
  ci <- cbind(cf, cf + ses %o% fac)
  if (eform) 
    ci <- exp(ci)
  dimnames(ci) <- list(specification, c("Estimate", pct))
  cbind(ci, x[-1, 3:4])
}

# format_perc (from biostat3 V0.1.6) - format percentage
format_perc = function (probs, digits){
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), 
        "%")
}

# Functions from sjstats version 0.18.1 ----
svyglm.nb = function (formula, design, ...) 
{
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("Package `survey` needed to for this function to work. Please install it.", 
         call. = FALSE)
  }
  dw <- stats::weights(design)
  design <- stats::update(design, scaled.weights = dw/mean(dw, 
                                                           na.rm = TRUE))
  mod <- MASS::glm.nb(formula, data = stats::model.frame(design), 
                      weights = scaled.weights, ...)
  fam <- stats::family(mod)
  svyfit <- survey::svymle(loglike = sjstats_loglik, grad = sjstats_score, 
                           design = design, formulas = list(theta = ~1, eta = formula), 
                           start = c(mod$theta, stats::coef(mod)), na.action = "na.omit")
  class(svyfit) <- c("svyglm.nb", class(svyfit))
  attr(svyfit, "nb.terms") <- all.vars(formula)
  attr(svyfit, "nb.formula") <- formula
  attr(svyfit, "family") <- fam
  attr(svyfit, "nb.theta") <- mod[["theta"]]
  attr(svyfit, "nb.theta.se") <- mod[["SE.theta"]]
  svyfit$deviance <- mod$deviance
  svyfit$df.residuals <- mod$df.residuals
  svyfit$df <- length(stats::coef(mod)) + 1
  svyfit$aic <- mod$aic
  svyfit
}

sjstats_loglik = function (y, theta, eta) 
{
  mu <- exp(eta)
  return(lgamma(theta + y) - lgamma(theta) - lgamma(y + 1) + 
           theta * log(theta) + y * log(mu + (y == 0)) - (theta + 
                                                            y) * log(theta + mu))
}

sjstats_score = function (y, theta, eta) 
{
  cbind(sjstats_dtheta(y, theta, eta), sjstats_deta(y, theta, 
                                                    eta))
}

sjstats_dtheta = function (y, theta, eta) 
{
  mu <- exp(eta)
  digamma(theta + y) - digamma(theta) + log(theta) + 1 - log(theta + 
                                                               mu) - (y + theta)/(mu + theta)
}

sjstats_deta = function (y, theta, eta) 
{
  mu <- exp(eta)
  dmu <- y/mu - (theta + y)/(theta + mu)
  dmu * mu
}

