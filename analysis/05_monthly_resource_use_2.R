
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)
library(furrr)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

poisson_test = function(numer, denom){
  if(denom > 0){
    test_object = poisson.test(numer, denom)
    output = c(test_object$estimate, test_object$conf.int)
  } else {
    output = c(NA_real_, NA_real_, NA_real_)
  }
  names(output) = c("estimate", "lower_ci", "upper_ci")
  return(output)
}

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round  = global_var$disclosure_count_round
count_redact = global_var$disclosure_redact

# Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Resource types, stratifications and presentations ----
## Resource types
resource_types = c("gp", "outpatient", "admissions", "beddays")

## Stratifying variables
startification_var = c(
  "age_group", "covid_status_tp", "comorbidity_count_factor",
  "mental_health_disorders", "neurodevelopmental_and_behavioural",
  "asthma", "cystic_fibrosis", "other_respiratory",
  "cardiovascular", "epilepsy", "headaches", 
  "other_neurological", "gastrointestinal_conditions", "genitourinary",
  "cancer", "non_malignant_haematological", "immunological",
  "chronic_infections", "rheumatology", "congenital_malformation",
  "diabetes", "other_endocrine", "metabolic", "transplant",
  "palliative_care", "other_neurological_with_other_respiratory",
  "other_neurological_with_cardiovascular", 
  "other_respiratory_with_cardiovascular"
)

## GP presentations
gp_presentations = c(
  "cardiovascular_system", "cellular_component_of_blood",
  "central_nervous_system", "congenital_disease",
  "connective_tissue", "digestive_system", "endocrine_system",
  "fetus_newborn", "genitourinary_system", "hematopoietic_structure",
  "immune_function", "infectious_disease", "labor_delivery", "mental_disorder",
  "metabolic_disease", "musculoskeletal_system", "neoplastic_disease",
  "nervous_system", "nutritional_disorder", "poisoning", "pregnancy_childbirth",
  "puerperium", "respiratory_system", "self_harm", "skin_subcutaneous_tissue",
  "traumatic_injury", "visual_system"
)

## Outpatient presentations
outpatient_presentations = c(
  "cardiology", "community_paediatrics", "dermatology", "dietetics",
  "endocrinology", "gastrointestinal", "general",
  "haematology", "immunology_and_allergy", "infectious_disease",
  "mental_health", "metabolic", "neurology", "occupational_therapy",
  "oncology", "pain_management_and_palliative_care", "physiology",
  "physiotherapy", "post_covid_clinic", "rehabilitation", "renal",
  "respiratory", "rheumatology", "speech_and_language_therapy"
)

## Admission presentations
admission_presentations = c(
  "01_infectious_and_parasitic_diseases", "02_neoplasms",
  "03_diseases_of_the_blood", "04_endocrine_nutritional_and_metabolic",
  "05_mental_and_behavioural_disorders", "06_nervous_system",
  "07_eye_and_adnexa", "08_ear_and_mastoid_process",
  "09_circulatory_system", "10_respiratory_system",
  "11_digestive_system", "12_skin_and_subcutaneous_tissue",
  "13_musculoskeletal_and_connective_tissue", "14_genitourinary_system",
  "15_pregnancy_childbirth_and_the_puerperium", 
  "16_conditions_in_the_perinatal_period", "17_congenital_malformations",
  "18_abnormal_clinical_and_laboratory_findings",
  "19_injury_and_poisoning_excluding_poisoning_by_drugs",
  "20_external_causes_excluding_self_harm",
  "21_factors_influencing_health_status",
  "22_codes_for_special_purposes",
  "23_self_harm_and_poisoning_by_drugs"
)

# Resource, presentation and stratification table ----
resource_combinations = tibble(
  resource_type = list("gp"),
  presentation = list(c("all", gp_presentations)),
  strata = list("overall")
) %>% 
  unnest(resource_type) %>% 
  unnest(presentation) %>%
  unnest(strata) %>% 
  bind_rows(
    tibble(
      resource_type = list("outpatient"),
      presentation = list(c("all", outpatient_presentations)),
      strata = list("overall")
    )  %>% 
      unnest(resource_type) %>% 
      unnest(presentation) %>%
      unnest(strata)
  ) %>% 
  bind_rows(
    tibble(
      resource_type = list("admissions"),
      presentation = list(c("all", admission_presentations)),
      strata = list("overall")
    )  %>% 
      unnest(resource_type) %>% 
      unnest(presentation) %>%
      unnest(strata)
  ) %>% 
  bind_rows(
    tibble(
      resource_type = list("beddays"),
      presentation = list(c("all", admission_presentations)),
      strata = list("overall")
    )  %>% 
      unnest(resource_type) %>% 
      unnest(presentation) %>%
      unnest(strata)
  ) %>% 
  bind_rows(
    tibble(
      resource_type = list(resource_types),
      presentation = list("all"),
      strata = list(startification_var)
    ) %>% 
      unnest(resource_type) %>% 
      unnest(presentation) %>%
      unnest(strata)
  ) %>% 
  bind_rows(
    tribble(
      ~resource_type, ~presentation,                        ~strata,
      "gp",           "mental_disorder",                     "age_group",
      "outpatient",   "mental_health",                       "age_group",
      "admissions",   "05_mental_and_behavioural_disorders", "age_group",
      "beddays",      "05_mental_and_behavioural_disorders", "age_group"
    )
  )

# Load cohort data ----
data_cohort = read_rds(here::here("output", "data", "data_cohort.rds"))

# Extract variable labels ----
var_labs = extract_variable_label(data_cohort[[1]])

data_cohort = data_cohort%>% 
  bind_rows() %>%
  ff_relabel(var_labs) %>% 
  mutate(overall = "Overall" %>% ff_label("Overall"))%>% 
  select(patient_id, cohort, overall, all_of(startification_var))

# Extract patient IDs for each year ----
patient_id_2019 = data_cohort %>% filter(cohort == 2019) %>% pull(patient_id)
patient_id_2020 = data_cohort %>% filter(cohort == 2020) %>% pull(patient_id)
patient_id_2021 = data_cohort %>% filter(cohort == 2021) %>% pull(patient_id)
patient_id_2022 = data_cohort %>% filter(cohort == 2022) %>% pull(patient_id)

# Plan multisession ----
plan(multisession, workers = 4)
options(future.globals.maxSize = 50000*1024^2) # 50 GB limit

# GP ----
data_gp = read_rds(here::here("output", "data", "data_gp.rds"))

data_gp = data_gp %>%
  filter(str_starts(code_type, "KM_") |
           str_starts(code_type, "mapped_1") |
           str_starts(code_type, "mapped_2"))

tbl_resource_gp = resource_combinations %>% 
  filter(resource_type == "gp") %>% 
  future_pmap(function(resource_type, presentation, strata,
                .data_resource, .data_cohort){
    
    .data_cohort = .data_cohort %>%
      select(patient_id, cohort, strata_level = all_of(strata))

    if(presentation == "all"){
      .data_resource = .data_resource %>%
        distinct(patient_id, gp_date) %>%
        mutate(month_date = floor_date(gp_date, "month")) %>%
        count(patient_id, month_date)
    } else{
      .data_resource = .data_resource %>%
        filter(code_type == paste0("KM_", presentation)) %>%
        distinct(patient_id, gp_date) %>%
        mutate(month_date = floor_date(gp_date, "month")) %>%
        count(patient_id, month_date)
    }
    
    monthly_count = .data_cohort %>%
      mutate(month_date = case_when(
        cohort == 2019 ~ list(seq(ymd("2019-01-01"), ymd("2019-12-01"), by = "month")),
        cohort == 2020 ~ list(seq(ymd("2020-01-01"), ymd("2020-12-01"), by = "month")),
        cohort == 2021 ~ list(seq(ymd("2021-01-01"), ymd("2021-12-01"), by = "month")),
        cohort == 2022 ~ list(seq(ymd("2022-01-01"), ymd("2022-04-01"), by = "month")))
      ) %>% 
      unnest(month_date) %>% 
      left_join(
        .data_resource, by = c("patient_id", "month_date")
      ) %>% 
      replace_na(list(n = 0)) %>% 
      group_by(cohort, month_date, strata_level) %>% 
      summarise(
        resource_type = resource_type,
        presentation = presentation,
        strata_var = strata,
        strata_label = attr(.data_cohort$strata_level, "label"),
        n_counts = sum(n),
        n_patient = length(unique(patient_id)),
        incidence = list(poisson_test(round(n_counts), n_patient))
      )
  },
  .data_resource = data_gp,
  .data_cohort = data_cohort) %>% 
  bind_rows() %>% 
  unnest_wider(incidence)

## Remove GP data 
rm(data_gp)

# Outpatient ----
data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds"))

tbl_resource_outpatient = resource_combinations %>% 
  filter(resource_type == "outpatient") %>% 
  future_pmap(function(resource_type, presentation, strata,
                .data_resource, .data_cohort){
    
    .data_cohort = .data_cohort %>%
      select(patient_id, cohort, strata_level = all_of(strata))
    
    if(presentation == "all"){
      .data_resource = .data_resource %>% 
        filter(is.na(specialty)) %>% 
        mutate(month_date = floor_date(outpatient_date, "month")) %>%
        group_by(patient_id, month_date) %>% 
        summarise(n = sum(outpatient_count)) %>% 
        ungroup()
    } else{
      .data_resource = .data_resource %>% 
        filter(specialty == paste0("TF_", presentation)) %>% 
        mutate(month_date = floor_date(outpatient_date, "month")) %>%
        group_by(patient_id, month_date) %>% 
        summarise(n = sum(outpatient_count)) %>% 
        ungroup()
    }
    
    monthly_count = .data_cohort %>%
      mutate(month_date = case_when(
        cohort == 2019 ~ list(seq(ymd("2019-04-01"), ymd("2019-12-01"), by = "month")),
        cohort == 2020 ~ list(seq(ymd("2020-01-01"), ymd("2020-12-01"), by = "month")),
        cohort == 2021 ~ list(seq(ymd("2021-01-01"), ymd("2021-12-01"), by = "month")),
        cohort == 2022 ~ list(seq(ymd("2022-01-01"), ymd("2022-04-01"), by = "month")))
      ) %>% 
      unnest(month_date) %>% 
      left_join(
        .data_resource, by = c("patient_id", "month_date")
      ) %>% 
      replace_na(list(n = 0)) %>% 
      group_by(cohort, month_date, strata_level) %>% 
      summarise(
        resource_type = resource_type,
        presentation = presentation,
        strata_var = strata,
        strata_label = attr(.data_cohort$strata_level, "label"),
        n_counts = sum(n),
        n_patient = length(unique(patient_id)),
        incidence = list(poisson_test(round(n_counts), n_patient))
      )
  },
  .data_resource = data_outpatient,
  .data_cohort = data_cohort) %>% 
  bind_rows() %>% 
  unnest_wider(incidence)

## Remove outpatient data ----
rm(data_outpatient)

# Admissions ----
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))

tbl_resource_admissions = resource_combinations %>% 
  filter(resource_type == "admissions") %>% 
  future_pmap(function(resource_type, presentation, strata,
                .data_resource, .data_cohort){
    
    .data_cohort = .data_cohort %>%
      select(patient_id, cohort, strata_level = all_of(strata))
    
    if(presentation != "all"){
      .data_resource = .data_resource %>% 
        mutate(primary_diagnosis.chapter = primary_diagnosis %>% 
                 icd10_code_to_chapter() %>%
                 str_to_lower() %>% 
                 str_replace_all(":", "") %>% 
                 str_replace_all(",", "") %>%
                 str_replace_all("\\(", "") %>%
                 str_replace_all("\\)", "") %>%
                 str_replace_all("-", "_") %>%
                 str_replace_all(" ", "_") %>% 
                 factor()) %>% 
        filter(primary_diagnosis.chapter == presentation)
    }
    
    .data_resource = .data_resource %>% 
      mutate(month_date = floor_date(admission_date, "month")) %>% 
      count(patient_id, month_date)
    
    monthly_count = .data_cohort %>%
      mutate(month_date = case_when(
        cohort == 2019 ~ list(seq(ymd("2019-01-01"), ymd("2019-12-01"), by = "month")),
        cohort == 2020 ~ list(seq(ymd("2020-01-01"), ymd("2020-12-01"), by = "month")),
        cohort == 2021 ~ list(seq(ymd("2021-01-01"), ymd("2021-12-01"), by = "month")),
        cohort == 2022 ~ list(seq(ymd("2022-01-01"), ymd("2022-04-01"), by = "month")))
      ) %>% 
      unnest(month_date) %>% 
      left_join(
        .data_resource, by = c("patient_id", "month_date")
      ) %>% 
      replace_na(list(n = 0)) %>% 
      group_by(cohort, month_date, strata_level) %>% 
      summarise(
        resource_type = resource_type,
        presentation = presentation,
        strata_var = strata,
        strata_label = attr(.data_cohort$strata_level, "label"),
        n_counts = sum(n),
        n_patient = length(unique(patient_id)),
        incidence = list(poisson_test(round(n_counts), n_patient))
      )
  },
  .data_resource = data_admissions,
  .data_cohort = data_cohort) %>% 
  bind_rows() %>% 
  unnest_wider(incidence)


# Bed-days ----

tbl_resource_beddays = resource_combinations %>% 
  filter(resource_type == "beddays") %>% 
  future_pmap(function(resource_type, presentation, strata,
                .data_resource, .data_cohort){
    
    .data_cohort = .data_cohort %>%
      select(patient_id, cohort, strata_level = all_of(strata))
    
    if(presentation != "all"){
      .data_resource = .data_resource %>% 
        mutate(primary_diagnosis.chapter = primary_diagnosis %>% 
                 icd10_code_to_chapter() %>%
                 str_to_lower() %>% 
                 str_replace_all(":", "") %>% 
                 str_replace_all(",", "") %>%
                 str_replace_all("\\(", "") %>%
                 str_replace_all("\\)", "") %>%
                 str_replace_all("-", "_") %>%
                 str_replace_all(" ", "_") %>% 
                 factor()) %>% 
        filter(primary_diagnosis.chapter == presentation)
    }
    
    .data_resource = seq(study_start_date,
                        study_end_date - days(1),
                        by = "month") %>%
      as.list() %>% 
      map(function(month_date){
        .data_resource %>%
          select(patient_id, admission_date, discharge_date) %>%
          mutate(
            start_date = month_date,
            end_date = (month_date + months(1) - days(1))
          ) %>%
          filter(admission_date <= end_date,
                 discharge_date >= start_date) %>% 
          mutate(
            length_of_stay = case_when(
              admission_date == discharge_date ~ 0.5, # day-case
              TRUE ~ (pmin(discharge_date, end_date) -
                        pmax(admission_date, start_date)) %>% as.numeric()
            )
          ) %>%
          group_by(patient_id) %>% 
          summarise(
            month_date = month_date,
            n = sum(length_of_stay)
          ) %>% 
          ungroup()
      }) %>% 
      bind_rows()
    
    monthly_count = .data_cohort %>%
      mutate(month_date = case_when(
        cohort == 2019 ~ list(seq(ymd("2019-01-01"), ymd("2019-12-01"), by = "month")),
        cohort == 2020 ~ list(seq(ymd("2020-01-01"), ymd("2020-12-01"), by = "month")),
        cohort == 2021 ~ list(seq(ymd("2021-01-01"), ymd("2021-12-01"), by = "month")),
        cohort == 2022 ~ list(seq(ymd("2022-01-01"), ymd("2022-04-01"), by = "month")))
      ) %>% 
      unnest(month_date) %>% 
      left_join(
        .data_resource, by = c("patient_id", "month_date")
      ) %>% 
      replace_na(list(n = 0)) %>% 
      group_by(cohort, month_date, strata_level) %>% 
      summarise(
        resource_type = resource_type,
        presentation = presentation,
        strata_var = strata,
        strata_label = attr(.data_cohort$strata_level, "label"),
        n_counts = sum(n),
        n_patient = length(unique(patient_id)),
        incidence = list(poisson_test(round(n_counts), n_patient))
      )
  },
  .data_resource = data_admissions,
  .data_cohort = data_cohort) %>% 
  bind_rows() %>% 
  unnest_wider(incidence)

# Combined resource type tables ----
tbl_resource_incidence = bind_rows(
  tbl_resource_gp, tbl_resource_outpatient,
  tbl_resource_admissions, tbl_resource_beddays
  )

# Round counts, redact low counts and clean table ----
tbl_resource_incidence = tbl_resource_incidence %>% 
  mutate(
    n_counts = if_else(n_counts < count_redact, NA_real_, n_counts),
    n_patient = if_else(n_patient < count_redact, NA_real_, n_patient),
    n_counts = n_counts %>% plyr::round_any(count_round),
    n_patient = n_patient %>% plyr::round_any(count_round),
    estimate = if_else(is.na(n_counts) | is.na(n_patient), NA_real_, estimate),
    lower_ci = if_else(is.na(n_counts) | is.na(n_patient), NA_real_, lower_ci),
    upper_ci = if_else(is.na(n_counts) | is.na(n_patient), NA_real_, upper_ci)
  ) %>% 
  mutate(across(.cols = c(n_counts, n_patient, estimate, lower_ci, upper_ci),
                .fns = as.character)) %>% 
  replace_na(list(n_counts = "[REDACTED]",
                  n_patient = "[REDACTED]",
                  estimate = "[REDACTED]",
                  lower_ci = "[REDACTED]",
                  upper_ci = "[REDACTED]"))

# Save resource incidence table ----
write_csv(tbl_resource_incidence, 
          here::here("output", "descriptives", "healthcare_use_2019_2022",
                     "tbl_resource_incidence.csv"))