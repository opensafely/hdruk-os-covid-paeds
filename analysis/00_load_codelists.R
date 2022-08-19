# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# Load .csv codelists
# 00_load_codelists.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
#
# This script loads .csv codelists into the R environment

library(dplyr)

# ICD-10 ----
## Asthma ----
codelist_icd10_asthma = read.csv(
  "analysis/codelists/ICD-10 Hardelid Asthma.csv"
) %>% pull(ICD.10)

## Cancer ----
codelist_icd10_cancer = read.csv(
  "analysis/codelists/ICD-10 Hardelid Cancer.csv"
) %>% pull(ICD.10)

## Cerebral palsy ----
codelist_icd10_cerebral_palsy = read.csv(
  "analysis/codelists_draft/Draft cerebral palsy 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Chronic infections ----
codelist_icd10_chronic_infections = read.csv(
  "analysis/codelists_draft/Draft chronic infections 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Cystic fibrosis ----
codelist_icd10_cystic_fibrosis = read.csv(
  "analysis/codelists/ICD-10 Hardelid CF.csv"
) %>% pull(ICD.10)

## Devices and stomas ----
codelist_icd10_devices_and_stomas = read.csv(
  "analysis/codelists_draft/Draft devices and stomas 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Diabetes ----
codelist_icd10_diabetes = read.csv(
  "analysis/codelists/ICD-10 Hardelid Diabetes.csv"
) %>% pull(ICD.10)

## Endocrine disorders ----
codelist_icd10_endocrine_disorders = read.csv(
  "analysis/codelists_draft/Draft endocrine 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Epilepsy ----
codelist_icd10_epilepsy = read.csv(
  "analysis/codelists/ICD-10 Hardelid Epilepsy.csv"
) %>% pull(ICD.10)

## Gastrointestinal_disorders ----
codelist_icd10_gastrointestinal_disorders = read.csv(
  "analysis/codelists_draft/Draft gastrointestinal 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Haematological disorders ----
codelist_icd10_haematological_disorders = read.csv(
  "analysis/codelists_draft/Draft haematology 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Immunological disorders ----
codelist_icd10_immunological_disorders = read.csv(
  "analysis/codelists_draft/Draft immunological 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Learning and behaviour difficulties ----
codelist_icd10_learning_and_behaviour_difficulties = read.csv(
  "analysis/codelists_draft/Draft learning difficulties and behavour 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Mental illness ----
codelist_icd10_mental_illness = read.csv(
  "analysis/codelists_draft/Draft mental illness 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Musculoskeletal and rheum ----
codelist_icd10_musculoskeletal_and_rheum = read.csv(
  "analysis/codelists_draft/Draft musculoskeletal and rheum 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## Severe mental illness ----
codelist_icd10_severe_mental_illness = read.csv(
  "analysis/codelists/ICD-10 Hardelid Severe mental illness.csv"
) %>% pull(ICD.10)

## Transplant ----
codelist_icd10_transplant = read.csv(
  "analysis/codelists_draft/Draft transplant 040722.csv"
) %>% filter(icd10_code != "") %>% pull(icd10_code)

## All in list ----
icd_10_codelist = list(
  asthma = codelist_icd10_asthma,
  cancer = codelist_icd10_cancer,
  cerebral_palsy = codelist_icd10_cerebral_palsy,
  chronic_infections = codelist_icd10_chronic_infections,
  cystic_fibrosis = codelist_icd10_cystic_fibrosis,
  devices_and_stomas = codelist_icd10_devices_and_stomas,
  diabetes = codelist_icd10_diabetes,
  endocrine_disorders = codelist_icd10_endocrine_disorders,
  epilepsy = codelist_icd10_epilepsy,
  gastrointestinal_disorders = codelist_icd10_gastrointestinal_disorders,
  haematological_disorders = codelist_icd10_haematological_disorders,
  immunological_disorders = codelist_icd10_immunological_disorders,
  learning_and_behaviour_difficulties = codelist_icd10_learning_and_behaviour_difficulties,
  mental_illness = codelist_icd10_mental_illness,
  musculoskeletal_and_rheum = codelist_icd10_musculoskeletal_and_rheum,
  severe_mental_illness = codelist_icd10_severe_mental_illness,
  transplant = codelist_icd10_transplant
)
