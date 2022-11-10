# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# Load .csv codelists
# 00_load_codelists.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
#
# This script loads .csv codelists into the R environment

library(dplyr)

# 1. Mental health disorders ----
## Mental illness ----
codelist_icd10_mental_illness = read.csv("analysis/codelists/icd10_mental_illness.csv") %>%
  pull(code)

## Severe mental illness ----
codelist_icd10_severe_mental_illness = read.csv("analysis/codelists/icd10_severe_mental_illness.csv") %>%
  pull(code)

# 2. Neurodevelopmental and behavioural conditions ----
codelist_icd10_behavioural_and_developmental_including_autism =
  read.csv("analysis/codelists/icd10_behavioural_and_developmental_including_autism.csv") %>%
  pull(code)

# 3. Asthma ----
codelist_icd10_asthma = read.csv("analysis/codelists/icd10_asthma.csv") %>%
  pull(code)

# 4. Cystic fibrosis ----
codelist_icd10_cystic_fibrosis = read.csv("analysis/codelists/icd10_cystic_fibrosis.csv") %>%
  pull(code)

# 5. Other respiratory ----
## Congenital respiratory conditions ----
codelist_icd10_resp_congenital = read.csv("analysis/codelists/icd10_resp_congenital.csv") %>%
  pull(code)

## Respiratory devices ----
codelist_icd10_resp_devices = read.csv("analysis/codelists/icd10_resp_devices.csv") %>%
  pull(code)

## Respiratory (not asthma or cystic fibrosis) ----
codelist_icd10_respiratory_not_asthma_or_cf =
  read.csv("analysis/codelists/icd10_respiratory_not_asthma_or_cf.csv") %>%
  pull(code)

# 6. Cardiovascular conditions ----
## Cardiovascular congenital ----
codelist_icd10_cardiovascular_congenital =
  read.csv("analysis/codelists/icd10_cardiovascular_congenital.csv") %>%
  pull(code)

## Cardiovascular devices ----
codelist_icd10_cardiovascular_devices =
  read.csv("analysis/codelists/icd10_cardiovascular_devices.csv") %>%
  pull(code)

## Cardiovascular (non-congenital) ----
codelist_icd10_cardiovascular_non_congenital =
  read.csv("analysis/codelists/icd10_cardiovascular_non_congenital.csv") %>%
  pull(code)

# 7. Epilepsy ----
codelist_icd10_epilepsy =
  read.csv("analysis/codelists/icd10_epilepsy.csv") %>%
  pull(code)

# 8. Headaches ----
codelist_icd10_headaches =
  read.csv("analysis/codelists/icd10_headaches.csv") %>%
  pull(code)

# 9. Other neurological ---- 
## Cerebral palsy or paralysis ----
codelist_icd10_cerebral_palsy_paralysis =
  read.csv("analysis/codelists/icd10_cerebral_palsy_paralysis.csv") %>%
  pull(code)

## Congenital neuro ----
codelist_icd10_congenital_neuro =
  read.csv("analysis/codelists/icd10_congenital_neuro.csv") %>%
  pull(code)

## Neuro devices ----
codelist_icd10_neuro_devices =
  read.csv("analysis/codelists/icd10_neuro_devices.csv") %>%
  pull(code)

## Neurological (no epilepsy, cp or headaches) ----
codelist_icd10_neurological_no_epilepsy_or_cp_headaches =
  read.csv("analysis/codelists/icd10_neurological_no_epilepsy_or_cp_headaches.csv") %>%
  pull(code)

# 10. Gastrointestinal conditions ----
## Gastrointestinal (non-device) ----
codelist_icd10_gastrointestinal =
  read.csv("analysis/codelists/icd10_gastrointestinal.csv") %>%
  pull(code)

## Gastrointestinal (devices) ----
codelist_icd10_gastrointestinal_devices =
  read.csv("analysis/codelists/icd10_gastrointestinal_devices.csv") %>%
  pull(code)

# 11. Genitourinary conditions ----
## Congenital renal ----
codelist_icd10_congenital_renal =
  read.csv("analysis/codelists/icd10_congenital_renal.csv") %>%
  pull(code)

## Congenital urogenital ----
codelist_icd10_congenital_urogenital =
  read.csv("analysis/codelists/icd10_congenital_urogenital.csv") %>%
  pull(code)

## Genitourinary non congenital ----
codelist_icd10_genitourinary_non_congenital =
  read.csv("analysis/codelists/icd10_genitourinary_non_congenital.csv") %>%
  pull(code)

## Renal devices ----
codelist_icd10_renal_devices =
  read.csv("analysis/codelists/icd10_renal_devices.csv") %>%
  pull(code)

# 12. Cancer ----
codelist_icd10_cancer =
  read.csv("analysis/codelists/icd10_cancer.csv") %>%
  pull(code)

# 13. Non-malignant haematological conditions ----
codelist_icd10_haematology =
  read.csv("analysis/codelists/icd10_haematology.csv") %>%
  pull(code)

# 14. Immunological conditions ----
codelist_icd10_immunological =
  read.csv("analysis/codelists/icd10_immunological.csv") %>%
  pull(code)

# 15. Chronic infections ----
codelist_icd10_chronic_infections =
  read.csv("analysis/codelists/icd10_chronic_infections.csv") %>%
  pull(code)

# 16. Rheumatology ----
codelist_icd10_rheumatology =
  read.csv("analysis/codelists/icd10_rheumatology.csv") %>%
  pull(code)

# 17. Other congenital multisystem syndromes and chromosomal abnormalities ----
codelist_icd10_congenital_malformation_syndromes_and_chromosomal =
  read.csv("analysis/codelists/icd10_congenital_malformation_syndromes_and_chromosomal.csv") %>%
  pull(code)

# 18. Diabetes ----
codelist_icd10_diabetes =
  read.csv("analysis/codelists/icd10_diabetes.csv") %>%
  pull(code)

# 19. Other endocrine ----
## Congenital endocrine ----
codelist_icd10_congenital_endocrine =
  read.csv("analysis/codelists/icd10_congenital_endocrine.csv") %>%
  pull(code)

## Endocrine (no dm) ----
codelist_icd10_endocrine_no_dm =
  read.csv("analysis/codelists/icd10_endocrine_no_dm.csv") %>%
  pull(code)

# 20. Metabolic conditions ----
codelist_icd10_metabolic =
  read.csv("analysis/codelists/icd10_metabolic.csv") %>%
  pull(code)

# 21. Obesity ----
codelist_icd10_obesity =
  read.csv("analysis/codelists/icd10_obesity.csv") %>%
  pull(code)

# 22. Transplant ----
codelist_icd10_transplant =
  read.csv("analysis/codelists/icd10_transplant.csv") %>%
  pull(code)

# 23. Palliative care ----
codelist_icd10_palliative_care =
  read.csv("analysis/codelists/icd10_palliative_care.csv") %>%
  pull(code)


## All in list ----
icd_10_codelist = list(
  asthma = codelist_icd10_asthma,
  behavioural_and_developmental_including_autism = codelist_icd10_behavioural_and_developmental_including_autism,
  cancer = codelist_icd10_cancer,
  cardiovascular_congenital = codelist_icd10_cardiovascular_congenital,
  cardiovascular_devices = codelist_icd10_cardiovascular_devices,
  cardiovascular_non_congenital = codelist_icd10_cardiovascular_non_congenital,
  cerebral_palsy_paralysis = codelist_icd10_cerebral_palsy_paralysis,
  chronic_infections = codelist_icd10_chronic_infections,
  congenital_endocrine = codelist_icd10_congenital_endocrine,
  congenital_malformation_syndromes_and_chromosomal = codelist_icd10_congenital_malformation_syndromes_and_chromosomal,
  congenital_neuro = codelist_icd10_congenital_neuro,
  congenital_renal = codelist_icd10_congenital_renal,
  congenital_urogenital = codelist_icd10_congenital_urogenital,
  cystic_fibrosis = codelist_icd10_cystic_fibrosis,
  diabetes = codelist_icd10_diabetes,
  endocrine_no_dm = codelist_icd10_endocrine_no_dm,
  epilepsy = codelist_icd10_epilepsy,
  gastrointestinal_devices = codelist_icd10_gastrointestinal_devices,
  gastrointestinal = codelist_icd10_gastrointestinal,
  genitourinary_non_congenital = codelist_icd10_genitourinary_non_congenital,
  haematology = codelist_icd10_haematology,
  headaches = codelist_icd10_headaches,
  immunological = codelist_icd10_immunological,
  mental_illness = codelist_icd10_mental_illness,
  metabolic = codelist_icd10_metabolic,
  neuro_devices = codelist_icd10_neuro_devices,
  neurological_no_epilepsy_or_cp_headaches = codelist_icd10_neurological_no_epilepsy_or_cp_headaches,
  obesity = codelist_icd10_obesity,
  palliative_care = codelist_icd10_palliative_care,
  renal_devices = codelist_icd10_renal_devices,
  resp_congenital = codelist_icd10_resp_congenital,
  resp_devices = codelist_icd10_resp_devices,
  respiratory_not_asthma_or_cf = codelist_icd10_respiratory_not_asthma_or_cf,
  rheumatology = codelist_icd10_rheumatology,
  severe_mental_illness = codelist_icd10_severe_mental_illness,
  transplant = codelist_icd10_transplant
)
