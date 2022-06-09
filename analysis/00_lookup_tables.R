
library(tidyverse)
library(finalfit)

# ICD-10 ----

lookup_icd10 = read_csv(file = here::here("analysis", "lookup_tables", "icd10cm_order_2022.csv"))

lookup_icd10 = lookup_icd10 %>% 
  mutate(
    code = code %>% ff_label("ICD-10 code"),
    description_short = description_short %>% ff_label("Condition"),
    description_long = description_long %>% ff_label("Condition"),
    chapter_long = case_when(
      str_sub(code, 1,1) == "A" ~ "01: Certain infectious and parasitic diseases",
      str_sub(code, 1,1) == "B" ~ "01: Certain infectious and parasitic diseases",
      str_sub(code, 1,1) == "C" ~ "02: Neoplasms",
      str_sub(code, 1,3) %in% sprintf("%s%02d", "D",  0:49) ~ "02: Neoplasms",
      str_sub(code, 1,3) %in% sprintf("%s%02d", "D",  50:89) ~ "03: Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
      str_sub(code, 1,1) == "E" ~ "04: Endocrine, nutritional and metabolic diseases",
      str_sub(code, 1,1) == "F" ~ "05: Mental and behavioural disorders",
      str_sub(code, 1,1) == "G" ~ "06: Diseases of the nervous system",
      str_sub(code, 1,3) %in% sprintf("%s%02d", "H",  0:59) ~ "07: Diseases of the eye and adnexa",
      str_sub(code, 1,3) %in% sprintf("%s%02d", "H",  60:95) ~ "08: Diseases of the ear and mastoid process",
      str_sub(code, 1,1) == "I" ~ "09: Diseases of the circulatory system",
      str_sub(code, 1,1) == "J" ~ "10: Diseases of the respiratory system",
      str_sub(code, 1,1) == "K" ~ "11: Diseases of the digestive system",
      str_sub(code, 1,1) == "L" ~ "12: Diseases of the skin and subcutaneous tissue",
      str_sub(code, 1,1) == "M" ~ "13: Diseases of the musculoskeletal system and connective tissue",
      str_sub(code, 1,1) == "N" ~ "14: Diseases of the genitourinary system",
      str_sub(code, 1,1) == "O" ~ "15: Pregnancy, childbirth and the puerperium",
      str_sub(code, 1,1) == "P" ~ "16: Certain conditions originating in the perinatal period",
      str_sub(code, 1,1) == "Q" ~ "17: Congenital malformations, deformations and chromosomal abnormalities",
      str_sub(code, 1,1) == "R" ~ "18: Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
      str_sub(code, 1,1) == "S" ~ "19: Injury, poisoning and certain other consequences of external causes",
      str_sub(code, 1,1) == "T" ~ "19: Injury, poisoning and certain other consequences of external causes",
      str_sub(code, 1,1) == "V" ~ "20: External causes of morbidity and mortality",
      str_sub(code, 1,1) == "W" ~ "20: External causes of morbidity and mortality",
      str_sub(code, 1,1) == "X" ~ "20: External causes of morbidity and mortality",
      str_sub(code, 1,1) == "Y" ~ "20: External causes of morbidity and mortality",
      str_sub(code, 1,1) == "Z" ~ "21: Factors influencing health status and contact with health services",
      str_sub(code, 1,1) == "U" ~ "22: Codes for special purposes",
    ) %>%
      factor() %>%
      ff_label("ICD-10 chapter"),
    chapter_short = case_when(
      str_sub(code, 1,1) == "A" ~ "01: Infectious and parasitic diseases",
      str_sub(code, 1,1) == "B" ~ "01: Infectious and parasitic diseases",
      str_sub(code, 1,1) == "C" ~ "02: Neoplasms",
      str_sub(code, 1,3) %in% sprintf("%s%02d", "D",  0:49) ~ "02: Neoplasms",
      str_sub(code, 1,3) %in% sprintf("%s%02d", "D",  50:89) ~ "03: Diseases of the blood",
      str_sub(code, 1,1) == "E" ~ "04: Endocrine, nutritional and metabolic",
      str_sub(code, 1,1) == "F" ~ "05: Mental and behavioural disorders",
      str_sub(code, 1,1) == "G" ~ "06: Nervous system",
      str_sub(code, 1,3) %in% sprintf("%s%02d", "H",  0:59) ~ "07: Eye and adnexa",
      str_sub(code, 1,3) %in% sprintf("%s%02d", "H",  60:95) ~ "08: Ear and mastoid process",
      str_sub(code, 1,1) == "I" ~ "09: Circulatory system",
      str_sub(code, 1,1) == "J" ~ "10: Respiratory system",
      str_sub(code, 1,1) == "K" ~ "11: Digestive system",
      str_sub(code, 1,1) == "L" ~ "12: Skin and subcutaneous tissue",
      str_sub(code, 1,1) == "M" ~ "13: Musculoskeletal and connective tissue",
      str_sub(code, 1,1) == "N" ~ "14: Genitourinary system",
      str_sub(code, 1,1) == "O" ~ "15: Pregnancy, childbirth and the puerperium",
      str_sub(code, 1,1) == "P" ~ "16: Conditions in the perinatal period",
      str_sub(code, 1,1) == "Q" ~ "17: Congenital malformations",
      str_sub(code, 1,1) == "R" ~ "18: Abnormal clinical and laboratory findings",
      str_sub(code, 1,1) == "S" ~ "19: Injury and poisoning",
      str_sub(code, 1,1) == "T" ~ "19: Injury and poisoning",
      str_sub(code, 1,1) == "V" ~ "20: External causes",
      str_sub(code, 1,1) == "W" ~ "20: External causes",
      str_sub(code, 1,1) == "X" ~ "20: External causes",
      str_sub(code, 1,1) == "Y" ~ "20: External causes",
      str_sub(code, 1,1) == "Z" ~ "21: Factors influencing health status",
      str_sub(code, 1,1) == "U" ~ "22: Codes for special purposes",
    ) %>%
      factor() %>%
      ff_label("ICD-10 chapter")
  )


# Treatment function code ----
lookup_treatment_function = read_csv(file = here::here("analysis", "lookup_tables", "treatment_function_code.csv"))

lookup_treatment_function = lookup_treatment_function %>% 
  mutate(
    Code = Code %>% as.character() %>% ff_label("Specialty code"),
    Title = Title %>% factor() %>% ff_label("Specialty"),
    Category = Category %>% factor() %>% ff_label("Specialty Category")
  )

# Admission method ----
lookup_admission_method = read_csv(file = here::here("analysis", "lookup_tables", "admission_method.csv"))

lookup_admission_method = lookup_admission_method %>% 
  mutate(
    Code = Code %>% as.character() %>% ff_label("Admission method"),
    Description = Description %>% factor() %>% ff_label("Admission method"),
    Category = Category %>% factor() %>% ff_label("Admission method")
  )