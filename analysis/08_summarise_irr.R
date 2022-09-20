
# Load packages ----
library(tidyverse)

# Parameters ----
model_type     = "negative_binomial"
pred_type      = "uni_var"

# Create output directory folders ----
dir.create(here::here("output", "descriptives", "matched_cohort", model_type,
                      paste0("summarised_", pred_type)))

# List regression files ----
list_coeff_files = list.files(here::here("output", "descriptives", "matched_cohort",
                                         model_type, pred_type, "tables"),
                              pattern = "^coeff_[a-zA-Z0-9_]+.csv$")
# Read in .csv files ----
coeff = list_coeff_files %>% 
  map(function(coeff_file){
    x = read_csv(here::here("output", "descriptives", "matched_cohort",
                        model_type, pred_type, "tables", coeff_file)) %>% 
      mutate(file = coeff_file)
  }) %>% 
  bind_rows()

# Extract resource type and condition ----
coeff = coeff %>% 
  mutate(
    resource = file %>% str_extract("(?<=coeff_)[a-z]+(?=_)"),
    condition = file %>% str_extract(paste0("(?<=coeff_", resource, "_)[a-zA-Z0-9_]+"))
  ) 

# Relabel ----
coeff = coeff %>% 
  mutate(
    resource = case_when(
      resource == "admissions" ~ "Hospital admissions",
      resource == "beddays"    ~ "Hospital bed-days",
      resource == "outpatient" ~ "Outpatient appointments",
      resource == "gp"         ~ "Healthcare episodes"  
    ) %>%
      factor() %>% 
      fct_relevel("Hospital admissions", "Hospital bed-days", "Outpatient appointments"),
    condition = case_when(
      condition == "all"                                    ~ "Overall",
      
      condition == "01_infectious_and_parasitic_diseases"   ~ "Infectious disease",
      condition == "infectious_disease"                     ~ "Infectious disease",
      
      condition == "02_neoplasms"                           ~ "Neoplasms",
      condition == "oncology"                               ~ "Neoplasms",
      condition == "neoplastic_disease"                     ~ "Neoplasms",
      
      condition == "03_diseases_of_the_blood"               ~ "Diseases of the blood",
      condition == "cellular_component_of_blood"            ~ "Diseases of the blood",
      condition == "haematology"                            ~ "Diseases of the blood",
      
      condition == "04_endocrine_nutritional_and_metabolic" ~ "Endocrine system",
      condition == "endocrine_system"                       ~ "Endocrine system",
      condition == "endocrinology"                          ~ "Endocrine system",
      
      condition == "05_mental_and_behavioural_disorders"    ~ "Mental health",
      condition == "mental_disorder"                        ~ "Mental health",
      condition == "mental_health"                          ~ "Mental health",
      
      condition == "06_nervous_system"                      ~ "Nervous system",
      condition == "neurology"                              ~ "Nervous system",
      condition == "nervous_system"                         ~ "Nervous system",
      
      condition == "09_circulatory_system"                  ~ "Circulatory system",
      condition == "cardiovascular_system"                  ~ "Circulatory system",
      condition == "cardiology"                             ~ "Circulatory system",
      
      condition == "10_respiratory_system"                  ~ "Respiratory system",
      condition == "respiratory_system"                     ~ "Respiratory system",
      condition == "respiratory"                            ~ "Respiratory system",
      
      condition == "11_digestive_system"                    ~ "Digestive system",
      condition == "digestive_system"                       ~ "Digestive system",
      condition == "gastrointestinal"                       ~ "Digestive system",
      
      condition == "12_skin_and_subcutaneous_tissue"        ~ "Skin/subcutaneous tissue",
      condition == "dermatology"                            ~ "Skin/subcutaneous tissue",
      condition == "skin_subcutaneous_tissue"               ~ "Skin/subcutaneous tissue",
      
      condition == "14_genitourinary_system"                ~ "Genitourinary system",
      condition == "genitourinary_system"                   ~ "Genitourinary system",
      condition == "renal"                                  ~ "Genitourinary system",
      
      condition == "23_self_harm_and_poisoning_by_drugs"    ~ "Self-harm and overdose",
      condition == "self_harm"                              ~ "Self-harm and overdose",
      
    ) %>% 
      factor() %>% 
      fct_relevel("Overall")
  )

# Create and save incidence rate ratio table ----
table_irr = coeff %>% 
  filter(reference_row == FALSE, label == "Positive") %>% 
  select(resource, condition, estimate, conf.low, conf.high)

write_csv(table_irr,
          here::here("output", "descriptives", "matched_cohort", model_type,
                     paste0("summarised_", pred_type), "table_irr.csv"))

# Plot incidence rate ratio ----
plot_irr = table_irr %>% 
  ggplot(aes(x = condition %>% fct_rev(),
             y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_point(colour = "blue", size = 1.5) + 
  geom_errorbar(colour = "blue", width=.2) +
  geom_hline(yintercept=1, lty=2) +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  labs(x = NULL) +
  ylab("Incidence rate ratio (95% CI)") +
  facet_wrap(~ resource, ncol = 4)

## Save plot ----
ggsave(filename = paste0("plot_irr.jpeg"),
       plot = plot_irr,
       path = here::here("output", "descriptives", "matched_cohort", model_type,
                         paste0("summarised_", pred_type)),
       width = 9, height = 6, units = "in")
  
