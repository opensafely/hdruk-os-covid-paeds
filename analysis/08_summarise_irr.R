
# Load packages ----
library(tidyverse)

#Plot theme
theme_set(theme_bw())

# Command arguments to set parameters ----
args = commandArgs(trailingOnly=TRUE)

if(length(args) == 0){
  model_type     = "poisson"
  pred_type      = "uni_var"
} else{
  model_type     = args[[1]]
  pred_type      = args[[2]]
}

# Create output directory folders ----
dir.create(here::here("output", "descriptives", "matched_cohort", model_type,
                      paste0("summarised_", pred_type)))

# List regression files ----
list_coeff_files = list.files(here::here("output", "descriptives", "matched_cohort",
                                         model_type, pred_type, "tables"),
                              pattern = "^coeff_[a-zA-Z0-9_]+.csv$")

list_crude_rate_files = list.files(here::here("output", "descriptives", "matched_cohort",
                                              model_type, pred_type, "tables"),
                                   pattern = "^crude_rates_[a-zA-Z0-9_]+.csv$")

# Read in crude rate .csv files ----
crude_rate = list_crude_rate_files %>% 
  map(function(crude_rate_file){
    x = read_csv(here::here("output", "descriptives", "matched_cohort",
                            model_type, pred_type, "tables", crude_rate_file),
                 col_types = c("ccccccc")) %>% 
      mutate(file = crude_rate_file)
  }) %>% 
  bind_rows()

write_csv(crude_rate,
          here::here("output", "descriptives", "matched_cohort", model_type,
                     paste0("summarised_", pred_type), "crude_rates.csv"))
  
# Read in coeff .csv files ----
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
      fct_relevel("Healthcare episodes", "Outpatient appointments",
                  "Hospital admissions", "Hospital bed-days"),
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

table_irr_all = coeff %>% 
  filter(reference_row == FALSE, label %in% c("Untested", "Positive")) %>% 
  select(resource, condition, covid_status = label, estimate, conf.low, conf.high)

write_csv(table_irr,
          here::here("output", "descriptives", "matched_cohort", model_type,
                     paste0("summarised_", pred_type), "table_irr.csv"))

# Plot incidence rate ratio ----
## Only positive
plot_irr_positive = table_irr_all %>%
  filter(covid_status == "Positive") %>% 
  ggplot(aes(y = covid_status %>% fct_rev(),
             x = estimate, xmin = conf.low, xmax = conf.high,
             colour = covid_status)) + 
  geom_point(size = 1.5) + 
  geom_errorbar(width = 0.2) +
  geom_vline(xintercept=1, lty=2) +
  scale_x_continuous(trans='log10') +
  labs(x = "Incidence rate ratio (95% CI)", y = NULL,
       colour = "SARS-CoV-2 status (testing period)") +
  facet_grid(condition ~ resource, switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") +
  scale_colour_manual(values = c("#648fff", "#dc267f"))

## Positive and Untested
plot_irr_all = table_irr_all %>% 
  ggplot(aes(y = covid_status %>% fct_rev(),
             x = estimate, xmin = conf.low, xmax = conf.high,
             colour = covid_status)) + 
  geom_point(size = 1.5) + 
  geom_errorbar(width = .2) +
  geom_vline(xintercept=1, lty=2) +
  scale_x_continuous(trans='log10') +
  labs(x = "Incidence rate ratio (95% CI)", y = NULL,
       colour = "SARS-CoV-2 status (testing period)") +
  facet_grid(condition ~ resource, switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  scale_colour_manual(values = c("#648fff", "#dc267f"))

## Save plot ----
ggsave(filename = paste0("plot_irr_positive.jpeg"),
       plot = plot_irr_positive,
       path = here::here("output", "descriptives", "matched_cohort", model_type,
                         paste0("summarised_", pred_type)),
       width = 9, height = 6, units = "in")

ggsave(filename = paste0("plot_irr_all.jpeg"),
       plot = plot_irr_all,
       path = here::here("output", "descriptives", "matched_cohort", model_type,
                         paste0("summarised_", pred_type)),
       width = 9, height = 6, units = "in")
  
