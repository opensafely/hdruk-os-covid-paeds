library(broom)
library(broom.helpers)
library(tidyverse)
library(lubridate)
library(finalfit)


# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round  = global_var$disclosure_count_round
count_redact = global_var$disclosure_redact

# Set seed ----
set.seed(24092022)
n_sample = 200000

# Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Plot theme
theme_set(theme_bw())

# Command arguments ----
args = commandArgs(trailingOnly=TRUE)

if(length(args) == 0){
  resource_type  = "admissions"
  model_type     = "poisson"
} else{
  resource_type  = args[[1]]
  model_type     = args[[2]]
}

# Create output directory folders ----
dir.create(here::here("output", "descriptives", "healthcare_use_2019_2022",
                      model_type, resource_type),
           showWarnings = FALSE, recursive=TRUE)

# Load data ----
data_cohort = read_rds(here::here("output", "data", "data_cohort.rds"))
 
## Extract dataset labels 
var_labs = data_cohort[[1]] %>% 
  extract_variable_label()

lookup_labels = tibble(
  variable = names(var_labs),
  var_label = var_labs
)

## Bind rows ----  
data_cohort = data_cohort %>% 
  bind_rows() %>% 
  rename(year = cohort) %>% 
  ff_relabel(var_labs)

## Select subset
unique_id = data_cohort %>%
  pull(patient_id) %>% 
  unique()

id_patient = sample(unique_id, 
                    size = min(length(unique_id), n_sample),
                    replace = FALSE)

data_cohort = data_cohort  %>%
  filter(patient_id %in% id_patient)

# Calculate yearly resource use ----
if(resource_type == "gp"){
  
  data_resource = read_rds(here::here("output", "data", "data_gp.rds"))  %>%
    filter(patient_id %in% id_patient)
  
  data_resource = data_resource %>%
    filter(str_starts(code_type, "KM_") |
             str_starts(code_type, "mapped_1") |
             str_starts(code_type, "mapped_2"))%>% 
    distinct(patient_id, gp_date) %>% 
    mutate(year = year(gp_date)) %>% 
    count(patient_id, year)
  
} else if (resource_type == "outpatient"){
  
  data_resource = read_rds(here::here("output", "data", "data_outpatient.rds"))  %>%
    filter(patient_id %in% id_patient)

  data_resource = data_resource %>% 
    filter(is.na(specialty)) %>% 
    mutate(year = year(outpatient_date)) %>%
    group_by(patient_id, year) %>% 
    summarise(n = sum(outpatient_count)) %>% 
    ungroup()
  
} else if (resource_type == "admissions" | resource_type == "beddays"){
  
  data_resource = read_rds(here::here("output", "data", "data_admissions.rds")) %>%
    filter(patient_id %in% id_patient)
  
  if (resource_type == "admissions"){
    
    data_resource = data_resource %>% 
      mutate(year = year(admission_date)) %>% 
      count(patient_id, year)
    
  } else {
    
    data_resource = seq(year(study_start_date),
                        year(study_end_date - days(1))) %>%
      as.list() %>% 
      map(function(year){
        data_resource %>%
          select(patient_id, admission_date, discharge_date) %>%
          mutate(
            year = year,
            start_date = ymd(paste0(year, "-01-01")),
            end_date = if_else(year == 2022,
                               ymd("2022-04-30"),
                               (start_date + years(1) - days(1)))
          ) %>%
          filter(admission_date <= end_date,
                 discharge_date >= start_date) %>% 
          mutate(
            length_of_stay = case_when(
              admission_date == discharge_date ~ 0.5, # day-case
              TRUE ~ (pmin(discharge_date, end_date) -
                        pmax(admission_date, start_date)) %>% as.numeric()
            )
          )%>%
          group_by(patient_id, year) %>% 
          summarise(
            n = sum(length_of_stay)
          ) %>% 
          ungroup()
      })%>% 
      bind_rows()
    }
}

# Join resource with cohort ----
data_cohort = data_cohort %>% 
  left_join(
    data_resource %>% 
      select(patient_id, year, n),
    by = c("patient_id", "year")
  ) %>% 
  replace_na(list(n = 0)) %>% 
  rename(health_contact = n)

# Calculate time under observation ----
data_cohort = data_cohort %>% 
  mutate(
    start_date = if_else(year == 2019 & resource_type == "outpatient",
                         ymd("2019-04-01"),
                         ymd(paste0(year, "-01-01"))),
    end_date = pmin(ymd(paste0(year, "-12-31")),
                    death_date,
                    study_end_date - days(1),
                    na.rm = TRUE),
    person_days = (end_date - start_date) %>% as.numeric(),
    year = year %>% factor() %>% 
      ff_label("Year")
    )

# Filter out missing data and zero person-time ----
data_cohort = data_cohort %>% 
  filter(
    !is.na(sex),
    !is.na(ethnicity),
    !is.na(imd_Q5_2019),
    !is.na(region_2019),
    !is.na(rural_urban_2019),
    person_days > 0
  )

# Count regression ----
## Predictors ----
# Demographics
predictors_demographics = c(
    "age_group", "sex", "ethnicity", "imd_Q5_2019",
    "region_2019", "rural_urban_2019"
)

# Comorbidities
predictors_comorb = c(

  "mental_health_disorders", "neurodevelopmental_and_behavioural",
  "asthma", "cystic_fibrosis", "other_respiratory",
  "cardiovascular", "epilepsy", "headaches", "other_neurological",
  "gastrointestinal_conditions", "genitourinary", "cancer",
  "non_malignant_haematological", "immunological", "chronic_infections",
  "rheumatology", "congenital_malformation", "diabetes", "other_endocrine",
  "metabolic", "obesity", "transplant", "palliative_care",
    
  # Intercations
  "neurodevelopmental_and_behavioural:other_respiratory",
  "neurodevelopmental_and_behavioural:cardiovascular",
  "other_respiratory:cardiovascular"
  )

## Formula ----
model_formula = paste0("health_contact ~ ",
                       paste(predictors_demographics, collapse = " + "), 
                       " + year*(",
                       paste(predictors_comorb, collapse = " + ")) %>%
  paste0(") + offset(log(person_days))") %>% 
  as.formula()

if(model_type == "poisson"){
  
  # Fit poisson model ----
  model_fit = glm(model_formula,
                  family = poisson,
                  data = data_cohort)

}

# Count person-time table ----
table_counts = data_cohort %>% 
  group_by(year) %>% 
  summarise(
    health_contact = sum(health_contact),
    person_years_000 = sum(person_days)/365.25/1000,
    crude_rate = health_contact/person_years_000
  )

## Save count table ----
write_csv(table_counts,
          here::here("output", "descriptives", "healthcare_use_2019_2022",
                     model_type, resource_type, "table_counts.csv"))

# Coefficient table ----
table_coeff = model_fit %>%
  tidy_and_attach(exponentiate = TRUE, conf.int = TRUE) %>% 
  tidy_add_term_labels()

## Save coefficient table ----
write_csv(table_coeff,
          here::here("output", "descriptives", "healthcare_use_2019_2022",
                     model_type, resource_type, "table_irr.csv"))

# Model stats ----
table_stats = model_fit %>% 
  glance()

## Save model stats ----
write_csv(table_stats,
          here::here("output", "descriptives", "healthcare_use_2019_2022",
                     model_type, resource_type, "table_stats.csv"))


# Format for plot ----
coeff_prep = table_coeff %>% 
  filter(term != "(Intercept)") %>%
  left_join(
    lookup_labels %>% 
      select(variable, var_label_2 = var_label)
  ) %>% 
  mutate(
    year = str_extract(term, "(?<=year)\\d{4}"),
    var_label = case_when(
      var_label == variable ~ var_label_2,
      str_starts(var_label, "Year \\* ") ~ str_remove(var_label, "^Year \\* "),
      var_label == "Year" ~ "Headline year effect",
      TRUE                  ~ var_label
    ),
    label = case_when(
      label == "Yes" ~ "",
      var_type == "interaction" ~ "",
      TRUE ~ label
    ),
    plot_label = case_when(
      label == "" | variable == "year" ~ var_label,
      TRUE ~ paste0(var_label, ": ", label)
    ),
    plot_label = case_when(
      plot_label == "Neurodevelopmental and behavioural conditions * Other respiratory conditions" ~
        "Neurodev. and behavioural & other respiratory conditions",
      plot_label == "Neurodevelopmental and behavioural conditions * Cardiovascular conditions" ~
        "Neurodev. and behavioural & cardiovascular conditions",
      plot_label == "Other respiratory conditions * Cardiovascular conditions" ~
        "Other respiratory conditions & cardiovascular conditions",
      TRUE ~ plot_label
    )
  ) %>% 
  replace_na(list(year = "Baseline effect")) %>% 
  mutate(
    year = year %>% factor() %>% fct_relevel("Baseline effect"),
    plot_label = plot_label %>% factor() %>% 
      fct_relevel(unique(plot_label)) %>% 
      fct_rev()
  )

## Plot coefficients ----
plot_irr = coeff_prep %>% 
  ggplot(aes(x = plot_label, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(colour = "blue", size = 1.5) + 
  geom_errorbar(colour = "blue", width=.2) +
  geom_hline(yintercept=1, lty=2) +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  labs(x = NULL) +
  ylab("Incidence rate ratio (95% CI)") +
  facet_wrap(~ year, ncol = 4) +
  theme_bw()

## Reduced plot ----
plot_irr_reduced = coeff_prep %>%
  filter(!variable %in% c("age_group", "sex", "ethnicity", "imd_Q5_2019",
                         "region_2019", "rural_urban_2019")) %>% 
  ggplot(aes(x = plot_label, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(colour = "blue", size = 1.5) + 
  geom_errorbar(colour = "blue", width=.2) +
  geom_hline(yintercept=1, lty=2) +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  labs(x = NULL) +
  ylab("Incidence rate ratio (95% CI)") +
  facet_wrap(~ year, ncol = 4) +
  theme_bw()


# Save plot ----
ggsave(filename = "plot_irr.jpeg",
       plot = plot_irr,
       path = here::here("output", "descriptives", "healthcare_use_2019_2022",
                         model_type, resource_type),
       width = 11, height = 7, units = "in")

ggsave(filename = "plot_irr_reduced.jpeg",
       plot = plot_irr_reduced,
       path = here::here("output", "descriptives", "healthcare_use_2019_2022",
                         model_type, resource_type),
       width = 11, height = 7, units = "in")
