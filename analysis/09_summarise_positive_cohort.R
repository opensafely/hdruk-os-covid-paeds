
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Output directories ----
dir.create(here::here("output", "descriptives", "positive_cohort"),
           showWarnings = FALSE, recursive=TRUE)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round  = global_var$disclosure_count_round
count_redact = global_var$disclosure_redact

# Load positive cohort data and resource use ----
data_resource = read_rds(here::here("output", "data", "data_resource.rds"))
data_positives = read_rds(here::here("output", "data", "data_positives.rds"))

# Create patient summary table ----
explanatory_var = c(
  
  # Follow-up
  "follow_up_days",
  
  # Demographics
  "age", "age_group", "sex", "ethnicity", "imd_Q5_2019", "region_2019",
  "rural_urban_2019",
  
  # Shielding
  "shielding",
  
  # Comorbidities
  "comorbidity_count_factor",
  "mental_health_disorders", "neurodevelopmental_and_behavioural",
  "asthma", "cystic_fibrosis", "other_respiratory",
  "cardiovascular", "epilepsy", "headaches", "other_neurological",
  "gastrointestinal_conditions", "genitourinary", "cancer",
  "non_malignant_haematological", "immunological", "chronic_infections",
  "rheumatology", "congenital_malformation", "diabetes", "other_endocrine",
  "metabolic", "transplant", "palliative_care",
  
  # Vaccination status
  "vaccination_status",
  
  # 2-weeks after postive test
  "pims_ts",
  "critical_care_2wks_flag", "critical_care_2wks", "critical_care_2wks_factor",
  "beddays_2wks_flag", "beddays_2wks", "beddays_2wks_factor",
  "outpatient_2wks_flag", "outpatient_2wks", "outpatient_2wks_factor", 
  "gp_2wks_flag", "gp_2wks", "gp_2wks_factor"
)

## Summary factorlist ----
tbl_cohort_summary = data_positives %>% 
  summary_factorlist(
    dependent = NULL,
    explanatory = explanatory_var,
    cont = "median",
    total_col = FALSE,
    add_col_totals = TRUE,
    na_include = TRUE
  ) %>% 
  ff_round_counts(count_round) %>% 
  ff_redact_counts(count_redact)

## Save cohort summary table ----
write_csv(tbl_cohort_summary, 
          here::here("output", "descriptives", "positive_cohort",
                     "tbl_cohort_summary.csv"))

# Resource use during follow up ----
## Mean bed-days ----
### Table ----
tbl_daily_beddays = data_resource %>% 
  group_by(date_indexed) %>% 
  summarise(
    n_patient = n(),
    mean_beddays = list(
      Hmisc::smean.cl.boot(n_beddays, conf.int = 0.95, B = 10)
    )) %>%
  unnest_wider(mean_beddays)

write_csv(tbl_daily_beddays, 
          here::here("output", "descriptives", "positive_cohort",
                     "tbl_daily_beddays.csv"))

### Plot ----
plot_daily_beddays = tbl_daily_beddays %>% 
  ggplot(aes(date_indexed, y = Mean, ymin = Lower, ymax = Upper)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  labs(x = "Follow-up day", y = "Bed-days")

ggsave(filename = here::here("output", "descriptives", "positive_cohort",
                             "plot_daily_beddays.jpeg"),
       plot = plot_daily_beddays,
       height = 6, width = 6, units = "in")

## Mean critical care ---- 
### Table ----
tbl_daily_critical_care = data_resource %>% 
  group_by(date_indexed) %>% 
  summarise(
    n_patient = n(),
    mean_critical_care = list(
      Hmisc::smean.cl.boot(n_critical_care, conf.int = 0.95, B = 10)
    )) %>%
  unnest_wider(mean_critical_care)

write_csv(tbl_daily_critical_care, 
          here::here("output", "descriptives", "positive_cohort",
                     "tbl_daily_critical_care.csv"))

#### Plot ----
plot_daily_critical_care = tbl_daily_critical_care %>% 
  ggplot(aes(date_indexed, y = Mean, ymin = Lower, ymax = Upper)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  labs(x = "Follow-up day", y = "Critical care")

ggsave(filename = here::here("output", "descriptives", "positive_cohort",
                             "plot_daily_critical_care.jpeg"),
       plot = plot_daily_critical_care,
       height = 6, width = 6, units = "in")

## Mean outpatient appointments  ----
### Table -----
tbl_daily_outpatient = data_resource %>% 
  group_by(date_indexed) %>% 
  summarise(
    n_patient = n(),
    mean_outpatient = list(
      Hmisc::smean.cl.boot(n_outpatient, conf.int = 0.95, B = 10)
    )) %>%
  unnest_wider(mean_outpatient)

write_csv(tbl_daily_outpatient, 
          here::here("output", "descriptives", "positive_cohort",
                     "tbl_daily_outpatient.csv"))

### Plot ----
plot_daily_outpatient = tbl_daily_outpatient %>% 
  ggplot(aes(date_indexed, y = Mean, ymin = Lower, ymax = Upper)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  labs(x = "Follow-up day", y = "Outpatient appointments")

ggsave(filename = here::here("output", "descriptives", "positive_cohort",
                             "plot_daily_outpatient.jpeg"),
       plot = plot_daily_outpatient,
       height = 6, width = 6, units = "in")

## Mean GP contact -----
### Table ----
tbl_daily_gp = data_resource %>% 
  group_by(date_indexed) %>% 
  summarise(
    n_patient = n(),
    mean_gp = list(
      Hmisc::smean.cl.boot(n_gp, conf.int = 0.95, B = 10)
    )) %>%
  unnest_wider(mean_gp)

write_csv(tbl_daily_gp, 
          here::here("output", "descriptives", "positive_cohort",
                     "tbl_daily_gp.csv"))

### Plot ----
plot_daily_gp = tbl_daily_gp %>% 
  ggplot(aes(date_indexed, y = Mean, ymin = Lower, ymax = Upper)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  labs(x = "Follow-up day", y = "GP contact")

ggsave(filename = here::here("output", "descriptives", "positive_cohort",
                             "plot_daily_gp.jpeg"),
       plot = plot_daily_gp,
       height = 6, width = 6, units = "in")




