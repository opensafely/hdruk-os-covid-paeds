

# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)
library(scales)
library(zoo)
library(knitr)

# Load custom functions and lookup tables ----
source(here::here("analysis", "00_utility_functions.R"))
source(here::here("analysis", "00_lookup_tables.R"))

# Output directories ----
dir.create(here::here("output", "descriptives", "summary_datasets"), showWarnings = FALSE, recursive=TRUE)

# Plot theme ----
theme_set(theme_bw())

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Disclosure control parameters ----
count_round = global_var$disclosure_count_round

# Load datasets ----
data_patient = read_rds(here::here("output", "data", "data_patient.rds"))
data_testing = read_rds(here::here("output", "data", "data_testing.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))
data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds"))
data_gp = read_rds(here::here("output", "data", "data_gp.rds"))


# Patient demographics ----

dependent = "covid_status_tp"
explanatory = c(
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
  "metabolic", "obesity", "transplant", "palliative_care"
  
)

tbl_patient_summary = data_patient %>% 
  summary_factorlist(
    dependent = dependent,
    explanatory = explanatory,
    total_col = TRUE,
    add_col_totals = TRUE,
    na_include = TRUE
  ) %>%
  ff_round_counts(accuracy = count_round)

write_csv(tbl_patient_summary, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_patient_summary.csv"))


# Covid-19 Testing ----

tbl_weekly_testing_by_result = data_testing %>% 
  mutate(
    date = test_date %>% cut(breaks = "week")
  ) %>% 
  count(date, result, .drop = FALSE) %>%
  group_by(result) %>% 
  mutate(roll_mean = rollmean(n, 4, align = "right", fill = NA),
         date = date %>% as_date(),
         n = n %>% plyr::round_any(count_round)) %>% 
  ungroup() 

write_csv(tbl_weekly_testing_by_result, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_weekly_testing_by_result.csv"))

plot_weekly_testing_by_result = tbl_weekly_testing_by_result %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-12-01"),
                            by = "6 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom") +
  facet_wrap(~ result)

ggsave("plot_weekly_testing_by_result.jpeg",
       plot_weekly_testing_by_result,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 5, width = 7)


# Hospital Admission Spells ----
## Combine with lookup table ----
### Primary diagnosis ----
data_admissions = data_admissions %>%
  mutate(primary_diagnosis.chapter = primary_diagnosis %>% 
           icd10_code_to_chapter() %>% 
           ff_label("Primary diagnosis"))

data_admissions = data_admissions %>% 
  left_join(
    lookup_icd10 %>% 
      select(primary_diagnosis = code,
             primary_diagnosis.description = description_short),
    by = "primary_diagnosis"
  )

### Treatment function ----
data_admissions = data_admissions %>%
  left_join(
    lookup_treatment_function %>% 
      select(
        treatment_function = Code,
        treatment_function.description = Title,
        treatment_function.category = Category
      ),
    by = "treatment_function"
  )

### Admission method ----
data_admissions = data_admissions %>%
  left_join(
    lookup_admission_method %>% 
      select(
        admission_method = Code,
        admission_method.description = Description,
        admission_method.category = Category
      ),
    by = "admission_method"
  )


## Factor summary ----
dependent = NULL
explanatory = c("admission_method.category",
                "treatment_function.category",
                "primary_diagnosis.chapter")

tbl_summary_admissions = data_admissions %>% 
  summary_factorlist(
    dependent = dependent,
    explanatory = explanatory,
    add_col_totals = TRUE,
    na_include = TRUE
  ) %>%
  ff_round_counts(accuracy = count_round)

write_csv(tbl_summary_admissions, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_summary_admissions.csv"))

## Weekly admissions ----

### Overall ----

tbl_weekly_admissions = data_admissions %>% 
  mutate(
    date = admission_date %>% cut(breaks = "week")
  ) %>% 
  count(date, .drop = FALSE) %>%
  mutate(roll_mean = rollmean(n, 4, align = "right", fill = NA),
         date = date %>% as_date(),
         n = n %>% plyr::round_any(count_round))

write_csv(tbl_weekly_admissions, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_weekly_admissions.csv"))

plot_weekly_admissions = tbl_weekly_admissions %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-12-01"), by = "3 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom")

ggsave("plot_weekly_admissions.jpeg",
       plot_weekly_admissions,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 5, width = 7)


### Admission method ----

tbl_weekly_admissions_by_method = data_admissions %>% 
  mutate(
    date = admission_date %>% cut(breaks = "week")
  ) %>%
  count(date, admission_method.category, .drop = FALSE) %>%
  group_by(admission_method.category) %>% 
  mutate(roll_mean = rollmean(n, 4, align = "right", fill = NA)) %>% 
  ungroup() %>% 
  mutate(date = date %>% as_date(),
         n = n %>% plyr::round_any(count_round))

write_csv(tbl_weekly_admissions_by_method, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_weekly_admissions_by_method.csv"))

plot_weekly_admission_by_method = tbl_weekly_admissions_by_method %>%
  drop_na(admission_method.category) %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-12-01"), by = "6 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom") +
  facet_wrap(~ admission_method.category, scales = "free_y")

ggsave("plot_weekly_admission_by_method.jpeg",
       plot_weekly_admission_by_method,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 5, width = 7)


### Specialty ----

tbl_weekly_admissions_by_specialty = data_admissions %>% 
  mutate(
    date = admission_date %>% cut(breaks = "week")
  ) %>%
  count(date, treatment_function.category, .drop = FALSE) %>%
  group_by(treatment_function.category) %>% 
  mutate(roll_mean = rollmean(n, 4, align = "right", fill = NA)) %>% 
  ungroup() %>% 
  mutate(date = date %>% as_date(),
         n = n %>% plyr::round_any(count_round))

write_csv(tbl_weekly_admissions_by_specialty, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_weekly_admissions_by_specialty.csv"))

plot_weekly_admissions_by_specialty = tbl_weekly_admissions_by_specialty %>%
  drop_na(treatment_function.category) %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-12-01"), by = "6 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom") +
  facet_wrap(~ treatment_function.category, scales = "free_y")

ggsave("plot_weekly_admissions_by_specialty.jpeg",
       plot_weekly_admissions_by_specialty,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 5, width = 7)


### Primary diagnosis ----

tbl_weekly_admissions_by_diagnosis = data_admissions %>% 
  mutate(
    date = admission_date %>% cut(breaks = "week")
  ) %>%
  count(date, primary_diagnosis.chapter, .drop = FALSE) %>%
  group_by(primary_diagnosis.chapter) %>% 
  mutate(roll_mean = rollmean(n, 4, align = "right", fill = NA)) %>% 
  ungroup() %>% 
  mutate(date = date %>% as_date(),
         n = n %>% plyr::round_any(count_round))

write_csv(tbl_weekly_admissions_by_diagnosis, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_weekly_admissions_by_diagnosis.csv"))

plot_weekly_admissions_by_diagnosis = tbl_weekly_admissions_by_diagnosis %>%
  drop_na(primary_diagnosis.chapter) %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-12-01"), by = "6 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom") +
  facet_wrap(~ primary_diagnosis.chapter, ncol = 4, scales = "free_y")

ggsave("plot_weekly_admissions_by_diagnosis.jpeg",
       plot_weekly_admissions_by_diagnosis,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 10, width = 14)

## Specialty summary ----
tbl_admissions_by_specialty = data_admissions %>%
  mutate(treatment_function.description = paste0(treatment_function, ": ", 
                                                 treatment_function.description)) %>% 
  count(treatment_function.description) %>% 
  arrange(desc(n)) %>%
  mutate(
    n = n %>% plyr::round_any(count_round),
    n = paste0(n, " (", format(round(n/sum(n)*100, 1), nsmall = 1), ")")) 

write_csv(tbl_admissions_by_specialty, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_admissions_by_specialty.csv"))


## Diagnosis summary ----
tbl_admissions_by_diagnosis = data_admissions %>%
  mutate(primary_diagnosis.description = paste0(primary_diagnosis, ": ",
                                                primary_diagnosis.description)) %>% 
  count(primary_diagnosis.description) %>% 
  arrange(desc(n)) %>%
  mutate(
    n = n %>% plyr::round_any(count_round),
    n = paste0(n, " (", format(round(n/sum(n)*100, 1), nsmall = 1), ")")) %>% 
  filter()

write_csv(tbl_admissions_by_diagnosis, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_admissions_by_diagnosis.csv"))

## Length of stay ----
tbl_length_of_stay = data_admissions %>%  
  mutate(
    length_of_stay = (discharge_date - admission_date) %>% 
      as.numeric(),
    length_of_stay.factor = length_of_stay %>% 
      cut(breaks = c(-Inf, 0:6, 13, 29, Inf)) %>% 
      fct_recode(
        "0" = "(-Inf,0]",
        "1" = "(0,1]",
        "2" = "(1,2]",
        "3" = "(2,3]",
        "4" = "(3,4]",
        "5" = "(4,5]",
        "6" = "(5,6]",
        "7-13" = "(6,13]",
        "14-29" = "(13,29]",
        "30+"   = "(29, Inf]",
      )
  ) %>% 
  count(length_of_stay.factor) %>% 
  mutate(n = n %>% plyr::round_any(count_round))

write_csv(tbl_length_of_stay, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_length_of_stay.csv"))

plot_length_of_stay = tbl_length_of_stay %>%
  ggplot(aes(length_of_stay.factor, n)) +
  geom_col() +
  labs(x = "Length of stay (days)")

ggsave("plot_length_of_stay.jpeg",
       plot_length_of_stay,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 5, width = 7)


# Outpatient Appointments ----
## All ----
tbl_weekly_outpatient = data_outpatient %>%
  filter(is.na(specialty)) %>% 
  mutate(
    date = outpatient_date %>% cut(breaks = "week")
  ) %>%
  group_by(date) %>% 
  summarise(
    n = sum(outpatient_count)
  ) %>%
  ungroup() %>% 
  mutate(roll_mean = rollmean(n, 4, align = "right", fill = NA),
         date = date %>% as_date(),
         n = n %>% plyr::round_any(count_round))

write_csv(tbl_weekly_outpatient, 
          here::here("output", "descriptives", "summary_datasets",
                     "tbl_weekly_outpatient.csv"))

plot_weekly_outpatient = tbl_weekly_outpatient %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-12-01"), by = "3 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom")

ggsave("plot_weekly_outpatient.jpeg",
       plot_weekly_outpatient,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 5, width = 7)

## By specialty ----
tbl_weekly_outpatient_specialty = data_outpatient %>%
  filter(!is.na(specialty)) %>% 
  mutate(
    date = outpatient_date %>% cut(breaks = "week"),
    specialty = specialty %>% 
      str_replace("TF_", "") %>%
      str_replace_all("_", " ") %>% 
      str_to_sentence()
  ) %>%
  group_by(date, specialty) %>% 
  summarise(
    n = sum(outpatient_count)
  ) %>%
  group_by(specialty) %>% 
  mutate(roll_mean = rollmean(n, 4, align = "right", fill = NA),
         date = date %>% as_date(),
         n = n %>% plyr::round_any(count_round))

write_csv(tbl_weekly_outpatient_specialty, 
          here::here("output", "descriptives", "summary_datasets",
                     "tbl_weekly_outpatient_specialty.csv"))

plot_weekly_outpatient_specialty = tbl_weekly_outpatient_specialty %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-12-01"), by = "3 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom") +
  facet_wrap(~ specialty, ncol = 4, scales = "free_y")

ggsave("plot_weekly_outpatient_specialty.jpeg",
       plot_weekly_outpatient_specialty,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 7, width = 12)


# GP Records ----
## Weekly contact-day count ----
tbl_weekly_gp = data_gp %>%
  distinct(patient_id, gp_date) %>% 
  mutate(
    date = gp_date %>% cut(breaks = "week")
  ) %>% 
  count(date, .drop = FALSE) %>%
  mutate(roll_mean = rollmean(n, 4, align = "right", fill = NA),
         date = date %>% as_date(),
         n = n %>% plyr::round_any(count_round))

write_csv(tbl_weekly_gp, 
          here::here("output", "descriptives", "summary_datasets",  "tbl_weekly_gp.csv"))

plot_weekly_gp = tbl_weekly_gp %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-12-01"), by = "3 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom")

ggsave("plot_weekly_gp.jpeg",
       plot_weekly_gp,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 5, width = 7)

## Weekly contact-day count by disorder ----
tbl_weekly_gp_by_disorder = data_gp %>%
  filter(str_starts(code_type, "KM_")) %>% 
  mutate(
    date = gp_date %>% cut(breaks = "week")
  ) %>% 
  count(date, code_type, .drop = FALSE) %>%
  group_by(code_type) %>% 
  mutate(
    roll_mean = rollmean(n, 4, align = "right", fill = NA)
  ) %>% 
  ungroup() %>% 
  mutate(date = date %>% as_date(),
         code_type = code_type %>%
           str_replace("KM_", "") %>% 
           str_replace_all("_", " ") %>% 
           str_to_sentence(),
         n = n %>% plyr::round_any(count_round))

write_csv(tbl_weekly_gp_by_disorder, 
          here::here("output", "descriptives", "summary_datasets",
                     "tbl_weekly_gp_by_disorder.csv"))

plot_weekly_gp_by_disorder = tbl_weekly_gp_by_disorder %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  facet_wrap(~code_type, scales = "free_y") +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-01-01"), by = "6 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom")

ggsave("plot_weekly_gp_by_disorder.jpeg",
       plot_weekly_gp_by_disorder,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 7, width = 12)


## Weekly contact-day count by read chapter ----
tbl_weekly_gp_by_chapter = data_gp %>%
  filter(str_starts(code_type, "mapped_")) %>% 
  mutate(
    date = gp_date %>% cut(breaks = "week"),
    code_type = code_type %>% 
      str_replace("mapped_", "") %>% 
      str_to_sentence()
  ) %>% 
  count(date, code_type, .drop = FALSE) %>%
  group_by(code_type) %>% 
  mutate(
    roll_mean = rollmean(n, 4, align = "right", fill = NA)
  ) %>% 
  ungroup() %>% 
  mutate(date = date %>% as_date(),
         code_type = code_type %>% 
           str_replace_all("_", " ") %>% 
           str_to_sentence(),
         n = n %>% plyr::round_any(count_round))

write_csv(tbl_weekly_gp_by_chapter, 
          here::here("output", "descriptives", "summary_datasets",
                     "tbl_weekly_gp_by_chapter.csv"))

plot_weekly_gp_by_chapter = tbl_weekly_gp_by_chapter %>% 
  ggplot(aes(date, n)) +
  geom_col() +
  facet_wrap(~code_type, scales = "free_y") +
  geom_line(aes(date, roll_mean, linetype = "4-week averge"), colour = "red") +
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = ymd("2015-01-01"), to = ymd("2022-01-01"), by = "6 month")) +
  labs(y = "Weekly count", x = NULL, linetype = NULL) +
  theme(legend.position = "bottom")

ggsave("plot_weekly_gp_by_chapter.jpeg",
       plot_weekly_gp_by_chapter,
       path = here::here("output", "descriptives", "summary_datasets"),
       height = 7, width = 12)
