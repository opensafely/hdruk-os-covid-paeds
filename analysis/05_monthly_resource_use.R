
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

# Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Create output directory folders ----
dir.create(here::here("output", "descriptives", "healthcare_use_2019_2022", "monthly_tables"),
           showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "healthcare_use_2019_2022", "monthly_plots"),
           showWarnings = FALSE, recursive=TRUE)

#Plot theme
theme_set(theme_bw())

args = commandArgs(trailingOnly=TRUE)

if(length(args) == 0){
  resource_type  = "gp"
  condition      = "all"
  stratification = "overall"
} else{
  resource_type  = args[[1]]
  condition      = args[[2]]
  stratification = args[[3]]
}

# Load cohort data ----
data_cohort = read_rds(here::here("output", "data", "data_cohort.rds"))

# Extract variable labels ----
var_labs = extract_variable_label(data_cohort[[1]])


data_cohort = data_cohort%>% 
  bind_rows() %>%
  ff_relabel(var_labs) %>% 
  mutate(overall = "Overall" %>% ff_label("Overall")) %>% 
  select(patient_id, cohort, strata_level = all_of(stratification))

# Extract patient IDs for each year ----
patient_id_2019 = data_cohort %>% filter(cohort == 2019) %>% pull(patient_id)
patient_id_2020 = data_cohort %>% filter(cohort == 2020) %>% pull(patient_id)
patient_id_2021 = data_cohort %>% filter(cohort == 2021) %>% pull(patient_id)
patient_id_2022 = data_cohort %>% filter(cohort == 2022) %>% pull(patient_id)


if(resource_type == "gp"){
  
  data_resource = read_rds(here::here("output", "data", "data_gp.rds"))
  
  if(condition == "all"){
    
    data_resource = data_resource %>% 
      filter(str_starts(code_type, "KM_") |
               str_starts(code_type, "mapped_1") |
               str_starts(code_type, "mapped_2"))%>% 
      distinct(patient_id, gp_date) %>% 
      rename(date = gp_date) %>% 
      mutate(month_date = floor_date(date, "month")) %>% 
      count(patient_id, month_date)
    
  } else{
    
    data_resource = data_resource %>% 
      filter(code_type == paste0("KM_", condition)) %>% 
      distinct(patient_id, gp_date) %>% 
      rename(date = gp_date) %>% 
      mutate(month_date = floor_date(date, "month")) %>% 
      count(patient_id, month_date)
    
  }
  
} else if(resource_type == "outpatient"){
  
  data_resource = read_rds(here::here("output", "data", "data_outpatient.rds"))
  
  if(condition == "all"){
    
    data_resource = data_resource %>% 
      filter(is.na(specialty)) %>% 
      rename(date = outpatient_date) %>% 
      mutate(month_date = floor_date(date, "month")) %>%
      group_by(patient_id, month_date) %>% 
      summarise(n = sum(outpatient_count)) %>% 
      ungroup()
    
  } else{
    
    data_resource = data_resource %>% 
      filter(specialty == paste0("TF_", condition)) %>% 
      rename(date = outpatient_date) %>% 
      mutate(month_date = floor_date(date, "month")) %>%
      group_by(patient_id, month_date) %>% 
      summarise(n = sum(outpatient_count)) %>% 
      ungroup()
    
  }
  
} else if(resource_type == "admissions" | resource_type == "beddays"){
  
  data_resource = read_rds(here::here("output", "data", "data_admissions.rds"))
  
  if(condition != "all"){
    data_resource = data_resource %>% 
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
      filter(primary_diagnosis.chapter == condition)
  }
  
  if (resource_type == "admissions"){
    
    data_resource = data_resource %>% 
      rename(date = admission_date) %>% 
      mutate(month_date = floor_date(date, "month")) %>% 
      count(patient_id, month_date)
    
  } else {
    
    data_resource = seq(study_start_date,
                        study_end_date - days(1),
                        by = "month") %>%
      as.list() %>% 
      map(function(month_date){
        data_resource %>%
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
    
  }
  
} else {
    stop("Unrecognised resource_type")
}


data_resource = data_resource %>% 
  mutate(cohort = year(month_date))%>% 
  filter(
    ((patient_id %in% patient_id_2019) & (cohort == 2019)) |
      ((patient_id %in% patient_id_2020) & (cohort == 2020)) |
      ((patient_id %in% patient_id_2021) & (cohort == 2021)) |
      ((patient_id %in% patient_id_2022) & (cohort == 2022))
  )

data_resource = data_resource %>% 
  left_join(data_cohort, by = c("patient_id", "cohort"))

data_resource = data_resource %>%
  group_by(cohort, month_date, strata_level) %>% 
  summarise(n_counts = round(sum(n))) %>% 
  ungroup()

if(resource_type == "outpatient"){
  monthly_count = data_cohort %>%
    group_by(strata_level) %>% 
    summarise(
      month_date = seq(ymd("2019-04-01"), study_end_date - days(1), by = "month"),
      cohort = year(month_date)
    )
} else {
  monthly_count = data_cohort %>%
    group_by(strata_level) %>% 
    summarise(
      month_date = seq(study_start_date, study_end_date - days(1), by = "month"),
      cohort = year(month_date)
    )
}

monthly_count = monthly_count %>%
  left_join(
    data_cohort %>%
      group_by(strata_level, cohort) %>%
      summarise(
        n_patient = n()
      ),
    by = c("strata_level", "cohort")
  ) %>% 
  left_join(
    data_resource,
    by = c("strata_level", "month_date", "cohort")) %>% 
  replace_na(list(n_counts = 0))

# Remove low counts ----
monthly_count = monthly_count %>% 
  mutate(
    resource_type = resource_type,
    condition = condition,
    strata_variable = stratification,
    strata_label = extract_variable_label(monthly_count)["strata_level"],
    n_patient = if_else(n_patient <= count_redact,
                        NA_real_,
                        n_patient %>% plyr::round_any(count_round)),
    n_counts = if_else(n_counts <= count_redact, NA_real_,
                       n_counts %>% plyr::round_any(count_round)),
    n_patient_000 = if_else(is.na(n_patient), NA_real_, n_patient/1000)
  ) %>% 
  rowwise() %>% 
  mutate(
    estimate = ifelse(is.na(n_patient) | is.na(n_counts), NA_real_,
                       poisson.test(n_counts,n_patient_000)$estimate),
    ci_lower = ifelse(is.na(n_patient) | is.na(n_counts), NA_real_,
                       poisson.test(n_counts,n_patient_000)$conf.int[1]),
    ci_upper = ifelse(is.na(n_patient) | is.na(n_counts), NA_real_,
                       poisson.test(n_counts,n_patient_000)$conf.int[2])
  ) %>% 
  ungroup() %>% 
  relocate(resource_type, condition, strata_variable, strata_label, strata_level,
           month_date, cohort, month_date)

# Assign y axis label based on resource type ----
if(resource_type == "gp"){
  y_lab = "Monthly healthcare episodes per 1,000 CYP"
} else if(resource_type == "outpatient"){
  y_lab = "Monthly outpatient appointments per 1,000 CYP"
} else if (resource_type == "admissions"){
  y_lab = "Monthly admission per 1,000 CYP"
} else if (resource_type == "beddays"){
  y_lab = "Monthly bed-days per 1,000 CYP"
}

# Plot monthly resource use ----
plot_monthly = monthly_count %>% 
  ggplot(aes(month_date, estimate, colour = strata_level, fill = strata_level)) +
  geom_point(size = 0.5) + geom_line() + 
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, size = 0.1, linetype=2) +
  labs(
    y = y_lab, x = NULL,
    colour = extract_variable_label(monthly_count)["strata_level"],
    fill = extract_variable_label(monthly_count)["strata_level"]
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom")

# Fill missing with redacted label ----
monthly_count = monthly_count %>% 
  mutate(n_counts  = n_counts %>% as.character(),
         n_patient = n_patient %>% as.character()) %>% 
  replace_na(list(n_counts  = "[REDACTED]",
                  n_patient = "[REDACTED]"))


# Save monthly count table ----
write_csv(monthly_count, 
          here::here("output", "descriptives", "healthcare_use_2019_2022", "monthly_tables",
                     paste0("monthly_", resource_type, "_", condition,
                            "_", stratification, ".csv")))

# Save monthly count plot ----
ggsave(paste0("monthly_", resource_type, "_", condition,
              "_", stratification, ".jpeg"),
       plot_monthly,
       path = here::here("output", "descriptives", "healthcare_use_2019_2022", "monthly_plots"),
       height = 5, width = 7)


