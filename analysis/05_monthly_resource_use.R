
library(tidyverse)
library(lubridate)
library(finalfit)

# Load custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round = global_var$disclosure_count_round

# Study dates ----
study_start_date = ymd(global_var$start_date)
study_end_date   = ymd(global_var$end_date)
tp_start_date    = ymd(global_var$tp_start_date)
tp_end_date      = ymd(global_var$tp_end_date)
fup_start_date   = ymd(global_var$fup_start_date)

# Create output directory folders ----
dir.create(here::here("output", "descriptives", "healthcare_use_2019_2022", "tables"),
           showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "healthcare_use_2019_2022", "plots"),
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

# resource_type  = "admissions"
# condition      = "12_skin_and_subcutaneous_tissue"
# stratification = "overall"

data_cohort = read_rds(here::here("output", "data", "data_cohort.rds"))

var_labs = extract_variable_label(data_cohort[[1]])

data_cohort = data_cohort%>% 
  bind_rows() %>%
  ff_relabel(var_labs) %>% 
  mutate(overall = "Overall" %>% ff_label("")) %>% 
  select(patient_id, cohort, stratification = all_of(stratification))

patient_id_2019 = data_cohort %>% filter(cohort == 2019) %>% pull(patient_id)
patient_id_2020 = data_cohort %>% filter(cohort == 2020) %>% pull(patient_id)
patient_id_2021 = data_cohort %>% filter(cohort == 2021) %>% pull(patient_id)
patient_id_2022 = data_cohort %>% filter(cohort == 2022) %>% pull(patient_id)


if(resource_type == "gp"){
  
  data_resource = read_rds(here::here("output", "data", "data_gp.rds"))
  
  if(condition == "all"){
    
    data_resource = data_resource %>% 
      filter(str_starts(code_type, "KM_") |
               str_starts(code_type, "mapped_1"))%>% 
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
    data_resource = data_resource %>%
      select(patient_id, admission_date, discharge_date) %>% 
      rowwise() %>% 
      mutate(date = list(seq(admission_date, discharge_date, by = "day"))) %>% 
      unnest(date) %>% 
      mutate(month_date = floor_date(date, "month")) %>% 
      mutate(
        n = case_when(
          admission_date == discharge_date ~ 0.5, # Day-case
          TRUE ~ 1
        )
      ) %>% 
      group_by(patient_id, month_date) %>% 
      summarise(n = sum(n))
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
  group_by(cohort, month_date, stratification) %>% 
  summarise(n_counts = round(sum(n))) %>% 
  ungroup()

if(resource_type == "outpatient"){
  monthly_count = data_cohort %>%
    group_by(stratification) %>% 
    summarise(
      month_date = seq(ymd("2019-04-01"), study_end_date - days(1), by = "month"),
      cohort = year(month_date)
    )
} else {
  monthly_count = data_cohort %>%
    group_by(stratification) %>% 
    summarise(
      month_date = seq(study_start_date, study_end_date - days(1), by = "month"),
      cohort = year(month_date)
    )
}

monthly_count = monthly_count %>% 
  left_join(data_resource %>% 
              select(-cohort),
            by = c("stratification", "month_date")) %>% 
  replace_na(list(n_counts = 0)) %>% 
  mutate(
    n_patient = case_when(
      cohort == 2019 ~ length(patient_id_2019),
      cohort == 2020 ~ length(patient_id_2020),
      cohort == 2021 ~ length(patient_id_2021),
      cohort == 2022 ~ length(patient_id_2022)
    )%>% 
      plyr::round_any(count_round),
    n_counts = n_counts %>% plyr::round_any(count_round),
    n_patient_000 = n_patient/1000
  ) %>% 
  rowwise() %>% 
  mutate(
    estimate = poisson.test(n_counts,n_patient_000)$estimate,
    ci_lower = poisson.test(n_counts,n_patient_000)$conf.int[1],
    ci_upper = poisson.test(n_counts,n_patient_000)$conf.int[2]
  ) %>% 
  ungroup()


if(resource_type == "gp"){
  y_lab = "Monthly healthcare episodes per 1,000 CYP"
} else if(resource_type == "outpatient"){
  y_lab = "Monthly outpatient appointments per 1,000 CYP"
} else if (resource_type == "admissions"){
  y_lab = "Monthly admission per 1,000 CYP"
} else if (resource_type == "beddays"){
  y_lab = "Monthly bed-days per 1,000 CYP"
}

plot_monthly = monthly_count %>% 
  ggplot(aes(month_date, estimate, colour = stratification, fill = stratification)) +
  geom_point(size = 0.5) + geom_line() + 
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, size = 0.1, linetype=2) +
  labs(
    y = y_lab, x = NULL,
    colour = extract_variable_label(monthly_count)["stratification"],
    fill = extract_variable_label(monthly_count)["stratification"]
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom")

# Save monthly count table ----
write_csv(monthly_count, 
          here::here("output", "descriptives", "healthcare_use_2019_2022", "tables",
                     paste0("monthly_", resource_type, "_", condition,
                            "_", stratification, ".csv")))

# Save monthly count plot ----
ggsave(paste0("monthly_", resource_type, "_", condition,
              "_", stratification, ".jpeg"),
       plot_monthly,
       path = here::here("output", "descriptives", "healthcare_use_2019_2022", "plots"),
       height = 5, width = 7)

