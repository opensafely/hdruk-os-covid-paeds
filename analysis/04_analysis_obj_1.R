
# Load library
library("tidyverse")
library("lubridate")
library("finalfit")
library("boot")

# Functions ----
meanfun = function(data, i){
  d = data[i]
  return(mean(d))   
}

boot_ci = function(x, statistic, R){
  bootstrap = boot(x, statistic, R)
  ci = bootstrap %>% 
    boot.ci(conf = 0.95, type = "perc")
  output = c(bootstrap$t0, ci$percent[4], ci$percent[5])
  return(output)
}

bootstrap_by_group = function(data, value.var = "n", groups = NULL, 
                              statistic = meanfun, R = 100) {
  
  data = data %>% 
    group_by(across(all_of(groups))) %>% 
    summarise_at(value.var, list("boot_ci"), 
                 statistic = statistic, R = R) %>% 
    mutate(statistic = c("mean", "ci_lower", "ci_upper")) %>% 
    pivot_wider(names_from = "statistic", values_from = "n")
  
  return(data)
}

# Create output directories ----
dir.create(here::here("output", "descriptive", "obj_1"),
           showWarnings = FALSE, recursive=TRUE)


# Read processed data  ----
data_patient_2019 = read_rds(here::here("output", "data", "cohorts", "data_patient_2019.rds"))
data_patient_2020 = read_rds(here::here("output", "data", "cohorts", "data_patient_2020.rds"))
data_patient_2021 = read_rds(here::here("output", "data", "cohorts", "data_patient_2021.rds"))
#data_admissions   = read_rds(here::here("output", "data", "data_admissions.rds"))
data_outpatient   = read_rds(here::here("output", "data", "data_outpatient.rds"))
#data_gp           = read_rds(here::here("output", "data", "data_gp.rds"))

# Extract variable labels
vlabels = extract_variable_label(data_patient_2019)

# Create summary table stratified by cohort year ----
dependent = "cohort_year"
explanatory = c("covid_status", "age", "age_factor", "sex", 
                "ethnicity", "ethnicity_6_sus", "ethnicity_comb",
                "region", "imd_Q5", "rural_urban",
                "asthma", "diabetes",
                "death_factor",
                "admission_count_factor", 
                "gp_contact_count_factor")

tbl_summary_cohort_year = data_patient_2019 %>% 
  select(all_of(c(dependent, explanatory))) %>% 
  bind_rows(data_patient_2020 %>% 
              select(all_of(c(dependent, explanatory)))) %>% 
  bind_rows(data_patient_2021 %>% 
              select(all_of(c(dependent, explanatory)))) %>% 
  summary_factorlist(dependent, explanatory, p = TRUE, 
                     add_col_totals = TRUE,
                     add_row_total = TRUE)

# Define variables to stratify plots by ----
#strat_var = c(NULL, "sex", "age_factor", "ethnicity_comb", "region", "imd_Q5",
#              "rural_urban", "asthma", "diabetes")
strat_var = c(NULL)

# Outpatient appointments ----
strat_var %>% 
  map(function(var){
    outpatient_id = data_patient_2019 %>% 
      select(patient_id, cohort_year, all_of(var)) %>%
      bind_rows(
        data_patient_2020 %>% 
          select(patient_id, cohort_year, all_of(var))
      ) %>% 
      bind_rows(
        data_patient_2021 %>% 
          select(patient_id, cohort_year, all_of(var))
      ) %>% 
      group_by(across(all_of(c("patient_id", "cohort_year", var)))) %>% 
      summarise(period_index = 1:52) %>%
      ungroup()
    
    outpatient_count_nonzero = data_outpatient %>% 
      mutate(period_index = week(outpatient_date),
             cohort_year = year(outpatient_date) %>% as.character()) %>% 
      group_by(patient_id, cohort_year, period_index) %>% 
      count()
    
    outpatient_count = outpatient_id %>% 
      left_join(outpatient_count_nonzero,
                by = c("patient_id", "cohort_year", "period_index")
      ) %>% 
      mutate(n = replace_na(n, 0))
    
    outpatient_mean = outpatient_count %>% 
      bootstrap_by_group(value.var = "n",
                         groups = c("cohort_year", "period_index", var)) %>% 
      mutate(date = paste0(cohort_year, "-01-01") %>% as.Date() +
               (period_index -1)*7 +3)
    
    plot_outpatient = outpatient_mean %>%
      ggplot(aes_string("date", "mean", colour = var, fill = var)) +
      geom_line() +
      geom_ribbon(aes_string(ymin = "ci_lower", ymax = "ci_upper"),
                  alpha = 0.2, linetype = "dotted") +
      theme_bw() +
      labs(x = NULL,
           y = "Outpatient appointments (number per patient per week)",
           colour = vlabels[vlabels %>% names()== var],
           fill   = vlabels[vlabels %>% names()== var])
    
    ggsave(paste0("obj1_outpatient_", var, ".jpeg"),
           plot = last_plot(),
           device = "jpeg",
           path = here::here("output", "descriptive", "obj_1"))
    
  })

# GP contact ----
# strat_var %>% 
#   map(function(var){
#     gp_id = data_patient_2019 %>% 
#       select(patient_id, cohort_year, all_of(var)) %>%
#       bind_rows(
#         data_patient_2020 %>% 
#           select(patient_id, cohort_year, all_of(var))
#       ) %>% 
#       bind_rows(
#         data_patient_2021 %>% 
#           select(patient_id, cohort_year, all_of(var))
#       ) %>% 
#       group_by(across(all_of(c("patient_id", "cohort_year", var)))) %>% 
#       summarise(period_index = 1:52) %>%
#       ungroup()
#     
#     gp_count_nonzero = data_gp %>% 
#       mutate(period_index = week(gp_contact_date),
#              cohort_year = year(gp_contact_date) %>% as.character()) %>% 
#       group_by(patient_id, cohort_year, period_index) %>% 
#       count()
#     
#     gp_count = gp_id %>% 
#       left_join(gp_count_nonzero,
#                 by = c("patient_id", "cohort_year", "period_index")
#       ) %>% 
#       mutate(n = replace_na(n, 0))
#     
#     gp_mean = gp_count %>% 
#       bootstrap_by_group(value.var = "n",
#                          groups = c("cohort_year", "period_index", var)) %>% 
#       mutate(date = paste0(cohort_year, "-01-01") %>% as.Date() +
#                (period_index -1)*7 +3)
#     
#     plot_gp = gp_mean %>%
#       ggplot(aes_string("date", "mean", colour = var, fill = var)) +
#       geom_line() +
#       geom_ribbon(aes_string(ymin = "ci_lower", ymax = "ci_upper"),
#                   alpha = 0.2, linetype = "dotted") +
#       theme_bw() +
#       labs(x = NULL,
#            y = "GP contacts (number per patient per week)",
#            colour = vlabels[vlabels %>% names()== var],
#            fill   = vlabels[vlabels %>% names()== var])
#     
#     ggsave(paste0("obj1_gp_", var, ".jpeg"),
#            plot = last_plot(),
#            device = "jpeg",
#            path = here::here("output", "descriptive", "obj_1"))
#   })
# 
# 
