

# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Output directories ----
dir.create(here::here("output", "descriptives", "epidemiology"),
           showWarnings = FALSE, recursive=TRUE)

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

# Plot settings ----
theme_set(theme_bw())
fig_width = 12
fig_height = 8

# Load datasets ----
data_patient = read_rds(here::here("output", "data", "data_patient.rds"))

# Define clac_epi_stats ----
# This calculates prevelance, cumulative incidence and incidence rate for each
# comorbidity
calc_epi_stats = function(index_date, data, condition, group = NULL, ...){
  data %>% 
    mutate(
      age_index = time_length(
        interval(date_of_birth, index_date),
        unit = "years"
      ),
      age_group_index = age_index %>%
        cut(
          breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
          labels = c("Under 4", "4-6", "7-10", "11-14", "15-17", "18+")
        )
    ) %>% 
    filter(
      (age_index >= 4) & (age_index < 18),
      covid_nosocomial == "No",
      covid_discrepant_test == "No",
      (death_date > index_date) | (is.na(death_date))
    )%>% 
    rename(condition_date = all_of(paste0(condition, "_first_date"))) %>%
    group_by(!!!syms(group)) %>%  
    summarise(
      condition,
      index_date,
      n_new_cases = sum((condition_date >= index_date) &
                          (condition_date < (index_date + months(1))),
                        na.rm = TRUE) %>%
        plyr::round_any(count_round),
      n_exg_cases = sum((condition_date < index_date),
                        na.rm = TRUE) %>%
        plyr::round_any(count_round),
      n_pop_total = n() - sum(death_date < index_date,
                              na.rm = TRUE) %>%
        plyr::round_any(count_round),
      n_pop_at_risk = (n_pop_total - n_exg_cases) %>%
        plyr::round_any(count_round),
      person_years = (pmin(death_date, condition_date, index_date + months(1), na.rm = TRUE) - 
                        pmin(death_date, condition_date, index_date, na.rm = TRUE)) %>%
        as.numeric() %>% 
        sum() / 365.25,
      prev = n_exg_cases / n_pop_total,
      prev_LL = prop.test(n_exg_cases,n_pop_total)$conf.int[1],
      prev_UL = prop.test(n_exg_cases,n_pop_total)$conf.int[2],
      cum_inc = n_new_cases / n_pop_at_risk,
      cum_inc_LL = prop.test(n_new_cases,n_pop_at_risk)$conf.int[1],
      cum_inc_UL = prop.test(n_new_cases,n_pop_at_risk)$conf.int[2],
      inc_rate = n_new_cases / person_years,
      inc_rate_LL = poisson.test(n_new_cases,person_years)$conf.int[1],
      inc_rate_UL = poisson.test(n_new_cases,person_years)$conf.int[2] 
    )
}


comorbidity_list = c("asthma", "cancer", "diabetes", "epilepsy", "severe_mental_illness",
                     "cerebral_palsy", "chronic_infections", "devices_and_stomas",
                     "endocrine_disorders", "gastrointestinal_disorders",
                     "haematological_disorders", "immunological_disorders",
                     "learning_and_behaviour_difficulties", "mental_illness",
                     "musculoskeletal_and_rheum", "transplant")

# Map each comorbidity to calc_epi_stats monthly ----
epi_stats_by_condition =  comorbidity_list %>% 
  map(
    function(condition, time_seq){
      time_seq %>% 
        map(calc_epi_stats, data = data_patient, condition) %>% 
        bind_rows()
    },
    time_seq = seq(study_start_date, study_end_date - months(1), by = "month")
  ) %>% 
  bind_rows() %>%
  mutate(
    condition = condition %>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence(),
    condition = if_else(condition == "Musculoskeletal and rheum",
                        "Musculoskeletal and rheumatological disorders",
                        condition)
  )

## Save .csv ----
write_csv(epi_stats_by_condition, 
          here::here("output", "descriptives", "epidemiology",
                     "epi_stats_by_condition.csv"))

# Map each comorbidity to calc_epi_stats monthly ----
epi_stats_by_condition_age =  comorbidity_list %>% 
  map(
    function(condition, time_seq){
      time_seq %>% 
        map(calc_epi_stats, data = data_patient, condition,
            group = "age_group_index") %>% 
        bind_rows()
    },
    time_seq = seq(study_start_date, study_end_date - months(1), by = "month")
  ) %>% 
  bind_rows() %>%
  mutate(
    condition = condition %>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence(),
    condition = if_else(condition == "Musculoskeletal and rheum",
                        "Musculoskeletal and rheumatological disorders",
                        condition),
    n_new_cases = n_new_cases %>% plyr::round_any(count_round),
    n_exg_cases = n_exg_cases %>% plyr::round_any(count_round),
    n_pop_total = n_pop_total %>% plyr::round_any(count_round)
  )

## Save .csv ----
write_csv(epi_stats_by_condition_age, 
          here::here("output", "descriptives", "epidemiology",
                     "epi_stats_by_condition_age.csv"))

# Plot epi stats by condition ----
## Prevelance ----
plot_prevelance_by_condition = epi_stats_by_condition   %>% 
  ggplot(aes(index_date, prev*100)) +
  geom_line() + geom_point(size = 0.5) +
  geom_ribbon(aes(ymin = prev_LL*100, ymax = prev_UL*100),
              alpha = 0.2) +
  facet_wrap(~condition, ncol = 4, scales = "free_y") +
  labs(
    y = "Prevelance (%)", x = NULL
  )

ggsave("plot_prevelance_by_condition.jpeg",
       plot_prevelance_by_condition,
       path = here::here("output", "descriptives", "epidemiology"),
       width = fig_width, height = fig_height)


## Cumulative incidence ----
plot_cuminc_by_condition = epi_stats_by_condition   %>% 
  ggplot(aes(index_date, cum_inc*100)) +
  geom_line() + geom_point(size = 0.5) +
  geom_ribbon(aes(ymin = cum_inc_LL*100, ymax = cum_inc_UL*100),
              alpha = 0.2) +
  facet_wrap(~condition, ncol = 4, scales = "free_y") +
  labs(
    y = "Monthly cumulative incidence (%)", x = NULL
  )

ggsave("plot_cuminc_by_condition.jpeg",
       plot_cuminc_by_condition,
       path = here::here("output", "descriptives", "epidemiology"),
       width = fig_width, height = fig_height)

## Incidence rate ----
plot_inc_rate_by_condition = epi_stats_by_condition   %>% 
  ggplot(aes(index_date, inc_rate*1000)) +
  geom_line() + geom_point(size = 0.5) +
  geom_ribbon(aes(ymin = inc_rate_LL*1000, ymax = inc_rate_UL*1000),
              alpha = 0.2) +
  facet_wrap(~condition, ncol = 4, scales = "free_y") +
  labs(
    y = "Incidence rate (new cases per 1,000 person-years)", x = NULL
  )

ggsave("plot_inc_rate_by_condition.jpeg",
       plot_inc_rate_by_condition,
       path = here::here("output", "descriptives", "epidemiology"),
       width = fig_width, height = fig_height)


# Plot epi stats by condition and age group ----
## Prevelance ----
plot_prevelance_by_condition_age = epi_stats_by_condition_age   %>% 
  ggplot(aes(index_date, prev*100, 
             colour = age_group_index, fill = age_group_index)) +
  geom_line() + geom_point(size = 0.5) +
  geom_ribbon(aes(ymin = prev_LL*100, ymax = prev_UL*100),
              alpha = 0.2, linetype = "dashed", size = 0.1) +
  facet_wrap(~condition, ncol = 4, scales = "free_y") +
  labs(
    y = "Prevelance (%)", x = NULL,
    fill = "Age group", colour = "Age group"
  ) +
  theme(legend.position="bottom")

ggsave("plot_prevelance_by_condition_age.jpeg",
       plot_prevelance_by_condition_age,
       path = here::here("output", "descriptives", "epidemiology"),
       width = fig_width, height = fig_height)


## Cumulative incidence ----
plot_cuminc_by_condition_age = epi_stats_by_condition_age %>% 
  ggplot(aes(index_date, cum_inc*100, 
             colour = age_group_index, fill = age_group_index)) +
  geom_line() + geom_point(size = 0.5) +
  geom_ribbon(aes(ymin = cum_inc_LL*100, ymax = cum_inc_UL*100),
              alpha = 0.2, linetype = "dashed", size = 0.1) +
  facet_wrap(~condition, ncol = 4, scales = "free_y") +
  labs(
    y = "Monthly cumulative incidence (%)", x = NULL,
    fill = "Age group", colour = "Age group"
  ) +
  theme(legend.position="bottom")

ggsave("plot_cuminc_by_condition_age.jpeg",
       plot_cuminc_by_condition_age,
       path = here::here("output", "descriptives", "epidemiology"),
       width = fig_width, height = fig_height)

## Incidence rate ----
plot_inc_rate_by_condition_age = epi_stats_by_condition_age %>% 
  ggplot(aes(index_date, inc_rate*1000, 
             colour = age_group_index, fill = age_group_index)) +
  geom_line() + geom_point(size = 0.5) +
  geom_ribbon(aes(ymin = inc_rate_LL*1000, ymax = inc_rate_UL*1000),
              alpha = 0.2, linetype = "dashed", size = 0.1) +
  facet_wrap(~condition, ncol = 4, scales = "free_y") +
  labs(
    y = "Incidence rate (new cases per 1,000 person-years)", x = NULL,
    fill = "Age group", colour = "Age group"
  ) +
  theme(legend.position="bottom")

ggsave("plot_inc_rate_by_condition_age.jpeg",
       plot_inc_rate_by_condition_age,
       path = here::here("output", "descriptives", "epidemiology"),
       width = fig_width, height = fig_height)

