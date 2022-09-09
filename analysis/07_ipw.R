# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# Calculate inverse probability weights
# 07_ipw.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
#
# This script calculates the inverse probability weights using multinomial 
# regression based on covid status during the testing period (positive, negative,
# untested). Balance across groups are assessed by calculating standardised mean
# differences.

# Load packages ----
library(tidyverse)
library(lubridate)
library(WeightIt)
library(cobalt)

# Load custom functions and lookup tables ----
source(here::here("analysis", "00_utility_functions.R"))
source(here::here("analysis", "00_lookup_tables.R"))

# Output directories ----
dir.create(here::here("output", "data"),
           showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "matched_cohort", "ipw"),
           showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "matched_cohort", "ipw", "balance_plot"),
           showWarnings = FALSE, recursive=TRUE)


# Load datasets ----
data_matched    = read_rds(here::here("output", "data", "data_matched.rds"))
data_patient    = read_rds(here::here("output", "data", "data_patient.rds"))
data_testing    = read_rds(here::here("output", "data", "data_testing.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))
data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds"))
data_gp         = read_rds(here::here("output", "data", "data_gp.rds"))

# Calculate weighting variables ----
## Calculate time-dependent variables on matched date ----
data_matched = data_matched %>%
  left_join(data_patient, by = c("patient_id", "covid_status_tp")) %>% 
  calc_indexed_variables(data_matched %>% pull(test_date)) 

## Ethnicity - explicitly code missing as factor
data_matched = data_matched %>% 
  mutate(ethnicity = ethnicity %>% 
           fct_explicit_na())

## Calculate number of covid tests days in prior year to match date ----
data_matched = data_matched %>% 
  left_join(
    data_testing %>% 
      left_join(data_matched %>% 
                  select(patient_id, match_date = test_date),
                by = "patient_id") %>% 
      filter(test_date >= match_date - years(1),
             test_date < match_date) %>% 
      distinct(patient_id, test_date) %>% 
      count(patient_id) %>% 
      rename(n_covid_tests = n),
    by = c("patient_id")
  ) %>% 
  replace_na(list(n_covid_tests = 0))

## Calculate number of beddays in prior year to match date ----
data_matched = data_matched %>% 
  left_join(
    data_admissions %>% 
      left_join(data_matched %>% 
                  select(patient_id, test_date),
                by = "patient_id") %>% 
      filter(admission_date >= test_date - years(1),
             admission_date < test_date) %>% 
      mutate(
        n_beddays = case_when(
          admission_date == discharge_date ~ 0.5,
          TRUE ~ (pmin(discharge_date, test_date - days(1)) - admission_date) %>% 
            as.numeric()
        )
      ) %>% 
      select(patient_id, n_beddays),
    by = "patient_id"
  ) %>% 
  replace_na(list(n_beddays = 0))

## Calculate number of outpatient appointments in prior year to match date ----
data_matched = data_matched %>% 
  left_join(
    data_outpatient %>%
      filter(is.na(specialty)) %>% 
      left_join(
        data_matched %>% 
          select(patient_id, test_date),
        by = "patient_id"
      ) %>%
      filter(outpatient_date >= test_date - years(1),
             outpatient_date <  test_date) %>% 
      group_by(patient_id) %>% 
      summarise(
        n_outpatient = sum(outpatient_count)
      ),
    by = "patient_id"
  ) %>% 
  replace_na(list(n_outpatient = 0))
    
## Calculate number of GP contact days in prior year to match date ----
data_matched = data_matched %>% 
  left_join(
    data_gp %>%
      filter(str_starts(code_type, "KM_") |
               str_starts(code_type, "mapped_1") |
               str_starts(code_type, "mapped_2")) %>% 
      left_join(
        data_matched %>% 
          select(patient_id, test_date),
        by = "patient_id"
      ) %>%
      filter(gp_date >= test_date - years(1),
             gp_date <  test_date) %>%
      distinct(patient_id, gp_date) %>%
      count(patient_id) %>% 
      rename(n_gp = n),
    by = "patient_id"
  ) %>% 
  replace_na(list(n_gp = 0))

# Calculate weighting ----
## Predictors
weight_variables = c(
  # Demographics
  "age_group", "sex", "ethnicity", "imd_Q5_2019",
  "region_2019", "rural_urban_2019",
  
  # Comorbidities
  "comorbidity_count.factor", 
  "asthma", "cancer", "cerebral_palsy", "chronic_infections", "cystic_fibrosis",
  "devices_and_stomas", "diabetes", "endocrine_disorders", 
  "epilepsy", "gastrointestinal_disorders", "haematological_disorders",
  "immunological_disorders", "learning_and_behaviour_difficulties",
  "mental_illness", "musculoskeletal_and_rheum",
  "severe_mental_illness", "transplant",
  
  # Resource use and covid testing
  "n_covid_tests",
  "n_beddays", "n_outpatient", "n_gp"
)

## Model forumla ----
model_formula = paste0("covid_status_tp ~ ",
                       paste(weight_variables, collapse = " + ")) %>% 
  as.formula()

## Using WeightIt to generate weights with multinomial logistic regression ----
data_weights = weightit(model_formula,
                 data = data_matched,
                 method = "ps",
                 use.mlogit = FALSE)

write_rds(x = data_weights,
          file = here::here("output", "data", "data_weights.rds"),
          compress="gz")

# Assess balance across groups ----
## Add weights to matched data ----
data_matched = data_matched %>% 
  mutate(weights = data_weights$weights)

## Summary of unweighted ----
summary_unweighted = data_matched %>% 
  summary_factorlist(dependent = "covid_status_tp",
                     explanatory = weight_variables)

write_csv(x = summary_unweighted,
          file = here::here("output", "descriptives", "matched_cohort", "ipw", 
                            "summary_unweighted.csv"))

## Summary of weighted ----
summary_weighted = data_matched %>% 
  summary_factorlist(dependent = "covid_status_tp",
                     explanatory = weight_variables,
                     weights = "weights")

write_csv(x = summary_weighted,
          file = here::here("output", "descriptives", "matched_cohort", "ipw",
                            "summary_weighted.csv"))

## Effective sample size ----
balance_summary = bal.tab(data_weights, un = TRUE)

table_effective_sample_size = balance_summary$Observations %>% 
  as_tibble(rownames = "Adjustment")

write_csv(x = table_effective_sample_size,
          file = here::here("output", "descriptives", "matched_cohort", "ipw",
                            "table_effective_sample_size.csv"))

## Assess balance for each pair of treatments ----
pair_balance = bal.tab(data_weights, un = TRUE, disp.means = TRUE, which.treat = .all)

table_pair_balance = map2(pair_balance$Pair.Balance,
                          names(pair_balance$Pair.Balance),
                          function(pair_balance_list, list_name){
  output = pair_balance_list$Balance %>%
    as_tibble(rownames = "var") %>% 
    mutate(pair = list_name)
}) %>% 
  bind_rows()

write_csv(x = table_pair_balance,
          file = here::here("output", "descriptives", "matched_cohort", "ipw",
                            "table_pair_balance.csv"))

## Assess balance graphically ----
weight_variables %>% 
  map(function(weight_var){
    balance_plot = bal.plot(data_weights, weight_var, which = "both")
    ggsave(
      filename = paste0("balance_plot_", weight_var, ".jpeg"),
      plot = balance_plot,
      path = here::here("output", "descriptives", "matched_cohort", "ipw",
                        "balance_plot"),
      width = 10, height = 7, units = "in")
  })

# Create love plot ----
love_plot = love.plot(data_weights, thresholds = c(m = .1), binary = "std",
                      which.treat = .all, abs = TRUE, position = "bottom")

ggsave(filename = paste0("love_plot.jpeg"),
       plot = love_plot,
       path = here::here("output", "descriptives", "matched_cohort", "ipw"),
       width = 10, height = 10, units = "in")



# 
# library(tidyverse)
# library(cobalt)
# 
# set.seed(42)
# n_pos_sample = 100
# match_ratio = 10
# n_total = n_pos_sample*(1 + 2*match_ratio)
# 
# df = tibble(
#   person_time = rnorm(n_total, 12, 2),
#   health_contact = rpois(n_total, 10),
#   status = c(
#     rep("Pos", n_pos_sample),
#     rep("Neg", n_pos_sample*match_ratio),
#     rep("Unt", n_pos_sample*match_ratio)) %>% 
#     factor() %>% 
#     fct_relevel("Unt")
# ) %>%
#   rowwise() %>% 
#   mutate(
#     sex = case_when(
#       status == "Pos" ~ sample(c("M", "F"), 1, replace = TRUE, prob = c(0.5, 0.5)),
#       status == "Neg" ~ sample(c("M", "F"), 1, replace = TRUE, prob = c(0.5, 0.5)),
#       status == "Unt" ~ sample(c("M", "F"), 1, replace = TRUE, prob = c(0.7, 0.3))
#     ),
#     asthma = case_when(
#       status == "Pos" ~ sample(c("Yes", "No"), 1, replace = TRUE, prob = c(0.7, 0.3)),
#       status == "Neg" ~ sample(c("Yes", "No"), 1, replace = TRUE, prob = c(0.5, 0.5)),
#       status == "Unt" ~ sample(c("Yes", "No"), 1, replace = TRUE, prob = c(0.3, 0.7))
#     ),
#     age_group = case_when(
#       status == "Pos" ~ sample(c("4-6", "7-10", "11-14", "15-18"), 1, prob = c(0.1, 0.15, 0.25, 0.5)),
#       status == "Neg" ~ sample(c("4-6", "7-10", "11-14", "15-18"), 1, prob = c(0.1, 0.25, 0.25, 0.4)),
#       status == "Unt" ~ sample(c("4-6", "7-10", "11-14", "15-18"), 1, prob = c(0.5, 0.15, 0.15, 0.1))
#     )
#   ) %>% 
#   ungroup() %>% 
#   mutate(
#     age_group = age_group %>% 
#       factor() %>% 
#       fct_relevel(c("4-6", "7-10", "11-14", "15-18")),
#   )
# 
# 
# #Using WeightIt to generate weights with multinomial
# #logistic regression
# W.out.mn <- WeightIt::weightit(status ~ sex + asthma + age_group, data = df,
#                                method = "ps", use.mlogit = FALSE)
# 
# #Balance summary across treatment pairs
# bal.tab(W.out.mn, un = TRUE)
# 
# #Assessing balance for each pair of treatments
# bal.tab(W.out.mn, un = TRUE, disp.means = TRUE, which.treat = .all)
# 
# x = bal.tab(W.out.mn, un = TRUE, disp.means = TRUE, which.treat = .all)
# 
# df = df %>%
#   ungroup() %>% 
#   mutate(weights = W.out.mn$weights)
# 
# (1/df$weights) %>% sum()
# 
# #Assessing balance graphically
# bal.plot(W.out.mn, "sex", which = "both")
# bal.plot(W.out.mn, "asthma", which = "both")
# bal.plot(W.out.mn, "age_group", which = "both")
# 
# #Summarizing balance in a Love plot
# love.plot(W.out.mn, thresholds = c(m = .1), binary = "std",
#           which.treat = .all, abs = TRUE, position = "bottom")
# 
# ## Model healthcare contacts using poisson regression
# library(MASS)
# library(broom)
# 
# fit_pois = glm(health_contact ~ status + sex + asthma + offset(person_time),
#                weights = W.out.mn$weights,
#                data = df,
#                family = poisson)
# fit_pois %>% 
#   tidy(conf.int = TRUE, exponentiate = TRUE)
# 
# ## Model healthcare contacts using negative binomial regression
# fit_nb = glm.nb(health_contact ~ status + sex + asthma + offset(person_time),
#                 weights = W.out.mn$weights,
#                 data = df,
#                 maxit = 100)
# fit_nb %>% 
#   tidy(conf.int = TRUE, exponentiate = TRUE)
