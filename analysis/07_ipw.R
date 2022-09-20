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
dir.create(here::here("output", "descriptives", "matched_cohort", "persontime"),
           showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "matched_cohort", "ipw"),
           showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptives", "matched_cohort", "ipw", "balance_plot"),
           showWarnings = FALSE, recursive=TRUE)

# Load global variables ----
global_var = jsonlite::read_json(path = here::here("analysis", "global_variables.json"))

# Disclosure control parameters ----
count_round = global_var$disclosure_count_round

## Study dates ----
start_date     = ymd(global_var$start_date)
end_date       = ymd(global_var$end_date)
tp_start_date  = ymd(global_var$tp_start_date)
tp_end_date    = ymd(global_var$tp_end_date)
fup_start_date = ymd(global_var$fup_start_date)

# Load datasets ----
data_matched    = read_rds(here::here("output", "data", "data_matched.rds"))
data_patient    = read_rds(here::here("output", "data", "data_patient.rds"))
data_testing    = read_rds(here::here("output", "data", "data_testing.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))
data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds"))
data_gp         = read_rds(here::here("output", "data", "data_gp.rds"))

# Extract variable labels ----
var_labels = extract_variable_label(data_patient)

# Calculate weighting variables ----
## Calculate time-dependent variables on matched date ----
data_matched = data_matched %>%
  left_join(data_patient, by = c("patient_id", "covid_status_tp")) %>% 
  calc_indexed_variables(data_matched %>% pull(test_date)) %>% 
  ff_relabel(var_labels)

## Ethnicity - explicitly code missing as factor
data_matched = data_matched %>% 
  mutate(ethnicity = ethnicity %>% 
           fct_explicit_na())

# Calculate number of covid tests days in prior year to match date ----
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
      rename(n_covid_tests = n) %>%
      mutate(
        n_covid_tests =  n_covid_tests %>%
          ff_label("SARS-CoV-2 RT-PCR tests in prior year"),
        n_covid_tests_Q = n_covid_tests %>% ntile(4)
      ),
    by = c("patient_id")
  ) %>%
  replace_na(list(n_covid_tests = 0)) %>%
  mutate(
    n_covid_tests_Q = case_when(
      n_covid_tests == 0 ~ "None",
      n_covid_tests_Q == 1 ~ "1 (low)",
      n_covid_tests_Q == 4 ~ "4 (high)",
      TRUE ~ n_covid_tests_Q %>% as.character()
    ) %>%
      factor() %>%
      fct_relevel("None") %>%
      ff_label("SARS-CoV-2 RT-PCR tests in prior year (quartile)"),
  )

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
      )%>%
      group_by(patient_id) %>%
      summarise(n_beddays = sum(n_beddays)) %>%
      ungroup() %>%
      mutate(n_beddays = n_beddays %>%
               ff_label("Bed-days in prior year"),
             n_beddays_Q = n_beddays %>%
               ntile(4)) %>%
      select(patient_id, n_beddays, n_beddays_Q),
    by = "patient_id"
  ) %>%
  replace_na(list(n_beddays = 0)) %>% 
  mutate(
    n_beddays_Q = case_when(
      n_beddays == 0 ~ "None",
      n_beddays_Q == 1 ~ "1 (low)",
      n_beddays_Q == 4 ~ "4 (high)",
      TRUE ~ n_beddays_Q %>% as.character()
    ) %>%
      factor() %>%
      fct_relevel("None") %>%
      ff_label("Bed-days in prior year (quartile)"),
  )

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
      ) %>%
      ungroup() %>%
      mutate(
        n_outpatient = n_outpatient %>%
          ff_label("Outpatient appointments in prior year"),
        n_outpatient_Q = n_outpatient %>% 
          ntile(4)
      ),
    by = "patient_id"
  ) %>%
  replace_na(list(n_outpatient = 0)) %>% 
  mutate(
    n_outpatient_Q = case_when(
      n_outpatient == 0 ~ "None",
      n_outpatient_Q == 1 ~ "1 (low)",
      n_outpatient_Q == 4 ~ "4 (high)",
      TRUE ~ n_outpatient_Q %>% as.character()
    ) %>%
      factor() %>%
      fct_relevel("None") %>%
      ff_label("Outpatient appointments in prior year (quartile)"),
  )

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
      rename(n_gp = n) %>%
      mutate(
        n_gp = n_gp %>%
          ff_label("Healthcare episodes in prior year"),
        n_gp_Q = n_gp %>% 
          ntile(4)
      ),
    by = "patient_id"
  ) %>%
  replace_na(list(n_gp = 0)) %>% 
  mutate(
    n_gp_Q = case_when(
      n_gp == 0 ~ "None",
      n_gp_Q == 1 ~ "1 (low)",
      n_gp_Q == 4 ~ "4 (high)",
      TRUE ~ n_gp_Q %>% as.character()
    ) %>%
      factor() %>%
      fct_relevel("None") %>%
      ff_label("Healthcare episodes in prior year (quartile)"),
  )

# Calculate person time ----
## Calculate censor dates indexed to match/test date ----
data_matched = data_matched %>% 
  mutate(
    followup_start_date = test_date + days(14),
    one_year_followup_date = followup_start_date + weeks(52),
    turned_positive_date = case_when(
      covid_status_tp == "Untested" ~ covid_test_date_pos_fup,
      covid_status_tp == "Negative" ~ covid_test_date_pos_fup,
      TRUE ~ NA_Date_
    ),
    followup_end_date = pmin(one_year_followup_date, turned_positive_date,
                             death_date, end_date, na.rm = TRUE)
  )

## Calculate person-time ----
data_matched = data_matched %>% 
  mutate(
    person_time = (followup_end_date - followup_start_date) %>% 
      as.numeric()
  ) %>% 
  group_by(match_id) %>% 
  mutate(
    person_time_grouped = min(person_time),
    followup_end_date_grouped = followup_start_date + days(person_time_grouped)
  ) %>% 
  ungroup()

## Save follow-up period statistics ----
table_followup = data_matched %>% 
  summary_factorlist(dependent = "covid_status_tp",
                     explanatory = "person_time_grouped",
                     cont = "median")

write_csv(table_followup,
          here::here("output", "descriptives", "matched_cohort", "persontime",
                     "tbl_followup_stats.csv"))

## Plot person-time/followup period distribution ----
plot_followup_distribution = data_matched %>% 
  ggplot(aes(person_time, colour = "Individual")) +
  geom_density() +
  geom_density(aes(person_time_grouped, colour = "Group minimum")) +
  theme_bw() +
  scale_x_continuous(limits = c(0, NA)) +
  labs(colour = NULL) +
  theme(
    legend.position = "bottom"
  )

ggsave(filename = "plot_followup_distribution.jpeg",
       plot = plot_followup_distribution,
       path = here::here("output", "descriptives", "matched_cohort", "persontime"),
       height = 7, width = 7, units = "in"
)

# Drop unused factors ----
var_labels = extract_variable_label(data_matched)
data_matched = data_matched %>% 
  droplevels() %>% 
  ff_relabel(var_labels)

# Calculate weighting ----
## Predictors
weight_variables = c(
  # Demographics
  "age_group",
  "sex", "ethnicity", "imd_Q5_2019",
  "region_2019", "rural_urban_2019",
  
  # Comorbidities
  "comorbidity_count.factor",
  "mental_health_disorders", "neurodevelopmental_and_behavioural",
  "asthma", "cystic_fibrosis", "other_respiratory",
  "cardiovascular", "epilepsy", "headaches", "other_neurological",
  "gastrointestinal_conditions", "genitourinary", "cancer",
  "non_malignant_haematological", "immunological", "chronic_infections",
  "rheumatology", "congenital_malformation", "diabetes", "other_endocrine",
  "metabolic", "obesity", "transplant", "palliative_care",
  
  # Vaccination status
  "vaccination_status",
  
  # Resource use and covid testing
  "n_beddays_Q", "n_outpatient_Q", "n_gp_Q"
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

# Assess balance across groups ----
## Add weights to matched data ----
data_weighted = data_matched %>% 
  mutate(weights = data_weights$weights)

write_rds(data_weighted,
          here::here("output", "data", "data_weighted.rds"),
          compress="gz")

## Summary of unweighted ----
summary_unweighted = data_weighted %>% 
  summary_factorlist(dependent = "covid_status_tp",
                     explanatory = weight_variables)

write_csv(summary_unweighted,
          here::here("output", "descriptives", "matched_cohort", "ipw", 
                     "summary_unweighted.csv"))

## Summary of weighted ---- 
# Latest finalfit version not installed: can't use weights input
# summary_weighted = data_weighted %>% 
#   summary_factorlist(dependent = "covid_status_tp",
#                      explanatory = weight_variables,
#                      weights = "weights")
# 
# write_csv(x = summary_weighted,
#           file = here::here("output", "descriptives", "matched_cohort", "ipw",
#                             "summary_weighted.csv"))

## Effective sample size ----
balance_summary = bal.tab(data_weights, un = TRUE)

table_effective_sample_size = balance_summary$Observations %>% 
  as_tibble(rownames = "Adjustment")

write_csv(table_effective_sample_size,
          here::here("output", "descriptives", "matched_cohort", "ipw",
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


# Create label lookup table ----
var_label_lookup = tibble(
  var_cobalt = var.names(pair_balance)
) %>%
  mutate(
    var = var_cobalt %>% 
      str_extract(".+(?=_[[:graph:] ^_]+$)"),
    level = var_cobalt %>% 
      str_remove(paste0(var, "_")),
  ) %>%
  left_join(
    tibble(var_labels = var_labels,
           var = names(var_labels)),
    by = "var"
  ) %>% 
  mutate(
    var_label_level = paste0(var_labels, ": ", level)
  )

## Vector used for labeling plots ----
var_label_level = var_label_lookup$var_label_level
names(var_label_level) = var_label_lookup$var_cobalt

# Create pair balance table ----
table_pair_balance = map(pair_balance$Pair.Balance,
     function(pair_balance_list){
       
       balance = pair_balance_list$Balance
       observations = pair_balance_list$Observations
       
       output = balance %>%
         as_tibble(rownames = "var_cobalt") %>% 
         mutate(M.0 = colnames(observations)[1],
                M.1 = colnames(observations)[2],
                M.0_n = observations[1,1],
                M.0_ESS = observations[2,1],
                M.1_n = observations[1,2],
                M.1_ESS = observations[2,2])
     }) %>% 
  bind_rows()

# Create balance summary table ----
table_balance_summmary = table_pair_balance %>%
  pivot_longer(
    cols = -c(var_cobalt, Type, M.0, M.1)
  ) %>%
  mutate(
    name = name %>%
      str_replace("M.0", M.0) %>% 
      str_replace("M.1", M.1) %>% 
      str_replace("Diff", paste0(M.0, "_", M.1, "_Diff"))
  ) %>% 
  select(-c(M.0, M.1, Type)) %>%
  distinct() %>% 
  pivot_wider() %>% 
  mutate(
    Negative.Un = paste0(
      plyr::round_any(Negative_n*Negative.Un, count_round), " (",
      round_tidy(Negative.Un*100,1), ")"
    ),
    Negative.Adj = paste0(
      round_tidy(Negative_ESS*Negative.Adj, 1), " (",
      round_tidy(Negative.Adj*100,1), ")"
    ),
    Positive.Un = paste0(
      plyr::round_any(Positive_n*Positive.Un, count_round), " (",
      round_tidy(Positive.Un*100,1), ")"
    ),
    Positive.Adj = paste0(
      round_tidy(Positive_ESS*Positive.Adj, 1), " (",
      round_tidy(Positive.Adj*100,1), ")"
    ),
    Untested.Un = paste0(
      plyr::round_any(Untested_n*Untested.Un, count_round), " (",
      round_tidy(Untested.Un*100,1), ")"
    ),
    Untested.Adj = paste0(
      round_tidy(Untested_ESS*Untested.Adj, 1), " (",
      round_tidy(Untested.Adj*100,1), ")"
    ),
    Abs_Max_Diff.Un = round_tidy(pmax(abs(Negative_Positive_Diff.Un),
                                      abs(Negative_Untested_Diff.Un),
                                      abs(Positive_Untested_Diff.Un))*100, 1),
    Abs_Max_Diff.Adj = round_tidy(pmax(abs(Negative_Positive_Diff.Adj),
                                       abs(Negative_Untested_Diff.Adj),
                                       abs(Positive_Untested_Diff.Adj))*100, 1),
    Negative_Positive_Diff.Un = round_tidy(Negative_Positive_Diff.Un*100, 1),
    Negative_Untested_Diff.Un = round_tidy(Negative_Untested_Diff.Un*100, 1),
    Positive_Untested_Diff.Un = round_tidy(Positive_Untested_Diff.Un*100, 1),
    Negative_Positive_Diff.Adj = round_tidy(Negative_Positive_Diff.Adj*100, 1),
    Negative_Untested_Diff.Adj = round_tidy(Negative_Untested_Diff.Adj*100, 1),
    Positive_Untested_Diff.Adj = round_tidy(Positive_Untested_Diff.Adj*100, 1),
  )

## Add variable labels to table ----
table_balance_summmary = table_balance_summmary %>% 
  left_join(var_label_lookup, by = "var_cobalt")

## Organise columns ----
table_balance_summmary = table_balance_summmary %>% 
  select(var_labels, level,
         Negative.Un, Positive.Un, Untested.Un,
         Negative.Adj, Positive.Adj, Untested.Adj,
         Negative_Positive_Diff.Un, Negative_Positive_Diff.Adj,
         Negative_Untested_Diff.Un, Negative_Untested_Diff.Adj,
         Positive_Untested_Diff.Un, Positive_Untested_Diff.Adj,
         Abs_Max_Diff.Un, Abs_Max_Diff.Adj)

## Save balance table and balance summary tables ----
write_csv(table_pair_balance,
          here::here("output", "descriptives", "matched_cohort", "ipw",
                     "table_pair_balance.csv"))

write_csv(table_balance_summmary,
          here::here("output", "descriptives", "matched_cohort", "ipw",
                     "table_balance_summmary.csv"))


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
                      which.treat = .all, abs = TRUE, position = "bottom",
                      var.names = var_label_level
                      )

ggsave(filename = paste0("love_plot.jpeg"),
       plot = love_plot,
       path = here::here("output", "descriptives", "matched_cohort", "ipw"),
       width = 10, height = 10, units = "in")
