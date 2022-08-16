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
library(WeightIt)
library(cobalt)

# Load custom functions and lookup tables ----
source(here::here("analysis", "00_utility_functions.R"))
source(here::here("analysis", "00_lookup_tables.R"))

# Load datasets ----
data_matched    = read_rds(here::here("output", "data", "data_matched.rds"))
data_patient    = read_rds(here::here("output", "data", "data_patient.rds"))

data_matched = data_matched %>%
  left_join(data_patient, by = c("patient_id", "covid_status_tp")) %>% 
  calc_indexed_variables(data_matched %>% pull(test_date)) 

data_matched %>% 
  select(sex, age_group, cerebral_palsy) %>%
  droplevels() %>% 
  summary()



#Using WeightIt to generate weights with multinomial logistic regression
W_out = weightit(covid_status_tp ~ sex + age_group + cerebral_palsy,
                 data = data_matched,
                 method = "ps",
                 use.mlogit = FALSE)





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
