library(tidyverse)
library(lubridate)
library(MatchIt)
library(finalfit)

# Functions ----
# prodNA: Assign a random proportion of a vector as NA 
prodNA = function(x, p = 0.1){
  index_na = sample(1:length(x), round(p*length(x)), replace = FALSE)
  x[index_na] = NA
  return(x)
}

# Study dates ----
start_date = ymd("2020-01-01")
end_date = ymd("2020-01-10")
vax_start_date = ymd("2020-01-05")

# Study parameters ----
set.seed(20220517) # Set seed
vax_window_days = 1 # days following vaccination to be considred vaccinated
match_ratio = 2 # 1:match_ratio - positive:negative/untested

# Create patient dataset ----
n_patients = 2000
data_patient = tibble(
  patient_id = 1:n_patients,
  sex = sample(c("Male", "Female"), n_patients, replace = TRUE) %>% 
    factor(),
  age = sample(c(4:17), n_patients, replace = TRUE),
  region = sample(c("A", "B", "C"), n_patients, replace = TRUE) %>% 
    factor(),
  asthma = sample(c("No", "Yes"), n_patients, replace = TRUE, prob = c(0.9, 0.1)) %>% 
    factor(),
  vax_date_1 = sample(seq(vax_start_date, end_date, by = "day"),
                      n_patients,
                      replace = TRUE) %>% 
    prodNA(p = 0.5)
)

# Create covid testing dataset ----
n_test = 2000
data_tests = tibble(
  patient_id = sample(data_patient$patient_id, n_test, replace = TRUE),
  test_date = sample(seq(start_date, end_date, by = "day"),
                     n_test,
                     replace = TRUE),
  test_result = sample(c("Positive", "Negative"), n_test, replace = TRUE,
                       prob = c(0.1,0.9)) %>% 
    factor()
) %>% 
  arrange(patient_id, test_date, test_result) %>% 
  distinct()

# Changed from here ----------------------------------
## Dataset to match on
## All negatives, first positives
data_tests = data_tests %>%
  arrange(patient_id, test_date) %>% 
  group_by(patient_id, test_result) %>% 
  filter((test_result == "Positive" & row_number() == 1) |
           test_result == "Negative")

## If positive, for now remove other negatives
patient_id_positive = data_tests %>% 
  filter(test_result == "Positive") %>% 
  pull(patient_id)

data_tests = data_tests %>%
  filter(
    test_result == "Positive" |
      (test_result == "Negative" & 
         !patient_id %in% patient_id_positive)
  )

matched = vector()
match_out = data_tests %>% 
  group_by(test_date) %>% 
  group_map(
    function(.x, .y){
      
      # Remove matched controls from last iteration
      df_in = .x %>%
        filter(!patient_id %in% matched)
      
      # Match
      df_out = matchit(test_result ~ test_date,
                       data = df_in,
                       exact = ~ test_date,
                       ratio = match_ratio,
                       replace = FALSE)
      
      # Unique ID for subclasses (date pasted with subclass)
      df_out = match.data(df_out) %>% 
        mutate(subclass = paste0(.y, subclass))
      
      # Update remove vector
      matched_iter = df_out %>%
        pull(patient_id)
      matched <<- c(matched, matched_iter)
      
      return(df_out)
    },
    .keep = TRUE
  ) %>% 
  map_df(bind_rows)

cohort = data_patient %>% 
  left_join(match_out %>% select(-c(distance, weights))) %>% 
  mutate(subclass = factor(subclass) %>% as.numeric()) %>% 
  arrange(subclass)

# Cohort Summary
dependent = "test_result"
explanatory = c("age", "sex", "region", "asthma")

cohort %>% 
  summary_factorlist(dependent, explanatory, 
                     add_col_totals   = TRUE)
