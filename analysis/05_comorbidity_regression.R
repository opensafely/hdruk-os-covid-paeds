library(tidyverse)
library(lubridate)
library(finalfit)
library(broom)
library(broom.helpers)

# Source custom functions ----
source(here::here("analysis", "00_utility_functions.R"))

# Command arguments ----
args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0){
  resource_type  = "gp"
  model_type     = "negative_binomial"
} else{
  resource_type  = args[[1]]
  model_type     = args[[2]]
}

# Check command argument validity ----
if(!resource_type %in% c("gp", "outpatient", "admissions", "beddays") |
   !model_type %in% c("poisson", "negative_binomial")){
  stop("Invalid command arguments")
}

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
dir.create(here::here("output", "comorbidity_multivar", model_type, resource_type, "model"),
           showWarnings = FALSE, recursive=TRUE)

# Load cohort data ----
data_cohort = read_rds(here::here("output", "data", "data_cohort.rds"))

# Extract variable labels ----
var_labs = extract_variable_label(data_cohort[[1]])

# Bind yearly extracts, drop unused levels, and relabel ----
data_cohort = data_cohort%>% 
  bind_rows() %>%
  mutate(across(where(is.factor), fct_drop)) %>% 
  ff_relabel(var_labs)

# Calculate days under observation ----
# Earliest of calander year-end, death date or study end date less 1st Jan
# (except for outpatient data where records begin from 1st April 2019)
data_cohort = data_cohort %>% 
  mutate(
    days = (pmin(ymd(paste0(cohort, "-12-31")), death_date, study_end_date,
                na.rm = TRUE) - 
              if_else(resource_type == "outpatient" & cohort == 2019,
                      ymd("2019-04-01"), ymd(paste0(cohort, "-01-01")))) %>% 
      as.numeric() + 1
  )

# List of explanatory variables----
var_explanatory = c(
  # Demographics ----
  "age_group", "sex", "ethnicity", "imd_Q5_2019", "region_2019",
  "rural_urban_2019",
  
  # Comorbidities ----
  "mental_health_disorders", "neurodevelopmental_and_behavioural",
  "asthma", "cystic_fibrosis", "other_respiratory",
  "cardiovascular", "epilepsy", "headaches", "other_neurological",
  "gastrointestinal_conditions", "genitourinary", "cancer",
  "non_malignant_haematological", "immunological", "chronic_infections",
  "rheumatology", "congenital_malformation", "diabetes", "other_endocrine",
  "metabolic", "transplant", "palliative_care",
  
  # Comorbidity interactions
  "other_neurological_with_other_respiratory",
  "other_neurological_with_cardiovascular",
  "other_respiratory_with_cardiovascular"
)

# Retain regression variables in dataset ----
data_cohort = data_cohort %>% 
  select(all_of(c("patient_id", "days", var_explanatory)), year = cohort) %>%
  mutate(year = year %>% factor()) %>% 
  drop_na() %>% 
  filter(days > 0)

# Summary table
tbl_cohort_summary = data_cohort %>% 
  summary_factorlist(
    dependent = "year",
    explanatory = var_explanatory,
    cont = "median",
    total_col = FALSE,
    add_col_totals = TRUE,
    na_include = TRUE
  ) %>% 
  ff_round_counts(count_round) %>% 
  ff_redact_counts(count_redact)

## Save Summary table -----
write_csv(tbl_cohort_summary, 
          here::here("output", "comorbidity_multivar", model_type, resource_type,
                     paste0("tbl_cohort_summary.csv")))

# Calculate yearly resource use ----
if(resource_type == "gp"){
  
  data_resource = read_rds(here::here("output", "data", "data_gp.rds"))
  
  data_resource = data_resource %>%
    filter(patient_id %in% data_cohort$patient_id) %>% 
    filter(str_starts(code_type, "KM_") |
             str_starts(code_type, "mapped_1") |
             str_starts(code_type, "mapped_2"))%>% 
    distinct(patient_id, gp_date) %>% 
    mutate(year = year(gp_date)) %>% 
    count(patient_id, year)
  
} else if (resource_type == "outpatient"){
  
  data_resource = read_rds(here::here("output", "data", "data_outpatient.rds"))
  
  data_resource = data_resource %>%
    filter(patient_id %in% data_cohort$patient_id) %>% 
    filter(is.na(specialty)) %>% 
    mutate(year = year(outpatient_date)) %>%
    group_by(patient_id, year) %>% 
    summarise(n = sum(outpatient_count)) %>% 
    ungroup()
  
} else if (resource_type == "admissions" | resource_type == "beddays"){
  
  data_resource = read_rds(here::here("output", "data", "data_admissions.rds"))
  
  if (resource_type == "admissions"){
    
    data_resource = data_resource %>%
      filter(patient_id %in% data_cohort$patient_id) %>% 
      mutate(year = year(admission_date)) %>% 
      count(patient_id, year)
    
  } else {
    
    data_resource = seq(year(study_start_date),
                        year(study_end_date - days(1))) %>%
      as.list() %>% 
      map(function(year){
        data_resource %>%
          filter(patient_id %in% data_cohort$patient_id) %>% 
          select(patient_id, admission_date, discharge_date) %>%
          mutate(
            year = year,
            start_date = ymd(paste0(year, "-01-01")),
            end_date = if_else(year == 2022,
                               ymd("2022-04-30"),
                               (start_date + years(1) - days(1)))
          ) %>%
          filter(admission_date <= end_date,
                 discharge_date >= start_date) %>% 
          mutate(
            length_of_stay = case_when(
              admission_date == discharge_date ~ 0.5, # day-case
              TRUE ~ (pmin(discharge_date, end_date) -
                        pmax(admission_date, start_date)) %>% as.numeric()
            )
          )%>%
          group_by(patient_id, year) %>% 
          summarise(
            n = sum(length_of_stay) %>% round()
          ) %>% 
          ungroup()
      })%>% 
      bind_rows()
  }
}

# Add resource data to patient data, replace NAs with 0 count ----
data_cohort = data_cohort %>% 
  left_join(
    data_resource %>%
      mutate(year = year %>% as.character()) %>% 
      select(patient_id, year, n),
    by = c("patient_id", "year")
  ) %>% 
  replace_na(list(n = 0)) %>% 
  rename(health_contact = n) %>% 
  mutate(year = year %>%
           factor() %>% 
           ff_label("Year"))

# Remove data_resource ----
rm(data_resource)

# Extract variable labels ----
var_labs = extract_variable_label(data_cohort)

# Label lookup table ----
lookup_label = tibble(
  var = names(var_labs),
  var_label = var_labs
)

# Model formula ----
model_formula = as.formula(
  paste0("health_contact ~ ",
         paste0("year*(", paste0(var_explanatory, collapse = " + "),
                ") + offset(log(days))")))

# Fit model ----
if(model_type == "poisson"){
  model_fit = glm(formula = model_formula, family = "poisson", data = data_cohort)
} else if (model_type == "negative_binomial"){
  model_fit = MASS::glm.nb(formula = model_formula, data = data_cohort,
                           model = FALSE, y = FALSE)
}

## Save model
write_rds(
  model_fit,
  here::here("output", "comorbidity_multivar", model_type,
                            resource_type, "model", "model_fit.rds"))

# Model coefficient ----
tbl_model_coef = model_fit %>%
  tidy() %>% 
  tidy_add_term_labels(model = model_fit) %>% 
  left_join(
    model_fit %>% 
      confint.default() %>% 
      as_tibble(rownames = "term") %>% 
      rename(ci_lower = "2.5 %", ci_upper = "97.5 %"),
    by = "term"
  )

## Tidy up table ----
tbl_model_coef = tbl_model_coef %>% 
  mutate(
    year = str_extract(term, "(?<=year)\\d{4}"),
    var_lab = paste0(str_remove(var_label, "Year \\* "), ": ",
                     str_remove(label, "\\d{4} \\* ")) %>% 
      str_remove(": \\d{4}"),
    var_lab = var_lab %>% 
      factor(levels = unique(var_lab)) %>% 
      fct_rev(),
    var_type = case_when(
      var_type == "interaction" ~ "Interaction",
      str_detect(term, "_with_") ~ "Interaction",
      variable == "year" ~ "Year effect",
      TRUE ~ "Baseline effect"
    ) %>% 
      factor() %>% 
      fct_relevel("Year effect", "Baseline effect")
  ) %>% 
  replace_na(list(year = "2019"))

write_csv(tbl_model_coef,
          here::here("output", "comorbidity_multivar", model_type, resource_type,
                     "tbl_model_coef.csv"))

# Plot coefficients ----
plot_model_coef = tbl_model_coef %>% 
  filter(!term == "(Intercept)") %>% 
  ggplot(aes(x = estimate, xmin = ci_lower, xmax = ci_upper,
             y = var_lab, colour = var_type)) +
  geom_point() +
  geom_errorbar(width = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ year, nrow = 1) + 
  labs(
    y = NULL, x = "Coefficient (95% confidence interval)",
    colour = "Variable type"
  ) +
  theme(
    legend.position = "bottom"
  )

## Save coefficient plot ----
ggsave(filename = "plot_model_coef.jpeg",
       plot = plot_model_coef,
       path = here::here("output", "comorbidity_multivar", model_type,
                         resource_type),
       width = 12, height = 8, units = "in")


# Model metrics ----
tbl_model_metrics = model_fit %>%
  glance()

write_csv(tbl_model_metrics,
          here::here("output", "comorbidity_multivar", model_type, resource_type,
                     "tbl_model_metrics.csv"))

# IRR - linear combination ----
## No comorbidity (i.e. only year effect) ----
tbl_irr_year = tibble(
  var_1 = "year",
  var_2 = "no_comorbidity",
  level_1 = data_cohort[,"year"] %>% levels(),
  level_2 = "Yes"
) %>% 
  mutate(
    ref_1 = if_else(row_number() == 1, TRUE, FALSE),
    ref_2 = FALSE,
    lincom_term = paste0(var_1, level_1)
  ) %>% 
  filter(ref_1 != TRUE)

## Combined comorbidity and year effects ----
tbl_irr_comorb = c(var_explanatory) %>% 
  map(function(var){
  
  # Extract levels
  level_1 = data_cohort[,"year"] %>% levels()
  level_2 = data_cohort[,var] %>% levels()
  
  tbl_irr = expand_grid(level_1, level_2) %>% 
    mutate(var_1 = "year",
           var_2 = var) %>% 
    relocate(var_1, var_2) %>% 
    group_by(level_2) %>% 
    mutate(ref_1 = if_else(row_number() == 1, TRUE, FALSE)) %>%
    group_by(level_1) %>% 
    mutate(ref_2 = if_else(row_number() == 1, TRUE, FALSE)) %>% 
    ungroup() %>% 
    filter(!(ref_2 == TRUE)) %>% # Remove both reference
    rowwise() %>% 
    mutate(
      lincom_term = case_when(
        
        # Comorbidity interactions
        str_detect(var, "_with_") & ref_1 == TRUE ~ 
          paste0(
            c(paste0(str_extract(var, "^[[:lower:]_]+(?=_with_)"), level_2),
              paste0(str_extract(var, "(?<=_with_)[[:lower:]_]+$"), level_2),
              paste0(var_2, level_2)
            ),
            collapse = "+"),
        str_detect(var, "_with_") & ref_1 == FALSE ~ 
          paste0(
            c(paste0(str_extract(var, "^[[:lower:]_]+(?=_with_)"), level_2),
              paste0(str_extract(var, "(?<=_with_)[[:lower:]_]+$"), level_2),
              paste0(var_2, level_2),
              paste0(var_1, level_1),
              paste0(var_1, level_1, ":", str_extract(var, "^[[:lower:]_]+(?=_with_)"), level_2),
              paste0(var_1, level_1, ":", str_extract(var, "(?<=_with_)[[:lower:]_]+$"), level_2),
              paste0(var_1, level_1, ":", var_2, level_2)
            ),
            collapse = "+"),
        
        # Other non-interaction explanatory variables
        ref_1 == TRUE  ~ paste0(var_2, level_2),
        ref_1 == FALSE ~ paste0(c(paste0(var_1, level_1),
                                  paste0(var_2, level_2),
                                  paste0(var_1, level_1, ":", var_2, level_2)),
                                collapse = "+"),
        TRUE ~ NA_character_
      )
    )
}) %>% 
  bind_rows()

## Combine tables ----
tbl_irr =  tbl_irr_year %>% 
  bind_rows(tbl_irr_comorb)

## Calculate IRR based on linear combination of coefficients
tbl_irr = tbl_irr %>% 
  left_join(
    lincom(model_fit, tbl_irr$lincom_term, eform = TRUE) %>% 
      as_tibble(rownames = "lincom_term"),
    by = "lincom_term"
  ) %>% 
  janitor::clean_names() %>% 
  rename(ci_lower = x2_5_percent, ci_upper = x97_5_percent) %>% 
  unnest(c(estimate, ci_lower, ci_upper, chisq, pr_chisq))

## Append labels and add label for no comorbidity ----
tbl_irr = tbl_irr %>% 
  left_join(
    lookup_label %>% 
      rename(var_2 = var, var_2_label = var_label),
    by = "var_2"
  ) %>% 
  mutate(
    var_2_label = if_else(var_2 == "no_comorbidity", "No comorbidities", var_2_label)
  )

## Save IRR table to file ----
write_csv(tbl_irr,
          here::here("output", "comorbidity_multivar", model_type, resource_type,
                     "tbl_irr.csv"))


