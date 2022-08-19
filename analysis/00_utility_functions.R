# Functions ----

# Applies general exclusion criteria to cohort
apply_exclusion_criteria = function(.data_patient){
  .data_patient  %>% 
    filter(
      (age >= 4) & (age < 18),
      covid_nosocomial == "No",
      covid_discrepant_test == "No",
      death == "No"
    )
}

# Bootstrap confidence intervals for incidence rate 
boot.incidence.rate = function(counts, persontime, name = "inc_rate",
                               R = 20, conf = 0.95, type = "perc", ...){
  bootfunc = function(c, i, pt){
    n = sum(c[i])
    d = sum(pt[i])
    return(n/d)
  }
  
  boot_ci = boot(counts, bootfunc, R = R, pt = persontime, ...) %>% 
    boot.ci(conf = conf, type = type)
  boot_result = c(boot_ci$t0, boot_ci$percent[4:5])
  names(boot_result) = paste0(name, c("_est", "_LL", "_UL"))
  return(boot_result)
}

# Calculates age and age group at index date
calc_age = function(.data_patient, index_date){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    mutate(
      age = time_length(
        interval(date_of_birth, index_date),
        unit = "years"
      ) %>% 
        ff_label("Age (years)"),
      age_group = age %>%
        cut(
          breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
          labels = c("Under 4", "4-6", "7-10", "11-14", "15-17", "18+")
        ) %>% 
        ff_label("Age group (years)")
    )
}

# Calculates comorbidity status given index date
calc_comorbidity_status = function(.data_patient, index_date){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    mutate(
      asthma = case_when(
        (asthma_first_date < index_date) &
          ((asthma_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Asthma"),
      
      cancer = case_when(
        (cancer_first_date < index_date) &
          ((cancer_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cancer"),
      
      cystic_fibrosis = case_when(
        (cystic_fibrosis_first_date < index_date) &
          ((cystic_fibrosis_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cystic fibrosis"),
      
      diabetes = case_when(
        (diabetes_first_date < index_date) &
          ((diabetes_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Diabetes"),
      
      epilepsy = case_when(
        (epilepsy_first_date < index_date) &
          ((epilepsy_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Epilepsy"),
      
      severe_mental_illness = case_when(
        (severe_mental_illness_first_date < index_date) &
          ((severe_mental_illness_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Severe mental illness"),
      
      cerebral_palsy = case_when(
        cerebral_palsy_first_date < index_date ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Cerebral palsy"),
      
      chronic_infections = case_when(
        (chronic_infections_first_date < index_date) &
          ((chronic_infections_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Chronic infections"),
      
      devices_and_stomas = case_when(
        (devices_and_stomas_first_date < index_date) &
          ((devices_and_stomas_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Devices and stomas"),
      
      endocrine_disorders = case_when(
        (endocrine_disorders_first_date < index_date) &
          ((endocrine_disorders_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Endocrine disorders"),
      
      gastrointestinal_disorders = case_when(
        (gastrointestinal_disorders_first_date < index_date) &
          ((gastrointestinal_disorders_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Gastrointestinal disorders"),
      
      haematological_disorders = case_when(
        (haematological_disorders_first_date < index_date) &
          ((haematological_disorders_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Haematological disorders"),
      
      immunological_disorders = case_when(
        (immunological_disorders_first_date < index_date) &
          ((immunological_disorders_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Immunological disorders"),
      
      learning_and_behaviour_difficulties = case_when(
        (learning_and_behaviour_difficulties_first_date < index_date) &
          ((learning_and_behaviour_difficulties_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Learning and behavioural difficulties"),
      
      mental_illness = case_when(
        (mental_illness_first_date < index_date) &
          ((mental_illness_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Mental illness"),
      
      musculoskeletal_and_rheum = case_when(
        (musculoskeletal_and_rheum_first_date < index_date) &
          ((musculoskeletal_and_rheum_last_date + years(5)) >= index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Musculoskeletal and rheumatic diseases"),
      
      transplant = case_when(
        transplant_first_date < index_date ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Transplant"),
    ) %>% 
    mutate(
      comorbidity_count = rowSums(
        select(.,
               asthma, cancer, cerebral_palsy, chronic_infections,
               cystic_fibrosis,
               devices_and_stomas, diabetes, endocrine_disorders,
               epilepsy, gastrointestinal_disorders,
               haematological_disorders, immunological_disorders,
               learning_and_behaviour_difficulties, mental_illness,
               musculoskeletal_and_rheum, severe_mental_illness,
               transplant) == "Yes") %>% 
        ff_label("Number of comorbidities"),
      
      comorbidity_count.factor = case_when(
        comorbidity_count == 0 ~ "0",
        comorbidity_count == 1 ~ "1",
        comorbidity_count <  6 ~ "2-5",
        comorbidity_count >= 6 ~ "6+",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("0", "1", "2-5", "6+")) %>% 
        ff_label("Comorbidity count")
    )
}

# Calculate death status at index date
calc_death_status = function(.data_patient, index_date){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    mutate(
      death = case_when(
        death_date < index_date ~ "Yes",
        is.na(death_date) ~ "No",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Death")
    )
}

# count exclusion criteria to cohort
count_exclusion_criteria = function(.data_patient){
  data_exclusion = .data_patient  %>% 
    transmute(
      is_aged_between_4_17 = (age >= 4) & (age < 18),
      is_not_covid_nosocomial = covid_nosocomial == "No",
      is_not_covid_discrepant_test = covid_discrepant_test == "No",
      is_alive = death == "No",
      include = is_aged_between_4_17 & is_not_covid_nosocomial &
        is_not_covid_discrepant_test & is_alive
    )
  if(any(is.na(data_exclusion$include))) warning("Inclusion column contains NAs")
  return(data_exclusion)
}

# Wrapper function for calculating time-varying variables
calc_indexed_variables = function(.data_patient, index_date){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    calc_age(index_date) %>%
    calc_comorbidity_status(index_date) %>%
    calc_vaccination_status(index_date) %>% 
    calc_death_status(index_date)
}

# Calculates vaccination status at index date
calc_vaccination_status = function(.data_patient, index_date){
  index_date = lubridate::ymd(index_date)
  .data_patient %>% 
    mutate(
      vaccination_status = case_when(
        (vax_covid_date_1 + days(21) < index_date) ~ "Yes",
        TRUE ~ "No"
      ) %>%
        factor() %>%
        ff_label("Vaccination status"),
    )
}

fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# ff_round_counts: Round counts from finalfit::summary_factorlist output
ff_round_counts = function (.data, accuracy = 15, ignore = c("label", "levels", "p")){ 
  if (!any(names(.data) == "label")) 
    stop("summary_factorlist() must include: add_dependent_label = FALSE")
  df.out = .data %>%
    dplyr::mutate(label = dplyr::if_else(label == "", NA_character_, label)) %>% 
    tidyr::fill(label) %>%
    dplyr::group_by(label) %>% 
    dplyr::mutate(across(-dplyr::any_of(ignore), 
                         function(.){
                           value_count = as.numeric(stringr::str_extract(., "[:digit:]+")) %>% 
                             plyr::round_any(accuracy)
                           value_perc = value_count/sum(value_count)*100
                           
                           dplyr::case_when(!levels %in% c("Mean (SD)", "Median (IQR)") ~ 
                                              format_n_percent(value_count, value_perc, 1), 
                                            TRUE ~ .)
                           
                         })) %>%
    dplyr::mutate(label = dplyr::if_else(dplyr::row_number()==1, label, "")) %>% 
    dplyr::ungroup()
  class(df.out) = c("data.frame.ff", class(df.out))
  return(df.out)
}

# icd10_code_to_chapter: 
icd10_code_to_chapter = function(icd10_code){
  case_when(
    str_sub(icd10_code, 1,1) == "A" ~ "01: Infectious and parasitic diseases",
    str_sub(icd10_code, 1,1) == "B" ~ "01: Infectious and parasitic diseases",
    str_sub(icd10_code, 1,1) == "C" ~ "02: Neoplasms",
    str_sub(icd10_code, 1,3) %in% sprintf("%s%02d", "D",  0:49) ~ "02: Neoplasms",
    str_sub(icd10_code, 1,3) %in% sprintf("%s%02d", "D",  50:89) ~ "03: Diseases of the blood",
    str_sub(icd10_code, 1,1) == "E" ~ "04: Endocrine, nutritional and metabolic",
    str_sub(icd10_code, 1,1) == "F" ~ "05: Mental and behavioural disorders",
    str_sub(icd10_code, 1,1) == "G" ~ "06: Nervous system",
    str_sub(icd10_code, 1,3) %in% sprintf("%s%02d", "H",  0:59) ~ "07: Eye and adnexa",
    str_sub(icd10_code, 1,3) %in% sprintf("%s%02d", "H",  60:95) ~ "08: Ear and mastoid process",
    str_sub(icd10_code, 1,1) == "I" ~ "09: Circulatory system",
    str_sub(icd10_code, 1,1) == "J" ~ "10: Respiratory system",
    str_sub(icd10_code, 1,1) == "K" ~ "11: Digestive system",
    str_sub(icd10_code, 1,1) == "L" ~ "12: Skin and subcutaneous tissue",
    str_sub(icd10_code, 1,1) == "M" ~ "13: Musculoskeletal and connective tissue",
    str_sub(icd10_code, 1,1) == "N" ~ "14: Genitourinary system",
    str_sub(icd10_code, 1,1) == "O" ~ "15: Pregnancy, childbirth and the puerperium",
    str_sub(icd10_code, 1,1) == "P" ~ "16: Conditions in the perinatal period",
    str_sub(icd10_code, 1,1) == "Q" ~ "17: Congenital malformations",
    str_sub(icd10_code, 1,1) == "R" ~ "18: Abnormal clinical and laboratory findings",
    str_sub(icd10_code, 1,1) == "S" ~ "19: Injury and poisoning",
    str_sub(icd10_code, 1,1) == "T" ~ "19: Injury and poisoning",
    str_sub(icd10_code, 1,1) == "V" ~ "20: External causes",
    str_sub(icd10_code, 1,1) == "W" ~ "20: External causes",
    str_sub(icd10_code, 1,1) == "X" ~ "20: External causes",
    str_sub(icd10_code, 1,1) == "Y" ~ "20: External causes",
    str_sub(icd10_code, 1,1) == "Z" ~ "21: Factors influencing health status",
    str_sub(icd10_code, 1,1) == "U" ~ "22: Codes for special purposes",
    TRUE ~ NA_character_
  )
}

# read_column_type: read column name to determine column data type
read_column_type = function(file){
  
  first_row = read_csv(
    file,
    n_max = 1,
    col_names = FALSE,
    col_types = cols(.default = "c")
  )
  
  type_string = tibble(
    column_names = c(first_row)) %>% 
    mutate(column_type = case_when(
      column_names == "patient_id" ~ "i",
      str_detect(column_names, "_date") ~ "D",
      str_detect(column_names, "imd_") ~ "i",
      str_detect(column_names, "age") ~ "d",
      str_detect(column_names, "_count") ~ "i",
      TRUE ~ "c"
    )) %>%
    pull(column_type) %>%
    paste(collapse = "")
  
  return(type_string)
}

# Returns vector of mean and confidence intervals from t.test()
mean.cl.ttest = function(x){
  t_test = t.test(x)
  x_ci = c(t_test$estimate, t_test$conf.int[1], t_test$conf.int[2])
  names(x_ci) = c("Mean", "Lower", "Upper")
  x_ci
}

# plot_hist: plot histogram given data and variable name
plot_hist = function(data, x, path = here::here("output"),
                     fill = NULL, bins = 50, my_theme = theme_bw()){
  if(is.null(fill)){
    ggplot(data = data, aes_string(x = x)) + 
      geom_histogram(bins = bins) +
      my_theme
  } else {
    ggplot(data = data, aes_string(x = x, fill = fill)) + 
      geom_histogram(bins = bins) +
      my_theme
  }
  ggsave(paste0("plot_hist_", x, ".jpeg"),
         plot = last_plot(),
         device = "jpeg",
         path = path)
}

# count_dates_by_period: count of date variables in data by period 
count_dates_by_period = function(data, var, start_date = NULL, end_date = NULL, 
                        period = "week"){
  
  if(is.null(start_date)) {
    start_date = data %>% pull(all_of(var)) %>% min(na.rm = FALSE)
  } else if (is.character(start_date)){
    start_date = ymd(start_date)
  }
  
  if(is.null(end_date)) {
    end_date = data %>% pull(all_of(var)) %>% max(na.rm = FALSE)
  } else if (is.character(end_date)){
    end_date = ymd(end_date)
  }
  
  tibble(
    date = seq(start_date, end_date, by=period) %>%
      floor_date(period) %>% 
      rep(length(var)),
    name = var) %>% 
    left_join(
      data %>%
        select(all_of(var)) %>% 
        rownames_to_column() %>% 
        pivot_longer(-rowname) %>% 
        mutate(date = floor_date(value, period)) %>% 
        group_by(name) %>% 
        count(date),
      by = c("date", "name")
    ) %>% 
    replace_na(list(n = 0))
} 


date_seq = function(dates, by = "month", week_start = 1){
  seq(min(floor_date(dates, unit = by, week_start = week_start), na.rm = TRUE), 
      max(floor_date(dates, unit = by, week_start = week_start), na.rm = TRUE),
      by = by)
}
