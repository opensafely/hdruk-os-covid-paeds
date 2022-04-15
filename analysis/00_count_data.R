
## Import libraries ----
library("tidyverse")
library("lubridate")
library("finalfit")

# Set defaults ----
my_theme = theme_bw()

# Functions ----
plot_hist = function(data, x, fill = NULL, bins = 50){
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
         path = here::here("output", "descriptive", "counts"))
}

# Set column type based on column name ----
col_type_data_contact_counts = tibble(
  column_names = c(read_csv(
    here::here("output", "input_contact_counts.csv.gz"),
    n_max = 1,
    col_names = FALSE
  ))) %>% 
  mutate(column_type = case_when(
    column_names == "patient_id" ~ "i",
    str_detect(column_names, "_count") ~ "i",
    TRUE ~ "c"
  ))

# Read high count admissions from csv ----
data_contact_counts = read_csv(
  here::here("output", "input_contact_counts.csv.gz"),
  col_types = col_type_data_contact_counts %>%
    pull(column_type) %>%
    paste(collapse = "")
)

# Create output directories ----
dir.create(here::here("output", "descriptive", "counts"),
           showWarnings = FALSE, recursive=TRUE)

# Cut counts and create factors
data_contact_counts = data_contact_counts %>% 
  mutate(across(contains(c("admission_", "outpatient_", "gp_", "covid_")),
                .fns = list(
                  factor = ~cut(., c(-Inf,0,5,10,25,50,100,250,Inf)) %>% 
                    fct_recode("0"       = "(-Inf,0]",
                               "1-5"     = "(0,5]",
                               "6-10"    = "(5,10]",
                               "11-25"   = "(10,25]",
                               "26-50"   = "(25,50]",
                               "51-100"  = "(50,100]",
                               "101-250" = "(100,250]",
                               "250+"    = "(250, Inf]")
                )))


# Create summary table ----
dependent = NULL
explanatory = c("admission_count_2018_factor",
                "admission_count_2019_factor",
                "admission_count_2020_factor",
                "admission_count_2021_factor",
                "admission_count_2022_factor",
                "outpatient_count_2019_factor",
                "outpatient_count_2020_factor",
                "outpatient_count_2021_factor",
                "outpatient_count_2022_factor",
                "gp_contact_count_2019_factor",
                "gp_contact_count_2020_factor",
                "gp_contact_count_2021_factor",
                "gp_contact_count_2022_factor",
                "covid_negative_test_count_factor",
                "covid_positive_test_count_factor")

count_summary_tbl = data_contact_counts %>%
  summary_factorlist(dependent, explanatory,
                     add_col_totals = TRUE,
                     add_row_total = TRUE)

# Save summary table ----
write_csv(count_summary_tbl, 
          here::here("output", "descriptive", "counts", "count_summary_tbl.csv"))


# Export maximum admissions and GP counts ----
max_counts = data_contact_counts %>% 
  summarise(n = n(),
            max_admission_count_2018 = max(admission_count_2018),
            max_admission_count_2019 = max(admission_count_2019),
            max_admission_count_2020 = max(admission_count_2020),
            max_admission_count_2021 = max(admission_count_2021),
            max_admission_count_2022 = max(admission_count_2022),
            max_outpatient_count_2019 = max(outpatient_count_2019),
            max_outpatient_count_2020 = max(outpatient_count_2020),
            max_outpatient_count_2021 = max(outpatient_count_2021),
            max_outpatient_count_2022 = max(outpatient_count_2022),
            max_gp_contact_count_2019 = max(gp_contact_count_2019),
            max_gp_contact_count_2020 = max(gp_contact_count_2020),
            max_gp_contact_count_2021 = max(gp_contact_count_2021),
            max_gp_contact_count_2022 = max(gp_contact_count_2022),
            max_covid_negative_test_count = max(covid_negative_test_count),
            max_covid_positive_test_count = max(covid_positive_test_count))

# Save max counts ----
write_csv(max_counts, here::here("output", "descriptive", "counts", "max_counts.csv"))



# Crete histogram plots ----
list("admission_count_2018",
     "admission_count_2019",
     "admission_count_2020",
     "admission_count_2021",
     "admission_count_2022",
     "outpatient_count_2019",
     "outpatient_count_2020",
     "outpatient_count_2021",
     "outpatient_count_2022",
     "gp_contact_count_2019",
     "gp_contact_count_2020",
     "gp_contact_count_2021",
     "gp_contact_count_2022",
     "covid_positive_test_count",
     "covid_negative_test_count") %>% 
  lapply(function(x){
    plot_hist(data_contact_counts, x)
  })
