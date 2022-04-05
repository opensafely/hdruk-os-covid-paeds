
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

# Create factors 
data_contact_counts = data_contact_counts %>% 
  mutate(
    admission_count_factor = case_when(
      admission_count == 0 ~ "0",
      admission_count <= 5 ~ "1-5",
      admission_count <= 10 ~ "6-10",
      admission_count <= 50 ~ "11-50",
      admission_count <= 100 ~ "51-100",
      admission_count <= 250 ~ "101-250",
      admission_count <= 500 ~ "251-500",
      admission_count <= 750 ~ "501-750",
      admission_count <= 1000 ~ "751-1000",
      admission_count > 1000 ~ "1001+"
    ) %>% 
      factor(levels = c("0", "1-5", "6-10", "11-50", "51-100",
                        "101-250", "251-500", "501-750", "751-1000", "1001+")),
    
    outpatient_count_factor = case_when(
      outpatient_count == 0 ~ "0",
      outpatient_count <= 5 ~ "1-5",
      outpatient_count <= 10 ~ "6-10",
      outpatient_count <= 50 ~ "11-50",
      outpatient_count <= 100 ~ "51-100",
      outpatient_count <= 250 ~ "101-250",
      outpatient_count <= 500 ~ "251-500",
      outpatient_count <= 750 ~ "501-750",
      outpatient_count <= 1000 ~ "751-1000",
      outpatient_count > 1000 ~ "1001+"
    ) %>% 
      factor(levels = c("0", "1-5", "6-10", "11-50", "51-100",
                        "101-250", "251-500", "501-750", "751-1000", "1001+")),
    
    gp_contact_count_factor = case_when(
      gp_contact_count == 0 ~ "0",
      gp_contact_count <= 5 ~ "1-5",
      gp_contact_count <= 10 ~ "6-10",
      gp_contact_count <= 50 ~ "11-50",
      gp_contact_count <= 100 ~ "51-100",
      gp_contact_count <= 250 ~ "101-250",
      gp_contact_count <= 500 ~ "251-500",
      gp_contact_count <= 750 ~ "501-750",
      gp_contact_count <= 1000 ~ "751-1000",
      gp_contact_count > 1000 ~ "1001+"
    ) %>% 
      factor(levels = c("0", "1-5", "6-10", "11-50", "51-100",
                        "101-250", "251-500", "501-750", "751-1000", "1001+")),
    
    covid_negative_test_count_factor = case_when(
      covid_negative_test_count == 0 ~ "0",
      covid_negative_test_count <= 5 ~ "1-5",
      covid_negative_test_count <= 10 ~ "6-10",
      covid_negative_test_count <= 25 ~ "11-25",
      covid_negative_test_count <= 50 ~ "26-50",
      covid_negative_test_count <= 100 ~ "51-100",
      covid_negative_test_count <= 250 ~ "101-250",
      covid_negative_test_count <= 500 ~ "251-500",
      covid_negative_test_count <= 750 ~ "501-750",
      covid_negative_test_count <= 1000 ~ "751-1000",
      covid_negative_test_count > 1000 ~ "1001+"
    ) %>% 
      factor(levels = c("0", "1-5", "6-10", "11-25", "26-50", "51-100",
                        "101-250", "251-500", "501-750", "751-1000", "1001+")),
    
    covid_positive_test_count_factor = case_when(
      covid_positive_test_count == 0 ~ "0",
      covid_positive_test_count <= 5 ~ "1-5",
      covid_positive_test_count <= 10 ~ "6-10",
      covid_positive_test_count <= 25 ~ "11-25",
      covid_positive_test_count <= 50 ~ "26-50",
      covid_positive_test_count <= 100 ~ "51-100",
      covid_positive_test_count <= 250 ~ "101-250",
      covid_positive_test_count <= 500 ~ "251-500",
      covid_positive_test_count <= 750 ~ "501-750",
      covid_positive_test_count <= 1000 ~ "751-1000",
      covid_positive_test_count > 1000 ~ "1001+"
    ) %>% 
      factor(levels = c("0", "1-5", "6-10", "11-25", "26-50", "51-100",
                        "101-250", "251-500", "501-750", "751-1000", "1001+")),
    
    
  )

# Create summary table ----
dependent = NULL
explanatory = c("admission_count_factor",
                "outpatient_count_factor",
                "gp_contact_count_factor",
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
            max_admissions_count = max(admission_count),
            max_outpatient_count = max(outpatient_count),
            max_gp_contact_count = max(gp_contact_count),
            max_covid_negative_test_count = max(covid_negative_test_count),
            max_covid_positive_test_count = max(covid_positive_test_count))

# Save max counts ----
write_csv(max_counts, here::here("output", "descriptive", "counts", "max_counts.csv"))



# Crete histogram plots ----
list("admission_count",
     "outpatient_count",
     "gp_contact_count",
     "covid_positive_test_count",
     "covid_negative_test_count") %>% 
  lapply(function(x){
    plot_hist(data_contact_counts, x)
  })
