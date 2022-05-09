
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


# Export maximum admissions, outpatient and GP counts ----
max_counts = data_contact_counts %>%
  summarise(across(matches(c("admission_count_\\d{4}_Q\\d",
                             "outpatient_count_\\d{4}_Q\\d",
                             "gp_contact_count_\\d{4}_Q\\d",
                             "covid_negative_test_count_\\d{4}",
                             "covid_positive_test_count_\\d{4}")),
                   list(max = max))) %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname) %>% 
  select(-rowname)

# Save max counts ----
write_csv(max_counts, here::here("output", "descriptive", "counts", "max_counts.csv"))

# Cut counts and create factors
data_contact_counts = data_contact_counts %>% 
  mutate(across(contains(c("admission_", "outpatient_", "gp_", "covid_")),
                .fns = list(
                  factor = ~cut(., c(-Inf,0,5,10,25,50,100,Inf)) %>% 
                    fct_recode("0"       = "(-Inf,0]",
                               "1-5"     = "(0,5]",
                               "6-10"    = "(5,10]",
                               "11-25"   = "(10,25]",
                               "26-50"   = "(25,50]",
                               "51-100"  = "(50,100]",
                               "101+"    = "(100, Inf]")
                )))


# Create summary table ----
dependent = NULL
explanatory = max_counts$name %>%
  str_replace("_max", "_factor")

count_summary_tbl = data_contact_counts %>%
  summary_factorlist(dependent, explanatory,
                     add_col_totals = TRUE,
                     add_row_total = TRUE)

# Save summary table ----
write_csv(count_summary_tbl, 
          here::here("output", "descriptive", "counts", "count_summary_tbl.csv"))

# Crete histogram plots ----
max_counts$name %>%
  str_replace("_max", "") %>%
  as.list() %>% 
  lapply(function(x){
    plot_hist(data_contact_counts, x)
  })
