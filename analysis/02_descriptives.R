
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
         path = here::here("output", "descriptive", "plots"))
}

# Create output directories ----
dir.create(here::here("output", "descriptive", "plots"),
           showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptive", "tables"),
           showWarnings = FALSE, recursive=TRUE)

# Read processed data  ----
data_patient    = read_rds(here::here("output", "data", "data_patient.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))
data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds"))
data_gp         = read_rds(here::here("output", "data", "data_gp.rds"))

# Export summary table ----
dependent = "covid_status"
explanatory = c("age", "age_factor", "sex", 
                "ethnicity", "ethnicity_6_sus", "ethnicity_comb",
                "region_2019", "imd_Q5_2019", "rural_urban_2019",
                #"asthma", "diabetes",
                "death_factor",
                "admission_count_factor", 
                "gp_contact_count_factor")

tbl_summary = data_patient %>%
  summary_factorlist(dependent, explanatory, p = TRUE, 
                     add_col_totals = TRUE,
                     add_row_total = TRUE)

write_csv(tbl_summary, here::here("output", "descriptive", "tables", "tbl_summary.csv"))

# Export maximum admissions and GP counts ----
max_counts = data_patient %>% 
  summarise(n = n(),
            max_admissions_count = max(admission_count),
            max_outpatient_count = max(outpatient_count),
            max_gp_contact_count = max(gp_contact_count),
            max_covid_negative_test_count = max(covid_negative_test_count),
            max_covid_positive_test_count = max(covid_positive_test_count))

write_csv(max_counts, here::here("output", "descriptive", "tables", "max_counts.csv"))

# Plot histograms  -----
list("age",
     "death_date",
     "covid_positive_test_date_1",
     "covid_negative_test_date_1",
     "admission_count",
     "outpatient_count",
     "gp_contact_count",
     "covid_positive_test_count",
     "covid_negative_test_count") %>% 
  lapply(function(x){
    plot_hist(data_patient, x)
  })

list("admission_date",
     "discharge_date"
     ) %>% 
  lapply(function(x){
    plot_hist(data_admissions, x)
  })

list("outpatient_date") %>% 
  lapply(function(x){
    plot_hist(data_outpatient, x)
  })

list("gp_contact_date") %>% 
  lapply(function(x){
    plot_hist(data_gp, x)
  })

