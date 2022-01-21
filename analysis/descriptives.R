
## Import libraries ----
library("tidyverse")
library("lubridate")


## create output directories ----
dir.create(here::here("output", "descriptive", "plots"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "descriptive", "tables"), showWarnings = FALSE, recursive=TRUE)


# Read processed data
data_extract = read_rds(here::here("output", "data", "data_processed.rds"))

## Summarise data
tbl_counts = data_extract %>% 
  summarise(n = n(),
            max_admissions = max(hospital_admissions_total),
            max_gp_interactions = max(gp_consultations_total))

write_csv(tbl_counts, here::here("output", "descriptive", "tables", "tbl_counts.csv"))



## Plot histogram
# Hospital admissions
plot_hist_hospital_admission_count = data_extract %>% 
  ggplot(aes(hospital_admissions_total)) +
  geom_histogram()

ggsave("plot_hist_hospital_admission_count.jpeg",
       plot_hist_hospital_admission_count,
       "jpeg",
       here::here("output", "descriptive", "plots"))

# GP-patient interaction
plot_hist_gp_count = data_extract %>% 
  ggplot(aes(gp_consultations_total)) +
  geom_histogram()

ggsave("plot_hist_gp_count.jpeg",
       plot_hist_gp_count,
       "jpeg",
       here::here("output", "descriptive", "plots"))
