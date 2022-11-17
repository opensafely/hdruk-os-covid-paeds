
# Load packages ----
library(tidyverse)

# List all csv files ---
list_files = list.files(
  here::here("output", "descriptives", "healthcare_use_2019_2022", "monthly_tables"),
  pattern = "*.csv")

# Load and aggregate csv files ----
monthly_count_summarised = list_files %>% 
  map_df(function(file){
    monthly_count = read_csv(
      here::here("output", "descriptives", "healthcare_use_2019_2022",
                 "monthly_tables", file),
      col_types = rep("c", 13)
    )
  }, .id = "set_id")

# Save resource summary table ----
write_csv(monthly_count_summarised, 
          here::here("output", "descriptives", "healthcare_use_2019_2022",
                     "tbl_monthly_count_summarised.csv"))