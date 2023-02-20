# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# Yearly resource-use by age

# Load packages ----
library(tidyverse)
library(lubridate)

# Directories ----
dir_resource_age_group = here::here("output", "diagnostics", "resource_by_age_group")

## Create new output directories ----
dir.create(dir_resource_age_group,     showWarnings = FALSE, recursive=TRUE)


# Load data ----
yearly_resource = read_csv(here::here("output", "input_resource_by_age.csv.gz"))

yearly_resource = yearly_resource %>% 
  pivot_longer(
    cols = -c(date_of_birth, age, patient_id),
    names_pattern = "(.*)_(\\d{4})",
    names_to = c("resource", "year"),
    values_to = "value"
  ) %>% 
  mutate(
    date_of_birth = paste0(date_of_birth, "-15") %>% ymd(),
    index_date = paste0(year, "-01-01") %>% ymd(),
    age_indexed = time_length(
      interval(date_of_birth, index_date),
      unit = "years"
    ),
    age_group_indexed = age_indexed %>% 
      cut(
        breaks = c(-Inf, 4, 7, 11, 15, 18, Inf),
        labels = c("Under 4", "4-6", "7-10", "11-14", "15-17", "18+"),
        right = FALSE
      )
  )

# Summary table
table_yearly_resource = yearly_resource %>% 
  group_by(index_date, resource, age_group_indexed) %>% 
  summarise(
    n_patient = n_distinct(patient_id),
    n_event = sum(value),
    incidence_1000 = n_event/n_patient *1000
  )

write_csv(table_yearly_resource,
          here::here("output", "diagnostics", "resource_by_age_group",
                     "table_yearly_resource.csv")
)

# Plot incidence
plot_yearly_resource = table_yearly_resource %>% 
  ggplot(aes(index_date, incidence_1000, colour = age_group_indexed)) +
  geom_line() + geom_point() +
  facet_wrap(~ resource, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA), name = "Incidence (count per 1000 CYP)")

ggsave(filename = here::here("output", "diagnostics", "resource_by_age_group",
                             "plot_yearly_resource.jpeg"),
       plot = plot_yearly_resource,
       height = 6, width = 6, units = "in")