library(tidyverse)
library(lubridate)
library(finalfit)

tbl_irr = c("gp", "outpatient", "admissions", "beddays") %>% 
  map(function(resource_type){
    irr_files = list.files(here::here("output", "comorbidity_yearly", resource_type,
                                      "incidence_rate_ratio"))
    irr_files %>% map(function(irr_file){
      read_csv(here::here("output", "comorbidity_yearly", resource_type,
                          "incidence_rate_ratio", irr_file)) %>% 
        mutate(
          file = irr_file,
          resource_type = resource_type
        )
    }) %>% 
      bind_rows()
  }) %>% 
  bind_rows()

# Clean table ----
x = tbl_irr %>% 
  filter(level_2 %in% c("Yes", "0")) %>% 
  select(resource_type, year = level_1, comorb = var_2,
         comorb_label = var_label, estimate, ci_lower, ci_upper) %>% 
  mutate(
    comorb = case_when(
      comorb == "comorbidity_count.factor" ~ "no_comorbidity",
      TRUE ~ comorb
    ),
    comorb_label = case_when(
      comorb == "comorbidity_count.factor" ~ "No comorbidity",
      TRUE ~ comorb_label
    ),
    resource_type = case_when(
      resource_type == "gp" ~ "Healthcare episode",
      resource_type == "outpatient" ~ "Outpatient appointment",
      resource_type == "admissions" ~ "Inpatient admissions",
      resource_type == "beddays" ~ "Inpatient bed-days"
    ) %>% 
      factor() %>% 
      fct_relevel("Healthcare episode", "Outpatient appointment",
                  "Inpatient admissions", "Inpatient bed-days"),
    year = year %>% 
      factor()
  )




x %>%
  filter(!str_detect(comorb, "_with")) %>% 
  ggplot(aes(y = year, x = estimate,
             xmin = ci_lower, xmax = ci_upper)) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(comorb_label ~ resource_type, switch = "y") + 
  scale_y_discrete(position = "right") +
  theme(strip.text.y.left = element_text(angle = 0)) +
  labs(
    y = NULL,
    x = "Incidence rate ratio"
  )


