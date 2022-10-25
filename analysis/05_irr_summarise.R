
# Load packages ----
library(tidyverse)
library(lubridate)

# Set plot theme ----
theme_set(theme_bw())

# Create output directory ----
dir.create(here::here("output", "comorbidity_yearly", "irr_summarised"),
           showWarnings = FALSE, recursive=TRUE)

# Import IRRs for each resource and comorbidity ----
tbl_yearly_irr = c("gp", "outpatient", "admissions", "beddays") %>% 
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
tbl_yearly_irr = tbl_yearly_irr %>% 
  filter(level_2 %in% c("Yes", "0")) %>% 
  select(resource_type, year = level_1, comorb = var_2,
         comorb_label = var_label, estimate, ci_lower, ci_upper) %>% 
  mutate(
    comorb = case_when(
      comorb == "comorbidity_count.factor" ~ "no_comorbidity",
      TRUE ~ comorb
    ),
    comorb_label = case_when(
      comorb_label == "Comorbidity count" ~ "No comorbidity",
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
      factor() %>% 
      fct_rev()
  ) %>% 
  mutate(
    comorb = comorb %>% 
      factor() %>% 
      fct_relevel(
        "no_comorbidity",
        "mental_health_disorders", "neurodevelopmental_and_behavioural",
        "asthma", "cystic_fibrosis", "other_respiratory",
        "cardiovascular", "epilepsy", "headaches", "other_neurological",
        "gastrointestinal_conditions", "genitourinary", "cancer",
        "non_malignant_haematological", "immunological", "chronic_infections",
        "rheumatology", "congenital_malformation", "diabetes", "other_endocrine",
        "metabolic", "transplant", "palliative_care",
        
        # Comorbidity interactions
        "asthma_with_cardiovascular", "cystic_fibrosis_with_cardiovascular",
        "other_respiratory_with_cardiovascular",
        "epilepsy_with_cardiovascular", "headaches_with_cardiovascular",
        "other_neurological_with_cardiovascular",
        "asthma_with_epilepsy", "cystic_fibrosis_with_epilepsy",
        "other_respiratory_with_epilepsy",
        "asthma_with_headaches", "cystic_fibrosis_with_headaches",
        "other_respiratory_with_headaches",
        "asthma_with_other_neurological", "cystic_fibrosis_with_other_neurological",
        "other_respiratory_with_other_neurological")
  ) %>% 
  arrange(resource_type, year, comorb) %>% 
  mutate(
    comorb_label = comorb_label %>% 
      factor() %>% 
      fct_relevel(unique(comorb_label))
  )

## Save IRR table
write_csv(tbl_yearly_irr, 
          here::here("output", "comorbidity_yearly", "irr_summarised",
                     "tbl_yearly_irr.csv"))

# Create IRR plot ----
plot_yearly_irr = tbl_yearly_irr %>%
  ggplot(aes(y = year, x = estimate, xmin = ci_lower, xmax = ci_upper,
             colour = year)) +
  geom_point() +
  geom_errorbar(width = 0.5) +
  geom_vline(xintercept = 0, size = 0.5) +
  facet_grid(comorb_label ~ resource_type, switch = "y") + 
  scale_y_discrete(position = "right") +
  theme(strip.text.y.left = element_text(angle = 0),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(
    y = NULL,
    x = "Incidence rate ratio",
    colour = "Year"
  ) + 
  guides(colour = guide_legend(reverse = TRUE))

# Save IRR plot ----
ggsave(filename = "plot_yearly_irr.jpeg",
       plot = plot_yearly_irr,
       path = here::here("output", "comorbidity_yearly", "irr_summarised"),
       width = 12, height = 12, units = "in")

