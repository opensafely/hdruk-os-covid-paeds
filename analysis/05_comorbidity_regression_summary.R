
# Load packages ----
library(tidyverse)
library(lubridate)
library(broom)
library(broom.helpers)

# Set plot theme ----
theme_set(theme_bw())

# Command arguments ----
args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0){
  model_type     = "negative_binomial"
} else{
  model_type     = args[[1]]
}

# Check command argument validity ----
if(!model_type %in% c("poisson", "negative_binomial")){
  stop("Invalid command arguments")
}


# Create output directory ----
dir.create(here::here("output", "comorbidity_multivar", model_type, "summary"),
           showWarnings = FALSE, recursive=TRUE)

# Import IRRs for each resource and comorbidity ----
tbl_irr_combined = c("gp", "outpatient", "admissions", "beddays") %>% 
  map(function(resource_type){
    tbl_irr = read_csv(here::here("output", "comorbidity_multivar", model_type,
                                    resource_type, "tbl_irr.csv")) %>% 
      mutate(resource_type = resource_type)
  }) %>% 
  bind_rows()

# Clean table ----
tbl_irr_combined = tbl_irr_combined %>%
  filter(!var_2 %in% c("age_group", "sex", "ethnicity", "imd_Q5_2019",
                       "region_2019", "rural_urban_2019")) %>% 
  select(resource_type, year = level_1, var = var_2_label,
         estimate, ci_lower, ci_upper) %>% 
  mutate(
    year = year %>% 
      factor(),
    var = var %>% 
      factor(levels = unique(var)),
    resource_type = case_when(
      resource_type == "admissions" ~ "Inpatient admissions",
      resource_type == "beddays"    ~ "Inpatient bed-days",
      resource_type == "outpatient" ~ "Outpatient appointments",
      resource_type == "gp"         ~ "Healthcare episodes"
    ) %>% 
      factor() %>% 
      fct_relevel("Healthcare episodes", "Outpatient appointments",
                  "Inpatient admissions", "Inpatient bed-days")
  )

## Save IRR table
write_csv(tbl_irr_combined, 
          here::here("output", "comorbidity_multivar", model_type, "summary",
                     "tbl_irr_combined.csv"))

# Plot combined IRR
plot_irr_combined = tbl_irr_combined %>%
  ggplot(aes(x = estimate, y = year, xmin = ci_lower, xmax = ci_upper,
             colour = year)) +
  geom_point(size = 1) +
  geom_errorbar(width = 0.7) +
  geom_vline(xintercept =  1) +
  scale_x_continuous(trans = "log10") +
  scale_y_discrete(limits = rev) +
  facet_grid(var ~ resource_type, switch = "y", scales = "free_x") +
  labs(x = "Incidence rate ratio (95% confidence interval)",
       y = NULL, colour = "Year") +
  theme(
    strip.text.y.left = element_text(angle = 0),
    legend.position = "bottom",
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )

## Save IRR plot ----
ggsave(filename = "plot_irr_combined.jpeg",
       plot = plot_irr_combined,
       path = here::here("output", "comorbidity_multivar", model_type, "summary"),
       width = 12, height = 10, units = "in")

