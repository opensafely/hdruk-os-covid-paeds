
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
  model_type     = "poisson"
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


c("gp", "outpatient", "admissions", "beddays") %>% 
  walk(function(resource_type){
    
    model_fit = read_rds(here::here("output", "comorbidity_multivar", model_type,
                                    resource_type, "model", "model_fit.rds"))
    
    tbl_model_coef = model_fit %>%
      tidy() %>% 
      tidy_add_term_labels(model = model_fit) %>% 
      left_join(
        model_fit %>% 
          confint.default() %>% 
          as_tibble(rownames = "term") %>% 
          rename(ci_lower = "2.5 %", ci_upper = "97.5 %"),
        by = "term"
      )
    
    write_csv(tbl_model_coef, 
              here::here("output", "comorbidity_multivar", model_type,
                         resource_type, "tbl_model_coef.csv"))
    
    # Read model coefficients
    tbl_model_coef = read_csv(here::here("output", "comorbidity_multivar", model_type,
                                         resource_type, "tbl_model_coef.csv"))
    # Tidy up table
    tbl_model_coef_tidy = tbl_model_coef %>% 
      filter(!term == "(Intercept)") %>% 
      mutate(
        year = str_extract(term, "(?<=year)\\d{4}"),
        var_lab = paste0(str_remove(var_label, "Year \\* "), ": ",
                         str_remove(label, "\\d{4} \\* ")) %>% 
          str_remove(": \\d{4}"),
        var_lab = var_lab %>% 
          factor(levels = unique(var_lab)) %>% 
          fct_rev(),
        var_type = case_when(
          var_type == "interaction" ~ "Interaction",
          str_detect(term, "_with_") ~ "Interaction",
          variable == "year" ~ "Year effect",
          TRUE ~ "Baseline effect"
        ) %>% 
          factor() %>% 
          fct_relevel("Year effect", "Baseline effect")
      ) %>% 
      replace_na(list(year = "2019"))
    
    
    plot_model_coef = tbl_model_coef_tidy %>% 
      ggplot(aes(x = estimate, xmin = ci_lower, xmax = ci_upper,
                 y = var_lab, colour = var_type)) +
      geom_point() +
      geom_errorbar(width = 0.25) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      facet_wrap(~ year, nrow = 1) + 
      labs(
        y = NULL, x = "Coefficient (95% confidence interval)",
        colour = "Variable type"
      ) +
      theme(
        legend.position = "bottom"
      )
    
    # Save IRR plot ----
    ggsave(filename = "plot_model_coef.jpeg",
           plot = plot_model_coef,
           path = here::here("output", "comorbidity_multivar", model_type,
                             resource_type),
           width = 12, height = 8, units = "in")
    
  }
  )

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

