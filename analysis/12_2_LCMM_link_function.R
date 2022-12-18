# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 12_LCMM_model.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#
# This script compares multiple 1 latent class LCMM models with different link
# functions.

# Load packages ----
library(tidyverse)
library(lcmm)
library(splines2)

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  resource_type = "beddays"
} else{
  resource_type = args[[1]]
}

# Plot theme ----
theme_set(theme_bw())

# Number cores for parallel computation ----
nproc = case_when(
  resource_type == "outpatient" ~ 4,
  resource_type == "gp" ~ 8,
  TRUE ~ 2
)

# Create output directories  ----
dir_lcmm_link_models = here::here("output", "lcmm", resource_type, 
                                  "link_function")

dir.create(dir_lcmm_link_models, showWarnings = FALSE, recursive=TRUE)

# Load resource data ----
data_resource_lcmm = read_rds(here::here("output", "data", "data_resource_lcmm.rds"))

data_resource_lcmm = data_resource_lcmm %>% 
  select(patient_id, followup_month, resource_use = all_of(paste0("n_", resource_type)))

# Only include patients with at least 1 episode of healthcare use ----
## Identify patient ids ----
patient_id_non_zero = data_resource_lcmm %>% 
  group_by(patient_id) %>% 
  summarise(total = sum(resource_use)) %>% 
  filter(total > 0) %>% 
  pull(patient_id)

## Filter for patients with 1+ resource use ----
data_resource_lcmm = data_resource_lcmm %>% 
  filter(patient_id %in% patient_id_non_zero)

# Convert to data.frame ----
data_resource_lcmm = as.data.frame(data_resource_lcmm)

# Run LCMM model ----
## Set model parameters ----
max_iter = 500 # Maximum number of iterations


# Create table of link functions to model ----
link_function = tribble(
  ~label,                                     ~link_function,      ~interior_nodes,
  "linear",                                   "linear",            NULL,
  "splines (5 equi-distant)",                 "5-equi-splines",    NULL,
  "splines (5 manual - knot: 1, 3, 5, 15)",   "6-manual-splines",  c(1, 3, 5, 15),
  "splines (5 manual - knot: 1, 3, 5)",       "5-manual-splines",  c(1, 3, 5),
  "splines (4 manual - knot: 1, 3)",          "4-manual-splines",  c(1, 3),
  "splines (4 manual - knot: 1, 5)",          "4-manual-splines",  c(1, 5),
  "splines (3 manual - knot: 1)",             "3-manual-splines",  c(1),
)

lcmm_models = link_function %>% 
  pmap(function(label, link_function, interior_nodes){
    
    # Run LCMM ----
    lcmm_model = lcmm(
      fixed = resource_use ~ bSpline(followup_month, degree = 3, knots = 6.5,
                                     Boundary.knots = c(1, 12)),
      link = link_function,
      intnodes = interior_nodes,
      subject = "patient_id",
      ng = 1,
      maxiter = max_iter,
      data = data_resource_lcmm,
      verbose = TRUE,
      nproc = nproc
    )
    
    # Add label to list
    lcmm_model$label = label
    
    return(lcmm_model)
  }
  )


# Create model summary table ----
tbl_link_model_fit = lcmm_models %>% 
  map(function(lcmm_model){
    
    # Create table of model statistics
    tibble(
        link_function = lcmm_model$label,
        AIC = lcmm_model$AIC,
        BIC = lcmm_model$BIC,
        loglik = lcmm_model$loglik,
        conv_status = lcmm_model$conv,
        runtime = lcmm_model$runtime,
        niter = lcmm_model$niter
      )
  }
  ) %>% 
  bind_rows()

## Save predicted trajectory
write_csv(tbl_link_model_fit,
          here::here("output", "lcmm", resource_type, 
                     "link_function", "tbl_link_model_fit.csv"))

# Create table of simulated values from estimated link function ----
tbl_estimlink = lcmm_models %>% 
  map(function(lcmm_model){
    
    # Calculate confidence bands of link using Monte Carlo
    estimlink = predictlink(lcmm_model, ndraws = 2000)
    
    table_estimlink = estimlink$pred  %>%
      as_tibble() %>% 
      mutate(link_function = lcmm_model$label) %>% 
      relocate(link_function)
    
    return(table_estimlink)
  }
  ) %>% 
  bind_rows()

write_csv(tbl_link_model_fit,
          here::here("output", "lcmm", resource_type, 
                     "link_function", "tbl_estimlink.csv"))

# Plot estimated link transformation -----
y_label_pred = case_when(
  resource_type == "beddays" ~ "Bed-days per 30 days",
  resource_type == "outpatient" ~ "Outpatient appointments per 30 days",
  resource_type == "gp" ~ "Healthcare contact days per 30 days",
  TRUE ~ "Error"
)

plot_estimlink = tbl_estimlink %>% 
  ggplot(aes(x = transfY_50, y = Yvalues,
             xmin = transfY_2.5, xmax = transfY_97.5, 
             colour = link_function, fill = link_function)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
  labs(x = "Latent process",
       y = y_label_pred,
       fill = "Link function", colour = "Link function") +
  scale_y_continuous(limits = c(0, NA))

ggsave(filename = here::here("output", "lcmm", resource_type, 
                             "link_function", "plot_estimlink.jpeg"),
       plot = plot_estimlink,
       height = 6, width = 10, units = "in")


