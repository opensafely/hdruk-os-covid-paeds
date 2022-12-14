# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 12_LCMM_model.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#
# This script clusters healthcare-use trajectories using a latent class linear 
# mixed model (LCLMM) with a spline basis. The resulting model is saved to file.

# Load packages ----
library(tidyverse)
library(lcmm)
library(splines2)
library(tictoc)

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  ng = 2
  resource_type = "beddays"
} else{
  ng = args[[1]] %>% as.integer()
  resource_type = args[[2]]
}

# Number cores for parallel computation
nproc = 4

# Create output directories  ----
dir_lcmm_models = here::here("output", "lcmm", resource_type, "models")
dir.create(dir_lcmm_models, showWarnings = FALSE, recursive=TRUE)

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
max_iter = 5000 # Maximum number of iterations

## Run lcmm ----
if (ng == 1){

  lcmm_model = lcmm(
    fixed = resource_use ~ bSpline(followup_month, degree = 3, knots = 6.5,
                                   Boundary.knots = c(1, 12)),
    link = "3-manual-splines",
    intnodes = c(3),
    subject = "patient_id",
    ng = ng,
    maxiter = max_iter,
    data = data_resource_lcmm,
    verbose = TRUE,
    nproc = nproc
  )
  
} else{
  
  # Load lcmm model with ng = 1
  lcmm_model_1 = read_rds(
    here::here("output", "lcmm", resource_type, "models", "lcmm_model_1.rds"))
  
  # Run lcmm ----
  lcmm_model = gridsearch(
    m = lcmm(
      fixed = resource_use ~ bSpline(followup_month, degree = 3, knots = 6.5,
                                     Boundary.knots = c(1, 12)),
      mixture = ~ bSpline(followup_month, degree = 3, knots = 6.5,
                          Boundary.knots = c(1, 12)),
      link = "3-manual-splines",
      intnodes = c(3),
      classmb = ~1,
      ng = ng,
      B = lcmm_model_1,
      data = data_resource_lcmm,
      subject = "patient_id",
      maxiter = max_iter,
      verbose = TRUE,
      nproc = nproc),
    rep = 20, maxiter = 5, minit = lcmm_model_1
  )
}

# Save lcmm_model ----
write_rds(x = lcmm_model,
          here::here("output", "lcmm", resource_type, "models", 
                     paste0("lcmm_model_", ng, ".rds")))

