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

# Output directories ----
dir_lcmm_models = here::here("output", "lcmm", "models")

# Create output directories ----
dir.create(dir_lcmm_models, showWarnings = FALSE, recursive=TRUE)

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  ng = 1 # will hard code ng = 1 to 5 in .yaml
} else{
  ng = args[[1]] %>% as.integer()
}

# Load resource data ----
data_resource_lcmm = read_rds(here::here("output", "data", "data_resource_lcmm.rds"))

# Convert to data.frame ----
data_resource_lcmm = as.data.frame(data_resource_lcmm)

# Run LCMM model ----
## Set model parameters ----
max_iter = 2000 # Maximum number of iterations

## Run lcmm ----
if (ng == 1){
  
  lcmm_model = hlme(hospital_use ~ bSpline(indexed_month, degree = 3, knots = 7),
                    random = ~bSpline(indexed_month, degree = 3, knots = 7),
                    subject = "patient_id", 
                    ng = ng,
                    maxiter = max_iter, 
                    data = data_resource_lcmm,
                    verbose = FALSE)
  
} else{
  
  # Load lcmm model with ng = 1
  lcmm_model_1 = read_rds(here::here("output", "lcmm", "models", "lcmm_model_1.rds"))
  
  # Run hlme ----
  lcmm_model = hlme(fixed = hospital_use~bSpline(indexed_month, degree = 3, knots = 7),
                    random= ~bSpline(indexed_month, degree = 3, knots = 7),
                    mixture = ~bSpline(indexed_month, degree = 3, knots = 7),
                    classmb = ~1, 
                    ng = ng,
                    B = lcmm_model_1,
                    data = data_resource_lcmm,
                    subject = "patient_id",
                    maxiter = max_iter,
                    verbose = FALSE)
}

# Save lcmm_model ----
write_rds(x = lcmm_model,
          here::here("output", "lcmm", "models",
                     paste0("lcmm_model_", ng, ".rds")))

