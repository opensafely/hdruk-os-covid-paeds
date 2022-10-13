#===============================================================================
# Script: LCMM.R
# Objective: Cluster trajectory of healthcare service usage using LCMM Clustering
# Last Edited by : Karthik Mohan, 6 Oct 2022
#===============================================================================

rm(list=ls())
#================================= Packages ====================================

# Load packages ----
library(tidyverse)
library(cowplot)
library(lcmm)
library(finalfit)
library(knitr)
library(splines2)

# require('LCTMtools')
# lapply(pkgs, require, character.only = TRUE)

getwd()
# setwd('SLICK/SLICK_R/')
# ============================================================================= 
# Insert appropriate data path
output_path = here::here("output", "lcmm")
path_model  = here::here("output", "lcmm", "models")

# Create output folders
dir.create(output_path, showWarnings = FALSE, recursive=TRUE)
dir.create(path_model,  showWarnings = FALSE, recursive=TRUE)


# Load data - assumes data is in dummy data format .rds
resource = readRDS(here::here("output", "data", "data_resource.rds"))
#===============================================================================

# Some preprocessing for resource RDS
# Create list of patients
id_list = unique(resource$patient_id)

# Create used service column
resource <- resource %>%
  mutate(service = case_when(n_critical_care > 0 ~ 'CC',
                             n_beddays > 0 & n_critical_care == 0 ~ 'BD',
                             n_outpatient > 0 & (n_beddays == 0 & n_critical_care == 0) ~ 'OP',
                             n_gp > 0 & (n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0) ~ 'Contact',
                             n_gp == 0 & n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0 ~ 'None')
  )

# list of services (5)
service_list = unique(resource$service)

resource <- resource %>% 
  mutate(hospital = case_when(service %in% c('CC','BD','OP') ~ 1,
                              service %in% c('Contact','None') ~ 0)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(patient_id) %>% 
  mutate(indexed_month = dense_rank(month))

resource_agg <- resource %>% 
  group_by(patient_id, indexed_month) %>% 
  summarise(hospital_use = sum(hospital))

resource_agg <- as.data.frame(resource_agg)
#=============================== Functions =====================================

# Set model parameters ----
max_ng = 5     # Maximum number of clusters
max_iter = 2000 # Maximum number of iterations
iter_save = 25  # Number of iterations after which model is saved
m_degree = 3
m_knots = 7

# Run lcmm loop across number of clusters ----
for(ng in 1:max_ng){
  if (ng == 1){
    print('Entered iteration for ng == 1')
    lcmm_model = hlme(hospital_use~bSpline(indexed_month, degree=3, knots=7),
                      random=~bSpline(indexed_month, degree=3, knots=7),
                      subject='patient_id', 
                      ng=ng,
                      maxiter=max_iter, 
                      data=resource_agg,
                      verbose = FALSE)
    B = lcmm_model
  }
  else{
    print(paste0('Entered iteration for ng == ', ng))
    lcmm_model = hlme(fixed = hospital_use~bSpline(indexed_month, degree=3, knots=7),
                      random= ~bSpline(indexed_month, degree=3, knots=7),
                      mixture = ~bSpline(indexed_month, degree=3, knots=7),
                      classmb = ~1, 
                      ng = ng,
                      B = B,
                      data = resource_agg,
                      subject = "patient_id",
                      maxiter = iter_save,
                      verbose = FALSE)
  }
  print(paste0('Iteration for ng = ',ng, ' completed.'))
}




