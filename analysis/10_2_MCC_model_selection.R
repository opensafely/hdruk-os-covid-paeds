# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 10_MCC_model.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#

# Load packages ----
library(tidyverse)

# Load custom functions ----
source(here::here("analysis", "10_0_MCC_functions.R"))


# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  n_clusters = 5 # will hard code n_clusters = 3 to 8 in .yaml
} else{
  n_clusters = args[[1]]
}

# Directories ----
dir_mcc_model      = here::here("output", "mcc", "model")

## Create output directories ----
dir.create(dir_mcc_model,      showWarnings = FALSE, recursive=TRUE)

# Load data ----
data_resource = read_rds(here::here("output", "data", "data_resource_mcc.rds"))

# Pre-processing for resource data ----
# Create lagged service column to calculate transition frequency matrix
resource <- data_resource %>% 
  group_by(patient_id) %>% 
  mutate(prev_service_idx = lag(service_idx, n = 1, default = NA)) %>% 
  mutate(prev_service_idx = tidyr::replace_na(prev_service_idx, 1))
  
# Initial State Vector
initial_state_vector <- resource$service_idx[which(resource$date_indexed==1)]

# Transitions vectors
tr_list <- vector("list", length = n)
id_list = unique(data_resource$patient_id)
n = length(id_list)

for (i in 1:n){
  tr_list[[i]] <- as.integer(resource$service_idx[which(resource$patient_id == patient_list[i])]-1) #added -1
}

njki <- dataListToNjki(tr_list)


# ==============================================================================
max_groupNr <- n_clusters # KM: upper bound of clusters to be tested. e.g. if 5, then clusters 1 to 5 will be tested
results <- list()

# Loop through for each cluster number k
# Creates n_clusters number of model results in dir_mcc_model
for (groupNr in 1:max_groupNr){ 
  results[[groupNr]] <- MCClust(
    Data = list(
      dataFile = njki, ## Transition frequency matrix
      storeDir = dir_mcc_model, # Outpath
      priorFile = NULL), # whether priors are supplied in external file
    Prior = list(H = groupNr, # number of clusters
                 e0 = 4, # Determines the value of the prior parameter of the Dirichlet-prior for the group sizes h
                 c = 1, # used for calculating prior parameter mx  ---- ONLY WHEN PRIORFILE = FALSE
                 cOff = 1, # used for calculating prior parameter mx ---- ONLY WHEN PRIORFILE = FALSE 
                 usePriorFile = FALSE, # prior info for transition probabilities provided as priorfile
                 xiPooled = FALSE, # set true only if priorfile = True
                 N0 = 5), # Determines a parameter for use in calculating the prior parameter matrix
    Initial = list(xi.start.ind = 3, 
                   pers = 0.7,
                   S.i.start = as.integer(initial_state_vector)),
    Mcmc = list(M = 150,
                M0 = 30,
                mOut = 50,
                mSave = 10,
                seed = 1234))
}


# Creates 3 R workspace files with model selection criterion based on whatToDoList
# Also creates a .csv with model selection criteria
modelSel(workDir=dir_mcc_model, myLabel = "model choice for ...",
                 H0 = 3,
                 whatToDoList =c("approxMCL", "approxML", "postMode"))


