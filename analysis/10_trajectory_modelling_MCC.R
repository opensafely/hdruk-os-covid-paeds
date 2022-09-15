#===============================================================================
# Script: Trajectory_modelling_MCC
# Objective: Cluster trajectory of healtcare service usage using MCC Clustering
# Last Edited by : Karthik Mohan, 14 Sept 2022
#===============================================================================

rm(list=ls())

# ====================  Packages ===============================================
pkgs <- c("dplyr", "ggplot2", "tidyr", "tidyverse")
# install.packages(pkgs)
lapply(pkgs, require, character.only = TRUE)

getwd()

# ================== Path ======================================================
# Insert appropriate data path
data_path = '../Data/output/data/'


#===============================================================================

# Load data
resource    <- readRDS(paste0(data_path, 'data_resource.rds'))

# ----------------- MCClust --------------------------
# Need to install package from source due to R version compatibility (requires RTools)
# Need to install dependencies : gplot, gtools, bayesm
# install.packages("C:/Users/katri/Downloads/bayesMCClust_1.0.tar.gz", repos = NULL, type = 'source')
# -------------------

library(bayesMCClust)

# ------- Save entropy_fix.R function script in same folder as this script, or edit below path
source('entropy_fix.R')  


# Current priority Crit > Bed > OP > Contact; but does this make sense? Trajectory can change in one day
# Critical Care never occurs in isolation (in this dataset)
# Create label for service used in day based on priority outlined above
resource <- resource %>%
  mutate(service = case_when(n_critical_care > 0 ~ 'CC',
                             n_beddays > 0 & n_critical_care == 0 ~ 'BD',
                             n_outpatient > 0 & (n_beddays == 0 & n_critical_care == 0) ~ 'OP',
                             n_gp > 0 & (n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0) ~ 'Contact',
                             n_gp == 0 & n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0 ~ 'None')
  )

# Label encoding 
# 1 - None 
# 2 - GP
# 3 - OP
# 4 - Bed
# 5 - Critical Care
levels = c('None', 'Contact', 'OP', 'BD', 'CC')
resource <- resource %>% mutate(service_idx = as.numeric(factor(service, levels=levels)))


# Create lagged service column to calculate transition frequency matrix
resource <- resource %>% 
  group_by(patient_id) %>% 
  mutate(prev_service_idx = lag(service_idx, n = 1, default = NA)) %>% 
  mutate(prev_service_idx = tidyr::replace_na(prev_service_idx, 1))

n = length(unique(resource$patient_id))
  
# Initial State Vector
initial_state_vector <- resource$service_idx[which(resource$date_indexed==1)]

# Transitions vectors
tr_list <- vector("list", length = n)
patient_list <- unique(resource$patient_id)

for (i in 1:n){
  tr_list[[i]] <- as.integer(resource$service_idx[which(resource$patient_id == patient_list[i])]-1) #added -1
}

njki <- dataListToNjki(tr_list)

myOutfilesDir <- data_path # This is where the MCClust function will save the output file

# ==============================================================================
groupNr <- 5 #Number of clusters

results <- test_wo_entrpy(
  Data = list(
    dataFile = njki, ## Transition frequency matrix
    storeDir = myOutfilesDir, # Outpath
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
  Mcmc = list(M = 15000,
              M0 = 3000,
              mOut = 500,
              mSave = 10,
              seed = 1234))

#===============================================================================
# Work with results
# Visualize transition probabilities per group

estTransProb <- results$estTransProb

# Reshape the 3D array of results to long dataframe for ggplot

df <- as.data.frame(estTransProb) %>% #dataframe
  rownames_to_column('InitState') %>% # get initial state as column
  gather('Group', value, -InitState) %>% # convert to long format by group
  mutate(Group = gsub('.Group',' ',Group)) %>% # some processing to separate group name and transition state
  separate(Group, c("TransState","Group")) %>% 
  mutate(
    InitState = factor(InitState, levels=0:4),  # converting to factors 
    TransState = factor(TransState, levels=0:4)
  ) 

# relabel factor levels
df$InitState <- recode_factor(df$InitState, `0` = 'None',`1`='Contact',`2`='OP',`3`='Bed',`4`='CC') 
df$TransState <- recode_factor(df$TransState, `0` = 'None',`1`='Contact',`2`='OP',`3`='Bed',`4`='CC')


# facet heatmap for transition probabilities
ggplot(df, aes(TransState, InitState)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "green") +
  facet_grid(rows=vars(Group))

#===============================================================================
