#===============================================================================
# Script: Trajectory_modelling_MCC - Visualization
# Objective: Visualize transition probabilities within clusters of MCC Clustering
# Last Edited by : Karthik Mohan, 8 Oct 2022
#===============================================================================

rm(list=ls())
library(tidyverse)
library(ggplot2)

# Load functions
source(here::here("analysis", "10_0_MCC_functions.R"))

# Directories ----
dir_mcc_plot       = here::here("output", "mcc", "plots")
dir_mcc_alloc       = here::here("output", "mcc", "allocations")

## Create output directories ----
dir.create(dir_mcc_plot, showWarnings = FALSE, recursive=TRUE)
dir.create(dir_mcc_alloc, showWarnings = FALSE, recursive=TRUE)

model_selection = read.csv(here::here("output", "mcc", "model", "model_selection_criteria.csv"))


#===============================================================================
# Select best cluster based on selection criteria
best_cluster = as.integer(names(which.max(table(apply(x,2,which.min)))))
model_name = paste0(dir_mcc_model, "MCC_H", best_cluster, "_.*\\.RData")

# Get class allocations for best clustering
selected_model = load(model_name)  # Insert path for selected model here
thin = 1
maxi = 50
M0 = selected_model$Mcmc$M0
plotPathsForEta = TRUE

op <- calcAllocationsMCC(selected_model,
                         thin = thin, 
                         maxi = maxi,
                         M0 = M0, 
                         plotPathsForEta = plotPathsForEta)

class_allocation_file_name = paste0(dir_mcc_alloc, model_name, '_class_allocations','.csv')
write.csv(x = op, file = class_allocation_file_name)

#===============================================================================
# Work with results
# Visualize transition probabilities per group
results <- selected_model
remove(selected_model)
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

ggsave(paste0(dir_mcc_plot, model_name, format(Sys.time(), "%Y%m%d_%H%M%S"),'_plot.png'))
#===============================================================================