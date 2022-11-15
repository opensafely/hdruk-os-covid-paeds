# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 11_DTW_tsclust.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#
# This script determines patient clusters based on time series clustering of
# healthcare resource use during the follow-up period. 

# Load packages ----
library(tidyverse)
library(lubridate)
library(dtwclust)

# Load function files ----
source(here::here("analysis", "11_0_DTW_functions.R"))

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  n_clusters = 5 # will hard code n_clusters = 3 to 8 in .yaml
} else{
  n_clusters = args[[1]]
}

# Load data ----
data_resource = read_rds(here::here("output", "data", "data_resource_dtw.rds"))

# Directories ----
dir_dtw_tsclust      = here::here("output", "dtw", "tsclust")
dir_dtw_cv_indicies  = here::here("output", "dtw", "cv_indicies")
dir_dtw_data_cluster = here::here("output", "dtw", "data_cluster")

## Create output directories ----
dir.create(dir_dtw_tsclust,      showWarnings = FALSE, recursive=TRUE)
dir.create(dir_dtw_cv_indicies,  showWarnings = FALSE, recursive=TRUE)
dir.create(dir_dtw_data_cluster, showWarnings = FALSE, recursive=TRUE)

# Pre-processing for resource data ----
## Create list of patients ----
id_list = unique(data_resource$patient_id)

## List of services (5) -----
service_list = levels(data_resource$service)# JF: Made this ordered

# Run the function (map to patient group in df) ----
id_resource_seq = data_resource %>%
  group_by(patient_id) %>%
  group_map(~get_indv_resource_traj(.x, .y$patient_id))

names(id_resource_seq) = map(id_resource_seq, attr, "name")
attr(id_resource_seq, "service_list") = service_list

regist_dist() 

# Perform time series clustering ----
ts_cluster = tsclust(id_resource_seq, 
                     k = n_clusters,
                     distance = "dtw_basic", #"nOverlap",
                     type = "partitional", # "hierarchical"
                     #control = hierarchical_control(method = "average"),
                     seed = 43)

# JF:
# distance = "nOverlap" is 1000x slower than default, with no material effect on
# CIV metrics or cluster. Also see warning() comment. Is the default (dtw_basic) suitable?
#
# For k = 5:
# distance = "dtw_basic":
# CIV:
# Sil          SF          CH          DB      DBstar           D         COP 
# 0.21827551  0.00000000 17.17414722  1.49438777  1.53006275  0.01274788  0.18880057
# proctime:
# user  system elapsed 
# 3.95    0.01    0.37 
# Cluster: 1 3 4 5 1 2 1 1 1 5 3 2 2 3 4 1 1 5 1 1 4 1 4 4 1
#
#
# distance = "nOverlap":
# CIV:
# Sil          SF          CH          DB      DBstar           D         COP 
# 0.235197668  0.516222700 18.314231695  1.503402641  1.687588665  0.009708738  0.223892228 
# proctime:
# user    system   elapsed 
# 330.44    4.47    341.71  
# Cluster: 1 3 4 5 1 2 1 1 1 5 3 3 2 3 4 1 1 5 1 1 4 1 4 4 1 


## Save time series clustering ----
write_rds(ts_cluster,
          here::here(dir_dtw_tsclust, paste0("tsclust_", n_clusters, ".rds")))

# Cluster validity indicies ----
cvi_extract = cvi(ts_cluster)

## Make into table ----
tbl_cv_indicies = tibble(
  n_cluster = ts_cluster@k,
  converged = ts_cluster@converged,
  cv_index  = names(cvi_extract),
  cv_value  = cvi_extract
)

## Save cluster validity table
write_csv(tbl_cv_indicies,
          here::here(dir_dtw_cv_indicies,
                     paste0("tbl_cv_indicies_", n_clusters, ".csv")))

# Save cluster assignments ----
data_cluster = tibble(
  patient_id = ts_cluster@datalist %>% names() %>% as.integer(),
  cluster    = ts_cluster@cluster
)

## Save cluster assignments ----
write_rds(data_cluster,
          here::here(dir_dtw_data_cluster,
                     paste0("data_cluster_", n_clusters, ".rds")))

