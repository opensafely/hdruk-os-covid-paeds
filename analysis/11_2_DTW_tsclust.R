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
library(tictoc)

# Load function files ----
source(here::here("analysis", "11_0_DTW_functions.R"))

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  n_clusters = 5
} else{
  n_clusters = args[[1]] %>% as.integer()
}

# Load data ----
data_timeseries_dtw = read_rds(here::here("output", "data", "data_timeseries_dtw.rds"))

# Create output directories ----
dir.create(here::here("output", "dtw", "tsclust"),      showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "dtw", "cv_indicies"),  showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("output", "dtw", "data_cluster"), showWarnings = FALSE, recursive=TRUE)

# Perform time series clustering ----
ts_cluster = tsclust(series = data_timeseries_dtw,
                     k = n_clusters,
                     distance = "dtw_basic",
                     type = "partitional",
                     trace = TRUE,
                     seed = 43)

## Save time series clustering ----
write_rds(ts_cluster,
          here::here("output", "dtw", "tsclust",
                     paste0("tsclust_", n_clusters, ".rds")))

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
          here::here("output", "dtw", "cv_indicies",
                     paste0("tbl_cv_indicies_", n_clusters, ".csv")))

# Save cluster assignments ----
data_cluster = tibble(
  patient_id = ts_cluster@datalist %>% names() %>% as.integer(),
  cluster    = ts_cluster@cluster
)

## Save cluster assignments ----
write_rds(data_cluster,
          here::here("output", "dtw", "data_cluster",
                     paste0("data_cluster_", n_clusters, ".rds")))
