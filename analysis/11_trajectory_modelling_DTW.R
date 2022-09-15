#===============================================================================
# Script: Trajectory_modelling_DTW
# Objective: Cluster trajectory of healtcare service usage using DTW Clustering
# Last Edited by : Karthik Mohan, 14 Sept 2022
#===============================================================================

rm(list=ls())
#================================= Packages ====================================

pkgs <- c('dplyr','ggplot2','dtwclust','tidyverse','lubridate')
# install.packages(pkgs)
lapply(pkgs, require, character.only = TRUE)

getwd()

# ============================================================================== 
# Insert appropriate data path
data_path = '../Data/output/data/'

# Load data - assumes data is in dummy data format .rds
resource    <- readRDS(paste0(data_path, 'data_resource.rds'))
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

#=============================== Functions =====================================

# Function to separate time series for each individual
get_indv_resource_traj = function(gdf, id){
  dates = decimal_date(gdf$date)
  start_date = dates[1]
  encode_cond = match(gdf$service, service_list)
  time_series = ts(encode_cond, start=start_date)
  attr(time_series, "name") <- id
  time_series
}

# Run the function (map to patient group in df)
id_resource_seq <- resource %>%
  group_by(patient_id) %>%
  group_map(~get_indv_resource_traj(.x, .y$patient_id))

names(id_resource_seq) <- map(id_resource_seq, attr, "name")
attr(id_resource_seq, "service_list") <- service_list

# Function to register custom distance function with proxy
regist_dist = function() {
  cat("[INFO] Registering the customized distance function with proxy...")
  dist_overlap = function(x, y) {
    dist = 1
    if (x == y){dist = 0}
    return(dist)
  }
  if (!pr_DB$entry_exists("overlap")){
    pr_DB$set_entry(FUN = dist_overlap, names=c("overlap"), loop = TRUE, type = "metric",
                    distance = TRUE, description = "The overlap distance counts number of matches")
  }
  noverlap = function(ts1, ts2, ...) {
    dtw(ts1, ts2, dist.method = "overlap", distance.only = TRUE, ...)$normalizedDistance
  }
  if (!pr_DB$entry_exists("nOverlap")){
    pr_DB$set_entry(FUN = noverlap, names=c("nOverlap"), loop = TRUE, type = "metric",
                    distance = TRUE, description = "The normalized global overlap distance")
  }
  cat("Done\n")
}

# Function to identify optimal clusters in n range
search_cvi = function(id_resource_seq, n_clusters) {
  start = n_clusters[1]
  end = n_clusters[2]
  cat(sprintf('[INFO] Searching for the best number of clusters (range: %d-%d)...\n', start, end))
  clusts = tsclust(id_resource_seq, k=start:end, distance = "nOverlap", seed=43) #,type = "hierarchical", control = hierarchical_control(method = "average")
  names(clusts) <- paste0("k_", start:end)
  cat('CVI for each clusters:\n')
  #Sil: max; SF: max; CH: max; DB: min; DBstar: min; D: max; COP: min
  cvis = sapply(clusts, cvi, type = "valid")
  print(cvis)
}

# Function to perform DTW clustering with n clusters
dtwclusting = function(id_resource_seq, n_clusters){
  cat(sprintf('[INFO] Using n_clusters = %d for clustering...\n', n_clusters))
  clust = tsclust(id_resource_seq, k=n_clusters, distance = "nOverlap", seed=43)#,type = "hierarchical", control = hierarchical_control(method = "average")
  #cat('Plotting...')
  #plot(clust, type = "sc")
  cat('done\n')
  cat("CVI:\n")
  print(cvi(clust))
  #cat('Cluster allocations:\n')
  #print(t(cbind(names(id_disease_seq), cluster = clust@cluster)))
  return(clust)
}

#===============================================================================

regist_dist()
# Insert appropriate outpath
outpath <- '../Data/output/data/DTWres/'


# Clustering parameters
n_clusters = 5
save = TRUE
limit = 0
search = FALSE
# if(length(n_clusters) > 1){search = TRUE}


# Search has been set to false for example, clustering with n=5
if(search){
  search_cvi(id_resource_seq, n_clusters)
}else {
  clust = dtwclusting(id_resource_seq, n_clusters)
  if(save){
    # Save model
    cat('[INFO] Saving DTW model...')
    saveRDS(clust, paste0(outpath,n_clusters,"_limit",limit,".rds"))
    cat('done\n')
  }
}

# Results are in clust object and in OP file in outpath
# Accessing clust object (S4, so use @ to access items instead of $)
# Creating df with patient IDs and clusters
cluster_df <- data.frame(id_list, clust@cluster)
