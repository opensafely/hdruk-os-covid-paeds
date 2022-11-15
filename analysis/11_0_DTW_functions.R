# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 11_DTW_functions.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#
# This script contains functions to be used in 11_DTW_tsclust.R

# Function to separate time series for each individual ---
get_indv_resource_traj = function(gdf, id){
  dates = decimal_date(gdf$date)
  start_date = dates[1]
  encode_cond = match(gdf$service, service_list)
  time_series = ts(encode_cond, start=start_date)
  attr(time_series, "name") <- id
  time_series
}

# Function to register custom distance function with proxy ----
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
