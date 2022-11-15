# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 11_DTW_selection.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#
# This script summarises the cluster validity indices for each k-clustering to
# aid in the selection of the optimum number of clusters (k). 

# Load packages ----
library(tidyverse)
library(dtwclust)

## Create output directories ----
dir.create(here::here("output", "dtw", "selection"), showWarnings = FALSE, recursive=TRUE)

# Plot theme ----
theme_set(theme_bw())

# Time series clustering files ----
cvi_files = list.files(here::here("output", "dtw", "cv_indicies"),
                       pattern = "tbl_cv_indicies_\\d+.csv")

# Cluster validity indicies ----
## Extract values from files, create table ----
tbl_cluster_validity = cvi_files %>% 
  map(function(cvi_file){
    # Load cvi files ----
    cv_indicies = read_csv(here::here("output", "dtw", "cv_indicies", cvi_file))
  }) %>% 
  bind_rows()

## Save table of cluster validity indicies
write_csv(tbl_cluster_validity,
          here::here("output", "dtw", "selection", "tbl_cluster_validity.csv"))

# Plot cluster validity indicies ----
plot_cluster_validity = tbl_cluster_validity %>% 
  ggplot(aes(x = n_cluster, y = cv_value, shape = converged)) +
  geom_point() + geom_line() +
  facet_wrap(~ cv_index, scales = "free_y") +
  labs(x = "Number of clusters", y = NULL,
       shape = "Converged")

## Save plot ----
ggsave(filename = here::here("output", "dtw", "selection",
                             paste0("plot_cluster_validity.jpeg")),
       plot = plot_cluster_validity,
       height = 7, width = 8, units = "in")
