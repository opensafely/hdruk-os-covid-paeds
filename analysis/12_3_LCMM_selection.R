# Studying the Long-term Impact of COVID-19 in Kids (SLICK)
# 
# 12_LCMM_model.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022
# School of Informatics, University of Edinburgh 2022
# Written by: Karthik Mohan, James Farrell
#
# This script summarises key metrics from the LCLMM models (eg. BIC,
# log-likihood) to aid in model selection. Predicted healthcare trajectories
# are plotted.

# Load packages ----
library(tidyverse)
library(lcmm)
library(splines2)

# Command arguments to set number of clusters ----
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  resource_type = "beddays"
} else{
  resource_type = args[[1]]
}

# Directories ----
dir_lcmm_models          = here::here("output", "lcmm", resource_type, "models")
dir_lcmm_selection       = here::here("output", "lcmm", resource_type, "selection")

## Create new output directories ----
dir.create(dir_lcmm_selection, showWarnings = FALSE, recursive=TRUE)

# Plot theme ----
theme_set(theme_bw())

# Load saved LCMM models ----
lcmm_models = list.files(dir_lcmm_models,
                         pattern = "lcmm_model_[0-9]+.rds") %>% 
  map(function(lcmm_file){
    lcmm_model = read_rds(here::here("output", "lcmm", resource_type, "models", lcmm_file))
  })

# Model metrics ----
## Extract model metrics ----
tbl_model_metrics = lcmm_models %>% 
  map(function(lcmm_model){
    tibble(
      n_cluster = lcmm_model$ng,
      AIC = lcmm_model$AIC,
      BIC = lcmm_model$BIC,
      loglik = lcmm_model$loglik,
      niter = lcmm_model$niter,
      conv_param = lcmm_model$gconv[1],
      conv_lik = lcmm_model$gconv[2],
      conv_der = lcmm_model$gconv[3],
      conv_status = lcmm_model$conv,
      runtime = lcmm_model$runtime
    )
  }) %>% 
  bind_rows() %>% 
  mutate(conv_status = case_when(
    conv_status == 1 ~ "Convergence satisfied",
    conv_status == 2 ~ "Max iterations reached",
    TRUE ~ "Problem occured"
  ))

## Save model metrics ----
write_csv(tbl_model_metrics,
          here::here("output", "lcmm", resource_type, "selection",
                     "tbl_model_metrics.csv"))

## Plot AIC by number of clusters ----
plot_AIC = tbl_model_metrics %>% 
  ggplot(aes(x = n_cluster, y = AIC, shape = conv_status)) +
  geom_line() + geom_point() +
  labs(x = "Number of clusters", y = "AIC", shape = "Convergence status") +
  theme(legend.position = "bottom")

ggsave(filename = here::here("output", "lcmm", resource_type, "selection", "plot_AIC.jpeg"),
       plot = plot_AIC,
       height = 6, width = 6, units = "in")

## Plot BIC by number of clusters ----
plot_BIC = tbl_model_metrics %>% 
  ggplot(aes(x = n_cluster, y = BIC, shape = conv_status)) +
  geom_line() + geom_point() +
  labs(x = "Number of clusters", y = "BIC", shape = "Convergence status") +
  theme(legend.position = "bottom")

ggsave(filename = here::here("output", "lcmm", resource_type, "selection", "plot_BIC.jpeg"),
       plot = plot_BIC,
       height = 6, width = 6, units = "in")

## Plot Log-liklihood by number of clusters ----
plot_loglik = tbl_model_metrics %>% 
  ggplot(aes(x = n_cluster, y = loglik, shape = conv_status)) +
  geom_line() + geom_point() +
  labs(x = "Number of clusters", y = "Log-likelihood",
       shape = "Convergence status") +
  theme(legend.position = "bottom")

ggsave(filename = here::here("output", "lcmm", resource_type, "selection", "plot_loglik.jpeg"),
       plot = plot_loglik,
       height = 6, width = 6, units = "in")

# Class probabilities ----
## Extract class probabilities ----
tbl_class_probability = lcmm_models %>% 
  map(function(lcmm_model){
    lcmm_model$pprob %>%
      pivot_longer(cols = -c(patient_id, class),
                   names_pattern = "prob(\\d+)$",
                   names_to = "class_pred") %>% 
      filter(class == class_pred) %>% 
      group_by(class) %>% 
      summarise(boots_stats = list(Hmisc::smean.cl.boot(value, B = 250))) %>% 
      unnest_wider(boots_stats) %>% 
      mutate(n_cluster = lcmm_model$ng) %>% 
      relocate(n_cluster)
  }) %>%
  bind_rows() %>% 
  mutate(
    n_cluster = n_cluster %>% factor(),
    class = class %>% factor()
  )

## Save class porbabilities ----
write_csv(tbl_class_probability,
          here::here("output", "lcmm", resource_type, "selection", "tbl_class_probability.csv"))

## Plot class probability by number of clusters ----
plot_class_probability = tbl_class_probability %>%
  ggplot(aes(x = n_cluster, y = Mean*100, 
             ymin = Lower*100,
             ymax = Upper*100,
             fill = class)) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  labs(x = "Number of clusters", fill = "Class",
       y = "Class membership probability (%)")

ggsave(filename = here::here("output", "lcmm", resource_type, "selection", "plot_class_probability.jpeg"),
       plot = plot_class_probability,
       height = 6, width = 6, units = "in")
