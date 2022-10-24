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

# Directories ----
dir_lcmm_models          = here::here("output", "lcmm", "models")
dir_lcmm_selection       = here::here("output", "lcmm", "selection")
dir_lcmm_pred_trajectory = here::here("output", "lcmm", "pred_trajectory")

## Create new output directories ----
dir.create(dir_lcmm_selection,       showWarnings = FALSE, recursive=TRUE)
dir.create(dir_lcmm_pred_trajectory, showWarnings = FALSE, recursive=TRUE)

# Plot theme ----
theme_set(theme_bw())

# Load saved LCMM models ----
lcmm_models = list.files(dir_lcmm_models,
                         pattern = "lcmm_model_[0-9]+.rds") %>% 
  map(function(lcmm_file){
    lcmm_model = read_rds(here::here("output", "lcmm", "models", lcmm_file))
  })

# Model metrics ----
## Extract model metrics ----
tbl_model_metrics = lcmm_models %>% 
  map(function(lcmm_model){
    tibble(
      n_cluster = lcmm_model$ng,
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
          here::here("output", "lcmm", "selection", "tbl_model_metrics.csv"))

## Plot BIC by number of clusters ----
plot_BIC = tbl_model_metrics %>% 
  ggplot(aes(x = n_cluster, y = BIC, shape = conv_status)) +
  geom_line() + geom_point() +
  labs(x = "Number of clusters", y = "BIC", shape = "Convergence status") +
  theme(legend.position = "bottom")

ggsave(filename = here::here("output", "lcmm", "selection", "plot_BIC.jpeg"),
       plot = plot_BIC,
       height = 6, width = 6, units = "in")

## Plot Log-liklihood by number of clusters ----
plot_loglik = tbl_model_metrics %>% 
  ggplot(aes(x = n_cluster, y = loglik, shape = conv_status)) +
  geom_line() + geom_point() +
  labs(x = "Number of clusters", y = "Log-likelihood",
       shape = "Convergence status") +
  theme(legend.position = "bottom")

ggsave(filename = here::here("output", "lcmm", "selection", "plot_loglik.jpeg"),
       plot = plot_loglik,
       height = 6, width = 6, units = "in")

# Class probabilities ----
## Extract class probabilities ----
tbl_class_probability = lcmm_models %>% 
  map(function(lcmm_model){
    lcmm_model$pprob %>% 
      summarise(across(starts_with("prob"),
                       list(mean = mean, sd = sd))) %>% 
      pivot_longer(everything(),
                   names_pattern = "prob(\\d+)_(.*)$",
                   names_to = c("class", "statistic")) %>% 
      mutate(n_cluster = lcmm_model$ng) %>% 
      relocate(n_cluster)
  }) %>%
  bind_rows() %>% 
  pivot_wider(names_from = statistic) %>% 
  mutate(
    n_cluster = n_cluster %>% factor(),
    class = class %>% factor()
  )

## Save class porbabilities ----
write_csv(tbl_class_probability,
          here::here("output", "lcmm", "selection", "tbl_class_probability.csv"))

## Plot class probability by number of clusters ----
plot_class_probability = tbl_class_probability %>%
  ggplot(aes(x = n_cluster, y = mean*100, 
             ymin = (mean - sd)*100,
             ymax = (mean + sd)*100,
             fill = class)) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  geom_hline(yintercept = 5, linetype = 2) + 
  labs(x = "Number of clusters", fill = "Class",
       y = "Class membership probability (%)")

ggsave(filename = here::here("output", "lcmm", "selection", "plot_class_probability.jpeg"),
       plot = plot_class_probability,
       height = 6, width = 6, units = "in")

# Predicted trajectories ----
lcmm_models %>%
  walk(function(lcmm_model){
    
    # New time data ----
    data_time = data.frame(indexed_month  = seq(1, 12, length = 100))
    
    # Predict resource trajectories ----
    predict_resource = predictY(lcmm_model, data_time,
                                var.time = "indexed_month", draws = T)
    
    # Table of predicted trajectories by class ----
    tbl_predicted_trajectory = predict_resource$pred %>% 
      as_tibble() %>% 
      bind_cols(predict_resource$times) %>% 
      pivot_longer(-indexed_month) %>%
      mutate(
        class = str_extract(name, "\\d+$"),
        class = if_else(is.na(class), "1", class),
        name = case_when(
          str_starts(name, "Ypred")    ~ "y",
          str_starts(name, "lower.Ypred") ~ "y_lower",
          str_starts(name, "upper.Ypred") ~ "y_upper"
        )
      ) %>%
      pivot_wider(names_from = name)
    
    write_csv(tbl_predicted_trajectory,
              here::here("output", "lcmm", "pred_trajectory",
                         paste0("tbl_predicted_trajectory_",
                                lcmm_model$ng,
                                ".csv")))
    
    # Plot predicted trajectory by class ----
    plot_predicted_trajectory = tbl_predicted_trajectory %>% 
      ggplot(aes(x = indexed_month, y = y,
                 ymin = y_lower, ymax = y_upper,
                 colour = class, fill = class)) +
      geom_line() +
      geom_ribbon(alpha = 0.2, linetype = 2, size = 0.25) +
      labs(x = "Follow-up period (months)",
           y = "Secondary care contact-days per 30 days",
           fill = "Class", colour = "Class") +
      scale_x_continuous(breaks = seq(1, 12, 1)) +
      scale_y_continuous(limits = c(0, NA))
    
    ggsave(filename = here::here("output", "lcmm", "pred_trajectory",
                                 paste0("plot_predicted_trajectory_",
                                        lcmm_model$ng, ".jpeg")),
           plot = plot_predicted_trajectory,
           height = 6, width = 6, units = "in")
  })
