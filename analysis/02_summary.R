
# Load packages ----
library(tidyverse)
library(lubridate)
library(finalfit)

# Create directory for summary descriptives ----
dir.create(here::here("output", "descriptive", "plots"), showWarnings = FALSE, recursive=TRUE)

# Read processed data  ----
data_patient    = read_rds(here::here("output", "data", "data_patient.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))
data_outpatient = read_rds(here::here("output", "data", "data_outpatient.rds"))
data_gp         = read_rds(here::here("output", "data", "data_gp.rds"))

# Plot weekly admissions ----
plot_admissions = data_admissions %>% 
  mutate(week = floor_date(admission_date, "week")) %>%
  group_by(week) %>% 
  count(week) %>% 
  ggplot(aes(week,n)) +
  geom_line()

ggsave("plot_weekly_admissions.jpeg",
       plot = plot_admissions,
       device = "jpeg",
       path = here::here("output", "descriptive", "plots"))

# Plot weekly outpatient ----
plot_outpatient = data_outpatient %>% 
  mutate(week = floor_date(outpatient_date, "week")) %>%
  group_by(week) %>% 
  count(week) %>% 
  ggplot(aes(week,n)) +
  geom_line()

ggsave("plot_weekly_outpatient.jpeg",
       plot = plot_outpatient,
       device = "jpeg",
       path = here::here("output", "descriptive", "plots"))

# Plot weekly GP contacts ----
plot_gp = data_gp %>% 
  mutate(week = floor_date(gp_date, "week")) %>%
  group_by(week) %>% 
  count(week) %>% 
  ggplot(aes(week,n)) +
  geom_line()

ggsave("plot_weekly_gp.jpeg",
       plot = plot_gp,
       device = "jpeg",
       path = here::here("output", "descriptive", "plots"))
