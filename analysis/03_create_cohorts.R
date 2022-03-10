# This script takes the processed data and filters patients based on the 
# exclusion and objective-specific inclusion criteria as set out in the 
# protocol
#
# - input:

# Load library
library("tidyverse")
library("lubridate")
library("consort")

# Read processed data  ----
data_patient =    read_rds(here::here("output", "data", "data_patient.rds"))
data_admissions = read_rds(here::here("output", "data", "data_admissions.rds"))

# Create data for consort diagram ----





