# Goal: SF-36 analyses for ABR subcohort of PREHAB trial
# Start date 24-10-2023

## load packages
library(tidyverse)

file.choose()

## load data
sf_36 <- read.csv("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\F4S_PREHAB_trial_SF36_JUIST_-_Nederlands_export_20231024.csv",
                  sep = ';') #sf-36 data
source('C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Source cleaning and codebook.R') #full_data from testroom

## load SF-36 syntax
source("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\SF36_syntax.R")

## filter testroom data to keep only ABR patients
bc_data  <- testroom_data %>% filter(surgery_type == 'Breast')


