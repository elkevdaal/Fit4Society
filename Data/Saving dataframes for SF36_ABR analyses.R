# Project: Fit4Society ABR subgroup analysis
# Goal: save different dataframes necessary for analyses
# Author: Elke van Daal

## Load packages
library(tidyverse)

## Set wd
setwd("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data")

## load data
sf_36 <- read.csv("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\F4S_PREHAB_trial_SF36_JUIST_-_Nederlands_export_20231024.csv",
                  sep = ';') #sf-36 data
source('C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Cleaning\\Source cleaning and codebook.R') #full_data from testroom

## filter testroom data to keep only ABR patients
bc_data  <- full_data %>% filter(surgery_type == 'Breast')

## Change name of ID variable in sf_36 data
sf_36 <- sf_36 %>%
  rename(id = Castor.Participant.ID) %>%
  mutate(id = as.integer(str_remove(id, 'F4S_'))) 

## semi join bc_data with raw sf_36 data to only keep the ABR patients in SF36 data
sf36_raw_bc <- semi_join(sf_36, bc_data, by = 'id')

## full join sf36_bc and bc_data to create df with all testroom data from abr cohort
## as well as all sf36 data from abr cohort
bc_testroom_sf36 <- full_join(sf36_raw_bc, bc_data, by = 'id')

## save dataframes
save(bc_data, file = "C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\testroom_data_abr.RData")
save(sf36_raw_bc, file = "C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\sf36_raw_abr.RData")
save(bc_testroom_sf36, file = "C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\testroom_sf36raw_abr.RData")

### Exit  ###