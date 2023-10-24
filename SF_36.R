# Load packages
library(tidyverse)
library(dplyr)

# Load data
file.choose()
sf_36 <- read.csv("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\F4S_PREHAB_trial_SF36_JUIST_-_Nederlands_export_20231009.csv",
                  sep = ';')
view(sf_36)

# select 36 columns to use sf36_syntax 
sf_36 <- sf_36 %>%
  select(P_SFvr1:P_SFvr11d)
# Use syntax 
sf36(sf_36)
# Calculate mean MCS and PCS (just for test)
sf36_calcs %>%
  group_by(!is.na(MCS)) %>%
  summarise(mean_MCS = mean(MCS),
            count = n())
sf36_calcs %>%
  group_by(!is.na(PCS)) %>%
  summarise(mean_PCS = mean(PCS),
            count = n())
# TO DO
# add patient numbers to sf36_calcs dataframe
# filter for surgery = ABR (before running sf36 function)
# check if columns have the right name in syntax 

# Change name of ID variable
sf_36 <- sf_36 %>%
  rename(id = Castor.Participant.ID) %>%
  mutate(id = as.integer(str_remove(id, 'F4S_')))

# Semi join with bc_data to keep participants from ABR cluster
sf36_bc <- semi_join(sf_36, bc_data, by = 'id')
sf36_bc <- sf36_bc %>% arrange(id) %>%
  mutate(survey_completed = case_when(
    Survey.Progress > 99 ~ 1,
    Survey.Progress < 100 ~ 0
  ))
view(sf36_bc)

sf36_bc %>%
  group_by(Survey.Package.Name) %>%
  summarise(mean_completion_rate = mean(survey_completed),
            count = n()) 
# full bc data including sf36
full_bc_sf36 <- full_join(sf36_bc, bc_data, by = 'id')

# filter for intervention
bc_sf36_int <- full_bc_sf36 %>% filter(group == 'Intervention') %>%
  group_by(Survey.Package.Name) %>%
  summarise(mean_completion_rate = mean(survey_completed),
            count = n()) 

bc_sf36_control <- full_bc_sf36 %>% filter(group == 'Control') %>%
  group_by(Survey.Package.Name) %>%
  summarise(mean_completion_rate = mean(survey_completed),
            count = n()) 
