# Load packages
library(tidyverse)

# Load data
file.choose()
sf_36 <- read.csv("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\F4S_PREHAB_trial_SF36_JUIST_-_Nederlands_export_20231009.csv",
                  sep = ';')
view(sf_36)

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
