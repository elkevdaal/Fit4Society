# Goal: SF-36 analyses for ABR subcohort of PREHAB trial
# Start date 24-10-2023

## load packages
library(tidyverse)

## load data
sf_36 <- read.csv("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\F4S_PREHAB_trial_SF36_JUIST_-_Nederlands_export_20231024.csv",
                  sep = ';') #sf-36 data
source('C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Source cleaning and codebook.R') #full_data from testroom

## load SF-36 syntax
source("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\SF36_syntax.R")

## filter testroom data to keep only ABR patients
bc_data  <- full_data %>% filter(surgery_type == 'Breast')
glimpse(bc_data)

## Change name of ID variable in sf_36 data
sf_36 <- sf_36 %>%
  rename(id = Castor.Participant.ID) %>%
  mutate(id = as.integer(str_remove(id, 'F4S_'))) 



## semi join bc_data with sf_36 data to keep ABR patients
sf36_bc <- semi_join(sf_36, bc_data, by = 'id')

## full join sf36_bc and bc_data to add valuable columns
full_sf36 <- full_join(sf36_bc, bc_data, by = 'id')
glimpse(full_sf36)

## select columns for full_sf36 data and sort on id number
df_full <- full_sf36 %>%
  select(id, Survey.Progress, Survey.Completed.On, Survey.Parent, 
         Survey.Package.Name, P_SFvr1:P_SFvr11d, surgery, group,
         age, sex, smoking, smoking_count, m1_length, m1_weight, m1_bmi,
         m1_bia_perc_fat, m1_hkk, m1_ar_vo2, m1_sr_vo2, m1_1rm_calc,
         hads_score, predis_score, int_sinefuma, 
         m2_length, m2_weight, m2_bmi, m2_bia_perc_fat, m2_hkk, 
         m2_ar_vo2, m2_sr_vo2, m2_1rm_calc, baseline_move,
         baseline_sport, baseline_alc, baseline_alc_amount,
         pre_surgery_smoking,pre_surgery_smoking_amount, pre_surgery_smr,
         pre_surgery_alc, pre_surgery_alc_amount, f4s_psych, f4s_psych_x)  %>%
  arrange(id)

# Collapse timepoints

df_full <- df_full %>% 
  mutate(time_point = as.factor(Survey.Package.Name),
         time_parent = as.factor(Survey.Parent)) %>%
  mutate(time_point = fct_collapse(time_point,
                                   m1 = c('Meetmoment 1 (interventiegroep)',
                                          'Meetmoment 1 (controlegroep)'),
                                   m2 = c('ALLES ONLINE - Meetmoment 2',
                                          'Meetmoment 2',
                                          'ALLES ONLINE - Meetmoment 2 (INTERVENTIE)',
                                          'ALLEEN Meetmoment 2',
                                          'ALLES ONLINE - ALLEEN Meetmoment 2'),
                                   m3 = c('ALLES ONLINE - Meetmoment 3 (3 maanden)',
                                          'Meetmoment 3 (3 maanden)'),
                                   m4 = c('Meetmoment 4 (6 maanden)',
                                          'Meetmoment 4 (Gynaecologie)'), 
                                   m5 = 'Meetmoment 5 (12 maanden)'))
                                 
         
df$time_point
levels(df$time_point)

# For domains 'RP' and 'RE' add 1 so there are no issues with syntax (scores are 1 and 2 instead of 0 and 1)
df_full <- df_full %>%
  mutate(P_SFvr4a = P_SFvr4a + 1,
         P_SFvr4b = P_SFvr4b + 1,
         P_SFvr4c = P_SFvr4c + 1,
         P_SFvr4d = P_SFvr4d + 1,
         P_SFvr5a = P_SFvr5a + 1,
         P_SFvr5b = P_SFvr5b + 1,
         P_SFvr5c = P_SFvr5c + 1) #To do: check if responses correspond with RP and RE scores

## select columns to determine SF36 scores
df_sf36 <- df_full %>% select(P_SFvr1:P_SFvr11d) #all timepoints

df_sf36_int <- df %>% filter(time_point == 'm1' | time_point == 'm2', #only timepoint m1 and m2
                          group == 'Intervention') %>% #only select intervention patients
                   select(id, P_SFvr1:P_SFvr11d)
View(df_sf36_int)
miss_case_table(df_sf36_int) #check missings --> many missings items 3e t/m 3j

vis_miss(df_sf36_int, cluster = TRUE)

# TO DO: check which ID's have missings 

## Calculate SF36 scores and return dataframe 'sf36_calcs'
sf36(df_sf36)
View(sf36_calcs) # lot of domains missing --> check this!!

## bind sf36_calcs with df_full (make sure ID number is sorted!!)
df_complete <- cbind(df_full, sf36_calcs)

## remove raw score colums 
df_complete <- df_complete %>% select(!P_SFvr1:P_SFvr11d)
glimpse(df_complete)

## filter for intervention group only, groupby time_point and calculate mhs, phs
df_complete %>%
  filter(group == 'Intervention') %>%
  group_by(time_point) %>%
  summarise(mean_mhs = mean(MCS, na.rm = TRUE),
            mean_phs = mean(PCS, na.rm = TRUE),
            count = n())
## same but now complete cases only
df_complete %>%
  select(id, MCS, PCS, group, time_point) %>%
  filter(group == 'Intervention',
         complete.cases(.)) %>%
  group_by(time_point) %>%
  summarise(mean_mhs = mean(MCS),
            mean_phs = mean(PCS),
            count = n())

# CHECK different measurement moments!!! How is this arranged?

# Calculate difference in MHS and PHS between m2 and m1


