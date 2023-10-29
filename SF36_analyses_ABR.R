# Goal: SF-36 analyses for ABR subcohort of PREHAB trial
# Start date 24-10-2023
# AUthor: Elke van Daal

## load packages
library(tidyverse)

## load data
load(file = "C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\testroom_data_abr.RData")
load(file = "C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\sf36_raw_abr.RData")
load(file = "C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\testroom_sf36raw_abr.RData")

## load SF-36 syntax
source("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\SF36_syntax.R")

## select columns for full_sf36 data and sort on id number
df_bc <- bc_testroom_sf36 %>%
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

df_bc <- df_bc %>% 
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
                                 
# For domains 'RP' and 'RE' add 1 so there are no issues with syntax (scores are 1 and 2 instead of 0 and 1)
df_bc <- df_bc %>%
  mutate(P_SFvr4a = P_SFvr4a + 1,
         P_SFvr4b = P_SFvr4b + 1,
         P_SFvr4c = P_SFvr4c + 1,
         P_SFvr4d = P_SFvr4d + 1,
         P_SFvr5a = P_SFvr5a + 1,
         P_SFvr5b = P_SFvr5b + 1,
         P_SFvr5c = P_SFvr5c + 1) #To do: check if responses correspond with RP and RE scores

## select columns to determine SF36 scores
df_sf36 <- df_bc %>% select(P_SFvr1:P_SFvr11d) #all timepoints

df_sf36_int <- df_bc %>% filter(time_point == 'm1' | time_point == 'm2', #only timepoint m1 and m2
                          group == 'Intervention') %>% #only select intervention patients
                   select(id, P_SFvr1:P_SFvr11d)
View(df_sf36_int)
miss_case_table(df_sf36_int) #check missings --> many missings items 3e t/m 3j

vis_miss(df_sf36_int, cluster = TRUE)

# TO DO: check which ID's have missings 

## Calculate SF36 scores and return dataframe 'sf36_calcs'
sf36(df_sf36)
View(sf36_calcs) # lot of domains missing --> check this!!

## bind sf36_calcs with df_bc (make sure ID number is sorted!!)
bc_full <- cbind(df_bc, sf36_calcs)

## remove raw score colums 
bc_full <- bc_full %>% select(!P_SFvr1:P_SFvr11d)
glimpse(df_complete)

## filter for intervention group only, groupby time_point and calculate mhs, phs
bc_full %>%
  filter(group == 'Intervention') %>%
  group_by(time_point) %>%
  summarise(mean_mhs = mean(MCS, na.rm = TRUE),
            mean_phs = mean(PCS, na.rm = TRUE),
            count = n())
## same but now complete cases only
bc_full %>%
  select(id, MCS, PCS, group, time_point) %>%
  filter(group == 'Intervention',
         complete.cases(.)) %>%
  group_by(time_point) %>%
  summarise(mean_mhs = mean(MCS),
            mean_phs = mean(PCS),
            count = n())

# CHECK different measurement moments!!! How is this arranged?

# Calculate difference in MHS and PHS between m2 and m1


