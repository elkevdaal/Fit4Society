# Goal: SF-36 analyses for ABR subcohort of PREHAB trial
# Start date 24-10-2023
# AUthor: Elke van Daal

## load packages
library(tidyverse)
library(naniar)
library(janitor)
library(writexl)

## Clear GE ##
rm(list = ls())

## load data ##
load(file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\testroom_sf36raw_abr.RData")

## load SF-36 syntax ##
source("C:\\Users\\Elke\\Documents\\R\\Syntaxes\\Syntax_SF36.R")

## select columns for full_sf36 data and sort on id number ##
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

## Collapse timepoints ##
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
                                 
## For domains 'RP' and 'RE' add 1 so there are no issues with syntax ##
## (scores are 1 and 2 instead of 0 and 1) ##
df_bc <- df_bc %>%
  mutate(P_SFvr4a = P_SFvr4a + 1,
         P_SFvr4b = P_SFvr4b + 1,
         P_SFvr4c = P_SFvr4c + 1,
         P_SFvr4d = P_SFvr4d + 1,
         P_SFvr5a = P_SFvr5a + 1,
         P_SFvr5b = P_SFvr5b + 1,
         P_SFvr5c = P_SFvr5c + 1) #To do: check if responses correspond with RP and RE scores

## select columns to determine SF36 scores ##
df_sf36 <- df_bc %>% select(P_SFvr1:P_SFvr11d) #all timepoints

## look into sf36 scores (and missings) of timepoint `1 and 2 of intervention group ##
df_sf36_int <- df_bc %>% filter(time_point == 'm1' | time_point == 'm2', #only timepoint m1 and m2
                          group == 'Intervention') %>% #only select intervention patients
                   select(id, P_SFvr1:P_SFvr11d)
miss_case_table(df_sf36_int) 
vis_miss(df_sf36_int, cluster = TRUE) #many missings items 3e t/m 3j

## select rows (id's) that have missings ##
df_sf36_na <- df_sf36_int[!complete.cases(df_sf36_int), ]
sf36_na <- df_sf36_na %>% remove_empty('rows', cutoff = 0.05) #remove rows that ONLY have NA (except ID number)

## return excel sheet to see which ID's have missings for which sf36 items ##
write_xlsx(sf36_na, "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\sf36_na.xlsx")

## Calculate SF36 scores and return dataframe 'sf36_calcs' ##
sf36(df_sf36)
sf36_calcs <- cbind(df_bc$id, df_bc$time_point, sf36_calcs)
View(sf36_calcs)
save(sf36_calcs, file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\sf36_summaryscores.RData")

## bind sf36_calcs with df_bc (make sure ID number is sorted!!) and save this data ##
bc_full <- cbind(df_bc, sf36_calcs)
save(bc_full, file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\full_testroom_sf36.RData")

## remove columns, sort, filter ##
df36 <- bc_full %>% select(!P_SFvr1:P_SFvr11d, -contains('Survey'), -sex, -time_parent,
                              -surgery, -contains('smoking'), -contains('m1_'),
                              -contains('m2_'), -hads_score, -predis_score, -int_sinefuma,
                              -contains('baseline'), -contains('pre_surgery'), -f4s_psych,
                              -f4s_psych_x) %>%
                   arrange(id) %>% #sort on ID 
                   filter(group == 'Intervention', time_point == 'm1' | time_point == 'm2') #filter for intervention group and timepoint 1 and 2

View(df36)

## calculate differences in mhs and phs scores between m2 and m1 ##
df36_wide <- df36 %>%
  pivot_wider(names_from = time_point, values_from = PF:MCS) %>% #from long to wide format
  unnest() %>% #unlist to usual dataframe
  mutate(across(4:23, round, 2)) %>% #round variables to 2 decimals
  mutate(diff_mcs = MCS_m2 - MCS_m1,
         diff_pcs = PCS_m2 - PCS_m1,
         perc_diff_mcs = (MCS_m2 - MCS_m1) / MCS_m1 * 100,
         perc_diff_pcs = (PCS_m2 - PCS_m1) / PCS_m1 * 100) #calculate difference 

View(df36_wide)

sf36_stats <- df36_wide %>% 
  filter(complete.cases(.)) %>% #filter for complete pairs
  summarise(mean_diff_mcs = mean(diff_mcs),
            sd_diff_mcs = sd(diff_mcs),
            mean_perc_diff_mcs = mean(perc_diff_mcs),
            sd_perc_diff_mcs = sd(perc_diff_mcs),
            mean_diff_pcs = mean(diff_pcs),
            sd_diff_pcs = sd(diff_pcs),
            mean_perc_diff_pcs = mean(perc_diff_pcs),
            sd_perc_diff_pcs = sd(perc_diff_pcs),
            count = n()) 
sf36_stats <- sf36_stats %>%
  mutate(variable = 'sf36')

## Test for statistical sign differences between m2 and m1 ##
## TO DO --> check normality, perform paired t-test / wilcoxon signed rank

## filter for intervention group only, groupby time_point and calculate mhs, phs ##
df36 %>%
  group_by(time_point) %>%
  summarise(mean_mhs = mean(MCS, na.rm = TRUE),
            mean_phs = mean(PCS, na.rm = TRUE),
            count = n())
## same but now complete cases only ##
df36 %>%
  select(id, MCS, PCS, group, time_point) %>%
  filter(group == 'Intervention',
         complete.cases(.)) %>%
  group_by(time_point) %>%
  summarise(mean_mhs = mean(MCS),
            mean_phs = mean(PCS),
            count = n())






