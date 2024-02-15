# Goal: Secondary analyses ABR
# Author: Elke van Daal

# Load packages
library(tidyverse)
library(naniar)
library(hablar)
library(janitor)
library(performance)
library(forecast)
library(skimr)
library(ggstatsplot)
library(report)
library(lme4)
library(dlookr)
library(plotly)

# load data (only data needed from participating intervention patients!)
rm(list = ls())
load(file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\ds.RData") 

# Step 1. Import castor data and join with ds

##import castor data
source("C:\\Users\\Elke\\Documents\\R\\Fit4Surgery\\Cleaning\\Source cleaning and codebook.R") #castor data from 20-12-2023

## deselect some (redundant) variables and rename id
full_data <- full_data %>%
  select(-group, -surgery_type, - incl_ic) %>%
  rename(study_id = id)

## inner join ds and full_data into sd
sd <- inner_join(ds, full_data, by = "study_id") 

# Step 2. create sf36 dataframe (sf_36) 

## load sf36 data
sf_36 <- read.csv("Z:\\Data PREHAB ABR\\F4S_PREHAB_trial_SF36_JUIST_-_Nederlands_export_20240214.csv",
                  sep = ';') #sf-36 data 14-2-2024

## Change name of ID variable in sf_36 data
sf_36 <- sf_36 %>%
  rename(study_id = Castor.Participant.ID) %>%
  mutate(study_id = as.integer(str_remove(study_id, 'F4S_'))) 

# Step 3. calculate MHS and PHS for sf_36 and join with sd

## load SF-36 syntax 
source("C:\\Users\\Elke\\Documents\\R\\Syntaxes\\Syntax_SF36.R")

## Collapse timepoints
sf_36 <- sf_36 %>% 
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

## For domains 'RP' and 'RE' add 1 so there are no issues with syntax 
## (scores are 1 and 2 instead of 0 and 1) 
sf_36 <- sf_36 %>%
  mutate(P_SFvr4a = P_SFvr4a + 1,
         P_SFvr4b = P_SFvr4b + 1,
         P_SFvr4c = P_SFvr4c + 1,
         P_SFvr4d = P_SFvr4d + 1,
         P_SFvr5a = P_SFvr5a + 1,
         P_SFvr5b = P_SFvr5b + 1,
         P_SFvr5c = P_SFvr5c + 1) #To do: check if responses correspond with RP and RE scores

## select columns to determine SF36 scores ##
d36 <- sf_36 %>% select(P_SFvr1:P_SFvr11d) #all timepoints

## Calculate SF36 scores and return dataframe 'sf36_calcs' 
sf36(d36)

## bind sf36_calcs with df_bc (make sure ID number is sorted!!) 
sf_36 <- cbind(sf_36, sf36_calcs)

## filter for time_point m1 and m2, filter for only abr patients
sf_36 <- sf_36 %>%
  filter(time_point == 'm1' | time_point == 'm2')

sf_36 <- semi_join(sf_36, sd, by = "study_id") #check patients who have same time_point >1
                                              #study_id: 904 (2x m1), 
## select columns
sf_36 <- sf_36 %>%
  select(study_id, time_point, PF:MCS) %>%
  arrange(study_id)

sf_36 <- sf_36[-27,] #remove row 27 (id 904) CHECK if row number is correct !!!

## to wide format 
dw <- sf_36 %>%
  pivot_wider(names_from = time_point, values_from = PF:MCS) %>% #from long to wide format
  mutate(across(2:21, round, 2)) # round to 2 decimals

## full join dw with sd
sd <- full_join(sd, dw, by = "study_id")

# Step 4. clean sd

## add column baseline measurement completed (yes/no) and add column fu measurement completed (yes/no)
## add column to see if sf36 was (partly) completed
sd <- sd %>%
  mutate(m1_present = as.factor(ifelse(!is.na(m1_length), "yes", "no")),
         m2_present = as.factor(ifelse(!is.na(m2_length), "yes", "no")),
         sf36_m1_present = as.factor(ifelse(is.na(PF_m1) &
                                       is.na(RP_m1) &
                                       is.na(BP_m1) &
                                       is.na(GH_m1) &
                                       is.na(VT_m1) &
                                       is.na(SF_m1) &
                                       is.na(RE_m1) &
                                       is.na(MH_m1),
                                       "no", "yes")),
         sf36_m2_present = as.factor(ifelse(is.na(PF_m2) &
                                              is.na(RP_m2) &
                                              is.na(BP_m2) &
                                              is.na(GH_m2) &
                                              is.na(VT_m2) &
                                              is.na(SF_m2) &
                                              is.na(RE_m2) &
                                              is.na(MH_m2),
                                            "no", "yes")))

table(sd$m1_present, sd$m2_present) 
table(sd$sf36_m1_present, sd$sf36_m2_present)

## select useful columns only
sd <- sd %>%
  select(1:11, 19:23, 26:28, 38, 43, 50:53, 55:57, 63, 75, 76, 81,
         84:86, 96, 101, 108:111, 113:115, 118, 119, 139:141, 143,
         163:167, 172:176, 178:184, 186, 187, 302:304, 310, 313:336)
#check surgery soon 

## relocate columns (m1 and m2 sorted)
sd <- sd %>%
  relocate(c(PF_m1, RP_m1, BP_m1, GH_m1, VT_m1, SF_m1, RE_m1, MH_m1, PCS_m1, MCS_m1),
           .after = m1_1rm_calc)
sd <- sd %>%
  relocate(c(PF_m2, RP_m2, BP_m2, GH_m2, VT_m2, SF_m2, RE_m2, MH_m2, PCS_m2, MCS_m2),
           .after = m2_1rm_calc)

# Save sd (for imputation)
save(sd, file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\sd_before_imp.RData")

### endproduct: dataset of subcohort of intervention patients who adhered to fit4surgery,
### including all testroom variables and sf36 results.
### this endproduct is ready to use for imputation, and afterwards secondary data analyses

  
### Part II: analysis, start after imputation ###

# Create V02max column in ml/min/kg
sd <- sd %>%
  mutate(m1_vo2 = (m1_sr_vo2 * 1000)/m1_weight,
        m2_vo2 = (m2_sr_vo2 * 1000)/m2_weight)

# Only select columns required for secondary analyses
sd <- sd %>% select(study_id, age, hads_score,predis_score, baseline_move,
                                   baseline_sport, f4s_training_amount,
                                   m1_length, m1_weight, m1_bmi, m1_bia_perc_fat,
                                m1_hkk, m1_1rm_calc, m2_length, m2_weight, m2_bmi, 
                                m2_bia_perc_fat, m2_hkk, m2_1rm_calc, m1_vo2, m2_vo2,
                                smoking, smoking_count, baseline_alc, baseline_alc_amount,
                                pre_surgery_alc, pre_surgery_alc_amount, pre_surgery_smoking,
                                pre_surgery_smoking_amount, pre_surgery_smr, MCS_m1,
                    MCS_m2, PCS_m1, PCS_m2) %>%
         mutate(smoking_count = ifelse(smoking == 'no' | smoking == 'quit', 0, smoking_count), #change NA's to 0 if applicable
         baseline_alc_amount = ifelse(baseline_alc == 'No', 0, baseline_alc_amount),
         pre_surgery_alc_amount = ifelse(pre_surgery_alc == "No", 0, pre_surgery_alc_amount))
sd$m1_bia_perc_fat <- as.numeric(sd$m1_bia_perc_fat)
sd$m2_bia_perc_fat <- as.numeric(sd$m2_bia_perc_fat)


# Calculate difference between m1 and m2
sd <- sd %>% mutate(diff_length = m2_length - m1_length,
                     diff_weight = m2_weight - m1_weight,
                     diff_bmi = m2_bmi - m1_bmi,
                     diff_fat = m2_bia_perc_fat - m1_bia_perc_fat,
                     diff_hkk = m2_hkk - m1_hkk, 
                     diff_1rm = m2_1rm_calc - m1_1rm_calc,
                     diff_vo2 = m2_vo2 - m1_vo2,
                     perc_diff_weight = ((m2_weight - m1_weight)/m1_weight) * 100,
                     perc_diff_bmi = ((m2_bmi - m1_bmi)/m1_bmi) * 100,
                     perc_diff_fat = ((m2_bia_perc_fat - m1_bia_perc_fat)/m1_bia_perc_fat) * 100,
                     perc_diff_hkk = ((m2_hkk - m1_hkk)/m1_hkk) * 100,
                     perc_diff_1rm = ((m2_1rm_calc - m1_1rm_calc)/m1_1rm_calc) * 100,
                     perc_diff_vo2 = ((m2_vo2 - m1_vo2)/m1_vo2) * 100,
                     diff_mcs = MCS_m2 - MCS_m1,
                     diff_pcs = PCS_m2 - PCS_m1,
                     perc_diff_mcs = (MCS_m2 - MCS_m1) / MCS_m1 * 100,
                     perc_diff_pcs = (PCS_m2 - PCS_m1) / PCS_m1 * 100)  

# Count number of patients who have clinically meaningful differences above 10%
cmd_rm <- sd %>% count(perc_diff_1rm >= 10) %>% #count number of people that achieve cmd
  filter(complete.cases(.)) %>% #only keep patients with 2 measurements
  mutate(percentage = (n / sum(n)) *100) #add percentage of people that achieve cmd
cmd_hkk <- sd %>% count(perc_diff_hkk >= 10) %>%
  filter(complete.cases(.)) %>%
  mutate(percentage = (n / sum(n)) * 100)
cmd_vo2 <- sd %>% count(perc_diff_vo2 >= 10) %>%
  filter(complete.cases(.)) %>%
  mutate(percentage = (n / sum(n)) * 100)

# From wide to long format and sort
sd_long <- sd %>% select(-baseline_alc, -baseline_alc_amount, -contains('smoking'),
                              -pre_surgery_smr, -pre_surgery_alc, -pre_surgery_alc_amount) %>%
pivot_longer(cols = !study_id, names_to = 'measurement', values_to = 'score') %>%
  arrange(measurement)

# Same but now including baseline variables (potential confounders)
bc_long2 <- bc_data %>% 
  pivot_longer(cols = -c(id,age, hads_score,predis_score, baseline_move, baseline_alc, baseline_alc_amount,
                         baseline_sport, f4s_training_amount, smoking, smoking_count, 
                         pre_surgery_smoking, pre_surgery_smoking_amount, pre_surgery_smr, 
                         pre_surgery_alc, pre_surgery_alc_amount),
                         names_to = 'measurement', values_to = 'score') %>%
  arrange(measurement)

# Make different dataframes per parameter
bmi <- sd_long %>% filter(measurement == 'm1_bmi' | measurement == 'm2_bmi')
fat <- sd_long %>% filter(measurement == 'm1_bia_perc_fat' | measurement == 'm2_bia_perc_fat')
hkk <- sd_long %>% filter(measurement == 'm1_hkk' | measurement == 'm2_hkk')
rm <- sd_long %>% filter(measurement == 'm1_1rm_calc' | measurement == 'm2_1rm_calc')
vo2 <- sd_long %>% filter(measurement == 'm1_vo2' | measurement == 'm2_vo2')
mcs <- sd_long %>% filter(measurement == 'MCS_m1' | measurement == 'MCS_m2')
pcs <- sd_long %>% filter(measurement == 'PCS_m1' | measurement == 'PCS_m2')

# Function to generate summary statistics
sumstats <- function(m1_var, m2_var, diff_var, perc_diff_var) {
  m1_var = enquo(arg = m1_var)
  m2_var = enquo(arg = m2_var)
  diff_var = enquo(arg = diff_var)
  perc_diff_var = enquo(arg = perc_diff_var)
  sd %>% select(study_id, !!m1_var, !!m2_var, !!diff_var, !!perc_diff_var) %>%
    filter(complete.cases(.)) %>%      #only include patients that completed m1 and m2
    summarise(mean_m1 = mean(!!m1_var),
              sd_m1 = sd(!!m1_var),
              mean_m2 = mean(!!m2_var),
              sd_m2 = sd(!!m2_var),
              mean_diff = mean(!!diff_var),
              sd_diff = sd(!!diff_var),
              mean_perc_diff = mean(!!perc_diff_var),
              sd_perc_diff = sd(!!perc_diff_var),
              count = n())
}

## Count total number of intervention patients
nrow(full_data_bc)

## BMI
bmi_stats <- sumstats(m1_bmi, m2_bmi, diff_bmi, perc_diff_bmi) %>%
  mutate(variable = 'bmi')
## 1RM
rm_stats <- sumstats(m1_1rm_calc, m2_1rm_calc, diff_1rm, perc_diff_1rm) %>%
  mutate(variable = '1rm')
## Fat
fat_stats <- sumstats(m1_bia_perc_fat, m2_bia_perc_fat, diff_fat, perc_diff_fat) %>%
  mutate(variable = 'fat percentage')
## HKK
hkk_stats <- sumstats(m1_hkk, m2_hkk, diff_hkk, perc_diff_hkk) %>%
  mutate(variable = 'hkk')
## Vo2max
vo2_stats <- sumstats(m1_vo2, m2_vo2, diff_vo2, perc_diff_vo2) %>%
  mutate(variable = 'V02max')
## MCS
mcs_stats <- sumstats(MCS_m1, MCS_m2, diff_mcs, perc_diff_mcs) %>%
  mutate(variable = 'mcs')
## PCS
pcs_stats <- sumstats(PCS_m1, PCS_m2, diff_pcs, perc_diff_pcs) %>%
  mutate(variable = 'pcs')
# Combine summary observations into 1 table
summary_stats <- bind_rows(bmi_stats, rm_stats, fat_stats, hkk_stats, vo2_stats, mcs_stats, pcs_stats)


## Check normality of difference
sd %>%
  plot_normality(diff_bmi)
check_normality(sd$diff_bmi)

## Paired t-test
ggwithinstats(
  data = bmi,
  x    = measurement, 
  y    = score, 
  type = "parametric" #using ggstatsplot
)

t.test(
  bmi$score[bmi$measurement == "m1_bmi"],
  bmi$score[bmi$measurement == "m2_bmi"],
  paired = TRUE
) #Using Base R

# Statistically test difference between m1 and m2 (fat)
## Check normality of difference
sd %>%
  plot_normality(diff_fat)
check_normality(sd$diff_fat)

## Wilcoxon signed rank test
ggwithinstats(
  data = fat,
  x    = measurement, 
  y    = score, 
  type = "nonparametric" #using ggstatsplot
)

wilcox.test(
  bc_fat$score[fat$measurement == "m1_bia_perc_fat"],
  bc_fat$score[fat$measurement == "m2_bia_perc_fat"],
  paired = TRUE
)

# Statistically test difference between m1 and m2 (hkk)
## Check normality of difference

sd %>%
  plot_normality(diff_hkk)
check_normality(sd$diff_hkk)

## Check outliers 
check_outliers(sd$diff_hkk)
outliers_hkk <- ggplot(sd, aes(m1_hkk, m2_hkk, group = study_id)) +
  geom_point()
ggplotly(outliers_hkk)

## Wilcoxon signed rank test
ggwithinstats(
  data = hkk,
  x    = measurement, 
  y    = score, 
  type = "nonparametric" #using ggstatsplot
)

wilcox.test(
  hkk$score[hkk$measurement == "m1_hkk"],
  hkk$score[hkk$measurement == "m2_hkk"],
  paired = TRUE
)

# Statistically test difference between m1 and m2 (1rm)
## Check normality of difference
sd %>%
  plot_normality(diff_1rm)
check_normality(sd$diff_1rm)

## Check outliers 
check_outliers(sd$diff_1rm)
outliers_1rm <- ggplot(sd, aes(m1_1rm_calc, m2_1rm_calc, group = study_id)) +
  geom_point()
ggplotly(outliers_1rm)


## Paired t-test
ggwithinstats(
  data = rm,
  x    = measurement, 
  y    = score, 
  type = "parametric" #using ggstatsplot
)

t.test(
  rm$score[rm$measurement == "m1_1rm_calc"],
  rm$score[rm$measurement == "m2_1rm_calc"],
  paired = TRUE
) #Using Base R

## Testing linear mixed effects models

model_1rm <- lmer(score ~ measurement + (1 | id), bc_1rm) #keep all observations (so differs from paired t test)
summary(model_1rm)

bc_1rm_complete <- bc_1rm %>%
  group_by(id) %>%
  filter(!any(is.na(score))) #remove incomplete pairs (long format) 
View(bc_1rm_complete)

model_1rm_complete <- lmer(score ~ measurement + (1 | id), bc_1rm_complete) # basic model
model_1rm_conf <- lmer(score ~ measurement + age +(1 | id), bc_1rm_complete) # model corrected for confounders
tab_model(model_1rm_complete, model_1rm_conf)
summary(model_1rm_complete) #provides same results as paired t test!

library(lmerTest)
initial_model <- lmer(score ~ measurement + age + hads_score + predis_score +
                        baseline_move + baseline_sport +
                        smoking + baseline_alc + (1|id), data = bc_1rm_complete) #check missings
tab_model(initial_model)

# Perform stepwise elimination based on AIC
final_model <- step(initial_model, direction = "backward")
final_model <- lmer(score ~ measurement +
                      baseline_move + (1|id), data = bc_1rm_complete) #patient 1729 removed due to missing baseline_move
tab_model(final_model)

# Statistically test difference between m1 and m2 (vo2)
## Check normality of difference
sd %>%
  plot_normality(diff_vo2)
check_normality(sd$diff_vo2)

## Check outliers
check_outliers(sd$diff_vo2)
outliers_vo2 <- ggplot(sd, aes(m1_vo2, m2_vo2, group = study_id)) +
  geom_point()
ggplotly(outliers_vo2)

## Paired t-test
ggwithinstats(
  data = vo2,
  x    = measurement, 
  y    = score, 
  type = "parametric" #using ggstatsplot
)

t.test(
  vo2$score[vo2$measurement == "m1_vo2"],
  vo2$score[vo2$measurement == "m2_vo2"],
  paired = TRUE
) #Using Base R

# Statistically test difference between m1 and m2 (pcs)
## Check normality of difference
sd %>%
  plot_normality(diff_pcs)
check_normality(sd$diff_pcs)

## Paired t-test
ggwithinstats(
  data = pcs,
  x    = measurement, 
  y    = score, 
  type = "parametric" #using ggstatsplot
)

t.test(
  pcs$score[pcs$measurement == "PCS_m1"],
  pcs$score[pcs$measurement == "PCS_m2"],
  paired = TRUE
) #Using Base R

# Statistically test difference between m1 and m2 (mcs)
## Check normality of difference
sd %>%
  plot_normality(diff_mcs)
check_normality(sd$diff_mcs)

## Paired t-test
ggwithinstats(
  data = mcs,
  x    = measurement, 
  y    = score, 
  type = "parametric" #using ggstatsplot
)

t.test(
  mcs$score[mcs$measurement == "MCS_m1"],
  mcs$score[mcs$measurement == "MCS_m2"],
  paired = TRUE
) #Using Base R

#TO Do
## check outliers

# Descriptive statistics regarding smoking and alcohol
toxic_data <- sd %>%
  select(study_id, smoking, smoking_count, baseline_alc, baseline_alc_amount,
         pre_surgery_smoking, pre_surgery_smoking_amount, pre_surgery_smr,
         pre_surgery_alc, pre_surgery_alc_amount) %>%
  mutate(m1_alc_num = as.numeric(as.character(
    fct_recode(baseline_alc,
               '0' = 'No',
               '1' = 'Yes'))),
    m1_smoking_num = as.numeric(as.character(
      fct_recode(smoking,
                 '0' = 'No',
                 '1' = 'Yes',
                 "99" = 'Quit'))),
    m2_alc_num = as.numeric(as.character(
      fct_recode(pre_surgery_alc,
                 '0' = 'No',
                 '1' = 'Yes'))),
    m2_smoking_num = as.numeric(as.character(
        fct_recode(pre_surgery_smoking,
                   '0' = 'No',
                   '1' = 'Yes'))),
    diff_alc = m2_alc_num - m1_alc_num,
    diff_smoking = m2_smoking_num - m1_smoking_num,
    alc_amount_diff = pre_surgery_alc_amount - baseline_alc_amount)
           
# Check if patients stopped smoking and drinking

table(toxic_data$diff_alc) #-1 indicates how many patients stopped drinking
table(toxic_data$diff_smoking) #-1 indicates how many patients stopped smoking --> check m2_smoking question

tabyl(toxic_data$diff_alc) %>%
  adorn_totals('row') %>% #add row totals
  adorn_pct_formatting() #format percentages to 1 decimal

tabyl(toxic_data$m1_alc_num) %>%
  adorn_totals('row') %>% #add row totals
  adorn_pct_formatting()#format percentages to 1 decimal

tabyl(toxic_data$m2_alc_num) %>%
  adorn_totals('row') %>% #add row totals
  adorn_pct_formatting()#format percentages to 1 decimal

toxic_data %>% 
  group_by(baseline_alc) %>%
  summarize(perc_changed_drinking = 100*mean(diff_alc, na.rm = TRUE),
            count = n()) #mean indicates proportion of patients that quit drinking (among drinkers)

toxic_data %>% 
  group_by(smoking) %>%
  summarize(perc_changed_smoking = 100*mean(diff_smoking, na.rm = TRUE),
            count = n())

# Check alcohol amount 

## change that not drinking alcohol equals 0 instead of NA
toxic_data$baseline_alc_amount <- ifelse(
  toxic_data$m1_alc_num == 0, 0, toxic_data$baseline_alc_amount)

tabyl(toxic_data$alc_amount_diff)
tabyl(toxic_data$baseline_alc_amount)
tabyl(toxic_data$pre_surgery_alc_amount)

toxic_data %>%
  group_by(baseline_alc) %>%
  summarise(changed_drinking = mean(alc_amount_diff, na.rm = TRUE),
            count = n())



