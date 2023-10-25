# Secondary analyses 

# install packages
#install.packages('ggstatsplot')
#install.packages('report')

# Load packages
library(tidyverse)
library(naniar)
library(tableone)
library(flextable)
library(sjPlot)
library(readxl)
library(hablar)
library(dlookr)
library(SmartEDA)
library(janitor)
library(DataExplorer)
library(performance)
library(forecast)
library(compareGroups)
library(skimr)
library(gt)
library(ggstatsplot)
library(report)

# set wd
setwd("C:/Users/Elke van Daal/Documents/Radboud UMC/Fit4Society")
getwd()

# Run datacleaning codes from other R-script (File for codebook)
source('C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Source cleaning and codebook.R') #full_data from testroom

# Create V02max column in ml/min/kg
full_data <- full_data %>%
  mutate(m1_vo2 = (m1_sr_vo2 * 1000)/m1_weight,
        m2_vo2 = (m2_sr_vo2 * 1000)/m2_weight)
  
# Filter data for cluster BC and intervention group
full_data_bc  <- full_data %>% filter(surgery_type == 'Breast', group == 'Intervention')
glimpse(full_data_bc)
View(full_data)

# Only select columns required for secondary analyses
glimpse(bc_data)
bc_data <- full_data_bc %>% select(id, m1_length, m1_weight, m1_bmi, m1_bia_perc_fat,
                                m1_hkk, m1_sr_vo2, m1_1rm_calc, m2_length, m2_weight, m2_bmi, 
                                m2_bia_perc_fat, m2_hkk, m2_sr_vo2, m2_1rm_calc, m1_vo2, m2_vo2,
                                smoking, smoking_count, baseline_alc, baseline_alc_amount,
                                pre_surgery_alc, pre_surgery_alc_amount, pre_surgery_smoking,
                                pre_surgery_smoking_amount, pre_surgery_smr)
bc_data$m1_bia_perc_fat <- as.numeric(bc_data$m1_bia_perc_fat)
bc_data$m2_bia_perc_fat <- as.numeric(bc_data$m2_bia_perc_fat)

View(full_data_bc)

# Calculate difference between m1 and m2
bc <- bc_data %>% mutate(diff_length = m2_length - m1_length,
                     diff_weight = m2_weight - m1_weight,
                     diff_bmi = m2_bmi - m1_bmi,
                     diff_fat = m2_bia_perc_fat - m1_bia_perc_fat,
                     diff_hkk = m2_hkk - m1_hkk, 
                     diff_1rm = m2_1rm_calc - m1_1rm_calc,
                     diff_v02_sr = m2_sr_vo2 - m1_sr_vo2,
                     diff_vo2 = m2_vo2 - m1_vo2)

# From wide to long format and sort
glimpse(bc_data_2)
bc_long <- bc_data %>%
pivot_longer(cols = !id, names_to = 'measurement', values_to = 'score') %>%
  arrange(measurement)
View(bc_long)

# Make different dataframes per parameter
bc_bmi <- bc_long %>% filter(measurement == 'm1_bmi' | measurement == 'm2_bmi')
bc_fat <- bc_long %>% filter(measurement == 'm1_bia_perc_fat' | measurement == 'm2_bia_perc_fat')
bc_hkk <- bc_long %>% filter(measurement == 'm1_hkk' | measurement == 'm2_hkk')
bc_1rm <- bc_long %>% filter(measurement == 'm1_1rm_calc' | measurement == 'm2_1rm_calc')
bc_vo2 <- bc_long %>% filter(measurement == 'm1_vo2' | measurement == 'm2_vo2')

# Statistically test difference between m1 and m2 (BMI)

## Check normality of difference
bc %>%
  plot_normality(diff_bmi)
check_normality(bc$diff_bmi)

## Paired t-test
ggwithinstats(
  data = bc_bmi,
  x    = measurement, 
  y    = score, 
  type = "parametric" #using ggstatsplot
)

t.test(
  bc_bmi$score[bc_bmi$measurement == "m1_bmi"],
  bc_bmi$score[bc_bmi$measurement == "m2_bmi"],
  paired = TRUE
) #Using Base R

# Statistically test difference between m1 and m2 (fat)
## Check normality of difference
bc %>%
  plot_normality(diff_fat)
check_normality(bc$diff_fat)

## Wilcoxon signed rank test
ggwithinstats(
  data = bc_fat,
  x    = measurement, 
  y    = score, 
  type = "nonparametric" #using ggstatsplot
)

wilcox.test(
  bc_fat$score[bc_fat$measurement == "m1_bia_perc_fat"],
  bc_fat$score[bc_fat$measurement == "m2_bia_perc_fat"],
  paired = TRUE
)

# Statistically test difference between m1 and m2 (hkk)
## Check normality of difference

bc %>%
  plot_normality(diff_hkk)
check_normality(bc$diff_hkk)

## Wilcoxon signed rank test
ggwithinstats(
  data = bc_hkk,
  x    = measurement, 
  y    = score, 
  type = "nonparametric" #using ggstatsplot
)

wilcox.test(
  bc_hkk$score[bc_hkk$measurement == "m1_hkk"],
  bc_hkk$score[bc_hkk$measurement == "m2_hkk"],
  paired = TRUE
)

# Statistically test difference between m1 and m2 (1rm)
## Check normality of difference
bc %>%
  plot_normality(diff_1rm)
check_normality(bc$diff_1rm)

## Paired t-test
ggwithinstats(
  data = bc_1rm,
  x    = measurement, 
  y    = score, 
  type = "parametric" #using ggstatsplot
)

t.test(
  bc_1rm$score[bc_1rm$measurement == "m1_1rm_calc"],
  bc_1rm$score[bc_1rm$measurement == "m2_1rm_calc"],
  paired = TRUE
) #Using Base R


# Statistically test difference between m1 and m2 (vo2)
## Check normality of difference
bc %>%
  plot_normality(diff_vo2)
check_normality(bc$diff_vo2)

## Paired t-test
ggwithinstats(
  data = bc_vo2,
  x    = measurement, 
  y    = score, 
  type = "parametric" #using ggstatsplot
)

t.test(
  bc_vo2$score[bc_vo2$measurement == "m1_vo2"],
  bc_vo2$score[bc_vo2$measurement == "m2_vo2"],
  paired = TRUE
) #Using Base R

#TO Do
## check outliers

# Descriptive statistics regarding smoking and alcohol
toxic_data <- bc_data %>%
  select(id, smoking, smoking_count, baseline_alc, baseline_alc_amount,
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

# Check alcohol amount --> change that not drinking alcohol equals 0 instead of NA
tabyl(toxic_data$alc_amount_diff)
tabyl(toxic_data$baseline_alc_amount)
tabyl(toxic_data$pre_surgery_alc_amount)


