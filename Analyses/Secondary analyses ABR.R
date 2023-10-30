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

# load data
load(file = "C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\testroom_data_abr.RData")

# Create V02max column in ml/min/kg
bc_data <- bc_data %>%
  mutate(m1_vo2 = (m1_sr_vo2 * 1000)/m1_weight,
        m2_vo2 = (m2_sr_vo2 * 1000)/m2_weight) %>%
  filter(group == 'Intervention') #filter for intervention group only

# Only select columns required for secondary analyses
bc_data <- bc_data %>% select(id, age, hads_score,predis_score, baseline_move,
                                   baseline_sport, f4s_training_amount,
                                   m1_length, m1_weight, m1_bmi, m1_bia_perc_fat,
                                m1_hkk, m1_sr_vo2, m1_1rm_calc, m2_length, m2_weight, m2_bmi, 
                                m2_bia_perc_fat, m2_hkk, m2_sr_vo2, m2_1rm_calc, m1_vo2, m2_vo2,
                                smoking, smoking_count, baseline_alc, baseline_alc_amount,
                                pre_surgery_alc, pre_surgery_alc_amount, pre_surgery_smoking,
                                pre_surgery_smoking_amount, pre_surgery_smr)
bc_data$m1_bia_perc_fat <- as.numeric(bc_data$m1_bia_perc_fat)
bc_data$m2_bia_perc_fat <- as.numeric(bc_data$m2_bia_perc_fat)

# Calculate difference between m1 and m2
bc <- bc_data %>% mutate(diff_length = m2_length - m1_length,
                     diff_weight = m2_weight - m1_weight,
                     diff_bmi = m2_bmi - m1_bmi,
                     diff_fat = m2_bia_perc_fat - m1_bia_perc_fat,
                     diff_hkk = m2_hkk - m1_hkk, 
                     diff_1rm = m2_1rm_calc - m1_1rm_calc,
                     diff_vo2_sr = m2_sr_vo2 - m1_sr_vo2,
                     diff_vo2 = m2_vo2 - m1_vo2,
                     perc_diff_weight = ((m2_weight - m1_weight)/m1_weight) * 100,
                     perc_diff_bmi = ((m2_bmi - m1_bmi)/m1_bmi) * 100,
                     perc_diff_fat = ((m2_bia_perc_fat - m1_bia_perc_fat)/m1_bia_perc_fat) * 100,
                     perc_diff_hkk = ((m2_hkk - m1_hkk)/m1_hkk) * 100,
                     perc_diff_1rm = ((m2_1rm_calc - m1_1rm_calc)/m1_1rm_calc) * 100,
                     perc_diff_vo2_sr = ((m2_sr_vo2 - m1_sr_vo2)/m1_sr_vo2) * 100,
                     perc_diff_vo2 = ((m2_vo2 - m1_vo2)/m1_vo2) * 100)

# Count number of patients who have clinically meaningful differences above 10%
cmd_rm <- bc %>% count(perc_diff_1rm >= 10) %>% #count number of people that achieve cmd
  filter(complete.cases(.)) %>% #only keep patients with 2 measurements
  mutate(percentage = (n / sum(n)) *100) #add percentage of people that achieve cmd
cmd_hkk <- bc %>% count(perc_diff_hkk >= 10) %>%
  filter(complete.cases(.)) %>%
  mutate(percentage = (n / sum(n)) * 100)
cmd_vo2 <- bc %>% count(perc_diff_vo2 >= 10) %>%
  filter(complete.cases(.)) %>%
  mutate(percentage = (n / sum(n)) * 100)

# From wide to long format and sort
bc_long <- bc %>% select(-baseline_alc, -baseline_alc_amount, -contains('smoking'),
                              -pre_surgery_smr, -pre_surgery_alc, -pre_surgery_alc_amount) %>%
pivot_longer(cols = !id, names_to = 'measurement', values_to = 'score') %>%
  arrange(measurement)

# Same but now including baseline variables (potential confounders)
bc_long2 <- bc_data %>% 
  pivot_longer(cols = -c(id,age, hads_score,predis_score, baseline_move, baseline_alc, baseline_alc_amount,
                         baseline_sport, f4s_training_amount,f4s_protein_days, smoking, smoking_count, 
                         pre_surgery_smoking, pre_surgery_smoking_amount, pre_surgery_smr, 
                         pre_surgery_alc, pre_surgery_alc_amount),
                         names_to = 'measurement', values_to = 'score') %>%
  arrange(measurement)

# Make different dataframes per parameter
bc_bmi <- bc_long %>% filter(measurement == 'm1_bmi' | measurement == 'm2_bmi')
bc_fat <- bc_long %>% filter(measurement == 'm1_bia_perc_fat' | measurement == 'm2_bia_perc_fat')
bc_hkk <- bc_long %>% filter(measurement == 'm1_hkk' | measurement == 'm2_hkk')
bc_1rm <- bc_long2 %>% filter(measurement == 'm1_1rm_calc' | measurement == 'm2_1rm_calc')
bc_vo2 <- bc_long %>% filter(measurement == 'm1_vo2' | measurement == 'm2_vo2')
View(bc_1rm)

# Function to generate summary statistics
sumstats <- function(m1_var, m2_var, diff_var, perc_diff_var) {
  m1_var = enquo(arg = m1_var)
  m2_var = enquo(arg = m2_var)
  diff_var = enquo(arg = diff_var)
  perc_diff_var = enquo(arg = perc_diff_var)
  bc %>% select(id, !!m1_var, !!m2_var, !!diff_var, !!perc_diff_var) %>%
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

# Combine summary observations into 1 table
summary_stats <- bind_rows(bmi_stats, rm_stats, fat_stats, hkk_stats, vo2_stats)

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



