# Secondary analyses 

# install packages
#install.packages('ggstatsplot')

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

# set wd
setwd("C:/Users/Elke van Daal/Documents/Radboud UMC/Fit4Society")
getwd()

# Run datacleaning codes from other R-script (File for codebook)
file.choose()
source("C:/Users/Elke van Daal/Documents/Radboud UMC/Fit4Society/File for codebook use as source 8-5.R")

# Filter data for cluster BC and intervention group
full_data_bc  <- full_data %>% filter(surgery_type == 'Breast', group == 'Intervention')
glimpse(full_data_bc)


# Only select columns required for secondary analyses
glimpse(bc_data)
bc_data_2 <- bc_data %>% select(id, m1_length, m1_weight, m1_bmi, m1_bia_perc_fat,
                                m1_hkk, m1_sr_vo2, m1_1rm_calc, m2_length, m2_weight, m2_bmi, 
                                m2_bia_perc_fat, m2_hkk, m2_sr_vo2, m2_1rm_calc)
bc_data_2$m1_bia_perc_fat <- as.numeric(bc_data_2$m1_bia_perc_fat)
bc_data_2$m2_bia_perc_fat <- as.numeric(bc_data_2$m2_bia_perc_fat)

# Calculate difference between m1 and m2
view(bc_data_2)
bc <- bc_data_2 %>% mutate(diff_length = m2_length - m1_length,
                     diff_weight = m2_weight - m1_weight,
                     diff_bmi = m2_bmi - m1_bmi,
                     diff_fat = m2_bia_perc_fat - m1_bia_perc_fat,
                     diff_hkk = m2_hkk - m1_hkk,
                     diff_1rm = m2_1rm_calc - m1_1rm_calc,
                     diff_v02_sr = m2_sr_vo2 - m1_sr_vo2)
view(bc)

# Check normality of difference
## BMI
bc %>%
  plot_normality(diff_bmi)
check_normality(bc$diff_bmi)
## Fat
bc %>%
  plot_normality(diff_fat)
check_normality(bc$diff_fat)
## HKK
bc %>%
  plot_normality(diff_hkk)
check_normality(bc$diff_hkk)
##1RM
bc %>%
  plot_normality(diff_1rm)
check_normality(bc$diff_1rm)
## V02
bc %>%
  plot_normality(diff_v02_sr)
check_normality(bc$diff_v02_sr)


# From wide to long format
glimpse(bc_data_2)
bc_long <- bc_data_2 %>%
pivot_longer(cols = !id, names_to = 'measurement', values_to = 'score')
view(bc_long)

# Make different dataframes per parameter
bc_bmi <- bc_long %>% filter(measurement == m1_bmi | m2_bmi | diff_bmi)

# Statistically test difference between m1 and m2
ggwithinstats(
  data = bc_long,
  x    = measurement, 
  y    = score, 
  type = "nonparametric"
)

