#Name: Elke van Daal
#Project: Fit4Society - Cleaning, Detecting out-of-range values
#Start Date: 17-4-2023

#Load packages
library(tidyverse)
library(readxl)
library(hablar)
library(forcats)
library(stringr)
library(naniar)
library(writexl)

#Open Excel export and save as full_data
full_data <- read_excel("")

#Rename columns with strings 
full_data <- full_data %>% rename(ID = 'Participant Id',
                                  ID_status = 'Participant Status',
                                  Site = 'Site Abbreviation',
                                  Start_date = 'Participant Creation Date',
                                  Surgery_gyn = '7.5|Operatie van afdeling gynaecologie?')

#change data type of full_data
full_data <- full_data %>% retype()

#Remove 'F4S_' in ID, and change to integers
full_data <- full_data %>% mutate(ID = as.integer(str_remove(ID, 'F4S_')))

#Change categorical variables from integers to factors and name levels, 
##Variable incl_groep
full_data <- full_data %>% mutate(incl_groep = as.factor(as.character(incl_groep))) %>%
  mutate(incl_groep = fct_recode(incl_groep, 'Control' = '0', 'Intervention' = '1')) %>%
  rename(Group = incl_groep)
#Variable M1_gesl
full_data <- full_data %>% mutate(M1_gesl = as.factor(as.character(M1_gesl))) %>%
  mutate(M1_gesl = fct_recode(M1_gesl, 'Male' = '1', 'Female' = '2')) %>%
  rename(Sex = M1_gesl)
#Variable M1_roken
full_data <- full_data %>% mutate(M1_roken = as.factor(as.character(M1_roken))) %>%
  mutate(M1_roken = fct_recode(M1_roken, 'No' = '0', 'Yes' = '1', 'Quit' = '2')) %>%
  rename(Smoking = M1_roken)

#Variable M1_fietstest
full_data <- full_data %>% mutate(M1_fietstest = as.factor(as.character(M1_fietstest))) %>%
  mutate(M1_fiestest = fct_recode(M1_fietstest, 'Steep Ramp' = '1', 'Astrand-Rhyming' = '2'))

#Variable M2_fietstest
full_data <- full_data %>% mutate(M2_fietstest = as.factor(as.character(M2_fietstest))) %>%
  mutate(M2_fiestest = fct_recode(M2_fietstest, 'Steep Ramp' = '1', 'Astrand-Rhyming' = '2'))

#Select first 1000 patients
df_1000 <- full_data %>% filter(ID >= 1000 & ID <= 1500)

#get column numbers of columns to be removed
match('Hemoglobin_Preoperative_Anesthesiology', names(df_1000))
match('Mortality_30_Days', names(df_1000))

#Remove irrelevant columns from df_1000, name it df_select
df_select <- df_1000 %>%
  select(-ID_status, -Site, -Start_date, -incl_IC_versie,
         -Testkamer_nummer_MM1, -M1_BMI_calc, -M1_BIA_afsthand, -M1_BIA_afstvoet, -M1_BIA_reactance,
         -M1_BIA_resistance, -M1_BIA_fh, -M1_BIA_vvm, -M1_HKK_stand, -M1_HKK_kant, -M1_PGSGA_calc,
         -M1_1RM_stand, -M1_1RM_calc, -starts_with('scr_G8'), -scr_ger, -PREDIS_uitkomst, -Verwijzing_Fysiotherapeut_2elijn,
         -Eindmeting_locatie, -Testkamer_nummer_MM2, -Surgery_gyn, -Afspraak_MM3,
         -M2_BMI_calc, -M2_BIA_afsthand, -M2_BIA_afstvoet, -M2_BIA_reactance,
         -M2_BIA_resistance, -M2_BIA_fh, -M2_BIA_vvm, -M2_HKK_kant, -M2_HKK_stand, -M2_PGSGA_calc,
         -M2_1RM_calc, -M2_1RM_stand, -gg_voorpreop_dieet, -gg_voorpreop_dietist, -gg_preop_dieet, -gg_preop_dietist,
         -Testkamer_nummer_MM3, -postop_tijd_calc, -M3_BMI_calc, -M3_BIA_reactance,
         -M3_BIA_resistance, -M3_BIA_fh, -M3_BIA_vvm, -M3_HKK_kant, -M3_PGSGA_calc,
         -M3_1RM_calc, -VB_MM3_Dieet_1, -VB_MM3_Dietist_1, -contains('Intention'), -contains('datum'),
         -contains('supp'), -contains('#'), -contains('date'), -contains('reason'), -contains('toelichting'),
         -F4S_SineFuma, -F4S_SineFuma_nee, -F4S_psych, -F4S_psych_x, -VB_MM3_Supplementen_1, -VB_MM3_Supplementen_Specificatie_1,
         -starts_with('Afspraak'), -Vragenlijsten_MM4_MM5, -Follow_Up_Moment, -Reason_Lost_to_Follow_Up,
         -F4S_car, -F4S_long, -F4S_ger, -Versie_IC_2, -(183:212), -Neoadjuvant_Therapy, -VB_1_6_1_1)

#write function to detect incorrect categories
incorrect_category <- function(target_variable) {
  target_variable = enquo(arg = target_variable)
  df_select %>%
    select(ID, !!target_variable) %>%
    filter(!!target_variable != 1)
}

#write function to detect out-of-range values
fail_range <- function(target_variable, min, max) {
  target_variable = enquo(arg = target_variable)
  df_select %>% 
    select(ID, !!target_variable) %>% 
    filter(!!target_variable <= min | !!target_variable >= max | is.na(!!target_variable))
}

#write function for variables that only account for intervention group
fail_int <- function(target_variable, min, max) {
  target_variable = enquo(arg = target_variable)
  df_select %>% filter(Group == 'Intervention') %>%
    select(ID, !!target_variable) %>%
    filter(!!target_variable <= min | !!target_variable >= max | is.na(!!target_variable))
}

#write function for variables that only account for control group
fail_con <- function(target_variable, min, max) {
  target_variable = enquo(arg = target_variable)
  df_select %>% filter(Group == 'Control') %>%
    select(ID, !!target_variable) %>%
    filter(!!target_variable <= min | !!target_variable >= max | is.na(!!target_variable))
}

#write function met general voorwaarde
fail_general <- function(target_variable, Grouping, Value, min, max) {
  target_variable = enquo(arg = target_variable)
  Grouping = enquo(arg = Grouping)
  df_select %>% filter(!!Grouping == Value) %>%
    select(ID, !!target_variable) %>%
    filter(!!target_variable <= min | !!target_variable >= max | is.na(!!target_variable))
}

#detect false values
#inclusievariabelen
age_16_fail <- incorrect_category(incl_16)
OK_fail <- incorrect_category(incl_OK)
OK_miss <- df_select %>% 
  select(ID, incl_OK_welke) %>%
  filter(is.na(incl_OK_welke))
IC_fail <- incorrect_category(incl_IC)
Group_fail <- df_select %>% 
  select(ID, Group) %>%
  filter(Group != 'Intervention' & Group != 'Control')

#patientkarakteristieken
age_fail <- fail_range(M1_jaar, 15, 95)
sex_fail <- df_select %>% 
  select(ID, Sex) %>% 
  filter(Sex != 'Male' & Sex != 'Female')
smoking_fail <- df_select %>% 
  select(ID, Smoking) %>% 
  filter(Smoking != 'Yes' & Smoking != 'No' & Smoking != 'Quit')
smoking_time_fail <- df_select %>% 
  filter(Smoking != 'No') %>% 
  select(ID, M1_rokentijd) %>% 
  filter(!M1_rokentijd %in% 0.0:75.0)
smoking_count_fail <- df_select %>% 
  filter(Smoking != 'No') %>% 
  select(ID, M1_rokenaantal) %>% 
  filter(!M1_rokenaantal %in% 0:60)

#MM1 
M1_length_fail <- fail_range(M1_lengte, 1.40, 2.10)
M1_weight_fail <- fail_range(M1_gewicht, 35, 150)
M1_fat_fail <- fail_range(M1_BIA_perc_calc, 5, 60)
M1_HKK_fail <- fail_range(M1_HKK, 10,75)
M1_PGSGA1_fail <- fail_range(M1_PGSGA1, -1, 2)
M1_PGSGA2_fail <- fail_range(M1_PGSGA2, -1, 5)
M1_PGSGA3_fail <- fail_range(M1_PGSGA3, -1, 21)
M1_PGSGA4_fail <- fail_range(M1_PGSGA4, -1, 4)
M1_HFR_fail <- fail_range(M1_HFrest, 40, 120)
M1_AR_W_fail <- df_select %>% 
  filter(M1_fiestest == 'Astrand-Rhyming') %>%
  select(ID, M1_AR_W) %>%
  filter(M1_AR_W <= 50 | M1_AR_W >= 200 | is.na(M1_AR_W))
M1_AR_HF_fail <- df_select %>% 
  filter(M1_fiestest == 'Astrand-Rhyming') %>%
  select(ID, M1_AR_HF) %>%
  filter(M1_AR_HF <= 70 | M1_AR_HF >= 200 | is.na(M1_AR_HF))
M1_AR_VO2_fail <- df_select %>% 
  filter(M1_fiestest == 'Astrand-Rhyming') %>%
  select(ID, M1_AR_VO2) %>%
  filter(M1_AR_VO2 <= 0.4 | M1_AR_VO2 >= 6 | is.na(M1_AR_VO2))
M1_AR_Borg_fail <- df_select %>% 
  filter(M1_fiestest == 'Astrand-Rhyming') %>%
  select(ID, M1_AR_BORG) %>%
  filter(M1_AR_BORG <= 5 | M1_AR_BORG >= 21 | is.na(M1_AR_BORG))
M1_SR_W_fail <- df_select %>%
  filter(M1_fiestest == 'Steep Ramp') %>%
  select(ID, M1_SR_W) %>%
  filter(M1_SR_W <= 25 | M1_SR_W >= 500 | is.na(M1_SR_W))
M1_SR_HF_fail <- df_select %>%
  filter(M1_fiestest == 'Steep Ramp') %>%
  select(ID, M1_SR_HF) %>%
  filter(M1_SR_HF <= 70 | M1_SR_HF >= 200 | is.na(M1_SR_HF))
M1_SR_VO2_fail <- df_select %>%
  filter(M1_fiestest == 'Steep Ramp') %>%
  select(ID, M1_SR_VO2) %>%
  filter(M1_SR_VO2 <= 0.4 | M1_SR_VO2 >= 6 | is.na(M1_SR_VO2))
M1_SR_Borg_fail <- df_select %>%
  filter(M1_fiestest == 'Steep Ramp') %>%
  select(ID, M1_SR_BORG) %>%
  filter(M1_SR_BORG <= 5 | M1_SR_BORG >= 21 | is.na(M1_SR_BORG))
M1_RM_W_fail <- fail_range(M1_1RM_W, 5, 200)
M1_RM_r_fail <- fail_range(M1_1RM_r, 0, 25)
M1_5CST_fail <- fail_range(M1_5CST, 3, 30)
M1_5CSTp_fail <- fail_range(M1_5CSTp, -1, 5)

#Screening F4S (only intervention group!)
scr_hb_fail <- fail_int(scr_hb, 3, 12)
scr_gluc_fail <- fail_int(scr_gluc, 1, 21)
scr_ACSM_fail <- fail_int(scr_ACSM, -1, 4)
HADS_fail <- fail_int(HADS_uitkomst, -1, 70)
SineFuma_fail <- df_select %>%
  filter(Smoking == 'Yes') %>%
  select(ID, int_SineFuma) %>%
  filter(int_SineFuma < 0 | int_SineFuma > 1 | is.na(int_SineFuma))

#MM2
preop_tijd_calc_fail <- fail_range(preop_tijd_calc, 5, 91)
M2_length_fail <- fail_range(M2_lengte, 1.40, 2.10)
M2_weight_fail <- fail_range(M2_gewicht, 35, 150)
M2_fat_fail <- fail_range(M2_BIA_perc_calc, 5, 60)
M2_HKK_fail <- fail_range(M2_HKK, 10,75)
M2_PGSGA_fail <- fail_range(M2_PGSGA1, -1, 2)
M2_PGSGA2_fail <- fail_range(M2_PGSGA2, -1, 5)
M2_PGSGA3_fail <- fail_range(M2_PGSGA3, -1, 21)
M2_PGSGA4_fail <- fail_range(M2_PGSGA4, -1, 4)
M2_HFR_fail <- fail_range(M2_HFrest, 40, 120)
M2_AR_W_fail <- df_select %>% 
  filter(M2_fiestest == 'Astrand-Rhyming') %>%
  select(ID, M2_AR_W) %>%
  filter(M2_AR_W <= 50 | M2_AR_W >= 200 | is.na(M2_AR_W))
M2_AR_HF_fail <- df_select %>% 
  filter(M2_fiestest == 'Astrand-Rhyming') %>%
  select(ID, M2_AR_HF) %>%
  filter(M2_AR_HF <= 70 | M2_AR_HF >= 200 | is.na(M2_AR_HF))
M2_AR_VO2_fail <- df_select %>% 
  filter(M2_fiestest == 'Astrand-Rhyming') %>%
  select(ID, M2_AR_VO2) %>%
  filter(M2_AR_VO2 <= 0.4 | M2_AR_VO2 >= 6 | is.na(M2_AR_VO2))
M2_AR_Borg_fail <- df_select %>% 
  filter(M2_fiestest == 'Astrand-Rhyming') %>%
  select(ID, M2_AR_BORG) %>%
  filter(M2_AR_BORG <= 5 | M2_AR_BORG >= 21 | is.na(M2_AR_BORG))
M2_SR_W_fail <- df_select %>%
  filter(M2_fiestest == 'Steep Ramp') %>%
  select(ID, M2_SR_W) %>%
  filter(M2_SR_W <= 25 | M2_SR_W >= 500 | is.na(M2_SR_W))
M2_SR_HF_fail <- df_select %>%
  filter(M2_fiestest == 'Steep Ramp') %>%
  select(ID, M2_SR_HF) %>%
  filter(M2_SR_HF <= 70 | M2_SR_HF >= 200 | is.na(M2_SR_HF))
M2_SR_VO2_fail <- df_select %>%
  filter(M2_fiestest == 'Steep Ramp') %>%
  select(ID, M2_SR_VO2) %>%
  filter(M2_SR_VO2 <= 0.4 | M2_SR_VO2 >= 6 | is.na(M2_SR_VO2))
M2_SR_Borg_fail <- df_select %>%
  filter(M2_fiestest == 'Steep Ramp') %>%
  select(ID, M2_SR_BORG) %>%
  filter(M2_SR_BORG <= 5 | M2_SR_BORG >= 21 | is.na(M2_SR_BORG))
M2_RM_W_fail <- fail_range(M2_1RM_W, 5, 200)
M2_RM_r_fail <- fail_range(M2_1RM_r, 0, 25)
M2_5CST_fail <- fail_range(M2_5CST, 4, 31)
M2_5CSTp_fail <- fail_range(M2_5CSTp, -1, 5)

#GG baseline phase
baseline_move_fail <- fail_range(gg_voorpreop_bewegen, -1, 8)
baseline_sport_fail <- fail_range(gg_voorpreop_sport, -1, 8)
baseline_alc_fail <- fail_general(gg_voorpreop_hoeveelalc, 
                                  gg_voorpreop_alc, 1, 0, 25)

#GG pre surgery phase
preop_move_fail <- fail_range(gg_preop_bewegen, -1, 8)
preop_sport_fail <- fail_con(gg_preop_sport, -1, 8)     #Only control
preop_fys_fail <- fail_int(gg_preop_fys, -1, 4)        #only intervention
preop_roken_fail <- fail_general(gg_preop_roken, Smoking, 'Yes', -1, 4)
preop_smr_fail <- fail_general(gg_preop_smr, gg_preop_roken, 1, 0, 8)
rokenaantal_fail <- df_select %>%
  filter(gg_preop_roken == 0 | gg_preop_roken == 3) %>%
  select(ID, gg_rokenaantal) %>%
  filter(gg_rokenaantal <= 0 | gg_rokenaantal >= 50 | is.na(gg_rokenaantal))
preop_alc_fail <- fail_general(gg_preop_hoeveelalc, gg_preop_alc, 1, 0, 25)

#F4s variables (only intervention)
F4S_duur_fail <- fail_int(F4S_duurtraining, 0, 50)
F4S_train_fail <- fail_int(F4S_xtrainen, -1, 30)
F4S_eiwit_fail <- fail_int(F4S_eiwit, -1, 2)
F4S_eiwitdagen_fail <- fail_general(F4S_eiwit_aantaldagen, F4S_eiwit, 0, -1, 50)
F4S_vit_fail <- fail_int(F4S_vit, -1, 2)
F4S_vitdagen_fail <- fail_general(F4S_vit_aantaldagen, F4S_vit, 0, -1, 50)

#Geen idee waar dat bij hoort
ASA_fail <- fail_range(ASA_Score, -1, 5)

#MM3
M3_adjuvant_fail <- fail_range(M3_adjuvante_therapie, -1, 4)
M3_length_fail <- fail_range(M3_lengte, 1.40, 2.10)
M3_weight_fail <- fail_range(M3_gewicht, 35, 150)
M3_fat_fail <- fail_range(M3_BIA_perc_calc, 5, 60)
M3_HKK_fail <- fail_range(M3_HKK, 10,75)
M3_PGSGA_fail <- fail_range(M3_PGSGA1, -1, 2)
M3_PGSGA2_fail <- fail_range(M3_PGSGA2, -1, 5)
M3_PGSGA3_fail <- fail_range(M3_PGSGA3, -1, 21)
M3_PGSGA4_fail <- fail_range(M3_PGSGA4, -1, 4)
M3_HFR_fail <- fail_range(M3_HFrest, 40, 120)
M3_SR_W_fail <- fail_range(M3_SR_W, 25, 500)
M3_SR_HF_fail <- fail_range(M3_SR_HF, 70, 200)
M3_SR_VO2_fail <- fail_range(M3_SR_VO2, 0.5, 6)
M3_SR_Borg_fail <- fail_range(M3_SR_BORG, 5, 21)
M3_RM_W_fail <- fail_range(M3_1RM_W, 5, 200)
M3_RM_r_fail <- fail_range(M3_1RM_r, 0, 25)
M3_5CST_fail <- fail_range(M3_5CST, 3, 30)
M3_5CSTp_fail <- fail_range(M3_5CSTp, -1, 5)

#GG post surgery
Post_move_fail <- fail_range(VB_MM3_Beweging_1, -1, 8)
Post_sport_fail <- fail_range(VB_MM3_Sport_1, -1, 8)
Post_roken_fail <- fail_general(VB_MM3_Roken_Aantal_1, VB_MM3_Roken_1, 1, 0, 30)
Post_alc_fail <- fail_general(VB_MM3_Alcohol_Aantal_1, VB_MM3_Alcohol_1, 1, 0, 30)

#list all out-of-range values into failures
failures <- list(age_16_fail, OK_fail, OK_miss, IC_fail, Group_fail,
                 age_fail, sex_fail, smoking_fail, smoking_time_fail, smoking_count_fail,
                 M1_length_fail, M1_weight_fail, M1_fat_fail, M1_HKK_fail, M1_PGSGA1_fail,
                 M1_PGSGA2_fail, M1_PGSGA3_fail, M1_PGSGA4_fail, M1_HFR_fail, M1_AR_W_fail,
                 M1_AR_HF_fail, M1_AR_VO2_fail, M1_AR_Borg_fail, M1_SR_W_fail, M1_SR_HF_fail,
                 M1_SR_VO2_fail, M1_SR_Borg_fail, M1_RM_W_fail, M1_RM_r_fail, M1_5CST_fail, M1_5CSTp_fail,
                 scr_hb_fail, scr_gluc_fail, scr_ACSM_fail, HADS_fail, SineFuma_fail,
                 preop_tijd_calc_fail, M2_length_fail, M2_weight_fail, M2_HKK_fail, M2_PGSGA_fail,
                 M2_PGSGA2_fail, M2_PGSGA3_fail, M2_PGSGA4_fail, M2_HFR_fail, M2_AR_W_fail,
                 M2_AR_HF_fail, M2_AR_VO2_fail, M2_AR_Borg_fail, M2_SR_W_fail, M2_SR_HF_fail,
                 M2_SR_VO2_fail, M2_SR_Borg_fail, M2_RM_W_fail, M2_RM_r_fail, M2_5CST_fail, M2_5CSTp_fail,
                 baseline_move_fail, baseline_sport_fail, baseline_alc_fail, preop_move_fail, 
                 preop_sport_fail, preop_fys_fail, preop_roken_fail, preop_smr_fail, rokenaantal_fail,
                 preop_alc_fail, F4S_duur_fail, F4S_train_fail, F4S_eiwit_fail, F4S_eiwitdagen_fail, 
                 F4S_vit_fail, F4S_vitdagen_fail, ASA_fail, M3_adjuvant_fail, M3_length_fail, M3_weight_fail,
                 M3_fat_fail, M3_HKK_fail, M3_PGSGA_fail, M3_PGSGA2_fail, M3_PGSGA3_fail, M3_PGSGA4_fail,
                 M3_HFR_fail, M3_SR_W_fail, M3_SR_HF_fail, M3_SR_VO2_fail, M3_SR_Borg_fail,
                 M3_RM_W_fail, M3_RM_r_fail, M3_5CST_fail, M3_5CSTp_fail, Post_move_fail, Post_sport_fail,
                 Post_roken_fail, Post_alc_fail)

#replace all unwanted NA's with value 999 
failures_2 <- lapply(failures, function(d) { d[is.na(d)] <- 999; d })

#join failures into dataframe (including NAs) and arrange by ID
failures2_df <- failures_2 %>% reduce(full_join, by = 'ID')
failures2_df <- failures2_df %>% arrange(ID)

#join failures into dataframe (excluding NAs) and arrange by ID remove empty columns and empty rows
failures_df <- failures %>% reduce(full_join, by = 'ID')
failures_df <- failures_df %>% arrange(ID)

#remove empty columns and empty rows
colSums(is.na(failures_df) | failures_df == -99 | failures_df == -96 | failures_df == -97 |
          failures_df == -98)                                                               #count number of empty rows in each column
empty_col <- colSums(is.na(failures_df) | failures_df == -99 | failures_df == -96 | failures_df == -97 |
                       failures_df == -98) == nrow(failures_df)                             #discover which columns only have empty rows
failures_df <- failures_df[ , !empty_col]                                                   #remove empty columns

#replace -96:-99 with NA
failures_df[failures_df == -99 | failures_df == -98 | failures_df == -97 | failures_df == -96 | failures_df == -95] <- NA

rowSums(is.na(failures_df))                                                                 #count number of empty values in each row
empty_row <- rowSums(is.na(failures_df)) == 63                                              #discover which rows only have empty values 
failures_df <- failures_df[!empty_row, ]                                          #delete empty rows out of failures_df

#Export failures2_df into excel file
write_xlsx(failures2_df,"xlsx")

#Export failures_df into excel file
write_xlsx(failures_df,"")

#####END#####




