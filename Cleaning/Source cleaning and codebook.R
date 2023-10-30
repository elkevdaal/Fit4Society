#Name: Elke van Daal
#Project: Data Cleaning F4S
#Start Date: 26-4-2023
################################################################################
#Load packages
library(tidyverse)
library(hablar)
library(naniar)
library(haven)
library(readxl)

#Open Excel export and save as full_data
full_data <- read_excel("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\F4S_PREHAB_trial_excel_export_20231024031037.xlsx")

#change data type of full_data
full_data <- full_data %>% retype()

#Rename columns  
#Q: Even navragen/nakijken wat te doen met deze variabelen
full_data <- full_data %>% rename(ID = 'Participant Id',ID_status = 'Participant Status',
site = 'Site Abbreviation',start_date = 'Participant Creation Date', surgery_gyn = '7.5|Operatie van afdeling gynaecologie?',
surgery = incl_OK,age = M1_jaar,smoking_time = M1_rokentijd,smoking_count = M1_rokenaantal,fysio_ref = Verwijzing_Fysiotherapeut_2elijn,
scr_G8_need = scr_G8_janee,final_measurement_loc = Eindmeting_locatie,time_m1_m2 = preop_tijd_calc,mm3_planned = Afspraak_MM3,
baseline_move = gg_voorpreop_bewegen,baseline_sport = gg_voorpreop_sport,baseline_diet = gg_voorpreop_dieet,baseline_dietician = gg_voorpreop_dietist,
baseline_supp = gg_voorpreop_supp,baseline_alc = gg_voorpreop_alc,baseline_alc_amount = gg_voorpreop_hoeveelalc,
pre_surgery_move = gg_preop_bewegen,pre_surgery_sport = gg_preop_sport,pre_surgery_fysio = gg_preop_fys,
pre_surgery_diet = gg_preop_dieet,pre_surgery_dietician = gg_preop_dietist, pre_surgery_supp = gg_preop_supp,
pre_surgery_smoking = gg_preop_roken,pre_surgery_smr = gg_preop_smr,pre_surgery_smoking_amount = gg_rokenaantal,
pre_surgery_alc = gg_preop_alc,pre_surgery_alc_amount = gg_preop_hoeveelalc,date_fysio = Datum_Fysiotherapeut_2elijn,
F4S_training_final_date = F4S_traininglaatste_date,F4S_training_period = F4S_duurtraining, F4S_training_amount = F4S_xtrainen,
no_training_reason = NoTraining_reason,no_training_reason_expl = NoTraining_reason_toelichting,F4S_protein = F4S_eiwit,
F4S_protein_days = F4S_eiwit_aantaldagen,F4S_protein_expl = F4S_eiwittoelichting,F4S_vit_days = F4S_vit_aantaldagen,
F4S_vit_expl = F4S_vittoelichting,F4S_SineFuma_no = F4S_SineFuma_nee,adjuvant_therapy = M3_adjuvante_therapie,
time_surgery_m3 = postop_tijd_calc,proms_mm4_mm5 = Vragenlijsten_MM4_MM5,baseline_supp_multivit = 'gg_voorpreop_suppwelke#Multivitamine',
baseline_supp_vita = 'gg_voorpreop_suppwelke#Vitamine A',baseline_supp_vitb = 'gg_voorpreop_suppwelke#Vitamine B',
baseline_supp_vitc = 'gg_voorpreop_suppwelke#Vitamine C',baseline_supp_vitd = 'gg_voorpreop_suppwelke#Vitamine D',
baseline_supp_natrium = 'gg_voorpreop_suppwelke#Natrium', baseline_supp_kalium = 'gg_voorpreop_suppwelke#Kalium',
baseline_supp_magnesium = 'gg_voorpreop_suppwelke#Magnesium',baseline_supp_calcium = 'gg_voorpreop_suppwelke#Calcium',
baseline_supp_glucosamine = 'gg_voorpreop_suppwelke#Glucosamine', baseline_supp_protein = 'gg_voorpreop_suppwelke#Eiwitdrank (Fresubin, Nutridrink, etc)',
baseline_supp_omega3 = 'gg_voorpreop_suppwelke#Omega-3',baseline_supp_probiotics = 'gg_voorpreop_suppwelke#Probiotica',
baseline_supp_fiber = 'gg_voorpreop_suppwelke#Vezels',baseline_supp_other = 'gg_voorpreop_suppwelke#Anders',
baseline_supp_other_expl = gg_voorpreop_suppanders,pre_surgery_supp_multivit = 'gg_preop_suppwelke#Multivitamine',
pre_surgery_supp_vita = 'gg_preop_suppwelke#Vitamine A',pre_surgery_supp_vitb = 'gg_preop_suppwelke#Vitamine B',
pre_surgery_supp_vitc = 'gg_preop_suppwelke#Vitamine C',pre_surgery_supp_vitd = 'gg_preop_suppwelke#Vitamine D',
pre_surgery_supp_natrium = 'gg_preop_suppwelke#Natrium',pre_surgery_supp_kalium = 'gg_preop_suppwelke#Kalium',
pre_surgery_supp_magnesium = 'gg_preop_suppwelke#Magnesium',pre_surgery_supp_calcium = 'gg_preop_suppwelke#Calcium',
pre_surgery_supp_glucosamine = 'gg_preop_suppwelke#Glucosamine',pre_surgery_supp_protein = 'gg_preop_suppwelke#Eiwitdrank (Fresubin, Nutridrink, etc)',
pre_surgery_supp_omega3 = 'gg_preop_suppwelke#Omega-3',pre_surgery_supp_probiotics = 'gg_preop_suppwelke#Probiotica',
pre_surgery_supp_fiber = 'gg_preop_suppwelke#Vezels',pre_surgery_supp_other = 'gg_preop_suppwelke#Anders',
pre_surgery_supp_other_expl = gg_preop_suppanders,post_surgery_move = VB_MM3_Beweging_1,post_surgery_sport = VB_MM3_Sport_1,
post_surgery_diet = VB_MM3_Dieet_1,post_surgery_dietician = VB_MM3_Dietist_1,post_surgery_supp = VB_MM3_Supplementen_1,
post_surgery_supp_spec = VB_MM3_Supplementen_Specificatie_1,post_surgery_supp_other_expl = VB_1_6_1_1,
post_surgery_smoking = VB_MM3_Roken_1,post_surgery_smoking_amount = VB_MM3_Roken_Aantal_1,
post_surgery_alc = VB_MM3_Alcohol_1,post_surgery_alc_amount = VB_MM3_Alcohol_Aantal_1,appoint_MM3 = Afspraak_Meetmoment3)

#replace dutch string parts for english strings
names(full_data) <- gsub(names(full_data), pattern = "lengte", replacement = "length")  
names(full_data) <- gsub(names(full_data), pattern = "gewicht", replacement = "weight")  
names(full_data) <- gsub(names(full_data), pattern = "datum", replacement = "date") 
names(full_data) <- gsub(names(full_data), pattern = "uitkomst", replacement = "score") 
names(full_data) <- gsub(names(full_data), pattern = "Testkamer_nummer_MM", replacement = "testroom_M")
names(full_data) <- gsub(names(full_data), pattern = "BMI_calc", replacement = "BMI")
names(full_data) <- gsub(names(full_data), pattern = "vvm", replacement = "ffm")
names(full_data) <- gsub(names(full_data), pattern = "perc_calc", replacement = "perc_fat")
names(full_data) <- gsub(names(full_data), pattern = "afsthand", replacement = "dist_hand")
names(full_data) <- gsub(names(full_data), pattern = "afstvoet", replacement = "dist_foot")
names(full_data) <- gsub(names(full_data), pattern = "_kant", replacement = "_side")
names(full_data) <- gsub(names(full_data), pattern = "fietstest", replacement = "cycling_test")
names(full_data) <- gsub(names(full_data), pattern = "Versie", replacement = "version")
names(full_data) <- gsub(names(full_data), pattern = "versie", replacement = "version")
names(full_data) <- gsub(names(full_data), pattern = "Afspraak", replacement = "appoint")

#Remove 'F4S_' in ID, and change to integers
full_data <- full_data %>% mutate(ID = as.integer(str_remove(ID, 'F4S_'))) %>%
#remove test ID
filter(ID != 110001)

#replace -95 t/m -99 with NA
full_data <- replace_with_na_all(full_data, condition = ~.x %in% c(-95, -96, -97, -98, -99))
                                                                   
#write general function to change vars to factors and recode 
to_factor_general <- function(target_variable, ...) {
  fct_recode(as.factor(as.character(target_variable)), ...)
}

##Change categorical variables from integers to factors and name levels, 
##optional: rename variablename (if not done already)

#variable incl_16
full_data$incl_16 <- to_factor_general(full_data$incl_16, 'No' = '0', 'Yes' = '1')

#variable incl_IC
full_data$incl_IC <- to_factor_general(full_data$incl_IC, 'No' = '0', 'Yes' = '1')

#Variable incl_groep
full_data <- full_data %>% mutate(incl_groep = as.factor(as.character(incl_groep))) %>%
  mutate(incl_groep = fct_recode(incl_groep, 'Control' = '0', 'Intervention' = '1')) %>%
  rename(group = incl_groep)
#Variable M1_gesl
full_data <- full_data %>% mutate(M1_gesl = as.factor(as.character(M1_gesl))) %>%
  mutate(M1_gesl = fct_recode(M1_gesl, 'Male' = '1', 'Female' = '2')) %>%
  rename(sex = M1_gesl)
#Variable M1_roken
full_data <- full_data %>% mutate(M1_roken = as.factor(as.character(M1_roken))) %>%
  mutate(M1_roken = fct_recode(M1_roken, 'No' = '0', 'Yes' = '1', 'Quit' = '2')) %>%
  rename(smoking = M1_roken)
#Variable incl_OK_welke
full_data <- full_data %>% 
  mutate(incl_OK_welke = as.factor(as.character(incl_OK_welke))) %>%
  mutate(incl_OK_welke = fct_recode(incl_OK_welke, 
  'Colon' = '1', 'Rectum' = '2','Liver' = '3', 'HIPEC' = '4', 'RPS' = '5','Oesphagus'= '6','Pancreas' = '7', 
  'AAA' = '8', 'EVAR' = '9', 'Supra meningioma' = '10', 'Free lap' = '11', 
  'Larynx' = '12', 'Breast' = '13','Total hip' = '14', 'Rev hip' = '15', 
  'Knee' = '16', 'Nefrectomy' = '18', 'Cystectomy' = '19', 'Vulva' = '20',
  'Ovarium' = '21', 'Endometrium' = '22','GIST' = '23')) %>% 
  rename(surgery_type = incl_OK_welke)
#variable M1_HKK_side / M2_HKK_side / M3_HKK_side
full_data$M1_HKK_side <- to_factor_general(full_data$M1_HKK_side, 'Left' = '1', 'Right' = '2')
full_data$M2_HKK_side <- to_factor_general(full_data$M2_HKK_side, 'Left' = '1', 'Right' = '2')
full_data$M3_HKK_side <- to_factor_general(full_data$M3_HKK_side, 'Left' = '1', 'Right' = '2')
#variable M1_fietstest / M2_fietstest
full_data$M1_cycling_test <- to_factor_general(full_data$M1_cycling_test, 
                                               'Steep Ramp' = '1',
                                               'Astrand-Rhyming' = '2')
full_data$M2_cycling_test <- to_factor_general(full_data$M2_cycling_test, 
                                               'Steep Ramp' = '1',
                                               'Astrand-Rhyming' = '2')
#variable scr_ACSM
full_data$scr_ACSM <- to_factor_general(full_data$scr_ACSM, 'No risk' = '0',
                                        'Ref to cardio' = '1',
                                        'Ref to pulmo' = '2',
                                        'Risk but no ref' = '3')
#variable fysio_ref
full_data$fysio_ref <- to_factor_general(full_data$fysio_ref, 'No' = '0',
                                         'Yes' = '1')
#variable scr_G8_need
full_data$scr_G8_need <- to_factor_general(full_data$scr_G8_need, 'No' = '0',
                                           'Yes' = '1')
#var scr_G8_1 t/m scr_G8_8
full_data$scr_G8_1 <- to_factor_general(full_data$scr_G8_1, 'Severe decrease' = '0',
                                        'Moderate decrease' = '1',
                                        'No decrease' = '2')
full_data$scr_G8_2 <- to_factor_general(full_data$scr_G8_2, '>3kg weight loss' = '0',
                                        'Does not know' = '1',
                                        'Weight loss 1-3kg' = '2',
                                        'No weight loss' = '3')
full_data$scr_G8_3 <- to_factor_general(full_data$scr_G8_3, 'Bed/chair bound' = '0',
                                        'Not bed/chair bound, but does not go out' = '1',
                                        'Goes out' = '2')
full_data$scr_G8_4 <- to_factor_general(full_data$scr_G8_4, 'Severe dementia/depression' = '0',
                                        'Mild dementia/depression' = '1',
                                        'No problems' = '2')
full_data$scr_G8_5 <- to_factor_general(full_data$scr_G8_5, 'BMI < 18.5' = '0',
                                        '18.5 > BMI < 21' = '1',
                                        '21 > BMI < 23' = '2',
                                        'BMI > 23' = '3')
full_data$scr_G8_6 <- to_factor_general(full_data$scr_G8_6,
                                        'Ja' = '0',
                                        'Nee' = '1')
full_data$scr_G8_7 <- to_factor_general(full_data$scr_G8_7, 'Not as good' = '0',
                                        'Does not know' = '0,5',
                                        'As good' = '1',
                                        'Better' = '2')
full_data$scr_G8_8 <- to_factor_general(full_data$scr_G8_8, '>85' = '0',
                                        '80-85' = '1',
                                        '<80' = '2')
#variable scr_ger
full_data$scr_ger <- to_factor_general(full_data$scr_ger, 'No' = '0',
                                       'Yes' = '1')
#variable int_SineFuma
full_data$int_SineFuma <- to_factor_general(full_data$int_SineFuma, 'No' = '0',
                                       'Yes' = '1')
#variable final_measurement_loc
full_data$final_measurement_loc <- to_factor_general(full_data$final_measurement_loc, 
                                                    'Radboud' = '0', 'Fysio' = '1')
#variable sugery_gyn
full_data$surgery_gyn <- to_factor_general(full_data$surgery_gyn,
                                           'No' = '1', 'Yes' = '0')
#variable mm3_planned
full_data$mm3_planned <- to_factor_general(full_data$mm3_planned,
                                           'No' = '0', 'Yes' = '1')
#var baseline_diet
full_data$baseline_diet <- to_factor_general(full_data$baseline_diet,
                                             'No' = '0', 'Yes' = '1')

#var baseline_dietician
full_data$baseline_dietician <- to_factor_general(full_data$baseline_dietician,
                                             'No' = '0', 'Yes' = '1')
#var baseline_supp
full_data$baseline_supp <- to_factor_general(full_data$baseline_supp,
                                                  'No' = '0', 'Yes' = '1')
#var baseline_supp_...
full_data$baseline_supp_multivit <- to_factor_general(full_data$baseline_supp_multivit,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_vita <- to_factor_general(full_data$baseline_supp_vita,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_vitb <- to_factor_general(full_data$baseline_supp_vitb,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_vitc <- to_factor_general(full_data$baseline_supp_vitc,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_vitd <- to_factor_general(full_data$baseline_supp_vitd,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_natrium <- to_factor_general(full_data$baseline_supp_natrium,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_kalium <- to_factor_general(full_data$baseline_supp_kalium,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_magnesium <- to_factor_general(full_data$baseline_supp_magnesium,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_calcium <- to_factor_general(full_data$baseline_supp_calcium,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_glucosamine <- to_factor_general(full_data$baseline_supp_glucosamine,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_protein <- to_factor_general(full_data$baseline_supp_protein,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_omega3 <- to_factor_general(full_data$baseline_supp_omega3,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_probiotics <- to_factor_general(full_data$baseline_supp_probiotics,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_fiber <- to_factor_general(full_data$baseline_supp_fiber,
                                                      'No' = '0', 'Yes' = '1')
full_data$baseline_supp_other <- to_factor_general(full_data$baseline_supp_other,
                                                      'No' = '0', 'Yes' = '1')

#var baseline_alc
full_data$baseline_alc <- to_factor_general(full_data$baseline_alc,
                                             'No' = '0', 'Yes' = '1')
#var pre_surgery_diet
full_data$pre_surgery_diet <- to_factor_general(full_data$pre_surgery_diet,
                                             'No' = '0', 'Yes' = '1')

#var pre_surgery_dietician
full_data$pre_surgery_dietician <- to_factor_general(full_data$pre_surgery_dietician,
                                                  'No' = '0', 'Yes' = '1')
#var pre_surgery_supp
full_data$pre_surgery_supp <- to_factor_general(full_data$pre_surgery_supp,
                                             'No' = '0', 'Yes' = '1')
#var pre_surgery_supp_...
full_data$pre_surgery_supp_multivit <- to_factor_general(full_data$pre_surgery_supp_multivit,
                                                      'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_vita <- to_factor_general(full_data$pre_surgery_supp_vita,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_vitb <- to_factor_general(full_data$pre_surgery_supp_vitb,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_vitc <- to_factor_general(full_data$pre_surgery_supp_vitc,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_vitd <- to_factor_general(full_data$pre_surgery_supp_vitd,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_natrium <- to_factor_general(full_data$pre_surgery_supp_natrium,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_kalium <- to_factor_general(full_data$pre_surgery_supp_kalium,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_magnesium <- to_factor_general(full_data$pre_surgery_supp_magnesium,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_calcium <- to_factor_general(full_data$pre_surgery_supp_calcium,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_glucosamine <- to_factor_general(full_data$pre_surgery_supp_glucosamine,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_protein <- to_factor_general(full_data$pre_surgery_supp_protein,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_omega3 <- to_factor_general(full_data$pre_surgery_supp_omega3,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_probiotics <- to_factor_general(full_data$pre_surgery_supp_probiotics,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_fiber <- to_factor_general(full_data$pre_surgery_supp_fiber,
                                                         'No' = '0', 'Yes' = '1')
full_data$pre_surgery_supp_other <- to_factor_general(full_data$pre_surgery_supp_other,
                                                         'No' = '0', 'Yes' = '1')

#var pre_surgery_smoking
full_data$pre_surgery_smoking <- to_factor_general(full_data$pre_surgery_smoking,
                                               'No' = '0', 'Yes' = '1',
                                               'No but less' = '3')
#var pre_surgery_alc
full_data$pre_surgery_alc <- to_factor_general(full_data$pre_surgery_alc,
                                            'No' = '0', 'Yes' = '1')
#var F4S_car
full_data$F4S_car <- to_factor_general(full_data$F4S_car,
                                       'No' = '0', 'Yes' = '1')
#var F4S_long
full_data$F4S_long <- to_factor_general(full_data$F4S_long,
                                        'No' = '0', 'Yes' = '1')
#var F4S_ger
full_data$F4S_ger <- to_factor_general(full_data$F4S_ger,
                                       'No' = '0', 'Yes' = '1')
#var no_training_reason
full_data$no_training_reason <- to_factor_general(full_data$no_training_reason,
'No motivation' = '0','Illness' = '1','Miscommunication' = '2',
'Surgery soon' = '3','Other' = '4','No surgery' = '5')
#var F4S_protein
full_data$F4S_protein <- to_factor_general(full_data$F4S_protein,
                                           'No' = '0',
                                           'Yes' = '1')
#var F4S_vit
full_data$F4S_vit <- to_factor_general(full_data$F4S_vit,
                                           'No' = '0',
                                           'Yes' = '1')
#var F4S_SineFuma
full_data$F4S_SineFuma <- to_factor_general(full_data$F4S_SineFuma,
                                       'No' = '0',
                                       'Yes' = '1')
#var F4S_psych
full_data$F4S_psych <- to_factor_general(full_data$F4S_psych,
                                            'No' = '0',
                                            'Yes' = '1')
#var Neoadjuvant_Therapy
full_data$Neoadjuvant_Therapy <- to_factor_general(full_data$Neoadjuvant_Therapy,
                                                   'No' = '0',
                                                   'Chemo' = '1',
                                                   'Radio' = '2',
                                                   'Chemo_radio' = '3')
#var Surgery_Performed
full_data$Surgery_Performed <- to_factor_general(full_data$Surgery_Performed,
                                                 'No' = '0',
                                                 'Yes' = '1')
#var Surgery_Technique
full_data$Surgery_Technique <- to_factor_general(full_data$Surgery_Technique,
                                                 'Open' = '0',
                                                 'Laparoscopic' = '1',
                                                 'Robot' = '2')
###var Surgery_Procedure
full_data$Surgery_Procedure <- to_factor_general(full_data$Surgery_Procedure,
'EVAR' = '0', 'FEVAR' = '1','Open AAA herstel' = '2','DIEP enkelzijdig' = '3',
'DIEP dubbelzijdig' = '4', 'LD enkelzijdig' = '5', 'LD dubbelzijdig' = '6', 'PAP enkelzijdig' = '7',
'PAP dubbelzijdig' = '8', 'Hemicolectomie rechts' = '9', 'Hemicolectomie links' = '10',
'Sigmo?dresectie' = '11','Subtotale colectomie' = '12', 'Cystectomie' = '13', 'Hysterectomie/adnexextirpatie' = '14',
'GIST resectie' = '15','GIST resectie/solitaire orgaanresectie' = '16','GIST resectie/multipele orgaanresectie' = '17',
'HIPEC alleen cytoreductie' = '18','HIPEC/solitaire orgaanresectie' = '19', 'HIPEC/multipele orgaanresectie' = '20',
'Hemihepatectomie rechts' = '21','Hemihepatectomie links' = '22','Lever segmentresectie' = '23',
'Meningeoom resectie' = '24','Nefrectomie' = '25','Parti?le nefrectomie' = '26','Nefroureterectomie' = '27',
'OCR thoracaal' = '28','OCR cervicaal' = '29','MICE cervicaal' = '30','OVHIPEC/solitaire orgaanresectie' = '31',
'OVHIPEC/multipele orgaanresectie' = '32','Whipple procedure' = '33','Totale pancreatectomie' = '34',
'Parti?le pancreatectomie' = '35','APR' = '36','LAR' = '37','Retroperitoneaal sarcoom resectie' = '38',
'Retroperitoneaal sarcoom/solitaire orgaanresectie' = '39','Retroperitoneaal sarcoom/multipele orgaanresectie'= '40',
'THP primair' = '41','THP revisie' = '42','TKP revisie' = '43','Totale laryngectomie' = '44',
'Vrije flap reconstructie met tracheostoma' = '45','Vrije flap reconstructie zonder tracheostoma' = '46','Vulvaresectie' = '47')

#var Different_Surgery
full_data$Different_Surgery <- to_factor_general(full_data$Different_Surgery,
                                                 'No' = '0',
                                                 'Yes' = '1')

#var Complication_Intraoperative
full_data$Complication_Intraoperative <- to_factor_general(full_data$Complication_Intraoperative,
'0' = 'None', '1' = 'Conversion', '2' = 'Bleeding', '3' = 'Latrogen', '4' = 'Other')
#var Complication_Postoperative_Categorical_...
full_data$Complication_Postoperative_Categorical_1 <- to_factor_general(full_data$Complication_Postoperative_Categorical_1,
'Cardial' = '0', 'Neurological' = '1', 'Pulmonary' = '2', 'Surgical' = '3',
'Thrombo-embolic' = '4', 'Urological' = '5', 'Elektrolyte imbalance' = '6',
'Infectious' = '7', 'Nefrological' = '8', 'Other' = '9')
full_data$Complication_Postoperative_Categorical_2 <- to_factor_general(full_data$Complication_Postoperative_Categorical_2,
                                                                        'Cardial' = '0', 'Neurological' = '1', 'Pulmonary' = '2', 'Surgical' = '3',
                                                                        'Thrombo-embolic' = '4', 'Urological' = '5', 'Elektrolyte imbalance' = '6',
                                                                        'Infectious' = '7', 'Nefrological' = '8', 'Other' = '9')
full_data$Complication_Postoperative_Categorical_3 <- to_factor_general(full_data$Complication_Postoperative_Categorical_3,
                                                                        'Cardial' = '0', 'Neurological' = '1', 'Pulmonary' = '2', 'Surgical' = '3',
                                                                        'Thrombo-embolic' = '4', 'Urological' = '5', 'Elektrolyte imbalance' = '6',
                                                                        'Infectious' = '7', 'Nefrological' = '8', 'Other' = '9')
full_data$Complication_Postoperative_Categorical_4 <- to_factor_general(full_data$Complication_Postoperative_Categorical_4,
                                                                        'Cardial' = '0', 'Neurological' = '1', 'Pulmonary' = '2', 'Surgical' = '3',
                                                                        'Thrombo-embolic' = '4', 'Urological' = '5', 'Elektrolyte imbalance' = '6',
                                                                        'Infectious' = '7', 'Nefrological' = '8', 'Other' = '9')
full_data$Complication_Postoperative_Categorical_5 <- to_factor_general(full_data$Complication_Postoperative_Categorical_5,
                                                                        'Cardial' = '0', 'Neurological' = '1', 'Pulmonary' = '2', 'Surgical' = '3',
                                                                        'Thrombo-embolic' = '4', 'Urological' = '5', 'Elektrolyte imbalance' = '6',
                                                                        'Infectious' = '7', 'Nefrological' = '8', 'Other' = '9')
#var Complication_Postoperative_CD_...
full_data$Complication_Postoperative_CD_1 <- to_factor_general(full_data$Complication_Postoperative_CD_1,
'1' = '1', '2' = '2', '3a' = '3', '3b' = '4', '4a' = '5', '4b' = '6', '5' = '7')
full_data$Complication_Postoperative_CD_2 <- to_factor_general(full_data$Complication_Postoperative_CD_2,
                                                               '1' = '1', '2' = '2', '3a' = '3', '3b' = '4', '4a' = '5', '4b' = '6', '5' = '7')
full_data$Complication_Postoperative_CD_3 <- to_factor_general(full_data$Complication_Postoperative_CD_3,
                                                               '1' = '1', '2' = '2', '3a' = '3', '3b' = '4', '4a' = '5', '4b' = '6', '5' = '7')
full_data$Complication_Postoperative_CD_4 <- to_factor_general(full_data$Complication_Postoperative_CD_4,
                                                               '1' = '1', '2' = '2', '3a' = '3', '3b' = '4', '4a' = '5', '4b' = '6', '5' = '7')
full_data$Complication_Postoperative_CD_5 <- to_factor_general(full_data$Complication_Postoperative_CD_5,
                                                               '1' = '1', '2' = '2', '3a' = '3', '3b' = '4', '4a' = '5', '4b' = '6', '5' = '7')
#var Readmission_30_Days
full_data$Readmission_30_Days <- to_factor_general(full_data$Readmission_30_Days,
                                                 'No' = '0',
                                                 'Yes' = '1')
#var Mortality_30_Days
full_data$Mortality_30_Days <- to_factor_general(full_data$Mortality_30_Days,
                                                 'No' = '0',
                                                 'Yes' = '1')
#var adjuvant_therapy
full_data$adjuvant_therapy <- to_factor_general(full_data$adjuvant_therapy,
                                                   'No' = '0',
                                                   'Chemo' = '1',
                                                   'Radio' = '2',
                                                   'Chemo_radio' = '3')
#var post_surgery_diet
full_data$post_surgery_diet <- to_factor_general(full_data$post_surgery_diet,
                                                 'No' = '0',
                                                 'Yes' = '1')
#var post_surgery_dietician
full_data$post_surgery_dietician <- to_factor_general(full_data$post_surgery_dietician,
                                                 'No' = '0',
                                                 'Yes' = '1')
#var post_surgery supp
full_data$post_surgery_supp <- to_factor_general(full_data$post_surgery_supp,
                                                 'No' = '0',
                                                 'Yes' = '1')
#var post_surgery_supp_spec
full_data$post_surgery_supp_spec <- to_factor_general(full_data$post_surgery_supp_spec,
'Vit C' = '0','Vit D' = '1','Multivit' = '2','Glucosamide' = '3',
'Magnesium' = '4','Omega-3' = '5','Other' = '6')
#var post_surgery_smoking
full_data$post_surgery_smoking <- to_factor_general(full_data$post_surgery_smoking,
                                                    'No' = '0',
                                                    'Yes' = '1')
#var post_surgery_alc
full_data$post_surgery_alc <- to_factor_general(full_data$post_surgery_alc,
                                                    'No' = '0',
                                                    'Yes' = '1')
#var Intention_to_Treat
full_data$Intention_to_Treat <- to_factor_general(full_data$Intention_to_Treat,
                                                  'Control' = '0',
                                                  'Intervention' = '1')
#var Deviation_Intention_to_Treat_Primary
full_data$Deviation_Intention_to_Treat_Primary <- to_factor_general(full_data$Deviation_Intention_to_Treat_Primary,
                                                    'No' = '0',
                                                    'Yes' = '1')
#var Reason_Deviation_Intention_to_Treat
full_data$Reason_Deviation_Intention_to_Treat <- to_factor_general(full_data$Reason_Deviation_Intention_to_Treat,
'No motivation' = '0', 'Logistic' = '1','Surgery soon' = '2','Illness' = '3',
'Contra intervention' = '4','Not feasible' = '5')
#var appoint_...
full_data$appoint_MM1 <- to_factor_general(full_data$appoint_MM1,
                                           'Physical' = '0',
                                           'Questionnaires' = '1',
                                           'None' = '2')
full_data$appoint_MM2 <- to_factor_general(full_data$appoint_MM2,
                                           'Physical' = '0',
                                           'Questionnaires' = '1',
                                           'None' = '2')
full_data$appoint_MM3 <- to_factor_general(full_data$appoint_MM3,
                                           'Physical' = '0',
                                           'Questionnaires' = '1',
                                           'None' = '2')
#var proms_mm4_mm5
full_data$proms_mm4_mm5 <- to_factor_general(full_data$proms_mm4_mm5,
'MM4' = '0', 'MM5' = '1','MM4 and MM5' = '2','None' = '3')
#var Follow_Up_Moment
full_data$Follow_Up_Moment <- to_factor_general(full_data$Follow_Up_Moment,
'MM1' = '0', 'MM2' = '1', 'MM3' = '2', 'MM4' = '3','MM5' = '4','Only IC' = '5')
#var Reason_Lost_to_Follow_Up
full_data$Reason_Lost_to_Follow_Up <- to_factor_general(full_data$Reason_Lost_to_Follow_Up,
'No motivation' = '0','Death' = '1','Logistic' = '2','No surgery' = '3','Second F4S' = '4')

#all collumn names to lowercase
names(full_data) <- tolower(names(full_data))

#####################END CODEBOOK#####################################











