###### Author: Elke van Daal ######################################
###### Project: Fit4Surgery PREHAB trial ABR cohort - create one dataset #####
###### Start date: 7-02-2024 #####################################

# load packages
library(tidyverse)
library(haven)
library(readxl)
library(xlsx)
library(gt)

# load excel sheet with cohort list 
rm(list = ls())
d <- read_excel("Z:\\Data PREHAB ABR\\overzicht inclusies abr.xlsx") #7-2-24, retrospective cohort to be added

# all cases to lower
d <- mutate_all(d, .funs = tolower)

# remove F4S in study_id number
d <- d %>% 
  mutate(study_id = as.integer(str_remove(study_id, 'f4s_')))

# study_id and mdn to numeric
d <- d %>%
  mutate(study_id = as.numeric(study_id),
         mdn = as.numeric(mdn))

# change group variable to factor and collapse
d <- d %>% 
  mutate(group = fct_collapse(as.factor(group),
                              control = c("control", "controle"),
                              intervention = c("intervention", "interventie"))
         )
         
# change each variable to correct data type
d$indication <- "autologous breast reconstruction"
d <- d %>%
  mutate(inclusion = as.factor(inclusion),
         adherence = as.factor(adherence),
         informed_consent = as.factor(informed_consent),
         study_cohort = as.factor(study_cohort),
         reason_exclusion = as.factor(reason_exclusion),
         reason_no_adherence = as.factor(reason_no_adherence),
         remarks = as.factor(remarks))

# flowchart output
table(d$remarks, d$group) # to check patients who were not part of target population (but included initially)
table(d$reason_exclusion) # to check patients who did not meet inclusion criteria
table(d$reason_no_adherence, d$group) # to check patients who did not adhere to fit4surgery (in control group adherence is nvt)

d %>% count() # count number of patients who were checked for eligibility
d %>% count(inclusion) # count inclusions and exclusions (patients with remarks were initially included)
d %>% count(inclusion, group) # count inclusions per group
d %>% count(adherence) # count adherence in intervention group

# only keep inclusions
d_in <- d %>%
  filter(inclusion == "yes")


# for primary outcomes
## next steps: 
## 1. Add retrospective cohort to excel sheet
## 2.inner join d_in with complications from spss (prospective cohort and retrospective cohort), by MDN / study_id to pd
      # (number of observations from dp should be equal to d_in) 
## 3. remove variable MDN from dp ###
## 4. calculate clavien-dindo, CCI into dp ###
## 5. inner join dp with clinical characteristics from spss into dp ###
## end product: dataset of complete cohort, with all primary outcomes and all clinical characteristics
## this end product is ready to use for data analysis
## 6. save dp dataframe

# for secondary outcomes
## 1. Import castor data
source("C:\\Users\\Elke\\Documents\\R\\Fit4Surgery\\Cleaning\\Source cleaning and codebook.R") #castor data from 20-12-2023

## 2. - select variables from castor(- group, zorgpad, other redundant variables?) 
castor_data <- full_data %>%
  select(-group, -surgery_type, -incl_ic) %>%
  rename(study_id = id)

## 3. semi-join castor data with intervention patients who adhered to f4s (possibly need control patients who gave IC for table 1)
d_f4s <- d_in %>%
  filter(adherence == "yes") # filter for patients who adhered to f4s

castor_match <- semi_join(castor_data, d_f4s, by = "study_id") # match d_f4s with castor_data

## 4. inner-join castor_match with d_f4s to create full dataset for secondary variables (ds)
ds <- inner_join(castor_match, d_f4s, by = "study_id")

## 5. join ds with sf36 data (check this!!)

## 6. organize ds
### relocate columns
ds <- ds %>%
  relocate(group, .after = study_id) %>%
  relocate(indication, .after = group) %>%
  relocate(inclusion, .after = indication) %>%
  relocate(adherence, .after = inclusion) %>%
  relocate(informed_consent, .after = adherence) %>%
  relocate(study_cohort, .after = informed_consent)

### add column baseline measurement completed (yes/no) and add column fu measurement completed (yes/no)
ds <- ds %>%
  mutate(m1_completed = as.factor(ifelse(!is.na(m1_length), "yes", "no")),
         m2_completed = as.factor(ifelse(!is.na(m2_length), "yes", "no")))

table(ds$m1_completed, ds$m2_completed, ds$group) #check completion of measurements (no control patients should be included here)

### remove redundant columns 
ds <- ds %>%
  select() ## only keep variables relevant for secondary analyses and baseline description

### endproduct: dataset of subcohort of intervention patients who adhered to fit4surgery,
### including all testroom variables and sf36 results.
### this endproduct is ready to use for imputation, and afterwards secondary data analyses
## 7. save ds dataframe
