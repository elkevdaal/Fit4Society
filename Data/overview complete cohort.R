###### Author: Elke van Daal ######################################
###### Project: Fit4Surgery PREHAB trial ABR cohort - create one dataset #####
###### Start date: 7-02-2024 #####################################

# next time: create R markdown file for all steps and explanations!

# load packages
library(tidyverse)
library(haven)
library(readxl)
library(xlsx)
library(gt)

# load excel sheet with cohort list 
rm(list = ls())
d <- read_excel("Z:\\Data PREHAB ABR\\overzicht inclusies abr.xlsx") #14-2-24, retrospective cohort to be added

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

# only keep inclusions for primary data (dp)
dp <- d %>%
  filter(inclusion == "yes") 

# only keep adherent patients for secondary data (ds)
ds <- d %>%
  filter(adherence == "yes")
 

# Save dataframes
save(dp, file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\dp.RData") #dataframe for primary analyses
save(ds, file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\ds.RData") #dataframe for secondary analyses


