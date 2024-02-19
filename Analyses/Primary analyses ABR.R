# Author: Elke van Daal
# Goal: Primary analyses ABR (complications)

## Load packages
library(tidyverse)
library(foreign)
library(haven)
library(performance)
library(ggstatsplot)
library(dlookr)
library(sjPlot)
library(janitor)
library(gamlss)
library(see)

## Load data 
rm(list = ls())
df <- read.spss("Z:\\Data PREHAB trial\\F4S PREHAB SPSS - Joelle.sav",
          to.data.frame = TRUE) #complication data 05-02-2024, all indications
load(file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\dp.Rdata") #abr cohort

## next steps: 
## 1.inner join dp with complications from spss (prospective cohort and retrospective cohort), by MDN / study_id to pd
# (number of observations from dp should be equal to dp) ### CHECK
## 2. calculate clavien-dindo, CCI into dp ### CHECK
## 3. inner join dp with clinical characteristics from spss ###
## end product: dataset of complete cohort, with all primary outcomes and all clinical characteristics
## this end product is ready to use for data analysis

# Step 1 (inner join dp with complications from spss)
## all cases to lower
df <- mutate_all(df, .funs = tolower)

## Remove F4S in study_id df
df <- df %>% 
  mutate(ParticipantId = as.integer(str_remove(ParticipantId, 'f4s_'))) %>%
  rename(study_id = ParticipantId)

## inner join dp and df into pd
pd <- inner_join(dp, df, by = "study_id") # number of observations of pd should be equal to dp

# Step 2 (create clavien-dindo scores and CCI scores)
## load cci syntax
source("C:\\Users\\Elke\\Documents\\R\\Syntaxes\\Syntax_CCI.R")
## clean pd
names(pd) <- tolower(names(pd))
pd <- pd  %>%
  mutate(complication1_cdc = as.character(complication_postoperative_cd_1),
         complication2_cdc = as.character(complication_postoperative_cd_2),
         complication3_cdc = as.character(complication_postoperative_cd_3),
         complication4_cdc = as.character(complication_postoperative_cd_4),
         complication5_cdc = as.character(complication_postoperative_cd_5)) %>%
  select(-complication_postoperative_cd_1, -complication_postoperative_cd_2,
         -complication_postoperative_cd_3, -complication_postoperative_cd_4,
         -complication_postoperative_cd_5) 
pd <- pd %>%
  select(-var00001, -var00002, -neoadjuvant_therapy, -hemoglobin_preoperative_anesthesiology,
         -surgery_cluster, -surge0, -surge1, -surge2, -surgery_technique,
         -diffe0, - diffe1, - diffe2, -complu, -complv, -complw, -complr,
         -compls, -complt, -complo, -complq, -complp, -compll, -complm, -compln,
         -compli, -complj, -complk, -complf, -complg, -complh, -complc, -compld, -comple,
         -compl9, -compla, -complb, -compl6, -compl7, -compl8,
         -compl3, -compl4, -compl5, -compl0, -compl1, -compl2,
         -adjuvant_therapy, -mdn) %>%
  mutate(surgery_indication = as.factor(surgery_indication),
         surgery_procedure = as.factor(surgery_procedure),
         asa_score = as.numeric(asa_score),
         hemoglobin_preoperative = as.numeric(hemoglobin_preoperative),
         different_surgery = as.factor(different_surgery),
         icu_planned = as.factor(icu_planned),
         icu_days_total = as.numeric(icu_days_total),
         er_visits_30_days = as.factor(er_visits_30_days),
         mortality_30_days = as.factor(mortality_30_days)) %>%
  arrange(study_id)

## replace NA's to 0 
pd[, c("complication1_cdc", "complication2_cdc", "complication3_cdc", "complication4_cdc", "complication5_cdc")] <- 
  apply(pd[, c("complication1_cdc", "complication2_cdc", "complication3_cdc", "complication4_cdc", "complication5_cdc")], 
        2, function(x) ifelse(is.na(x), 0, x)) #replace NA's to 0 

## Count occurences of Clavin Dindo scores by row (check if rows are correct)
cd_1 <- rowSums(pd[, c(51:55)] == '1')
cd_2 <- rowSums(pd[, c(51:55)] == '2')
cd_3a <- rowSums(pd[, c(51:55)] == '3a')
cd_3b <- rowSums(pd[, c(51:55)] == '3b')
cd_4a <- rowSums(pd[, c(51:55)] == '4a')
cd_4b <- rowSums(pd[, c(51:55)] == '4b')
cd_5 <- rowSums(pd[, c(51:55)] == '5')

## Bind rows to dataframe for CCI syntax
cdc_counts <- cbind(cd_1, cd_2, cd_3a, cd_3b, cd_4a, cd_4b, cd_5) ## input for CCI syntax

pd <- cbind(pd, cdc_counts)

## Calculate CCI scores with cci syntax and bind with dataframe (make sure ids are sorted)
cci(cdc_counts)
pd <- cbind(pd, cci_scores)

## add column to indicate presence of cdc score >= 2
pd <- pd %>%
  mutate(cd_2_or_higher = cd_2 > 0 | cd_3a >0 | cd_3b > 0 |
         cd_4a > 0 | cd_4b > 0 | cd_5 >0,
         cd_2_or_higher = as.factor(cd_2_or_higher),
         cd_2_or_higher = fct_recode(cd_2_or_higher, 'Yes' = 'TRUE',
                                     'No' = 'FALSE'),
         cd_2_or_higher_num = as.numeric(as.character(fct_recode(cd_2_or_higher, '1' = 'Yes',
                                                                '0' = 'No'))))

# Step 3
## join pd with clinical characteristics from spss into pd
## check if all study_id's are in common, number of observations of pd should remain the same

### Part II: analyses ###

## Descriptives of cd_2_or_higher and cci
pd %>%
  select(study_id, group, cci) %>%
  group_by(group) %>%
  summarise(mean_cci = mean(cci),
            sd_cci = sd(cci),
            min_cci = min(cci),
            max_cci = max(cci),
            count = n())
pd %>%
  select(study_id, group, cd_2_or_higher_num) %>%
  group_by(group) %>%
  summarise(perc_cd2 = mean(cd_2_or_higher_num * 100),
            sd_perc_cd2 = sd(cd_2_or_higher_num),
            count = n())

pd %>%
  group_by(group) %>%
  count(cd_2_or_higher)

## visualize data
pd %>%
  filter(!is.na(cci)) %>%
  filter(!is.na(group)) %>%
  ggplot(aes(group, cci)) +
  geom_boxplot()

pd %>%
  filter(!is.na(cci)) %>%
  filter(!is.na(group)) %>%
  ggplot(aes(cd_2_or_higher, fill = group)) +
  geom_bar(position = "dodge")

pd %>%
  ggplot(aes(x = cci)) +
  geom_histogram(bins = 5, binwidth = 10)
  

## Statistically test differences in cci between control-int

pd %>%
  plot_normality(cci) #distribution
check_normality(pd$cci)

## linear model
lin_model <- lm(cci ~ group, data = pd) #normal distribution
check_model(lin_model) # check assumptions of linear regression

tab_model(lin_model,
          show.reflvl = T,
          show.aic = T,
          p.style = "numeric_stars") #very high AIC, model does not describe data well

## use of zero inflated beta regression
### rescale cci
pd_bezi <- pd %>%
  mutate(cci_rescaled = cci / 100) %>%
  select(group, cci_rescaled)

beinf <- BEINF(mu.link = "logit", sigma.link = "logit", nu.link = "log", tau.link = "log") #beta inflated family
        # similar to bezi but allows zeros and ones as values for response variable

model1 <- gamlss(cci_rescaled ~ group, 
                  nu.formula = cci_rescaled ~ group, data = pd_bezi, family = beinf) #fit model for mu and nu
model2 <- gamlss(cci_rescaled ~ group, data = pd_bezi, family = beinf) #fit model for mu  

summary(model1) 
summary(model2) #AIC a lot lower compared to linear model, quite good model fit

pdf.plot(model1, from = 0, to = 1) #check model fit
pdf.plot(model2, from = 0, to = 1)

## interpretation of model
### 1. mu section shows effect of predictors on mean outcome within range of non-zero values (when complications occur)
### e.g. if coefficient for group is negative and stat. sign., it suggests that the intervention is associated
### with a lower mean value of CCI within the range of non-zero values.

### 2. nu section shows effect of predictors on probability of excess zeros (no complications),
### i.e. the probability of observing zero's beyond what would be expected based on the beta distribution alone
### e.g. if coefficient for group is positive and stat. sign., it suggests that the intervention is associated
### with a higher probability of excess zero's in CCI (so greater likelihood of no complications).

### adjust model for potential confounders from EPD (age, BMI, indication, procedure, chemo, radiation, hormonal..)

## Statistically test differences in cd_2_or_higher between control-int
logRR_model <- glm(cd_2_or_higher ~ group, family = 'binomial'(link = "log"), data = pd) #fit logistic model with risk ratios
logOR_model <- glm(cd_2_or_higher ~ group, family = 'binomial', data = pd) #fit logistic model with odds ratios
tab_model(logOR_model, logRR_model,
          show.reflvl = T,
          show.aic = T,
          p.style = "numeric_stars") #AIC indicates good model fit

## adjust model for potential confounders from EPD (age, BMI, indication, procedure, chemo, radiation, hormonal..)

