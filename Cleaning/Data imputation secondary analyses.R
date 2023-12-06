# Author: Elke van Daal
# Goal: Data imputation for sf36 analyses ABR

## Load packages
library(mice)
library(janitor)
library(tidyverse)
library(naniar)

## Load SF36 data
load(file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\sf36_summaryscores.RData")

## to do: check groups(intervention or both int and control)

## Remove completely empty rows
sf36_before_imp <- sf36_calcs %>%
  rename(id = `df_bc$id`, time_point = `df_bc$time_point`) %>%
  select(-MCS, -PCS) %>% # delete summary scores to only keep domain scores
  remove_empty('rows', cutoff = 0.60) # only keep participants who have at least half of domain scores calculated
View(sf36_before_imp)
View(sf36_calcs)
  remove_empty('rows', cutoff = 0.20) # what percentage of sf36 items should be completed for imputation? 
View(sf36_before_imp)

## calculate % missings
m <- round(pct_miss_case(sf36_before_imp)) # calculate % incomplete observations

## Check prediction matrix
predict <- quickpred(sf36_before_imp[,3:10], mincor = 0.2) # exclude id and time columns from predictions

## Impute data for analyses on SF36 
sf36_imp <- mice(sf36_before_imp, m = m, method = "pmm", predictorMatrix = predict,
                 seed = 1610, maxit = 20, print = FALSE)


## Calculate MHS and PHS scores in all imputed dataframes
imp_long <- complete(sf36_imp, action = 'long', include = TRUE) # include original dataframe?
View(imp)

source("C:\\Users\\Elke\\Documents\\R\\Syntaxes\\Syntax_SF36_SS.R") # load sf36_ss syntax

imp_ss <- imp_long %>%
  select(PF, RP, BP, GH, VT, SF, RE, MH) # select variables needed as input for syntax

sf36_ss(imp_ss) # run syntax
View(sf36_m1_m2)

complete_sf36_imp <- cbind(imp_long[ , 1:4], sf36_ss) # bind id and time_point variables to complete sf36 df

sf36_mids <- as.mids(sf36_m1_m2) # store as mids function for further analyses


sf36_m1_m2 <- complete_sf36_imp %>%
  filter(time_point == 'm1' | time_point == 'm2') %>%
  group_by(id) %>%
  mutate(count_id = n()) %>%
  ungroup() %>%
  filter(count_id == 76) # make sure that only patients with both m1 and m2 completed are included


predict <- quickpred(sf36_before_imp, mincor = 0.20)

## Impute data for analyses on SF36 
sf36_imp <- mice(sf36_before_imp, m = m, method = "pmm", seed = 1610, maxit = 20, print = FALSE)

## Complete imputations to dataframe
imp <- complete(sf36_imp, action = 'long', include = FALSE) # include original dataframe?
View(imp)

## Calculate summary scores
imp_ss <- imp %>%
  select(PF, RP, BP, GH, VT, SF, RE, MH)

source("C:\\Users\\Elke\\Documents\\R\\Syntaxes\\Syntax_SF36_SS.R") # load sf36_ss syntax
sf36_ss(imp_ss)

## bind id and time_point variables to complete sf36 df
complete_sf36_imp <- cbind(imp[ , 1:4], sf36_ss)
View(complete_sf36_imp)

## save imputed dataframe for analysis
save(complete_sf36_imp, file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\complete_sf36_imp.RData")


test <- complete_sf36_imp %>%
  filter(time_point == 'm1' | time_point == 'm2')

complete_sf36_imp %>%
  group_by(id, time_point) %>%
  summarise(mean(GH), sd(GH))


# Percentage incomplete patients (same as pct_miss_case function)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(sf36_before_imp, 2, pMiss)
apply(sf36_before_imp, 1, pMiss)
m <- round(sum(complete.cases(sf36_before_imp) == FALSE)/nrow(sf36_before_imp)*100)
