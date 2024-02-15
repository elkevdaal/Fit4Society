# Author: Elke van Daal
# Goal: Data imputation for sf36 analyses ABR

## Load packages
library(mice)
library(janitor)
library(tidyverse)
library(naniar)

## Load SF36 data
rm(list = ls())
load(file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\sd_before_imp.RData")


# PART 1. Imputation for sf36
sd36 <- sd %>%
  select(study_id, 29:36, 55:62) %>% #only keep domain scores from both m1 and m2
  remove_empty('rows', cutoff = 0.60) # only keep participants who have at least half of domain scores calculated

## calculate % missings
m36 <- round(pct_miss_case(sd36)) # calculate % incomplete observations

## Check prediction matrix
predict <- quickpred(sd36[,2:17], mincor = 0.2) # exclude id and time columns from predictions

## Impute data for analyses on SF36 
imp36 <- mice(sd36, m = m36, method = "pmm", predictorMatrix = predict,
                 seed = 1610, maxit = 20, print = FALSE)

## store imputed data in long format
long36 <- complete(imp36, action = 'long', include = FALSE) # include original dataframe?


## load SF-36 syntax to calculate index scores
source("C:\\Users\\Elke\\Documents\\R\\Syntaxes\\Syntax_SF36_index_scores.R")

## calculate MHS and PHS
long36_m1 <- long36 %>%
  select(4:11) # select variables from m1 needed as input for syntax

long36_m2 <- long36 %>%
  select(12:19) # select variables from m2 needed as input for syntax


sf36_index_syntax(long36_m1) # run syntax to calculate index scores for m1
sf36_index_scores_m1 <- sf36_index_scores
sf36_index_syntax(long36_m2) # run syntax to calculate index scores for m2
sf36_index_scores_m2 <- sf36_index_scores


long36_m1 <- cbind(long36[ , 1:3], sf36_index_scores_m1) # bind id variables to complete sf36 df
long36_m2 <- cbind(long36[ , 1:3], sf36_index_scores_m2) # bind id variables to complete sf36 df
long36 <- inner_join(long36_m1, long36_m2, by = c(".imp", ".id", "study_id")) # complete cohort 

## change .x to _m1 and .y to _m2
names(long36) <- gsub(names(long36), pattern = ".x", replacement = "_m1")
names(long36) <- gsub(names(long36), pattern = ".y", replacement = "_m2")

## save imputed dataframe for analysis
save(long36, file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\imp36.RData") #test if analysis works like this

## make overview with questions!!!

# PART 2. Imputation for other variables


# Percentage incomplete patients (same as pct_miss_case function)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(sf36_before_imp, 2, pMiss)
apply(sf36_before_imp, 1, pMiss)
m <- round(sum(complete.cases(sf36_before_imp) == FALSE)/nrow(sf36_before_imp)*100)
