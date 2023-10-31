# Author: Elke van Daal
# Goal: Primary analyses ABR (complications)

## Load packages
library(tidyverse)
library(foreign)
library(haven)

## Load data
df <- read.spss("C:\\Users\\Elke van Daal\\Documents\\R\\Fit4Society\\Data\\Complications.sav",
          to.data.frame = TRUE)
glimpse(df)
names(df) <- tolower(names(df))

## Clean df
df <- df %>% 
  rename(id = participantid) %>%
  mutate(id = as.integer(str_remove(id, 'F4S_'))) %>%
  select(id, surgery_cluster, surgery_indication, surgery_procedure,
         complication_intraoperative, complication_postoperative_yes_no,
         complication_postoperative_cd_1, complication_postoperative_cd_2,
         complication_postoperative_cd_3, complication_postoperative_cd_4,
         complication_postoperative_cd_5) %>%
  mutate(complication1_cdc = as.character(complication_postoperative_cd_1),
         complication2_cdc = as.character(complication_postoperative_cd_2),
         complication3_cdc = as.character(complication_postoperative_cd_3),
         complication4_cdc = as.character(complication_postoperative_cd_4),
         complication5_cdc = as.character(complication_postoperative_cd_5)) %>%
  select(-complication_postoperative_cd_1, -complication_postoperative_cd_2,
         -complication_postoperative_cd_3, -complication_postoperative_cd_4,
         -complication_postoperative_cd_5) %>%
  arrange(id)
View(df)
glimpse(df)

## replace NA's to 0 
df[, c("complication1_cdc", "complication2_cdc", "complication3_cdc", "complication4_cdc", "complication5_cdc")] <- 
  apply(df[, c("complication1_cdc", "complication2_cdc", "complication3_cdc", "complication4_cdc", "complication5_cdc")], 
        2, function(x) ifelse(is.na(x), 0, x)) #replace NA's to 0 

## Count occurences of Clavin Dindo scores by row
cd_1 <- rowSums(df[, c(7:11)] == '1')
cd_2 <- rowSums(df[, c(7:11)] == '2')
cd_3a <- rowSums(df[, c(7:11)] == '3a')
cd_3b <- rowSums(df[, c(7:11)] == '3b')
cd_4a <- rowSums(df[, c(7:11)] == '4a')
cd_4b <- rowSums(df[, c(7:11)] == '4b')
cd_5 <- rowSums(df[, c(7:11)] == '5')

## Bind rows to datarame
cdc_counts <- cbind(cd_1, cd_2, cd_3a, cd_3b, cd_4a, cd_4b, cd_5) ## input for CCI syntax
View(cdc_counts)
df_cdc <- cbind(df, cdc_counts)

## Calculate CCI scores with cci syntax and bind with dataframe 
cci(cdc_counts)
df_comp <- cbind(df_cdc, cci_scores)
View(df_comp)
