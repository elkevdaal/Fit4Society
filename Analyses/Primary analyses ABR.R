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

file.choose()
## Load data
df <- read.spss("Z:\\Data PREHAB trial\\F4S PREHAB SPSS - Luuk.sav",
          to.data.frame = TRUE) #complication data
load(file = "C:\\Users\\Elke\\Documents\\R\\Fit4Society\\Data\\testroom_data_abr.Rdata") #testroom data abr cohort

## Load cci syntax
source("C:\\Users\\Elke\\Documents\\R\\Syntaxes\\Syntax_CCI.R")

## Clean df
names(df) <- tolower(names(df))
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

## Only keep ABR patients and add column to indicate presence of cdc score >= 2
comp_bc <- df_comp %>%
  filter(surgery_cluster == 'Autologous Breast Reconstruction') %>%
  mutate(cd_2_or_higher = cd_2 > 0 | cd_3a >0 | cd_3b > 0 |
         cd_4a > 0 | cd_4b > 0 | cd_5 >0,
         cd_2_or_higher = as.factor(cd_2_or_higher),
         cd_2_or_higher = fct_recode(cd_2_or_higher, 'Yes' = 'TRUE',
                                     'No' = 'FALSE'),
         cd_2_or_higher_num = as.numeric(as.character(fct_recode(cd_2_or_higher, '1' = 'Yes',
                                                                '0' = 'No'))))

## Add group to comp_bc (control or intervention) 
bc_group <- bc_data %>%
  select(id, group)
comp_bc <- full_join(comp_bc, bc_group, by = 'id')
View(comp_bc)

## Descriptives of cd_2_or_higher and cci
comp_bc %>%
  select(id, group, cci) %>%
  filter(complete.cases(.)) %>%
  group_by(group) %>%
  summarise(mean_cci = mean(cci),
            sd_cci = sd(cci),
            count = n())
comp_bc %>%
  select(id, group, cd_2_or_higher_num) %>%
  filter(complete.cases(.)) %>%
  group_by(group) %>%
  summarise(perc_cd2 = mean(cd_2_or_higher_num * 100),
            sd_perc_cd2 = sd(cd_2_or_higher_num),
            count = n())

comp_bc %>%
  group_by(group) %>%
  count(cd_2_or_higher)

## Statistically test differences in cd_2_or_higher and cci between control-int
### cci
comp_bc %>%
  plot_normality(cci) #poisson distribution? (count data)
check_normality(comp_bc$cci)

ggbetweenstats(
  data = comp_bc,
  x = group,
  y = cci,
  type = 'nonparametric'
)

### cd_2_or_higher
log_model <- glm(cd_2_or_higher ~ group, family = 'binomial', data = comp_bc) #fit logistic model
tab_model(log_model,
          show.reflvl = T,
          show.aic = T,
          p.style = "numeric_stars")
