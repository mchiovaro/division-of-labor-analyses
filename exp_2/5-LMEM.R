##### Experiment 2: Check H1-H4 #####
#
# Here, I test H1-H4 for experiment 2.
#
# Code by: @mchiovaro
# Last updated: 2023_04_10

##### 1. Set up #####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_2")
source("../libraries_and_functions.R")

# read in data
data = read.table('./data/mdrqa_and_conditions.csv', 
                       sep=',', header = TRUE)

#### 3. Standardize variables ####
data_st = data %>% select(c(4,17:22)) %>%
  mutate_all(~(scale(.) %>% as.vector))

###### 2. Construct the model for H1-H3 ######
completion.model = lmer(timer ~ 
              # fixed effects
              td_condition + ie_condition + 
              com_condition + round_number +
              td_condition*ie_condition +
              td_condition*com_condition +
              ie_condition*com_condition +
              td_condition*ie_condition*com_condition +
              # random effects of dyad
              (1 + td_condition | dyad),
            data = data_st, REML = FALSE)
pander_lme(completion.model,stats.caption = TRUE)

###### 2. Construct the model for H4 ######
coordination.model = lmer(DET ~ 
               # fixed effects
               td_condition + ie_condition + 
               com_condition + round_number +
               td_condition*ie_condition +
               td_condition*com_condition +
               ie_condition*com_condition +
               td_condition*ie_condition*com_condition +
               # random effects of dyad
               (1 + td_condition | dyad),
             data = data_st, REML = FALSE)
pander_lme(coordination.model,stats.caption = TRUE)
