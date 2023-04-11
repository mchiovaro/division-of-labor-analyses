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

#### 3. Standardize variables and create factors ####
data_st = data %>% 
  mutate(DET = scale(DET)) %>% 
  mutate(timer = scale(timer)) %>% 
  mutate(round_number = scale(round_number)) %>%
  mutate(dyad = as.factor(dyad)) %>%
  mutate(td_condition = as.factor(td_condition)) %>%
  mutate(ie_condition = as.factor(ie_condition)) %>%
  mutate(com_condition = as.factor(com_condition))
data = data %>%
  mutate(dyad = as.factor(dyad)) %>%
  mutate(td_condition = as.factor(td_condition)) %>%
  mutate(ie_condition = as.factor(ie_condition)) %>%
  mutate(com_condition = as.factor(com_condition))

###### 2. Construct the model for H1-H3 ######

# raw model
completion.model.raw = lmer(timer ~ 
              # fixed effects
              td_condition + ie_condition + 
              com_condition + round_number +
              td_condition*ie_condition +
              td_condition*com_condition +
              ie_condition*com_condition +
              td_condition*ie_condition*com_condition +
              # random effects of dyad
              (1 | dyad),
            data = data, REML = FALSE)
pander_lme(completion.model.raw,stats.caption = TRUE)

# standardized model
completion.model.st = lmer(timer ~ 
                              # fixed effects
                              td_condition + ie_condition + 
                              com_condition + round_number +
                              td_condition*ie_condition +
                              td_condition*com_condition +
                              ie_condition*com_condition +
                              td_condition*ie_condition*com_condition +
                              # random effects of dyad
                              (1 | dyad),
                            data = data_st, REML = FALSE)
pander_lme(completion.model.st,stats.caption = TRUE)

###### 2. Construct the model for H4 ######

# raw model
coordination.model.raw = lmer(DET ~ 
               # fixed effects
               td_condition + ie_condition + 
               com_condition + round_number +
               td_condition*ie_condition +
               td_condition*com_condition +
               ie_condition*com_condition +
               td_condition*ie_condition*com_condition +
               # random effects of dyad
               (1 | dyad),
             data = data, REML = FALSE)
pander_lme(coordination.model.raw,stats.caption = TRUE)

# standardized model
coordination.model.st = lmer(DET ~ 
                                # fixed effects
                                td_condition + ie_condition + 
                                com_condition + round_number +
                                td_condition*ie_condition +
                                td_condition*com_condition +
                                ie_condition*com_condition +
                                td_condition*ie_condition*com_condition +
                                # random effects of dyad
                                (1 | dyad),
                              data = data_st, REML = FALSE)
pander_lme(coordination.model.st,stats.caption = TRUE)
