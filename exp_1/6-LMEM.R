##### Experiment 1: Check H1-H4 #####
#
# Here, I test H1-H4.
#
# Code by: @mchiovaro
# Last updated: 2023_04_10

##### set up #####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_1")
source("../libraries_and_functions.R")

# read in data
shuffled_data = read.table('./data/shuffled_mdrqa_and_conditions.csv', 
                  sep=',', header = TRUE)
real_data = read.table('./data/mdrqa_and_conditions.csv', 
                       sep=',', header = TRUE)
relphase_freq_data = read.table('./data/freq_tables-exp_1.csv', 
                                sep=',', header = TRUE)

#### H1: More deterministic than chance ####

# add an indicator ("data") for real (.5) or surrogate (-.5) data ######
real_data$data <- .5
shuffled_data$data <- -.5

# specify variables to keep and bind the data
shared_variables = names(real_data)[names(real_data) %in% names(shuffled_data)]
shared_variables = shared_variables[c(4,17:20, 22:23)]
all_data = rbind.data.frame(dplyr::select(real_data,one_of(shared_variables)),
                                 dplyr::select(shuffled_data,one_of(shared_variables)))
real_data = select(real_data,one_of(shared_variables))

# standardize continuous variables to interpret effect sizes (Keith, 2005) and create factors for IVs
real_data = real_data %>%
  mutate(dyad = as.factor(dyad)) %>%
  mutate(td_condition = as.factor(td_condition)) %>%
  mutate(ie_condition = as.factor(ie_condition)) %>%
  select(-c(data))
real_data_st = real_data %>% 
  mutate(DET = scale(DET)) %>% 
  mutate(timer = scale(timer)) 
all_data = all_data %>%
  mutate(dyad = as.factor(dyad)) %>%
  mutate(td_condition = as.factor(td_condition)) %>%
  mutate(ie_condition = as.factor(ie_condition)) %>%
  mutate(data = as.factor(data))
all_data_st = all_data %>% 
  mutate(DET = scale(DET)) %>% 
  mutate(timer = scale(timer))

# construct the raw model
surrogate.model.raw = lmer(DET ~ 
                             # fixed effects
                             data + td_condition + ie_condition + round_number +
                             data*td_condition + data*ie_condition + data*round_number +
                             td_condition*ie_condition + td_condition*round_number + ie_condition*round_number +
                             data*td_condition*ie_condition + data*td_condition*round_number + data*ie_condition*round_number + td_condition*ie_condition*round_number + 
                             data*td_condition*ie_condition*round_number +
                             # maximal random effect structure allowable
                             (1 + td_condition | dyad),
                           data = all_data, REML = FALSE)
pander_lme(surrogate.model.raw,stats.caption = TRUE)

# construct the standardized model
surrogate.model.st = lmer(DET ~ 
                            # fixed effects
                            data + td_condition + ie_condition + round_number +
                            data*td_condition + data*ie_condition + data*round_number +
                            td_condition*ie_condition + td_condition*round_number + ie_condition*round_number +
                            data*td_condition*ie_condition + data*td_condition*round_number + data*ie_condition*round_number + td_condition*ie_condition*round_number + 
                            data*td_condition*ie_condition*round_number +
                            # maximal random effect structure allowable
                            (1 + td_condition | dyad),
                       data = all_data_st, REML = FALSE)
pander_lme(surrogate.model.st,stats.caption = TRUE)

#### H2: Task switching leads to lower completion times. #### 

# raw model
completion.model.raw = lmer(timer ~ 
                              # fixed effects
                              td_condition + ie_condition + round_number +
                              td_condition*ie_condition + td_condition*round_number + ie_condition*round_number +
                              td_condition*ie_condition*round_number + 
                              # random effects of dyad
                              (1 | dyad),
                        data = real_data, REML = FALSE)
pander_lme(completion.model.raw,stats.caption = TRUE)

# standrardized model
completion.model.st = lmer(timer ~ 
                             # fixed effects
                             td_condition + ie_condition + round_number +
                             td_condition*ie_condition + td_condition*round_number + ie_condition*round_number +
                             td_condition*ie_condition*round_number + 
                             # random effects of dyad
                             (1 | dyad),
                            data = real_data_st, REML = FALSE)
pander_lme(completion.model.st,stats.caption = TRUE)

#### H3: Task switching leads to more time in relative coordination. ####

# raw model
coordination.model.raw = lmer(DET ~ 
                            # fixed effects
                            td_condition + ie_condition + round_number +
                            td_condition*ie_condition + td_condition*round_number + ie_condition*round_number +
                            td_condition*ie_condition*round_number + 
                            # random effects of dyad
                            (1 + td_condition | dyad),
                        data = real_data, REML = FALSE)
pander_lme(coordination.model.raw,stats.caption = TRUE)

# standardized model
coordination.model.st = lmer(DET ~ 
                               # fixed effects
                               td_condition + ie_condition + round_number +
                               td_condition*ie_condition + td_condition*round_number + ie_condition*round_number +
                               td_condition*ie_condition*round_number + 
                               # random effects of dyad
                               (1 + td_condition | dyad),
                              data = real_data_st, REML = FALSE)
pander_lme(coordination.model.st,stats.caption = TRUE)

#### H4: In I.E. condition 2, P2 will adapt to (follow) P1 ####

# create orthogonal polynomials for determining leading-following (first-order) and sychrony (second-order).
raw_relphase = min(relphase_freq_data$Var1):max(relphase_freq_data$Var1)
relphase_vals = data.frame(raw_relphase)
phase_offset = (0-min(raw_relphase)) + 1
t = stats::poly((raw_relphase + phase_offset), 2)
relphase_vals[, paste("ot", 1:2, sep="")] = t[relphase_vals$raw_relphase + phase_offset, 1:2]

# join it to the original data table
relphase_freq_data = left_join(relphase_freq_data,relphase_vals, by = c("Var1" = "raw_relphase"))

# create factors for raw model
relphase_freq_data_raw = relphase_freq_data %>% ungroup() %>%
  mutate(dyad = as.factor(dyad)) %>%
  mutate(td_condition = as.factor(td_condition)) %>%
  mutate(ie_condition = as.factor(ie_condition))

# standardize variables and create factors
relphase_freq_data_st = relphase_freq_data_raw %>% 
  mutate(Var1 = scale(Var1)) %>%
  mutate(Freq = scale(Freq))

# raw model
relphase.gca.raw = lmer(Freq ~ 
                          # fixed effects
                          ie_condition + td_condition + ot1 + ot2 +
                          ie_condition*td_condition + ot1*ot2 + td_condition*ot1 + 
                          ie_condition*ot1 + td_condition*ot2 + ie_condition*ot2 +
                          td_condition*ie_condition*ot1 + td_condition*ie_condition*ot2 + td_condition*ot1*ot2 + ie_condition*ot1*ot2 +
                          td_condition*ie_condition*ot1*ot2 +
                          # random effect (ot1 or ot2?)
                          (1 + ot1 | dyad),
                        data=relphase_freq_data_raw, REML=FALSE)
pander_lme(relphase.gca.raw,stats.caption = TRUE)

# standardized model
relphase.gca.st = lmer(Freq ~ 
                         # fixed effects
                         ie_condition + td_condition + ot1 + ot2 +
                         ie_condition*td_condition + ot1*ot2 + td_condition*ot1 + 
                         ie_condition*ot1 + td_condition*ot2 + ie_condition*ot2 +
                         td_condition*ie_condition*ot1 + td_condition*ie_condition*ot2 + td_condition*ot1*ot2 + ie_condition*ot1*ot2 +
                         td_condition*ie_condition*ot1*ot2 +
                         # random effect
                         (1 + ot1 | dyad),
                        data=relphase_freq_data_st, REML=FALSE)
pander_lme(relphase.gca.st,stats.caption = TRUE)

