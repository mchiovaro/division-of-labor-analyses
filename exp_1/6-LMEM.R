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

###### 1. Add an indicator ("data") for real (.5) or surrogate (-.5) data ######
real_data$data<- .5
shuffled_data$data <- -.5
# specify variables to keep
shared_variables = names(real_data)[names(real_data) %in% names(shuffled_data)]
shared_variables = shared_variables[c(4,(17:23))]
# bind the data
all_data = rbind.data.frame(dplyr::select(real_data,one_of(shared_variables)),
                                 dplyr::select(shuffled_data,one_of(shared_variables)))

# standardize all variables to interpret effect sizes (Keith, 2005, *Multiple regression and beyond*).
all_data_st = all_data %>% mutate_all(~(scale(.) %>% as.vector))
real_data_st = real_data %>% select(one_of(shared_variables)) %>%
  select(-c(com_condition, data)) %>% # drop com_condition and data since not relevant here
  mutate_all(~(scale(.) %>% as.vector))

###### 2. Construct the model ######
surrogate.model = lmer(DET ~ 
                         # fixed effects
                         data + td_condition + ie_condition + round_number +
                         data*td_condition + data*ie_condition +
                         td_condition*ie_condition +
                         data*td_condition*ie_condition +
                         # random effects of dyad
                         (1 + td_condition + td_condition*ie_condition | dyad),
                       data = all_data_st, REML = FALSE)
pander_lme(surrogate.model,stats.caption = TRUE)

#### H2: Task switching leads to lower completion times. #### 
completion.model = lmer(timer ~ 
                         # fixed effects
                         td_condition + ie_condition + round_number +
                         td_condition*ie_condition +
                         # random effects of dyad
                         (1 + td_condition + td_condition*ie_condition | dyad),
                       data = real_data_st, REML = FALSE)
pander_lme(completion.model,stats.caption = TRUE)

#### H3: Task switching leads to more time in relative coordination. ####
coordination.model = lmer(DET ~ 
                          # fixed effects
                          td_condition + ie_condition + round_number +
                          td_condition*ie_condition +
                          # random effects of dyad
                          (1 + td_condition + td_condition*ie_condition | dyad),
                        data = real_data_st, REML = FALSE)
pander_lme(coordination.model,stats.caption = TRUE)

#### H4: In I.E. condition 2, P2 will adapt to (follow) P1 ####

# create orthogonal polynomials for determining leading-following (first-order) and sychrony (second-order).
raw_relphase = min(relphase_freq_data$Var1):max(relphase_freq_data$Var1)
relphase_vals = data.frame(raw_relphase)
phase_offset = (0-min(raw_relphase)) + 1
t = stats::poly((raw_relphase + phase_offset), 2)
relphase_vals[, paste("ot", 1:2, sep="")] = t[relphase_vals$raw_relphase + phase_offset, 1:2]

# join it to the original data table
relphase_freq_data = left_join(relphase_freq_data,relphase_vals, by = c("Var1" = "raw_relphase"))

# create interaction terms
relphase_freq_data = relphase_freq_data %>% ungroup() %>%
  
  # primary interaction
  mutate(ie.td = ie_condition * td_condition) %>%
  
  # first-order polynomials
  mutate(ie.ot1 = ie_condition * ot1) %>%
  mutate(td.ot1 = td_condition * ot1) %>%
  mutate(ie.td.ot1 = ie_condition * td_condition * ot1) %>%
  
  # second-order polynomials
  mutate(ie.ot2 = ie_condition * ot2) %>%
  mutate(td.ot2 = td_condition * ot2) %>%
  mutate(ie.td.ot2 = ie_condition * td_condition * ot2) %>%
  
  # polynomial interactions
  mutate(ot1.ot2 = ot1 * ot2) %>%
  mutate(ie.ot1.ot2 = ie_condition * ot1 * ot2) %>%
  mutate(td.ot1.ot2 = td_condition * ot1 * ot2) %>%
  mutate(ie.td.ot1.ot2 = ie_condition * td_condition * ot1 * ot2)

# standardize all variables to interpret effect sizes (Keith, 2005, *Multiple regression and beyond*).
relphase_freq_data_st = relphase_freq_data %>% mutate_all(~(scale(.) %>% as.vector))

# standardized maximal random-effects model
relphase_gca_st = lmer(Freq ~ ie_condition + td_condition + ot1 + ot2 +
                                      ie.td + ot1.ot2 +
                                      td.ot1 + ie.ot1 + ie.td.ot1 +
                                      td.ot2 + ie.ot2 + ie.td.ot2 +
                                      td.ot1.ot2 + ie.ot1.ot2 + ie.td.ot1.ot2 +
                                      (1 + ot1 + ot2 + ie_condition | dyad),
                                    data=relphase_freq_data_st, REML=FALSE)
pander_lme(relphase_gca_st,stats.caption = TRUE)

