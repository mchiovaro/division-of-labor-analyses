##### 4. Conduct phase-randomization for baseline #####
#
# Here, I conduct phase-randomization for comparing
# the actual MdRQA metrics to a baseline.
# With thanks to: https://github.com/a-paxton/dual-conversation-constraints/blob/master/dual-conversation-constraints.Rmd
#
# Code by: @mchiovaro
# Last updated: 2023_03_14

#### 1. set up ####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_1")

# read in data
data = read.table('./data/data_with_radii.csv', 
                  sep=',', header = TRUE)

# use the MdRQA file
source(file = "mdrqa.R")

# install and load packages
install.packages("nonlinearTseries")
library(nonlinearTseries)

#### 2. Calculate MdRQA for surrogate data ####

# set up
mdrqa_results = data.frame()
dyad.rounds <- unique(data$dyad.round)

# set rescale type to mean
rescale_type = 'mean'

# cycle through each dyad-round
for (n in 1:length(dyad.rounds)){
  
  # select next dyad.round
  chosen.participant = dyad.rounds[n]
  
  # subset the data
  next.participant = data %>%
    dplyr::filter(dyad.round == chosen.participant)
  
  # isolate radius
  chosen.radius = unique(next.participant$chosen.radius)
  
  # rescale data by mean or max
  if (rescale_type == 'mean'){
    sin = next.participant$sin / mean(next.participant$sin)
    cos = next.participant$cos / mean(next.participant$cos)
  } else if (rescale_type == 'max'){
    sin = next.participant$sin / max(next.participant$sin)
    cos = next.participant$cos / max(next.participant$cos)
  }
  
  # re-create the surrogate dyad and run again
  for (i in 1:10){
    
    # create phase-randomized baseline for each participant
    shuffle0 = nonlinearTseries::FFTsurrogate(sin, n.samples = 1)
    shuffle1 = nonlinearTseries::FFTsurrogate(cos, n.samples = 1)
   
    # run MdRQA
    rec_analysis = mdrqa(data = as.matrix(shuffle0, shuffle1), 
                         emb = 1, # standard for MdRQA
                         del = 1, # standard for MdRQA
                         norm = "euc", 
                         rad = chosen.radius)
    
    # save to dataframe
    next_data_line = data.frame(c(i,
                                  chosen.participant,
                                  rec_analysis[1:15]))
    names(next_data_line) = c('run',"dyad.round",names(rec_analysis[1:15]))
    mdrqa_results = rbind.data.frame(mdrqa_results,next_data_line)

  }

}

#### 3. Merge with other experiment data and save ####

# grab information about experiment condition
additional_dyad_info = data %>% ungroup() %>%
  select(dyad.round,dyad,round_number,td_condition,ie_condition,com_condition,timer,chosen.radius) %>% 
  distinct()

# merge recurrence analyses and condition information
shuffled_recurrence_df = plyr::join(mdrqa_results, additional_dyad_info,
                           by=c('dyad.round'))
# save to file
write.table(shuffled_recurrence_df,'./data/shuffled_mdrqa_and_conditions.csv',sep=',')
