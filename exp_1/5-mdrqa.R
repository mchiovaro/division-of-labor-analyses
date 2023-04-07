##### 5. Calculate MdRQA results for actual data #####
#
# Calculate MdRQA results and save.
# 
# Code by: @mchiovaro
# Last updated: 2023_03_22

#### 1. set up ####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_1")

# read in data
data = read.table('./data/data_with_radii.csv', 
                  sep=',', header = TRUE)

# use the MdRQA file
source(file = "mdrqa.R")

# install and load packages
# install.packages("nonlinearTseries")
library(nonlinearTseries)

#### 2. Calculate MdRQA for real data ####

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
  next.round = data %>%
    dplyr::filter(dyad.round == chosen.participant)
  
  # isolate radius
  chosen.radius = unique(next.round$chosen.radius)
  
  # rescale data by mean or max
  if (rescale_type == 'mean'){
    sin = next.round$sin / mean(next.round$sin)
    cos = next.round$cos / mean(next.round$cos)
  } else if (rescale_type == 'max'){
    sin = next.round$sin / max(next.round$sin)
    cos = next.round$cos / max(next.round$cos)
  } 
  
  # print update
  print(paste("CRQA: dyad.round ", unique(next.round$dyad.round)))
  
  # run MdRQA
  rec_analysis = mdrqa(data = as.matrix(sin, cos), 
                       emb = 1, # standard for MdRQA
                       del = 1, # standard for MdRQA
                       norm = "euc", 
                       rad = next.round$chosen.radius)
  
  # save to dataframe
  next_data_line = data.frame(c(unique(next.round$dyad.round),
                                rec_analysis[1:15]))
  names(next_data_line) = c("dyad.round",names(rec_analysis[1:15]))
  mdrqa_results = rbind.data.frame(mdrqa_results,next_data_line)
  
}

# grab information about experiment condition
additional_dyad_info = data %>% ungroup() %>%
  select(dyad.round,dyad,round_number,td_condition,ie_condition,com_condition,timer) %>% 
  distinct()

# merge recurrence analyses and condition information
recurrence_df = plyr::join(mdrqa_results, additional_dyad_info,
                           by=c('dyad.round'))
# save to file
write.table(recurrence_df,'./data/mdrqa_and_conditions.csv',sep=',')
