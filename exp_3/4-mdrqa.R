##### 4. Check radii #####
#
# Run MdRQA with identified radii for experiment 3.
# 
# Code by: @mchiovaro
# Last updated: 2023_04_14

#### 1. set up ####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_3")

# read in data
data = read.table('./data/data_with_radii.csv', 
                  sep=',', header = TRUE)

# use the MdRQA file
source(file = "mdrqa.R")

# slice up the data so that we have one dataset per dyad-round
split_rounds = split(data,
                     list(data$dyad.round))

# remove the dyad.rounds that had too high rr
split_rounds[c(38, 63, 138)] = NULL

# empty frame for results
mdrqa_results = data.frame()

# set rescaling type
rescale_type = 'mean'

for (next_round in split_rounds){
  
  # isolate parameters for next dyad.round
  chosen.delay = 1
  chosen.embed = 1
  chosen.radius = unique(next_round$chosen.radius)
  
  # print update
  print(paste("CRQA: dyad.round ", unique(next_round$dyad.round)))
  
  # create sine and cosine
  next_round = next_round %>%
    mutate(sin = sin(cluster_phase_rad)) %>%
    mutate(cos = cos(cluster_phase_rad)) 
  
  # rescale data by mean or max
  if (rescale_type == 'mean'){
    sin = next_round$sin / mean(next_round$sin)
    cos = next_round$cos / mean(next_round$cos)
  } else if (rescale_type == 'max'){
    sin = next_round$sin / max(next_round$sin)
    cos = next_round$cos / max(next_round$cos)
  }
  
  # run MdRQA
  rec_analysis = mdrqa(data = as.matrix(sin, cos), 
                       emb = 1, # standard for MdRQA
                       del = 1, # standard for MdRQA
                       norm = "euc", 
                       rad = next_round$chosen.radius)
  
  # save to dataframe
  next_data_line = data.frame(c(unique(next_round$dyad.round),
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