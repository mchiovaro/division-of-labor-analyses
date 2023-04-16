##### 4. Conduct phase-randomization for baseline #####
#
# Here, I conduct phase-randomization for comparing
# the actual MdRQA metrics to a baseline.
# With thanks to: https://github.com/a-paxton/dual-conversation-constraints/blob/master/dual-conversation-constraints.Rmd
#
# Code by: @mchiovaro
# Last updated: 2023_04_14

#### 1. set up ####

# grab the array id value from the environment variable passed from sbatch
slurm_arrayid = Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
n = as.numeric(slurm_arrayid)

# set seed for reproducibility 
set.seed(2023)

# read in data
data = read.table('./data/data_with_radii.csv', 
                  sep=',', header = TRUE)

# load libraries
library(dplyr)

# use the MdRQA and FFTsurrogate files
source(file = "mdrqa.R")
source(file = "surrogate.data.R")

#### 2. Calculate MdRQA for surrogate data ####

# set up
mdrqa_results = data.frame()
dyad.rounds <- unique(data$dyad.round)

# select next dyad.round
chosen.participant = dyad.rounds[n]

# subset the data and drop NAs
next.participant = data %>%
  dplyr::filter(dyad.round == chosen.participant)
# drop rows before the group phase statistic can be calculated
next.participant = next.participant[complete.cases(next.participant$rel_phase), ]

# isolate radius
chosen.radius = unique(next.participant$chosen.radius)
dyad = unique(next.participant$dyad)
round_number = unique(next.participant$round_number)

# re-create the surrogate dyad and run again
for (i in 1:10){
  
  print(paste0("surrogate = ", i))
  
  # create phase-randomized baseline for each participant
  shuffle0 = FFTsurrogate(next.participant$sin, n.samples = 1)
  shuffle1 = FFTsurrogate(next.participant$cos, n.samples = 1)
 
  # run MdRQA
  rec_analysis = mdrqa(data = as.matrix(shuffle0, shuffle1), 
                       emb = 1, # standard for MdRQA
                       del = 1, # standard for MdRQA
                       norm = "euc", 
                       rad = chosen.radius)
  
  # save individual radius calculations
  write.table(cbind.data.frame(dyad,
                               round_number,
                               i,
                               rec_analysis[1:15]),
              paste('./data/phase_randomization/',dyad,'.',round_number,'-',i,'.csv', sep=''),
              sep=',',row.names=FALSE,col.names=TRUE)

}

# #### 3. Merge with other experiment data and save ####
# 
# # load in files
# phase_random_files = list.files('./data/phase_randomization/',
#                           full.names = TRUE)
# 
# # bind the files
# phase_random_all = rbind.data.frame(rbindlist(lapply(phase_random_files, fread, sep=","))) %>%
#   ungroup()
# 
# # grab information about experiment condition
# additional_dyad_info = data %>% ungroup() %>%
#   select(dyad.round,dyad,round_number,td_condition,ie_condition,com_condition,timer,chosen.radius) %>%
#   distinct()
# 
# # merge recurrence analyses and condition information
# shuffled_recurrence_df = plyr::join(phase_random_all, additional_dyad_info,
#                            by=c('dyad','round_number'))
# # save to file
# write.table(shuffled_recurrence_df,'./data/shuffled_mdrqa_and_conditions.csv',sep=',')
