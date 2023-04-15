#! /usr/bin/env Rscript

#### Parallelizing RQA radius search ####

# With thanks to https://github.com/FredHutch/slurm-examples/blob/master/centipede/example.R
# and https://sph.umich.edu/biostat/computing/cluster/examples/r.html
# and https://github.com/dyscord-lab/SA_Analysis/blob/master/scripts/test_one_crqa_radius.R.

# Current version performs a nested radius search, gradually narrowing in to 
# reduce computational load and number of subsequent manual searches required.
#
# Code by: @mchiovaro
# Last updated: 2023_02_22

#### 1. Set up ####

# grab the array id value from the environment variable passed from sbatch
slurm_arrayid = Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
n = as.numeric(slurm_arrayid)

# set seed for reproducibility
set.seed(2023)

# load libraries
library(dplyr)

# use the MdRQA file
source(file = "mdrqa.R")

# read the data
data = read.table('./data/data_prepped-exp_3.csv', 
                  sep=',', header = TRUE)
# drop rows before the group phase statistic can be calculated
data = data[complete.cases(data$mean_team_sync), ]

# identify what participant-round we're working with
dyad.rounds <- unique(data$dyad.round)
chosen.participant = dyad.rounds[n]
print(paste("Chosen dyad.round: ", chosen.participant))

# subset the data
next.participant = data %>%
  dplyr::filter(dyad.round == chosen.participant)
#rm(data) # get rid of the big data set

# create list of radii to search
next.radius = as.data.frame(seq(1e-50, .9, by=.1))
colnames(next.radius) = c("chosen.radius")

# set target rr (smallest possible rr while under target.rr from exp 2 [11.597%])
target.rr = 5

# set target distance as large so the loop starts the search
from.target = 99
from.target.last = 100
rr = 0

# create an empty dataframe for radii calculations
radius_selection = data.frame(chosen.participant = numeric(),
                              chosen.radius = numeric(),
                              rr = numeric(),
                              from.target = numeric())

#### 2. Perform first high-level radius search ####

# for each of the radii in the grid, perform MdRQA and save rr
for(i in 1:nrow(next.radius)) {

  # so long as the target is too far, continue to check the next radius
  if(from.target <= from.target.last & rr < target.rr) {

    # update last target distance for comparison
    from.target.last = from.target
    rr.last = rr
    chosen.radius = next.radius$chosen.radius[i]
    if(i>1){ 
      chosen.radius.last = next.radius$chosen.radius[i-1] 
    } else {
      chosen.radius.last = 0
    }
    
    # run MdRQA and grab recurrence rate (REC)
    rec_analysis = mdrqa(data = as.matrix(next.participant$sin, next.participant$cos), 
                         emb = 1, # standard for MdRQA
                         del = 1, # standard for MdRQA
                         norm = "euc", 
                         rad = chosen.radius)
    rr = rec_analysis$REC
    
    # clear it so we don't take up too much memory
    rm(rec_analysis)
    
    # identify how far off the RR is from our target (target.rr)
    from.target = abs(rr - target.rr)
    print(paste("from.target :", from.target))
    
    # append to data frame
    radius_selection = rbind.data.frame(radius_selection,
                                        cbind.data.frame(chosen.participant,
                                                         chosen.radius,
                                                         rr,
                                                         from.target))

    # save individual radius calculations
    write.table(cbind.data.frame(chosen.participant,
                                 next.radius$chosen.radius[i],
                                 rr,
                                 from.target),
                paste('./data/radii/radii',next.radius$chosen.radius[i],'-',unique(next.participant$dyad),'_',unique(next.participant$round_number),'.csv', sep=''),
                sep=',',row.names=FALSE,col.names=TRUE)
      
  }
  
}

#### 3. Perform additional searches at finer granularity ####

# set parameter for narrowing in on the radius by a tenth each time
exponents = c(1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8)

# if we still haven't gotten close enough to target.rr
for(k in 1:length(exponents)) {
  
  # create new radius list between the two last searched radii
  narrower.radii = seq(chosen.radius.last, chosen.radius, by=exponents[k])
  
  # continue searching if we still aren't within bounds
  if(from.target > .05 & chosen.radius.last != 0) {
    
    # set target distance as large so the loop starts the search
    from.target = 99
    from.target.last = 100
    rr = 0
    
    # check each of the new radii in narrow.radii
    for(i in 1:length(narrower.radii)) {
      
      # so long as the target is still too far (but getting closer), continue to check the next radius
      # add a cut off of being within .05 so that we don't run over calculation time
      if(from.target <= from.target.last & rr < target.rr) {
        
        # update last target distance for comparison
        from.target.last = from.target
        rr.last = rr
        chosen.radius = narrower.radii[i]
        if(i>1) { chosen.radius.last = narrower.radii[i-1] 
        } else {
          chosen.radius.last = 0
        }
        
        # run MdRQA and grab recurrence rate (REC)
        rec_analysis = mdrqa(data = as.matrix(next.participant$sin, next.participant$cos), 
                             emb = 1, # standard for MdRQA
                             del = 1, # standard for MdRQA
                             norm = "euc", 
                             rad = chosen.radius)
        rr = rec_analysis$REC
        
        # clear it so we don't take up too much memory
        rm(rec_analysis)
        
        # identify how far off the RR is from our target (target.rr)
        from.target = abs(rr - target.rr)
        print(from.target)
        
        # append to data frame
        radius_selection = rbind.data.frame(radius_selection,
                                            cbind.data.frame(chosen.participant,
                                                             chosen.radius,
                                                             rr,
                                                             from.target))
        
        # save individual radius calculations
        write.table(cbind.data.frame(chosen.participant,
                                     chosen.radius,
                                     rr,
                                     from.target),
                    paste('./data/radii/radii',chosen.radius,'-',unique(next.participant$dyad),'_',unique(next.participant$round_number),'.csv', sep=''),
                    sep=',',row.names=FALSE,col.names=TRUE)
        
      }
      
    } 
    
  }
    
}
