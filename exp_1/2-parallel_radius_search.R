#! /usr/bin/env Rscript

#### Parallelizing RQA radius search ####

# With thanks to https://github.com/FredHutch/slurm-examples/blob/master/centipede/example.R
# and https://sph.umich.edu/biostat/computing/cluster/examples/r.html
# and https://github.com/dyscord-lab/SA_Analysis/blob/master/scripts/test_one_crqa_radius.R.

# grab the array id value from the environment variable passed from sbatch
slurm_arrayid = Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
n = as.numeric(slurm_arrayid)

# load libraries
library(dplyr)
# install.packages("remotes")
# remotes::install_version("SDMTools", "1.1-221")

# use the MdRQA file
source(file = "mdrqa.R")

# read the data
data = read.table('./data/data_prepped-exp_1.csv', 
                  sep=',', header = TRUE)


# identify radius for calculations
radius.list = seq(.002, .009, by=.00000001)

# create a grid
radius_grid_search = expand.grid(radius.list,
                                 unique(data$dyad.round))

# identify what set we're doing right now
chosen.radius = as.numeric(radius_grid_search[n,1])
print('Chosen radius: ')
print(chosen.radius)
chosen.participant = radius_grid_search[n,2]
print('Chosen participant: ')
print(chosen.participant)

# subset the data
next.participant = data %>%
  dplyr::filter(dyad.round == chosen.participant)
rm(data) # get rid of the big data set

# run MdRQA and grab recurrence rate (REC)
rec_analysis = mdrqa(data = as.matrix(next.participant$sin, next.participant$cos), 
                     emb = 1, # standard for MdRQA
                     del = 1, # standard for MdRQA
                     norm = "euc", 
                     rad = chosen.radius)
rr = rec_analysis$REC

# clear it so we don't take up too much memory
rm(rec_analysis)

# identify how far off the RR is from our target (5%)
from.target = abs(rr - 5)
print(rr)
print(from.target)

# save individual radius calculations
write.table(cbind.data.frame(chosen.participant,
                             chosen.delay,
                             chosen.embed,
                             chosen.radius,
                             rr,
                             det,
                             meanL,
                             from.target),
            paste('./data/radii/radii',chosen.radius,'-',dyad,'_',round_number,'.csv', sep=''),
            sep=',',row.names=FALSE,col.names=TRUE)
