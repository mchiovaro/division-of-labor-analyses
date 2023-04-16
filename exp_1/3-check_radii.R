##### 3. Check radii #####
#
# Check how the radius search went and see if we need to expand anywhere.
# 
# Code by: @mchiovaro
# Last updated: 2023_02_23

#### 1. set up ####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_1")

# read in data
data = read.table('./data/data_prepped-exp_1.csv', 
                  sep=',', header = TRUE)

# load in files
radius_files = list.files('./data/radii', 
                          pattern='radii*',
                          full.names = TRUE)

#### 2. Double check and save final radii ####

# identify the target radii 
radius_stats_all = rbind.data.frame(rbindlist(lapply(radius_files, fread, sep=","))) %>%
  ungroup() %>%
  group_by(chosen.participant) %>%
  dplyr::filter(from.target==min(from.target)) %>%
  dplyr::arrange(chosen.participant) %>%
  dplyr::rename(dyad.round = chosen.participant) %>%
  filter(row_number()==n())

# check whether some rounds are still too far from our target recurrence rate
recheck_radii = radius_stats_all %>% ungroup() %>%
  dplyr::filter(from.target > .5) %>%
  dplyr::select(dyad.round, chosen.radius,rr, from.target)

# filter and save the final radii
radii_final = radius_stats_all %>% ungroup() %>%
  dplyr::filter(from.target < .5) %>%
  dplyr::select(dyad.round, chosen.radius, rr, from.target)
data_with_radii = full_join(x=radii_final,
                            y=data, by=c("dyad.round"="dyad.round"))
write.table(data_with_radii,'./data/data_with_radii.csv', sep=',',row.names=FALSE,col.names=TRUE)
