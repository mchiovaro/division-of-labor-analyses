##### Check radii #####
#
# Check how the radius search went and see if we need to expand anywhere.
# 
# Code by: @mchiovaro
# Last updated: 2023_02_15

#### 1. set up ####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_2")

# read in data
data = read.table('./data/data_prepped-exp_2.csv', 
                  sep=',', header = TRUE)

# read radii file names
radius_files_all = list.files('./data/radii', 
                                  pattern='radii*',
                                  full.names = TRUE)

# bind radii files and filter for best radius estimate
radius_stats_all = rbind.data.frame(rbindlist(lapply(radius_files_all, fread, sep=","))) %>%
  ungroup() %>%
  group_by(chosen.participant) %>%
  dplyr::filter(from.target==min(from.target)) %>%
  dplyr::arrange(chosen.participant) %>%
  dplyr::rename(dyad.round = chosen.participant) %>%
  filter(row_number()==n())

# identify dyad.rounds that are still too far from target rr and save
recheck_radii = radius_stats_all %>% ungroup() %>%
  dplyr::filter(from.target > .5) %>%
  dplyr::select(dyad.round, chosen.radius, rr, from.target)
write.table(recheck_radii,'./data/dropped_dyad.rounds.csv', sep=',',row.names=FALSE,col.names=TRUE)

# filter and save the final radii
radii_final = radius_stats_all %>% ungroup() %>%
  dplyr::filter(from.target < .5) %>%
  dplyr::select(dyad.round, chosen.radius, rr, from.target)
data_with_radii = full_join(x=radii_final,
                  y=data, by=c("dyad.round"="dyad.round"))
write.table(data_with_radii,'./data/data_with_radii.csv', sep=',',row.names=FALSE,col.names=TRUE)
