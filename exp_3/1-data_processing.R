##### Process data #####
#
# Processing data for experiment 3: 
# - Trimming files which had restarts and behavior issues.
# - Re-scaling player data to match on x-axis.
# - Calculating player phases from 0-360 and turning to radians.
# - Calculating cluster phase, player-team relative phase, and whole-team synchrony.
# - Add in task switching markers for each player.
# 
# Code by: @mchiovaro
# Last updated: 2023_03_17

#### 1. set up ####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_3")
# install.packages("pracma")
library(pracma) # for deg2rad function
set.seed(2023)

#### 2. read in raw data  ####
d46 <- read.csv("./data/raw/46-movementdata.csv", header=FALSE) 
d47 <- read.csv("./data/raw/47-movementdata.csv", header=FALSE)
d48 <- read.csv("./data/raw/48-movementdata.csv", header=FALSE)
d49 <- read.csv("./data/raw/49-movementdata.csv", header=FALSE)
d50 <- read.csv("./data/raw/50-movementdata.csv", header=FALSE)
d51 <- read.csv("./data/raw/51-movementdata.csv", header=FALSE)
d54 <- read.csv("./data/raw/54-movementdata.csv", header=FALSE)
d55 <- read.csv("./data/raw/55-movementdata.csv", header=FALSE)
d56 <- read.csv("./data/raw/56-movementdata.csv", header=FALSE)
d57 <- read.csv("./data/raw/57-movementdata.csv", header=FALSE)
d58 <- read.csv("./data/raw/58-movementdata.csv", header=FALSE)
d59 <- read.csv("./data/raw/59-movementdata.csv", header=FALSE)
d60 <- read.csv("./data/raw/60-movementdata.csv", header=FALSE)
d61 <- read.csv("./data/raw/61-movementdata.csv", header=FALSE)
d62 <- read.csv("./data/raw/62-movementdata.csv", header=FALSE)
d63 <- read.csv("./data/raw/63-movementdata.csv", header=FALSE)
d64 <- read.csv("./data/raw/64-movementdata.csv", header=FALSE)
d66 <- read.csv("./data/raw/66-movementdata.csv", header=FALSE)
d67 <- read.csv("./data/raw/67-movementdata.csv", header=FALSE)
d68 <- read.csv("./data/raw/68-movementdata.csv", header=FALSE)
d69 <- read.csv("./data/raw/69-movementdata.csv", header=FALSE)
d70 <- read.csv("./data/raw/70-movementdata.csv", header=FALSE)
d71 <- read.csv("./data/raw/71-movementdata.csv", header=FALSE)
d72 <- read.csv("./data/raw/72-movementdata.csv", header=FALSE)
d73 <- read.csv("./data/raw/73-movementdata.csv", header=FALSE)
d74 <- read.csv("./data/raw/74-movementdata.csv", header=FALSE)
d75 <- read.csv("./data/raw/75-movementdata.csv", header=FALSE)
d76 <- read.csv("./data/raw/76-movementdata.csv", header=FALSE)
d77 <- read.csv("./data/raw/77-movementdata.csv", header=FALSE)
d78 <- read.csv("./data/raw/78-movementdata.csv", header=FALSE)
d80 <- read.csv("./data/raw/80-movementdata.csv", header=FALSE)
d81 <- read.csv("./data/raw/81-movementdata.csv", header=FALSE)
d82 <- read.csv("./data/raw/82-movementdata.csv", header=FALSE)
d86 <- read.csv("./data/raw/86-movementdata.csv", header=FALSE)
d87 <- read.csv("./data/raw/87-movementdata.csv", header=FALSE)
d88 <- read.csv("./data/raw/88-movementdata.csv", header=FALSE)

# rename variables
dfs <- list(d46, d47, d48, d49, d50, d51, d54, d55, d56, d57, d58, d59, 
            d60, d61, d62, d63, d64, d66, d67, d68, d69, d70, d71, d72, 
            d73, d74, d75, d76, d77, d78, d80, d81, d82, d86, d87, d88)

changenames <- function(x) {
  names(x) <- c("System.DateTime.Now","Time","ParticipantNumber",
                "round_number","ie_condition","td_condition[round_number]",
                "com_condition","size_condition","experiment_num",
                "beeFree.transform.position.x","beeFree.transform.position.y",
                "beeRestrict.transform.position.x","beeRestrict.transform.position.y", 
                "beeFree2.transform.position.x","beeFree2.transform.position.y",
                "beeRestrict2.transform.position.x","beeRestrict2.transform.position.y")
  return(x)
}
dfs <- lapply(dfs, changenames)

# split them out
d46 <- dfs[[1]]
d47 <- dfs[[2]]
d48 <- dfs[[3]]
d49 <- dfs[[4]]
d50 <- dfs[[5]]
d51 <- dfs[[6]]
d54 <- dfs[[7]]
d55 <- dfs[[8]]
d56 <- dfs[[9]]
d57 <- dfs[[10]]
d58 <- dfs[[11]]
d59 <- dfs[[12]]
d60 <- dfs[[13]]
d61 <- dfs[[14]]
d62 <- dfs[[15]]
d63 <- dfs[[16]]
d64 <- dfs[[17]]
d66 <- dfs[[18]]
d67 <- dfs[[19]]
d68 <- dfs[[20]]
d69 <- dfs[[21]]
d70 <- dfs[[22]]
d71 <- dfs[[23]]
d72 <- dfs[[24]]
d73 <- dfs[[25]]
d74 <- dfs[[26]]
d75 <- dfs[[27]]
d76 <- dfs[[28]]
d77 <- dfs[[29]]
d78 <- dfs[[30]]
d80 <- dfs[[31]]
d81 <- dfs[[32]]
d82 <- dfs[[33]]
d86 <- dfs[[34]]
d87 <- dfs[[35]]
d88 <- dfs[[36]]

#### 3. Check and trim data as needed ####

# trim data with re-starts
d46_filt <- d46 %>% filter(Time > 347.56) #r1 restarted 
d47_filt <- d47 %>% filter(Time < 397.38 | Time > 448.22) # r4 restarted 
d47_filt <- d47_filt %>% filter(Time < 556.46 | Time > 595.24) # r5 restarted 
d48_filt <- d48 %>% filter(round_number != 2) %>% # r2 removed entirely
  filter(Time < 1002.32 | Time > 1034) # r3 restarted 
d48_filt <- d48_filt %>% filter(Time < 1134.04 | Time > 1192.48) # r4 restarted 
d48_filt <- d48_filt %>% filter(round_number != 5) # r5 removed entirely
d49_filt <- d49 %>% filter(Time > 181.32) # r1 restarted 
d54_filt <- d54 %>% filter(Time < 1090.7 | Time > 1111.4) # r4 restarted 
d54_filt <- d54_filt %>% filter(Time < 1231.72 | Time > 1241.28) # r5 restarted 
d56_filt <- d56 %>% filter(Time > 386.86) # r1 restarted 2x 
d56_filt <- d56_filt %>% filter(Time < 568.32 | Time > 600.54) # r3 restarted 
d56_filt <- d56_filt %>% filter(Time < 816.8 | Time > 835.36) # r6 restarted  
d57_filt <- d57 %>% filter(Time < 329.3 | Time > 345.7) # r2 restarted 
d58_filt <- d58 %>% filter(Time < 1047.16 | Time > 1100.58) # r2 restarted 
d59_filt <- d59 %>% filter(!(Time >= 172.48  & Time <= 252.54 & round_number == 1)) # r2 glitched out; keep first r1 
d59_filt <- d59_filt %>% filter(!(Time >= 1397.54 & Time <= 1416.82 & round_number == 2)) # r2 glitched out; keep second r2 
d62_filt <- d62 %>% filter(round_number != 3) # remove entire round 3 
d63_filt <- d63 %>% filter(Time < 275.1 | Time > 333.06) # r2 restarted 
d66_filt <- d66 %>% filter(round_number != 1) # remove entire round 1
d66_filt <- d66_filt %>% filter(Time < 1389.64 | Time > 1421.72) # restarted after r2 # keep second r2
d66_filt <- d66_filt %>% filter(Time < 300.78 | Time > 333.74) # r4 restarted
d68_filt <- d68 %>% filter(Time < 1721.1 | Time > 1778.82) # r6 restarted
d71_filt <- d71 %>% filter(Time < 1130.38 | Time > 1151.92) # r2 restarted
d77_filt <- d77 %>% filter(Time > 1157.46) # r1 restarted
d77_filt <- d77_filt %>% filter(Time < 1328.24 | Time > 1463.94) # r3 restarted 2x
d77_filt <- d77_filt %>% filter(Time < 1670.9 | Time > 1708.22) # r6 restarted
d69_filt <- d69 %>% filter(Time < 69.98 | Time > 166.08) # r3 whole game restarted bc glitch # remove second r1 and r2
d69_filt <- d69_filt %>% filter(Time < 1268.32 | Time > 1287.6) # remove first r3
d73_filt <- d73 %>% filter(Time > 1463.1) # r1 restarted multiple times
d73_filt <- d73_filt %>% filter(Time < 1584.84 | Time > 1636.3) # r2 restarted
d74_filt <- d74 %>% filter(Time < 1449.36 | Time > 1512.34) # r4 restarted
d75_filt <- d75 %>% filter(Time < 1072.8) # r1 restarted multiple times
d75_filt <- d75_filt %>% filter(Time > 115.48) # r1 restarted multiple times
d76_filt <- d76 %>% filter(round_number != 1) # r1 removed entirely due to behavioral issue
d81_filt <- d81 %>% filter(Time > 480.74) # r1 restarted 
d81_filt <- d81_filt %>% filter(Time < 835.14 | Time > 887.12) # r5 restarted
d80_filt <- d80 %>% filter(Time < 293.46 | Time > 382.32) # r2 restarted
d86_filt <- d86 %>% filter(Time > 945.8) # r1 restarted
d86_filt <- d86_filt %>% filter(Time < 1132.76 | Time > 1160.1) # r3 restarted
d87_filt <- d87 %>% filter(Time < 965.48 | Time > 982.52) # r2 restarted
d88_filt <- d88 %>% filter(Time < 419.96 | Time > 464.66) # r3 restarted
d82_filt <- d82 %>% filter(Time < 1272.1 | Time > 1341.06) # r4 restarted 2x
d82_filt <- d82_filt %>% filter(Time < 1423.32 | Time > 1455.1) # r5 restarted
d82_filt <- d82_filt %>% filter(round_number != 6) # r6 removed entirely bc of glitch

# bind data
formatted_all <- rbind(d46_filt, d47_filt, d48_filt, d49_filt, d50, 
                       d51, d54_filt, d55, d56_filt, d57_filt, 
                       d58_filt, d59_filt, d60, d61, d62_filt, 
                       d63_filt, d64, d66_filt, d67, d68_filt, 
                       d69_filt, d70, d71_filt, d72, d73_filt, 
                       d74_filt, d75_filt, d76_filt, d77_filt, d78, 
                       d80_filt, d81_filt, d82_filt, d86_filt, d87_filt, 
                       d88_filt)

#### 4. Generate velocity and timer and rescale free player(s) x-axis ####
names(formatted_all) <- c("System.DateTime.Now","Time","dyad","round_number",
                          "ie_condition","td_condition","com_condition",
                          "size_condition","experiment_num",
                          "x1","y1","x2","y2",
                          "x3","y3","x4","y4")
data_prepped <- formatted_all %>% 
  group_by(dyad, round_number) %>%
  arrange(Time) %>%
  mutate(velocity1 = as.numeric((x1-lag(x1))/.02)) %>% # calculate velocity to remove times where players are just working
  mutate(velocity2 = as.numeric((x2-lag(x2))/.02)) %>% 
  mutate(velocity3 = as.numeric((x3-lag(x3))/.02)) %>% 
  mutate(velocity4 = as.numeric((x4-lag(x4))/.02)) %>% 
  mutate(round_number = as.numeric(round_number)) %>%
  filter(!(x1==-3.25 & y1==0 & x2==3.25 & y2==0 & x3==-3.25 & y3==0 & x4==3.25 & y4==0)) %>% # trim off the beginning before someone starts moving
  mutate(timer = max(Time) - min(Time)) %>% # calculate time to complete each round
  ungroup() %>%
  na.omit()

# create new variables and rescale beeFree x locations to match the directions of beeRestrict
data_prepped['rescale_free_x'] <- NA
data_prepped['rescale_free2_x'] <- NA
data_prepped['rescale_restrict_x'] <- NA
data_prepped['rescale_restrict2_x'] <- NA
for(i in 1:nrow(data_prepped)){
  if(data_prepped$x1[i] <= -.9) { data_prepped$rescale_free_x[i] = data_prepped$x1[i] + 6.5 
  } else { data_prepped$rescale_free_x[i] = data_prepped$x1[i] }  
  if(data_prepped$x3[i] <= -.9) { data_prepped$rescale_free2_x[i] = data_prepped$x3[i] + 6.5 
  } else { data_prepped$rescale_free2_x[i] = data_prepped$x3[i] } 
  
  if(data_prepped$x2[i] <= -.9) { data_prepped$rescale_restrict_x[i] = data_prepped$x2[i] + 6.5 
  } else { data_prepped$rescale_restrict_x[i] = data_prepped$x2[i] }  
  if(data_prepped$x4[i] <= -.9) { data_prepped$rescale_restrict2_x[i] = data_prepped$x4[i] + 6.5 
  } else { data_prepped$rescale_restrict2_x[i] = data_prepped$x4[i] } 
}

# save placeholder rescaled data to save time
write.table(x = data_prepped,
            file=paste0("./data/data_positive_x_vals-exp_3.csv"),
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

#### 5. Identify directions of movement ####

# create empty data frame
directions = data.frame(Time = numeric(),
                        dyad = numeric(),
                        round_number = numeric(),
                        x1 = numeric(),
                        x2 = numeric(),
                        x3 = numeric(),
                        x4 = numeric(),
                        # movement markers
                        left_free = numeric(),
                        right_free = numeric(),
                        left_free2 = numeric(),
                        right_free2 = numeric(),
                        left_restrict = numeric(),
                        right_restrict = numeric(),
                        left_restrict2 = numeric(),
                        right_restrict2 = numeric(),
                        # rescale to degrees variables
                        rescaled_free_right = numeric(),
                        rescaled_free_left = numeric(),
                        rescaled_free2_right = numeric(),
                        rescaled_free2_left = numeric(),
                        rescaled_restrict_right = numeric(),
                        rescaled_restrict_left = numeric(),
                        rescaled_restrict2_right = numeric(),
                        rescaled_restrict2_left = numeric(),
                        # degrees phase (continous)
                        free_phase = numeric(),
                        free2_phase = numeric(),
                        restrict_phase = numeric(),
                        restrict2_phase = numeric(),
                        # radian phase (continous)
                        free_phase_rad = numeric(),
                        free2_phase_rad = numeric(),
                        restrict_phase_rad = numeric(),
                        restrict2_phase_rad = numeric(),
                        # cluster phase variables (continous)
                        cluster_phase_complex = numeric(),
                        cluster_phase_rad = numeric(),
                        sin = numeric(),
                        cos = numeric(),
                        # relative phase for each participant to the cluster (continous)
                        rel_phase_free = numeric(),
                        rel_phase_free2 = numeric(),
                        rel_phase_restrict = numeric(),
                        rel_phase_restrict2 = numeric(),
                        # dyad.round marker
                        dyad.round = character(),
                        # mean complex relative phase (singular)
                        mean_rel_phase_complex_free = numeric(),
                        mean_rel_phase_complex_free2 = numeric(),
                        mean_rel_phase_complex_restrict = numeric(),
                        mean_rel_phase_complex_restrict2 = numeric(),
                        # mean radian relative phase (singluar)
                        mean_rel_phase_rad_free = numeric(),
                        mean_rel_phase_rad_free2 = numeric(),
                        mean_rel_phase_rad_restrict = numeric(),
                        mean_rel_phase_rad_restrict2 = numeric(),
                        # degree of synchrony for each player to the team (singular)
                        deg_sync_free = numeric(),
                        deg_sync_free2 = numeric(),
                        deg_sync_restrict = numeric(),
                        deg_sync_restrict2 = numeric(),
                        # whole team synchrony (continuous and singular)
                        continuous_team_sync = numeric(),
                        mean_team_sync = numeric())
                        
# cycle through all rounds for each dyad
rounds = split(data_prepped,list(data_prepped$dyad,data_prepped$round_number))

# # remove the rounds that were dropped due to errors
# rounds = rounds[c(-17, -27, -38, -84, -143, -207)]

# run through each round and mark switches
for (round in names(rounds)){
  
  # pick out the data to identify movement direction
  data = dplyr::select(rounds[[round]], c(2,3,4,23,24,25,26))
  
  # if it isn't a dyad.round that was dropped
  if(nrow(data)!=0) {
  
    data['left_free'] <- NA
    data['right_free'] <- NA
    data['left_free2'] <- NA
    data['right_free2'] <- NA
    data['left_restrict'] <- NA
    data['right_restrict'] <- NA
    data['left_restrict2'] <- NA
    data['right_restrict2'] <- NA
    data['transition_free'] <- NA
    data['transition_restrict'] <- NA
    data['transition_free2'] <- NA
    data['transition_restrict2'] <- NA
    
    # sort data in ascending time
    data <- data[order(data$Time),]
    
    # for each line, check for movement direction
    for(i in 1:(nrow(data)-1)){
      
      # free: if moving right and between .9 and 5.6, mark as right free movement
      if((data$rescale_free_x[i] - data$rescale_free_x[i+1] < 0) & data$rescale_free_x[i] <= 5.6 & data$rescale_free_x[i+1] <= 5.6 & data$rescale_free_x[i] >= .9 & data$rescale_free_x[i+1] >= .9) { 
        data$right_free[i] = data$rescale_free_x[i] 
        # if moving left and between .9 and 5.6, mark as left free movement
      } else if((data$rescale_free_x[i] - data$rescale_free_x[i+1] > 0) & data$rescale_free_x[i] <= 5.6 & data$rescale_free_x[i+1] <= 5.6 & data$rescale_free_x[i] >= .9 & data$rescale_free_x[i+1] >= .9) {
        data$left_free[i] = data$rescale_free_x[i]
        # if moving left between -.9 and .9, mark as being at -99
      } else if((data$rescale_free_x[i] - data$rescale_free_x[i+1] > 0) & (data$rescale_free_x[i] > -.9 & data$rescale_free_x[i] < .9) & (data$rescale_free_x[i+1] > -.9 & data$rescale_free_x[i+1] < .9)) {
        data$transition_free[i] = -99
        # if moving right between -.9 and .9, mark as being at 180
      } else if((data$rescale_free_x[i] - data$rescale_free_x[i+1] < 0) & (data$rescale_free_x[i] > -.9 & data$rescale_free_x[i] < .9) & (data$rescale_free_x[i+1] > -.9 & data$rescale_free_x[i+1] < .9)) {
        data$transition_free[i] = -99
      } else {
        data$right_free[i] = NA
        data$left_free[i] = NA
      }
      
      # free2: if moving right and between .9 and 5.6, mark as right free movement
      if((data$rescale_free2_x[i] - data$rescale_free2_x[i+1] < 0) & data$rescale_free2_x[i] <= 5.6 & data$rescale_free2_x[i+1] <= 5.6 & data$rescale_free2_x[i] >= .9 & data$rescale_free2_x[i+1] >= .9) {
        data$right_free2[i] = data$rescale_free2_x[i]
        # if moving left and between .9 and 5.6, mark as left free movement
      } else if((data$rescale_free2_x[i] - data$rescale_free2_x[i+1] > 0) & data$rescale_free2_x[i] <= 5.6 & data$rescale_free2_x[i+1] <= 5.6 & data$rescale_free2_x[i] >= .9 & data$rescale_free2_x[i+1] >= .9) {
        data$left_free2[i] = data$rescale_free2_x[i]
        # if moving left between -.9 and .9, mark as being at -99
      } else if((data$rescale_free2_x[i] - data$rescale_free2_x[i+1] > 0) & (data$rescale_free2_x[i] > -.9 & data$rescale_free2_x[i] < .9) & (data$rescale_free2_x[i+1] > -.9 & data$rescale_free2_x[i+1] < .9)) {
        data$transition_free2[i] = -99
        # if moving right between -.9 and .9, mark as being at 180
      } else if((data$rescale_free2_x[i] - data$rescale_free2_x[i+1] < 0) & (data$rescale_free2_x[i] > -.9 & data$rescale_free2_x[i] < .9) & (data$rescale_free2_x[i+1] > -.9 & data$rescale_free2_x[i+1] < .9)) {
        data$transition_free2[i] = -99
      } else {
        data$right_free2[i] = NA
        data$left_free2[i] = NA
      }
  
      # restrict: if moving right and between .9 and 5.6, mark as right restrict movement
      if((data$rescale_restrict_x[i] - data$rescale_restrict_x[i+1] < 0) & data$rescale_restrict_x[i] <= 5.6 & data$rescale_restrict_x[i+1] <= 5.6 & data$rescale_restrict_x[i] >= .9 & data$rescale_restrict_x[i+1] >= .9) {
        data$right_restrict[i] = data$rescale_restrict_x[i]
        # if moving left and between .9 and 5.6, mark as left restrict movement
      } else if((data$rescale_restrict_x[i] - data$rescale_restrict_x[i+1] > 0) & data$rescale_restrict_x[i] <= 5.6 & data$rescale_restrict_x[i+1] <= 5.6 & data$rescale_restrict_x[i] >= .9 & data$rescale_restrict_x[i+1] >= .9) {
        data$left_restrict[i] = data$rescale_restrict_x[i]
        # if moving left between -.9 and .9, mark as being at -99
      } else if((data$rescale_restrict_x[i] - data$rescale_restrict_x[i+1] > 0) & (data$rescale_restrict_x[i] > -.9 & data$rescale_restrict_x[i] < .9) & (data$rescale_restrict_x[i+1] > -.9 & data$rescale_restrict_x[i+1] < .9)) {
        data$transition_restrict[i] = -99
        # if moving right between -.9 and .9, mark as being at 180
      } else if((data$rescale_restrict_x[i] - data$rescale_restrict_x[i+1] < 0) & (data$rescale_restrict_x[i] > -.9 & data$rescale_restrict_x[i] < .9) & (data$rescale_restrict_x[i+1] > -.9 & data$rescale_restrict_x[i+1] < .9)) {
        data$transition_restrict[i] = -99
      } else {
        data$right_restrict[i] = NA
        data$left_restrict[i] = NA
      }
  
      # restrict2: if moving right and between .9 and 5.6, mark as right restrict movement
      if((data$rescale_restrict2_x[i] - data$rescale_restrict2_x[i+1] < 0) & data$rescale_restrict2_x[i] <= 5.6 & data$rescale_restrict2_x[i+1] <= 5.6 & data$rescale_restrict2_x[i] >= .9 & data$rescale_restrict2_x[i+1] >= .9) {
        data$right_restrict2[i] = data$rescale_restrict2_x[i]
        # if moving left and between .9 and 5.6, mark as left restrict movement
      } else if((data$rescale_restrict2_x[i] - data$rescale_restrict2_x[i+1] > 0) & data$rescale_restrict2_x[i] <= 5.6 & data$rescale_restrict2_x[i+1] <= 5.6 & data$rescale_restrict2_x[i] >= .9 & data$rescale_restrict2_x[i+1] >= .9) {
        data$left_restrict2[i] = data$rescale_restrict2_x[i]
        # if moving left between -.9 and .9, mark as being at -99
      } else if((data$rescale_restrict2_x[i] - data$rescale_restrict2_x[i+1] > 0) & (data$rescale_restrict2_x[i] > -.9 & data$rescale_restrict2_x[i] < .9) & (data$rescale_restrict2_x[i+1] > -.9 & data$rescale_restrict2_x[i+1] < .9)) {
        data$transition_restrict2[i] = -99
        # if moving right between -.9 and .9, mark as being at 180
      } else if((data$rescale_restrict2_x[i] - data$rescale_restrict2_x[i+1] < 0) & (data$rescale_restrict2_x[i] > -.9 & data$rescale_restrict2_x[i] < .9) & (data$rescale_restrict2_x[i+1] > -.9 & data$rescale_restrict2_x[i+1] < .9)) {
        data$transition_restrict2[i] = -99
      } else {
        data$right_restrict2[i] = NA
        data$left_restrict2[i] = NA
      }
  
    }
    
    # rescale the data to be in degrees
    data$rescaled_free_right = rescale(data$right_free, to = c(0, 180), from = range(.9, 5.6))
    data$rescaled_free_left = rescale(data$left_free, to = c(360, 180), from = range(.9, 5.6))
    data$rescaled_free2_right = rescale(data$right_free2, to = c(0, 180), from = range(.9, 5.6))
    data$rescaled_free2_left = rescale(data$left_free2, to = c(360, 180), from = range(.9, 5.6))
    data$rescaled_restrict_right = rescale(data$right_restrict, to = c(0, 180), from = range(.9, 5.6))
    data$rescaled_restrict_left = rescale(data$left_restrict, to = c(360, 180), from = range(.9, 5.6))
    data$rescaled_restrict2_right = rescale(data$right_restrict2, to = c(0, 180), from = range(.9, 5.6))
    data$rescaled_restrict2_left = rescale(data$left_restrict2, to = c(360, 180), from = range(.9, 5.6))
    
    # merge phase directions and task switch markers for each player to one column
    data$free_phase <- coalesce(data$rescaled_free_left, data$rescaled_free_right, data$transition_free)
    data$free2_phase <- coalesce(data$rescaled_free2_left, data$rescaled_free2_right, data$transition_free2)
    data$restrict_phase <- coalesce(data$rescaled_restrict_left, data$rescaled_restrict_right, data$transition_restrict)
    data$restrict2_phase <- coalesce(data$rescaled_restrict2_left, data$rescaled_restrict2_right, data$transition_restrict2)
    
    # finish processing phase information
    data <- data %>% 
      
      # fill in NAs with the previous phase (i.e., where they are currently stopped to work)
      fill(free_phase) %>% 
      fill(free2_phase) %>% 
      fill(restrict_phase) %>%
      fill(restrict2_phase) %>%
      
      # drop NAs so that we only have data from when they've all started moving
      drop_na(free_phase) %>%
      drop_na(free2_phase) %>%
      drop_na(restrict_phase) %>%
      drop_na(restrict2_phase) %>%
      
      # replace task switch holders with NAs for relative phase calculation
      mutate(free_phase = ifelse(free_phase == -99, NA, free_phase),
             free2_phase = ifelse(free2_phase == -99, NA, free2_phase),
             restrict_phase = ifelse(restrict_phase == -99, NA, restrict_phase),
             restrict2_phase = ifelse(restrict2_phase == -99, NA, restrict2_phase)) %>%
      
      #### 6. Calculate cluster phase and relative phases for each player ####
       
      # transform time series to radians
      mutate(free_phase_rad = deg2rad(free_phase),
             free2_phase_rad = deg2rad(free2_phase),
             restrict_phase_rad = deg2rad(restrict_phase),
             restrict2_phase_rad = deg2rad(restrict2_phase)) %>%
      
      # calculate cluster phase (complex form)
      mutate(cluster_phase_complex = 
              (exp(sqrt(as.complex(-1))*free_phase_rad) + 
              exp(sqrt(as.complex(-1))*free2_phase_rad) + 
              exp(sqrt(as.complex(-1))*restrict_phase_rad) + 
              exp(sqrt(as.complex(-1))*restrict2_phase_rad))/4) %>%
      
      # calculate cluster phase (radian form, [-pi,pi])
      mutate(cluster_phase_rad = atan2(Im(cluster_phase_complex), Re(cluster_phase_complex))) %>%
      
      # calculate sine and cosine
      mutate(sin = sin(cluster_phase_rad)) %>%
      mutate(cos = cos(cluster_phase_rad)) %>%
      
      # mean scale sine and cosine
      mutate(sin = sin/mean(sin, na.rm = TRUE)) %>%
      mutate(cos = cos/mean(cos, na.rm = TRUE)) %>%
      
      # fill NAs with high numbers so that we don't get recurrence
      mutate(sin = replace_na(sin, 99999)) %>%
      mutate(cos = replace_na(cos, -99999)) %>%
      
      # calculate relative phases for each player-team combo
      mutate(rel_phase_free = free_phase_rad - cluster_phase_rad) %>%
      mutate(rel_phase_free2 = free2_phase_rad - cluster_phase_rad) %>%
      mutate(rel_phase_restrict = restrict_phase_rad - cluster_phase_rad) %>%
      mutate(rel_phase_restrict2 = restrict2_phase_rad - cluster_phase_rad) %>%
      
      # add dyad-round marker
      mutate(dyad.round = paste(dyad, ".", round_number)) %>%
      ungroup()
    
    #### 7. Calculate mean relative phase and degree of synchrony for each player ####
    
    # initialize variables
    mean_rel_phase_complex_free = 0
    mean_rel_phase_complex_free2 = 0
    mean_rel_phase_complex_restrict = 0
    mean_rel_phase_complex_restrict2 = 0
    
    # do summation
    for(i in 1:nrow(data)) {
      # complex form
      if(!is.na(data$free_phase_rad[i])){mean_rel_phase_complex_free = mean_rel_phase_complex_free+exp(sqrt(as.complex(-1))*data$free_phase_rad[i])}
      if(!is.na(data$free2_phase_rad[i])){mean_rel_phase_complex_free2 = mean_rel_phase_complex_free2+exp(sqrt(as.complex(-1))*data$free2_phase_rad[i])}
      if(!is.na(data$restrict_phase_rad[i])){mean_rel_phase_complex_restrict = mean_rel_phase_complex_restrict+exp(sqrt(as.complex(-1))*data$restrict_phase_rad[i])}
      if(!is.na(data$restrict2_phase_rad[i])){mean_rel_phase_complex_restrict2 = mean_rel_phase_complex_restrict2+exp(sqrt(as.complex(-1))*data$restrict2_phase_rad[i])}
    }
    
    # take average of complex relative phase
    data$mean_rel_phase_complex_free = mean_rel_phase_complex_free/nrow(data)
    data$mean_rel_phase_complex_free2 = mean_rel_phase_complex_free2/nrow(data)
    data$mean_rel_phase_complex_restrict = mean_rel_phase_complex_restrict/nrow(data)
    data$mean_rel_phase_complex_restrict2 = mean_rel_phase_complex_restrict2/nrow(data)
    
    # radian form
    data$mean_rel_phase_rad_free = atan2(Im(data$mean_rel_phase_complex_free), Re(data$mean_rel_phase_complex_free))
    data$mean_rel_phase_rad_free2 = atan2(Im(data$mean_rel_phase_complex_free2), Re(data$mean_rel_phase_complex_free2))
    data$mean_rel_phase_rad_restrict = atan2(Im(data$mean_rel_phase_complex_restrict), Re(data$mean_rel_phase_complex_restrict))
    data$mean_rel_phase_rad_restrict2 = atan2(Im(data$mean_rel_phase_complex_restrict2), Re(data$mean_rel_phase_complex_restrict2))
    
    # calculate degree of synchrony
    data$deg_sync_free = abs(data$mean_rel_phase_complex_free)
    data$deg_sync_free2 = abs(data$mean_rel_phase_complex_free2)
    data$deg_sync_restrict = abs(data$mean_rel_phase_complex_restrict)
    data$deg_sync_restrict2 = abs(data$mean_rel_phase_complex_restrict2)
    
    #### 8. Calculate whole-team continuous synchronization ####
    data <-  data %>%
      
      # whole-team sync
      mutate(continuous_team_sync =
               case_when(!is.na(free_phase_rad) & !is.na(free2_phase_rad) & !is.na(restrict_phase_rad) & !is.na(restrict2_phase_rad)
                         ~ abs((exp(sqrt(as.complex(-1))*(free_phase_rad - mean_rel_phase_rad_free)) + 
                                exp(sqrt(as.complex(-1))*(free2_phase_rad - mean_rel_phase_rad_free2)) + 
                                exp(sqrt(as.complex(-1))*(restrict_phase_rad - mean_rel_phase_rad_restrict)) + 
                                exp(sqrt(as.complex(-1))*(restrict2_phase_rad - mean_rel_phase_rad_restrict2)))/4)))
    
    #### 9. Calculate mean degree of group synchronization ####
    
    # initialize variable
    mean_team_sync = 0
    
    # successively add the sync values at each time point
    for(k in 1:nrow(data)){
      if(!is.na(data$continuous_team_sync[k])) { mean_team_sync = mean_team_sync + data$continuous_team_sync[k] }
    }
    
    # take the mean
    data$mean_team_sync = mean_team_sync/nrow(data)
    
    #### 10. Bind everything to dataframe ####
    
    # bind everything to data frame
    directions = rbind.data.frame(directions,
                                  data)
  

  }
  
}

# bind phase data to original data frame
data_directions = data_prepped %>% 
  left_join(directions,by=c("Time", "dyad", "round_number", "rescale_free_x", "rescale_free2_x", "rescale_restrict_x", "rescale_restrict2_x")) %>%
  group_by(dyad, round_number) %>%
  arrange('Time') %>%
  drop_na(dyad.round) %>%
  ungroup()

#### 12. Create indicator for task switching (1 = switch; 2 = switch back; 0 = nothing) ####

# cycle through all rounds for each dyad
rounds = split(data_directions,list(data_directions$dyad,data_directions$round_number))

# create empty data frame
task_switches = data.frame(Time = numeric(),
                           dyad = numeric(),
                           round_number = numeric(),
                           x1 = numeric(),
                           x2 = numeric(),
                           x3 = numeric(),
                           x4 = numeric(),
                           task_switch_free = numeric(),
                           task_switch_free2 = numeric(),
                           task_switch_restrict = numeric(),
                           task_switch_restrict2 = numeric())

# run through each round and mark switches
for (round in names(rounds)){

  # pick out the data to identify task switches
  data = dplyr::select(rounds[[round]], c(2,3,4,10,12,14,16))
  
  # skip the rounds that were removed
  if(nrow(data)!=0) {
    
    data['task_switch_free'] <- NA
    data['task_switch_free2'] <- NA
    data['task_switch_restrict'] <- NA
    data['task_switch_restrict2'] <- NA
  
    # sort data in ascending time
    data <- data[order(data$Time),]
  
    # for each line, check for a switch
    for(i in 1:(nrow(data)-1)){
  
      # free: check for task switches
      if(data$x1[i] <= 0 & data$x1[i+1] >= 0) { # identify task switches
        data$task_switch_free[i] = 1
      } else if (data$x1[i] >= 0 & data$x1[i+1] <= 0) { # identify switching back
        data$task_switch_free[i] = 2
      } else {
        data$task_switch_free[i] = 0
      }
  
      # free2: check for task switches
      if(data$x3[i] <= 0 & data$x3[i+1] >= 0) { # identify task switches
        data$task_switch_free2[i] = 1
      } else if (data$x3[i] >= 0 & data$x3[i+1] <= 0) { # identify switching back
        data$task_switch_free2[i] = 2
      } else {
        data$task_switch_free2[i] = 0
      }
  
      # restrict: identify task switches
      if(data$x2[i] >= 0 & data$x2[i+1] <= 0) { # identify task switches
        data$task_switch_restrict[i] = 1
      } else if(data$x2[i] <= 0 & data$x2[i+1] >= 0) { # identify switching back
        data$task_switch_restrict[i] = 2
      } else {
        data$task_switch_restrict[i] = 0
      }
  
      # restrict2: identify task switches
      if(data$x4[i] >= 0 & data$x4[i+1] <= 0) { # identify task switches
        data$task_switch_restrict2[i] = 1
      } else if(data$x4[i] <= 0 & data$x4[i+1] >= 0) { # identify switching back
        data$task_switch_restrict2[i] = 2
      } else {
        data$task_switch_restrict2[i] = 0
      }
  
    }
  
    # bind everything to data frame
    task_switches = rbind.data.frame(task_switches,
                                     data)
  }
  
}

# bind switching markers to original data frame
data_final = data_directions %>%
  left_join(task_switches,by=c("Time", "dyad", "round_number", "x1", "x2", "x3", "x4"))

# check that we have no NAs in the relevant time series
sum(is.na(data_final$sin))

# save data
write.table(x = data_final,
            file=paste0("./data/data_prepped-exp_3.csv"),
            sep=",",
            col.names=TRUE,
            row.names=FALSE)
