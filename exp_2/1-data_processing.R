##### Process data #####
#
# Processing data for experiment 2: 
# - Trimming files which had restarts and behavior issues.
# - Re-scaling player data to match on x-axis.
# - Calculating relative phase and splitting into two time series for MdRQA.
#
# Code by: @mchiovaro
# Last updated: 2022_01_18

#### 1. Set up ####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_2")
source('../required_packages.r')
source('../libraries_and_functions.r')
set.seed(2022)

#### 2. Read in raw data  ####
d1 <- read.csv("./data/raw/1-movementdata.csv", header=FALSE) 
d2 <- read.csv("./data/raw/2-movementdata.csv", header=FALSE)
d3 <- read.csv("./data/raw/3-movementdata.csv", header=FALSE)
d5 <- read.csv("./data/raw/5-movementdata.csv", header=FALSE)
d6 <- read.csv("./data/raw/6-movementdata.csv", header=FALSE)
d7 <- read.csv("./data/raw/7-movementdata.csv", header=FALSE)
d8 <- read.csv("./data/raw/8-movementdata.csv", header=FALSE)
d9 <- read.csv("./data/raw/9-movementdata.csv", header=FALSE)
d10 <- read.csv("./data/raw/10-movementdata.csv", header=FALSE)
d11 <- read.csv("./data/raw/11-movementdata.csv", header=FALSE)
d12 <- read.csv("./data/raw/12-movementdata.csv", header=FALSE)
d13 <- read.csv("./data/raw/13-movementdata.csv", header=FALSE)
d14 <- read.csv("./data/raw/14-movementdata.csv", header=FALSE)
d15 <- read.csv("./data/raw/15-movementdata.csv", header=FALSE)
d16 <- read.csv("./data/raw/16-movementdata.csv", header=FALSE)
d17 <- read.csv("./data/raw/17-movementdata.csv", header=FALSE)
d18 <- read.csv("./data/raw/18-movementdata.csv", header=FALSE)
d19 <- read.csv("./data/raw/19-movementdata.csv", header=FALSE)
d20 <- read.csv("./data/raw/20-movementdata.csv", header=FALSE)
d21 <- read.csv("./data/raw/21-movementdata.csv", header=FALSE)
d22 <- read.csv("./data/raw/22-movementdata.csv", header=FALSE)
d23 <- read.csv("./data/raw/23-movementdata.csv", header=FALSE)
d24 <- read.csv("./data/raw/24-movementdata.csv", header=FALSE)
d25 <- read.csv("./data/raw/25-movementdata.csv", header=FALSE)
d26 <- read.csv("./data/raw/26-movementdata.csv", header=FALSE)
d27 <- read.csv("./data/raw/27-movementdata.csv", header=FALSE)
d28 <- read.csv("./data/raw/28-movementdata.csv", header=FALSE)
d29 <- read.csv("./data/raw/29-movementdata.csv", header=FALSE)
d30 <- read.csv("./data/raw/30-movementdata.csv", header=FALSE)
d31 <- read.csv("./data/raw/31-movementdata.csv", header=FALSE)
d32 <- read.csv("./data/raw/32-movementdata.csv", header=FALSE)
d33 <- read.csv("./data/raw/33-movementdata.csv", header=FALSE)
d35 <- read.csv("./data/raw/35-movementdata.csv", header=FALSE)
d37 <- read.csv("./data/raw/37-movementdata.csv", header=FALSE)
d38 <- read.csv("./data/raw/38-movementdata.csv", header=FALSE)
d40 <- read.csv("./data/raw/40-movementdata.csv", header=FALSE)
d42 <- read.csv("./data/raw/42-movementdata.csv", header=FALSE)
d43 <- read.csv("./data/raw/43-movementdata.csv", header=FALSE)
d44 <- read.csv("./data/raw/44-movementdata.csv", header=FALSE)

# rename variables
dfs <- list(d1, d2, d3, d5, d6, d7, d8, d9, d10, d11, d12, d13, 
            d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, 
            d25, d26, d27, d28, d29, d30, d31, d32, d33, d35, d37, d38, 
            d40, d42, d43, d44)

changenames <- function(x) {
  names(x) <- c("System.DateTime.Now","Time","ParticipantNumber","round_number","ie_condition","td_condition[round_number]","com_condition","size_condition","experiment_num","beeFree.transform.position.x","beeFree.transform.position.y","beeRestrict.transform.position.x","beeRestrict.transform.position.y")
  return(x)
}
dfs <- lapply(dfs, changenames)

# split them out
d1<-dfs[[1]]
d2<-dfs[[2]]
d3<-dfs[[3]]
d5<-dfs[[4]]
d6<-dfs[[5]]
d7<-dfs[[6]]
d8<-dfs[[7]]
d9<-dfs[[8]]
d10<-dfs[[9]]
d11<-dfs[[10]]
d12<-dfs[[11]]
d13<-dfs[[12]]
d14<-dfs[[13]]
d15<-dfs[[14]]
d16<-dfs[[15]]
d17<-dfs[[16]]
d18<-dfs[[17]]
d19<-dfs[[18]]
d20<-dfs[[19]]
d21<-dfs[[20]]
d22<-dfs[[21]]
d23<-dfs[[22]]
d24<-dfs[[23]]
d25<-dfs[[24]]
d26<-dfs[[25]]
d27<-dfs[[26]]
d28<-dfs[[27]]
d29<-dfs[[28]]
d30<-dfs[[29]]
d31<-dfs[[30]]
d32<-dfs[[31]]
d33<-dfs[[32]]
d35<-dfs[[33]]
d37<-dfs[[34]]
d38<-dfs[[35]]
d40<-dfs[[36]]
d42<-dfs[[37]]
d43<-dfs[[38]]
d44<-dfs[[39]]

#### 3. Check and trim data as needed ####

# trim data with re-starts
d1_filt <- d1 %>% filter(round_number != 1) # r1 removed entirely due to behavior issue 
d2_filt <- d2 %>% filter(Time < 601.1 | Time > 797.72) # r3 restarted 2x 
d5_filt <- d5 %>% filter(Time < 1543.72 | Time > 1569.18) # r2 restarted 
d7_filt <- d7 %>% filter(round_number != 2) # r2 removed entirely due to participant misunderstanding 
d9_filt <- d9 %>% filter(Time > 379.32) %>% # r1 restarted 
                  mutate(com_condition = 3) # changing to correct com_condition
d10_filt <- d10 %>% filter(Time < 1021.32 | Time > 1083.88) # r3 restarted 
d11_filt <- d11 %>% filter(Time < 1055.48 | Time > 1065.1) # r3 restarted 
d12_filt <- d12 %>% filter(Time > 910.1) #r1 restarted 
d12_filt <- d12_filt %>% filter(Time < 1412.22 | Time > 1428.82) #r5 restarted 
d13_filt <- d13 %>% filter(Time < 371.4 | Time > 434.56) # r3 restarted 
d14_filt <- d14 %>% filter(Time < 922.42 | Time > 934.54) # r2 restarted 
d14_filt <- d14_filt %>% filter(Time < 1261.86 | Time > 1325.16) # r5 restarted 
d15_filt <- d15 %>% filter(Time < 1628.66 | Time > 1634.84) # r6 restarted 
d16_filt <- d16 %>% filter(Time < 2556.42 | Time > 2629.82) # r6 restarted 
d17_filt <- d17 %>% filter(Time > 1345.56) # r1 restarted 
d18_filt <- d18 %>% filter(Time < 1103.72 | Time > 1172.18) # r2 restarted 
d20_filt <- d20 %>% filter(Time < 1604.44 | Time > 1661.18) # r5 restarted 
d21_filt <- d21 %>% filter(Time > 1082.54) # r1 restarted 
d21_filt <- d21_filt %>% filter(round_number != 3) # r3 removed entirely 
d21_filt <- d21_filt %>% filter(Time < 1407.92 | Time > 1442.54) # r4 restarted 
d22_filt <- d22 %>% filter(Time < 1724.58 | Time > 1763.42) # r2 restarted 
d23_filt <- d23 %>% filter(Time > 1386.94) # r1 restarted 2x 
d24_filt <- d24 %>% filter(round_number != 4) # r4 removed entirely due to behavior issue  (consider further)
d24_filt <- d24_filt %>% filter(Time < 1597.92 | Time > 1644.98) # r6 restarted 2x 
d27_filt <- d27 %>% filter(Time < 1523.22 | Time > 1540.2) # r5 restarted 
d28_filt <- d28 %>% filter(Time < 993.46 | Time > 1082.76) # r3 restarted 
d28_filt <- d28_filt %>% filter(Time < 1333.08 | Time > 1520.54) # r5 restarted 4x 
d29_filt <- d29 %>% filter(Time > 539.36) # r1 restarted 
d29_filt <- d29_filt %>% filter(!(Time > 188.87 & Time < 280.63 & round_number == 1)) # entire game restarted - deleting second run through of round 1 
d29_filt <- d29_filt %>% filter(!(Time >  310.31 & Time < 548.3 & round_number == 2)) # entire game restarted and round 2 restarted again multiple times 
d29_filt <- d29_filt %>% filter(Time < 1029.32 | Time > 1079.46) # r6 restarted 
d30_filt <- d30 %>% filter(round_number != 4) # r4 removed entirely due to behavior issue 
d31_filt <- d31 %>% filter(Time <  792.56 | Time > 864.5) # r2 restarted 
d31_filt <- d31_filt %>% filter(Time < 1228.8 | Time > 1282.56) # r6 restarted 
d35_filt <- d35 %>% filter(Time > 826.12) # r1 restarted 
d35_filt <- d35_filt %>% filter(Time < 1030.02 | Time > 1052.8) # r3 restarted 
d37_filt <- d37 %>% filter(Time > 1082.04) %>% # r1 restarted 
                    mutate(com_condition = 3) # changing to correct com_condition
d38_filt <- d38 %>% filter(Time > 742.6) # r1 restarted 
d38_filt <- d38_filt %>% filter(Time < 1225.46 | Time > 1260.26) # r6 restarted 
d42_filt <- d42 %>% filter(Time > 1093.36) # r1 restarted 
d42_filt <- d42_filt %>% filter(Time < 1458.96 | Time > 1474.22) # r5 restarted 

# bind data
formatted_all <- rbind(d1_filt, d2_filt, d3, d5_filt, d6,
                       d7_filt, d8, d9_filt, d10_filt,
                       d11_filt, d12_filt, d13_filt, d14_filt, d15_filt, 
                       d16_filt, d17_filt, d18_filt, d19, d20_filt, 
                       d21_filt, d22_filt, d23_filt, d24_filt, d25, d26,
                       d27_filt, d28_filt, d29_filt, d30_filt, d31_filt,
                       d32, d33, d35_filt, d37_filt, d38_filt, d40,
                       d42_filt, d43, d44)


#### 4. Generate velocity and timer and rescale free player x-axis ####
names(formatted_all) <- c("System.DateTime.Now","Time","dyad","round_number","ie_condition","td_condition","com_condition","size_condition","experiment_num","x1","y1","x2","y2")
data_prepped <- formatted_all %>% 
  group_by(dyad, round_number) %>%
  arrange(Time) %>%
  mutate(velocity1 = as.numeric((x1-lag(x1))/.02)) %>% # calculate velocity to remove times where players are just working
  mutate(velocity2 = as.numeric((x2-lag(x2))/.02)) %>% # calculate velocity to remove times where players are just working
  mutate(round_number = as.numeric(round_number)) %>%
  filter(!(x1==-3.25 & y1==0 & x2==3.25 & y2==0)) %>% # trim off the beginning before someone starts moving
  mutate(timer = max(Time) - min(Time)) %>% # calculate time to complete each round
  ungroup() %>%
  na.omit()
# create new variables and rescale beeFree x locations to match the directions of beeRestrict
data_prepped['rescale_free_x'] <- NA
for(i in 1:nrow(data_prepped)){
  if(data_prepped$x1[i] <= -.9) { data_prepped$rescale_free_x[i] = data_prepped$x1[i] + 6.5 
  } else { data_prepped$rescale_free_x[i] = data_prepped$x1[i]
  }  
}

#### 5. Identify directions of movement ####

# create empty data frame
directions = data.frame(Time = numeric(),
                        dyad = numeric(),
                        round_number = numeric(),
                        x1 = numeric(),
                        x2 = numeric(),
                        left_free = numeric(),
                        right_free = numeric(),
                        left_restrict = numeric(),
                        right_restrict = numeric(),
                        rescaled_free_right = numeric(),
                        rescaled_free_left = numeric(),
                        rescaled_restrict_right = numeric(),
                        rescaled_restrict_left = numeric(),
                        free_phase = numeric(),
                        restrict_phase = numeric(),
                        rel_phase = numeric(),
                        sin = numeric(),
                        cos = numeric(),
                        dyad.round = character())

# cycle through all rounds for each dyad
rounds = split(data_prepped,list(data_prepped$dyad,data_prepped$round_number))

# remove the rounds that were dropped due to errors
rounds = rounds[c(-1, -45, -98, -140, -146)]

# run through each round and mark switches
for (round in names(rounds)){
  
  # pick out the data to identify task switches
  data = dplyr::select(rounds[[round]], c(2,3,4,17,12))
  data['left_free'] <- NA
  data['right_free'] <- NA
  data['left_restrict'] <- NA
  data['right_restrict'] <- NA
  data['transition_free'] <- NA
  data['transition_restrict'] <- NA
  
  # sort data in ascending time
  data <- data[order(data$Time),]
  
  # for each line, check for a switch
  for(i in 1:(nrow(data)-1)){
    
    # if moving right and between .9 and 5.6, mark as right free movement
    if((data$rescale_free_x[i] - data$rescale_free_x[i+1] < 0) & data$rescale_free_x[i] <= 5.6 & data$rescale_free_x[i+1] <= 5.6 & data$rescale_free_x[i] >= .9 & data$rescale_free_x[i+1] >= .9) { 
      data$right_free[i] = data$rescale_free_x[i] 
    # if moving left and between .9 and 5.6, mark as left free movement
    } else if((data$rescale_free_x[i] - data$rescale_free_x[i+1] > 0) & data$rescale_free_x[i] <= 5.6 & data$rescale_free_x[i+1] <= 5.6 & data$rescale_free_x[i] >= .9 & data$rescale_free_x[i+1] >= .9) {
      data$left_free[i] = data$rescale_free_x[i]
    # if moving between -.9 and .9, mark as task switching
    } else if((data$rescale_free_x[i] > -.9 & data$rescale_free_x[i] < .9) | (data$rescale_free_x[i+1] > -.9 & data$rescale_free_x[i+1] < .9)) {
      data$transition_free[i] = 99
    } else {
      data$right_free[i] = NA
      data$left_free[i] = NA
    }
    
    # if moving right and between .9 and 5.6, mark as right restrict movement
    if((data$x2[i] - data$x2[i+1] < 0) & data$x2[i] <= 5.6 & data$x2[i+1] <= 5.6 & data$x2[i] >= .9 & data$x2[i+1] >= .9) { 
      data$right_restrict[i] = data$x2[i] 
      # if moving left and between .9 and 5.6, mark as left restrict movement
    } else if((data$x2[i] - data$x2[i+1] > 0) & data$x2[i] <= 5.6 & data$x2[i+1] <= 5.6 & data$x2[i] >= .9 & data$x2[i+1] >= .9) {
      data$left_restrict[i] = data$x2[i]
      # if moving between -.9 and .9, mark as task switching
    } else if((data$x2[i] > -.9 & data$x2[i] < .9) | (data$x2[i+1] > -.9 & data$x2[i+1] < .9)) {
      data$transition_restrict[i] = 99
    } else {
      data$right_restrict[i] = NA
      data$left_restrict[i] = NA
    }
    
  }
  
  # rescale the data to be in degrees
  data$rescaled_free_right = rescale(data$right_free, to = c(0, 180), from = range(.9, 5.6))
  data$rescaled_free_left = rescale(data$left_free, to = c(360, 180), from = range(.9, 5.6))
  data$rescaled_restrict_right = rescale(data$right_restrict, to = c(0, 180), from = range(.9, 5.6))
  data$rescaled_restrict_left = rescale(data$left_restrict, to = c(360, 180), from = range(.9, 5.6))
  
  # merge phase directions and task switch markers for each player to one column
  data$free_phase <- coalesce(data$rescaled_free_left, data$rescaled_free_right, data$transition_free)
  data$restrict_phase <- coalesce(data$rescaled_restrict_left, data$rescaled_restrict_right, data$transition_restrict)
  
  # finish processing phase information
  data <- data %>% 
    
    # fill in NAs with the previous phase (i.e., where they are currently stopped to work)
    fill(free_phase) %>% 
    fill(restrict_phase) %>%
    
    # replace task switch holders with NAs for relative phase calculation
    mutate(free_phase = ifelse(free_phase == 99, 'NA', free_phase),
            restrict_phase = ifelse(restrict_phase == 99, 'NA', restrict_phase)) %>%
    
    # calculate relative phase - introduces NAs by coercion (but that's due to the task switching NAs)
    mutate(rel_phase = case_when(
            !is.na(free_phase) & !is.na(restrict_phase) ~ as.double(free_phase) - as.double(restrict_phase))
    ) %>%
  
    # calculate sinine and cosine
    mutate(sin = sin(rel_phase*pi/180)) %>%
    mutate(cos = cos(rel_phase*(pi/180))) %>%
    
    # fill NAs with high numbers so that we don't get recurrence
    mutate(sin = replace_na(sin, 99)) %>%
    mutate(cos = replace_na(cos, -99)) %>%
    
    # add dyad-round marker
    mutate(dyad.round = paste(dyad, ".", round_number)) %>%
    ungroup()
  
  # bind everything to data frame
  directions = rbind.data.frame(directions,
                                data)
  
}

# bind phase data to original data frame
data_prepped_directions = data_prepped %>% 
  left_join(directions,by=c("Time", "dyad", "round_number", "rescale_free_x", "x2")) %>%
  group_by(dyad, round_number) %>%
  arrange('Time') %>%
  ungroup()

#### 6. Create indicator for task switching (1 = switch; 2 = switch back; 0 = nothing) ####

# create empty data frame
task_switches = data.frame(Time = numeric(),
                           dyad = numeric(),
                           round_number = numeric(),
                           x1 = numeric(),
                           x2 = numeric(),
                           task_switch_free = numeric(),
                           task_switch_restrict = numeric())

# run through each round and mark switches
for (round in names(rounds)){
  
  # pick out the data to identify task switches
  data = dplyr::select(rounds[[round]], c(2,3,4,10,12))
  data['task_switch_free'] <- NA
  data['task_switch_restrict'] <- NA
  
  # sort data in ascending time
  data <- data[order(data$Time),]
  
  # for each line, check for a switch
  for(i in 1:(nrow(data)-1)){
    
    if(data$x1[i] <= 0 & data$x1[i+1] >= 0) { # identify task switches for beeFree
      data$task_switch_free[i] = 1
    } else if (data$x1[i] >= 0 & data$x1[i+1] <= 0) { # identify switching back for beeFree
      data$task_switch_free[i] = 2
    } else { 
      data$task_switch_free[i] = 0 
    }
    
    if(data$x2[i] >= 0 & data$x2[i+1] <= 0) { # identify task switches for beeRestrict
      data$task_switch_restrict[i] = 1
    } else if(data$x2[i] <= 0 & data$x2[i+1] >= 0) { # identify switching back for beeRestrict
      data$task_switch_restrict[i] = 2
    } else {
      data$task_switch_restrict[i] = 0
    }
    
  }
  
  # bind everything to data frame
  task_switches = rbind.data.frame(task_switches,
                                   data)
  
}

# bind switching markers to original data frame
data_prepped_final = data_prepped_directions %>% left_join(task_switches,by=c("Time", "dyad", "round_number", "x1", "x2"))

# # check out the number of switches
# switches <- data_prepped_final %>%
#   filter(task_switch_free > 0 | task_switch_restrict > 0)

# save data
write.table(x = data_prepped_final,
            file=paste0("./data/data_prepped-exp_2.csv"),
            sep=",",
            col.names=TRUE,
            row.names=FALSE)
