##### Process data #####
#
# Processing data for experiment 1. 
# Trimming files which had restarts and behavior issues.
# 
# Code by: @mchiovaro
# Last updated: 2022_11_10

#### 1. set up ####
rm(list=ls())
setwd("./Documents/github/division-of-labor-analyses/exp_1-CogSci")
install.packages("tseriesChaos", "crqa","dplyr")
install.packages("dplyr")
library(tseriesChaos, crqa)
library(dplyr)
set.seed(2022)

#### 2. read in raw data  ####
d1 <- read.csv("./data/raw/1-movementdata.csv", header=FALSE) 
d2 <- read.csv("./data/raw/2-movementdata.csv", header=FALSE)
d3 <- read.csv("./data/raw/3-movementdata.csv", header=FALSE)
d4 <- read.csv("./data/raw/4-movementdata.csv", header=FALSE)
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
d26 <- read.csv("./data/raw/26-movementdata.csv", header=FALSE)
d27 <- read.csv("./data/raw/27-movementdata.csv", header=FALSE)
d31 <- read.csv("./data/raw/31-movementdata.csv", header=FALSE)

# rename variables
dfs <- list(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d26, d27, d31)
changenames <- function(x) {
  names(x) <- c("System.DateTime.Now","Time","ParticipantNumber","round_number","ie_condition","td_condition[round_number]","com_condition","size_condition","experiment_num","beeFree.transform.position.x","beeFree.transform.position.y","beeRestrict.transform.position.x","beeRestrict.transform.position.y")
  return(x)
}
dfs <- lapply(dfs, changenames)
# split them out
d1<-dfs[[1]]
d2<-dfs[[2]]
d3<-dfs[[3]]
d4<-dfs[[4]]
d5<-dfs[[5]]
d6<-dfs[[6]]
d7<-dfs[[7]]
d8<-dfs[[8]]
d9<-dfs[[9]]
d10<-dfs[[10]]
d11<-dfs[[11]]
d12<-dfs[[12]]
d13<-dfs[[13]]
d14<-dfs[[14]]
d15<-dfs[[15]]
d16<-dfs[[16]]
d17<-dfs[[17]]
d18<-dfs[[18]]
d19<-dfs[[19]]
d20<-dfs[[20]]
d21<-dfs[[21]]
d22<-dfs[[22]]
d23<-dfs[[23]]
d24<-dfs[[24]]
d26<-dfs[[25]]
d27<-dfs[[26]]
d31<-dfs[[27]]

#### 3. Check and trim data as needed ####

# trim data with re-starts
d2_filt <- d2 %>% filter(Time < 1157.48 | Time > 1267.94) # r6 started over - done
# check if d6 was restarted in an actual round or just a practice round
d6 %>% filter(d6$beeFree.transform.position.x == -3.25 &
                d6$beeFree.transform.position.y == 0 &
                d6$beeRestrict.transform.position.x == 3.25 &
                d6$beeRestrict.transform.position.y == 0 &
                # if the previous row isn't the starting position
                (dplyr::lag(d6$beeFree.transform.position.x) != -3.25 |
                   dplyr::lag(d6$beeFree.transform.position.y) != 0 |
                   dplyr::lag(d6$beeRestrict.transform.position.x) != 3.25 |
                   dplyr::lag(d6$beeRestrict.transform.position.y) != 0) &  
                # if the previous row is the same round
                d6$round_number == dplyr::lag(d6$round_number)) # produces no rows, so no actual restarts
d8_filt <- d8 %>% filter(Time < 1804.40 | Time > 1857.54) # r5 restarted - done
d9_filt <- d9 %>% filter(Time > 829.22) # HANDLE THIS LATER?? Filtering is fine here. r1 restarted # done. Not sure what the other note is about.
d10_filt <- d10 %>% filter(round_number != 4) # r4 wasn't restarted in time but there was a behavior error - removing entire round # done
d12_filt <- d12 %>% filter(Time < 682.46 | Time > 697.94) #r1 restarted # done
d12_filt <- d12_filt %>% filter(Time < 945.5200 | Time > 952.3200) # r3 restarted # done
d13_filt <- d13 %>% filter(Time < 970.0400 | Time > 1060.30) # r1 restarted 2x # done
d13_filt <- d13_filt %>% filter(Time < 1248.56 | Time > 1282.38) # r2 restarted 2x # done
d13_filt <- d13_filt %>% filter(Time < 1562.12 | Time > 1564.94) # r5 restarted # done
d15_filt <- d15 %>% filter(Time < 290.04 | Time > 311.90) # r1 restarted # done
d15_filt <- d15 %>% filter(round_number != 5) #r5 removed entirely due to behavior issue
d16_filt <- d16 %>% filter(round_number!=1) # r1 removed entirely due to participant misunderstanding instructions # done
d17_filt <- d17 %>% filter(Time <  878.68 | Time > 888.74) # r2 restarted # done
d17_filt <- d17_filt %>% filter(Time <  1126.80 | Time > 1216.88) # r4 restarted # done
d17_filt <- d17_filt %>% filter(Time <  1431.46 | Time > 1491.60) # r6 restarted # done
d19_filt <- d19 %>% filter(Time <  822.68 | Time > 887.6200) # r1 restarted # done
d20_filt <- d20 %>% filter(Time <  631.86 | Time > 643.72) # r1 restarted # done
d21_filt <- d21 %>% filter(!(Time >= 169.30 & Time <= 235.82 & round_number==1)) # r1 keeping first run through and removing second (because game crashed at second round) # done
d21_filt <- d21_filt %>% filter(!(Time >= 829.12  & Time <= 865.42 & round_number==2)) # r2 glitched out # done
d21_filt <- d21_filt %>% filter(!(Time >=  328.84 & Time <= 381.48 & round_number==3)) # r3 restarted # done
d22_filt <- d22 %>% filter(Time <  451.68 | Time > 496.28) # r4 restarted # done
d23_filt <- d23 %>% filter(Time <  960.9 | Time > 987.5200) # r3 restarted # done
d24_filt <- d24 %>% filter(Time <  803.44 | Time > 891.82) # r1 restarted # done
d26_filt <- d26 %>% filter(Time <  1166.96 | Time > 1195.18) # entire game restarted after r1 # done
d26_filt <- d26_filt %>% filter(Time <  273.30 | Time > 320.10) # r3 restarted # done
d26_filt <- d26_filt %>% filter(Time <  568.06 | Time > 585.06) # r6 restarted # done

# # checking code
# d_test <- d
# d_test <- d_test %>% filter(round_number == 2)
# d_test$lag <- d_test$Time - shift(d_test$Time, 1)

# bind data
formatted_all <- rbind(d1, d2_filt, d3, d4, d5, 
                       d6, # corrupt file
                       d7, d8_filt, d9_filt, d10_filt,
                       d11, d12_filt, d13_filt, d14, d15_filt, 
                       d16_filt, # corrupt file
                       d17_filt, d18, d19_filt, d20_filt, 
                       d21_filt, # corrupt file
                       d22_filt, d23_filt, d24_filt, 
                       d26_filt, d27, d31)

# save the data
write.table(formatted_all,
            paste('./data/formatted/formatted_data.csv', sep = ''),
            sep=',', row.names=FALSE)


#### 2. generate velocity ####
names(formatted_all) <- c("System.DateTime.Now","Time","dyad","round_number","ie_condition","td_condition","com_condition","size_condition","experiment_num","x1","y1","x2","y2")
data_prepped <- formatted_all %>% 
  group_by(dyad, round_number) %>%
  arrange(Time) %>%
  mutate(velocity0 = as.numeric((x1-lag(x1))/.02)) %>%
  mutate(velocity1 = as.numeric((x2-lag(x2))/.02)) %>%
  mutate(round_number = as.numeric(round_number)) %>%
  filter(!(x1==-3.25 & y1==0 & x2==3.25 & y2==0)) %>% # trim off the beginning before someone starts moving
  filter(!(velocity0 == 0 & velocity1 == 0 & 
             (dplyr::lead(velocity0) == 0 & dplyr::lead(velocity1) == 0))) %>% # remove where they are both just working
  na.omit()

# save data
write.table(x = data_prepped,
            file=paste0("./data/data_prepped.csv"),
            sep=",",
            col.names=TRUE,
            row.names=FALSE)
 
# Don't need this because we do have things to compare this time!
# #### 3. Create permuted time series ####
# 
# # create lists to loop through
# dyad_list <- c(unique(data_prepped$dyad))
# var_list <- c("velocity0", "velocity1")
# 
# for (i in dyad_list) {
#   
#   # create round counter (because one has missing round)
#   tbl <- data_prepped %>% filter(dyad==i)
#   round_list <- c(unique(tbl$round_number))
# 
#   for (j in round_list) {
# 
#     # take the subset of data we want
#     tbl <- tbl %>% filter(round_number==j)
# 
#     for (k in var_list) {
# 
#       # create empty data frame
#       shuffled_data = data.frame()
# 
#       # generate 1000 random time series and bind to rows
#       for (a in 1:1000){
#         shuffled <- sample_n(tbl = tbl[,c(k)],
#                              size = nrow(tbl),
#                              replace = FALSE)
#         sample <- t(as.data.frame(shuffled))
#         shuffled_data <- rbind(shuffled_data, sample)
#       }
# 
#       # take the original time series and add it as a row
#       original <- as.data.frame(tbl[,c(k)])
#       original <- as.data.frame(t(original))
#       shuffled_data <- rbind(shuffled_data, original)
# 
#       # check to see if we have 1001 distinct time series
#       print(paste0("Total distinct shuffled time series for CRQA: ",
#                    nrow(distinct(shuffled_data))))
#       if(nrow(distinct(shuffled_data)) != 1001){
#         print("WARNING: Duplicates in surrogate time series.")
#         print("Unique rows:")
#         print(sum(nrow(distinct(shuffled_data))))
#       }
# 
#       # transform rows to columns for binding
#       shuffled_data <- as.data.frame(t(shuffled_data))
# 
#       # remove real time series from shuffled data frame and combine with original data
#       shuffled_data <- shuffled_data[c(1:1000)]
#       tbl <- cbind(tbl, shuffled_data)
# 
#     }
# 
#     # save data
#     write.table(x = tbl,
#                 file=paste0("./data/shuffled/shuffled_data_", i,"_", j, ".csv"),
#                 sep=",",
#                 col.names=TRUE,
#                 row.names=FALSE)
# 
#   }
# 
# }

