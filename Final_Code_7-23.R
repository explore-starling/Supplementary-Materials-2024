######LIBRARY############
setwd("C:/")

library(tidyverse) #
library(dplyr) #
library(emmeans) #
library(factoextra) #
library(lme4) #
library(psych)
library(afex) #
library(moments) #
library(survival) #
library(survminer) #
library(TraMineR) #
library(TraMineRextras) #
library(reshape2) #
library(nlme) #
library(rstatix) #
library(ggh4x) #
library(GGally)
library(VGAM)
library(tables) #
library(flextable) #
library(devtools) #
library(maditr)
library(DescTools)
library(ggplot2) #
library(ggsignif) #
library(ggfortify) #
library(ggbreak) #
library(ggpattern) #
library(ggbiplot) #
library(ggalluvial) #

#####VISION DATA PREP (DLC output)#####
#####REQUIRES RAW DATA -- TOO BIG TO UPLOAD FOR REVIEW####

# GoPros output videos 11:47 min long, so each trial is three videos (30 min total)
# all csv's for first clip of videos
base1.df <- list.files(pattern = "^Gh01.*\\.csv$*") %>% map_df(~read_csv(.))
# all csv's for second clips of videos
base2.df <- list.files(pattern = "^Gh02.*\\.csv$*") %>% map_df(~read_csv(.))
# second videos directly follow first videos in trial (always same length)
base2.df$frame <- base2.df$frame + 42420  # adjust the frame to be accurate to time in trial
# all csv's for third clips of videos
base3.df <- list.files(pattern = "^Gh03.*\\.csv$*") %>% map_df(~read_csv(.))
# third videos directly follow second videos in trial (always same length)
base3.df$frame <- base3.df$frame + 84840  # adjust the frame to be accurate to time in trial

# combine datasets
vision.df <- rbind(base1.df, base2.df, base3.df)
rm(base1.df, base2.df, base3.df)

# sort by ID, then Frame
vision.df <- vision.df[order(vision.df$ID, vision.df$frame),]
# keep only the rows in which the likelihood of the BEAK MID and BEAK BRIDGE being correctly labelled is above or equal to 0.6
vision.df <- vision.df[(vision.df$BeakMid_likelihood >= 0.6 & vision.df$BeakBridge_likelihood >= 0.6),]
# keep only labels used for following calculations
vision.df <- subset(vision.df, select = c(ID, frame, BeakBridge_x, BeakBridge_y, BeakBridge_likelihood,
                                          BeakMid_x, BeakMid_y, BeakMid_likelihood,
                                          Left_x, Left_y, Left_likelihood, Right_x, Right_y, Right_likelihood,
                                          Front_x, Front_y, Front_likelihood, Back_x, Back_y, Back_likelihood,
                                          LeftShoulder_x,LeftShoulder_y, LeftShoulder_likelihood,
                                          RightShoulder_x, RightShoulder_y, RightShoulder_likelihood))

# CALCULATING ANGLE AND DISTANCE
# creating mid box point -- needs two opposite points to be identified with equal to or greater than 0.6 likelihood
vision.df$Box_x <- ifelse(vision.df$Left_likelihood >= 0.6 & vision.df$Right_likelihood >= 0.6, (vision.df$Left_x + vision.df$Right_x)/2, 
                          ifelse(vision.df$Front_likelihood >= 0.6 & vision.df$Back_likelihood >= 0.6, (vision.df$Front_x + vision.df$Back_x)/2, "null")) # x-coordinate
vision.df$Box_y <- ifelse(vision.df$Left_likelihood >= 0.6 & vision.df$Right_likelihood >= 0.6, (vision.df$Left_y + vision.df$Right_y)/2, 
                          ifelse(vision.df$Front_likelihood >= 0.6 & vision.df$Back_likelihood >= 0.6, (vision.df$Front_y + vision.df$Back_y)/2, "null")) # y-coordinate
# eliminating frames in which the mid box point could not be calculated
vision.df$Box_x <- as.numeric(vision.df$Box_x)  # creates NA values from "null"
vision.df$Box_y <- as.numeric(vision.df$Box_y)  # creates NA values from "null"
vision.df <- na.omit(vision.df)  # eliminates rows that do not have the box coordinates

# vector calculations for ANGLE
w1 <- (vision.df$BeakMid_x - vision.df$BeakBridge_x)  # x vector for beak
w2 <- (vision.df$BeakMid_y - vision.df$BeakBridge_y)  # y vector for beak
v1 <- (vision.df$BeakMid_x - vision.df$Box_x)         # x vector from box to beak
v2 <- (vision.df$BeakMid_y - vision.df$Box_y)         # y vector from box to beak

# atan calculations -- angle from vectors with consideration for left/right
anglerad <- atan2((w2 * v1) - (w1 * v2), (w1 * v1) + (w2 * v2)) 
# convert the radians to degrees
angledeg <- anglerad * 180/pi
# allows for 360 degrees of measurements
vision.df$base <- 180 - angledeg
# convert each ANGLE value into the area of the VISUAL FIELD it falls in
vision.df$visualfield <- cut(vision.df$base, breaks = c(0, 13, 52, 73, 110, 148, 212, 251, 287, 308, 347, 360),  # perpendicular with box (binocular) at 0
                           labels = c("Binocular", "Right Front Periphery", "Right Fovea", "Right Rear Periphery A", "Right Rear Periphery B",
                                      "Blind Spot", "Left Rear Periphery B", "Left Rear Periphery A", "Left Fovea", "Left Front Periphery", "Binocular"))
# order visual field factor for plots
vision.df$visualfield <- factor(vision.df$visualfield, levels = c("Left Rear Periphery B", "Left Rear Periphery A", "Left Fovea", "Left Front Periphery", "Binocular",
                                                                  "Right Front Periphery", "Right Fovea", "Right Rear Periphery A", "Right Rear Periphery B", "Blind Spot"))
# puts binocular at 180 and blind spot at 360 (full field, both sides)
vision.df$Angle <- ifelse((vision.df$base + 180) > 360, vision.df$base - 180, 
                         ifelse((vision.df$base + 180) <= 360, vision.df$base + 180, NA))
vision.df$Angle180 <- abs(vision.df$Angle - 180)  # angle on a scale of 180 deg -- 0 binocular, 180 blind spot (does not include left/right)
vision.df <- subset(vision.df, select=-c(base)) # removes unused angle column

# distance to box -- calculated by box-to-beak vector
vision.df$distcm <- (sqrt((v1)^2 + (v2)^2) / 18)  # convert into cm (18 pixels per cm)

# CHECK FOR ERRORS IN TRIAL
# VIDEO LENGTH
vision.df$time <- vision.df$frame / 60 / 60  # 60 frames per second, 60 seconds a minute
aggregate(time ~ ID, max, data= vision.df)  # should not go above 30 -- if it ends very early, check video
vision.df <- vision.df[!(vision.df$time >=31),]  # removes frames from videos that ran long
# removes ID 3175, whose trial was only recorded for 10 minutes due to camera issue
vision.df <- vision.df[!(vision.df$ID == "3175"),]

# BOX MOVEMENT
aggregate(Box_x ~ ID, sd, data=vision.df)  # checks each video for changes in x-coordinates of box
aggregate(Box_y ~ ID, sd, data=vision.df)  # checks each video for changes in x-coordinates of box
# find frames in which the lid is misplaced
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}  
boxerror <- subset(vision.df, select = c(ID, frame, Box_x, Box_y)) # dataframe for calculations
# most common box position
boxerror <- boxerror %>%
  group_by(ID) %>%
  mutate(mx = Mode(Box_x)) %>%
  mutate(my = Mode(Box_y))
# change in coordinates between mode and point
boxerror$changex <- abs(boxerror$mx - boxerror$Box_x)
boxerror$changey <- abs(boxerror$my - boxerror$Box_y)
# gets the first point at which the coordinates of the box change by more than a cm (18 pixels)
boxerror$errortime <- ifelse(boxerror$changex > 18 | boxerror$changey > 18, boxerror$frame, NA)
aggregate(errortime ~ ID, min, data=boxerror) # check videos to see if the box moved, the lid came off, or the camera shifted (ok)
rm(boxerror)
# removes ID 3135, whose lid came off early in the trial
vision.df <- vision.df[!(vision.df$ID == "3135"),]

# DISTANCE FROM BOX
aggregate(distcm ~ ID, max, data=vision.df)  # if dist is high, check video for incorrect labels
vision.df <- vision.df[!(vision.df$distcm > 30.0),] # eliminates any rows above 30 cm distance (uncommon) after checking for errors

# TIME ON SCREEN
time <- vision.df %>%
  group_by(ID) %>%
  summarise(n=n()) %>%  # total frames with points identified by ID
  mutate(Total.Time = n/60/60) # 60 frames per second, 60 seconds in a minute
time # view times for all individuals
vision.df <- merge(vision.df, time, by="ID") # adds Total.Time to main dataframe
rm(time)
# removes IDs 3152, 3190, 3223, 3141, who were on screen for less than a minute
# IDs 3227 and 3155 do not appear in dataset, due to no points being identified in the trial
vision.df <- vision.df[!(vision.df$ID == "3152" | vision.df$ID == "3190" | vision.df$ID == "3223" | vision.df$ID == "3141"),]

##TO MAKE SMALLER FILES##
# keep only relevant variables
vision.df <- subset(vision.df, select=c(ID, frame, visualfield, Angle, Angle180, distcm))
# split into multiple files
df1 <- vision.df[1:272815,]
df2 <- vision.df[272816:545630,]
df3 <- vision.df[545631:818245,]
df4 <- vision.df[818246:1091260,]
# write.csv(x=df1, file="C:/DLC1.csv")
# write.csv(x=df2, file="C:/DLC2.csv")
# write.csv(x=df3, file="C:/DLC3.csv")
# write.csv(x=df4, file="C:/DLC4.csv")


#####PICK UP HERE WITH PROVIDED DATA FILES#####
DF1 <- read.csv("DLC1.csv", na.strings = c("","na"), header=TRUE) # opens file)
DF2 <- read.csv("DLC2.csv", na.strings = c("","na"), header=TRUE) # opens file)
DF3 <- read.csv("DLC3.csv", na.strings = c("","na"), header=TRUE) # opens file)
DF4 <- read.csv("DLC4.csv", na.strings = c("","na"), header=TRUE) # opens file)

vision.df <- rbind(DF1, DF2, DF3, DF4)
vision.df <- subset(vision.df, select = -X)

# creates dataframe for use in fixation code -- keeps ID as INTEGER
fixprep <- subset(vision.df, select=c("ID", "frame", "Angle"))


########DESCRIPTIVE DATA########

cond.df <- read.csv("Condition_Data.csv", na.strings = c("","na"), header=TRUE)

# experimental error
cond.df <- cond.df[!(cond.df$Metal.ID == "3135" | cond.df$Metal.ID == "3175"),]
# never on screen
cond.df <- cond.df[!(cond.df$Metal.ID == "3227" | cond.df$Metal.ID == "3155"),]
# on screen for less than a minute
cond.df <- cond.df[!(cond.df$Metal.ID == "3152" | cond.df$Metal.ID == "3190" | cond.df$Metal.ID == "3223" | cond.df$Metal.ID == "3141"),]

# trimming dataset
cond.df <- subset(cond.df, select = -c(Color.ID, Video, R.H., Temp, Light))
# renaming columns to variable name
colnames(cond.df)[1] = "ID"
colnames(cond.df)[4] = "BoxType"
cond.df <- cond.df %>% mutate(BoxType = recode_factor(BoxType, "None" = "Open Control", "Control" = "Blocked Control", "Lift" = "Lift", "Push" = "Push", "Slide" = "Slide"))

# calculating bird weights (subtracting weight of bag)
cond.df$startweight <- (cond.df$Start.Weight - cond.df$Bag.Weight2)
cond.df$Trial.Weight <- as.numeric(cond.df$Trial.Weight)
cond.df$trialweight <- (cond.df$Trial.Weight - cond.df$Bag.Weight1)        
# removes the now irrelevant columns
cond.df <- subset(cond.df, select = -c(Bag.Weight1, Trial.Weight, Bag.Weight2, Start.Weight))

# calculating time between food deprivation and trial start in MINUTES
cond.df$food.dep <- difftime(strptime(cond.df$Deprivation.Time, format = "%H:%M"), strptime(cond.df$Trial.Time, format = "%H:%M"), units = "mins")
cond.df$food.dep <- as.numeric(cond.df$food.dep)

# add info to vision.df
vision.df <- merge(cond.df, vision.df, by="ID")
# makes ID into a factor
vision.df$ID <- as.factor(vision.df$ID)
# corrects variable type
vision.df$BoxType <- as.factor(vision.df$BoxType)
# orders BoxTypes as intended
vision.df$BoxType <- factor(vision.df$BoxType, levels = c("Blocked Control", "Open Control", "Lift", "Push", "Slide"))

# BINS DISTANCE into three categories (cm)
# three groups allows higher resolution of analysis
vision.df$distance <- ifelse(vision.df$distcm <= 10, "00to10",
                            ifelse(vision.df$distcm > 10 & vision.df$distcm <= 20, "10to20",
                                   ifelse(vision.df$distcm > 20 & vision.df$distcm <= 30, "20to30", NA)))
# BINS DISTANCE into two categories (cm)
# two groups creates a more even distribution of data points per group
vision.df$twodist <- ifelse(vision.df$distcm <= 15, "near",
                            ifelse(vision.df$distcm > 15, "far", NA))
# creates BODY x and y positions
vision.df$Body_x <- (vision.df$LeftShoulder_x + vision.df$RightShoulder_x)/2
vision.df$Body_y <- (vision.df$LeftShoulder_y + vision.df$RightShoulder_y)/2
# calculates DISTANCE MOVED of the body between captured frames
vision.df$deltaBp <- ifelse(vision.df$ID == lag(vision.df$ID), sqrt((vision.df$Body_x - lag(vision.df$Body_x))^2 + (vision.df$Body_y - lag(vision.df$Body_y))^2), 0 )
vision.df$deltaBp <- vision.df$deltaBp %>% replace_na(0)  # replaces na from change in ID with 0
vision.df$deltaB <- vision.df$deltaBp / 18  # changes to cm from pixels
# calculates TIME PASSED between captured frames
vision.df$deltaSf <- ifelse(vision.df$ID == lag(vision.df$ID), vision.df$frame - lag(vision.df$frame), 0 )
vision.df$deltaSf <- vision.df$deltaSf %>% replace_na(0) # replaces na from change in ID with 0
vision.df$deltaS <- vision.df$deltaSf / 60  # converts from frames to seconds

# creates column indicating eye use
vision.df$eye <- ifelse(vision.df$visualfield == "Left Front Periphery" | vision.df$visualfield == "Left Fovea" | vision.df$visualfield == "Left Rear Periphery A" | vision.df$visualfield == "Left Rear Periphery B", "Left Eye",
                           ifelse(vision.df$visualfield == "Right Front Periphery" | vision.df$visualfield == "Right Fovea" | vision.df$visualfield == "Right Rear Periphery A" | vision.df$visualfield == "Right Rear Periphery B", "Right Eye",
                                  ifelse(vision.df$visualfield == "Binocular", "Binocular",
                                         ifelse(vision.df$visualfield == "Blind Spot", "Blind Spot", NA))))
vision.df$eye <- factor(vision.df$eye, levels=c("Binocular", "Left Eye", "Right Eye", "Blind Spot")) # orders levels of the factor
# CREATES QUADRANTS of arena
vision.df$area <- ifelse(vision.df$BeakMid_x < vision.df$Box_x &  vision.df$BeakMid_y < vision.df$Box_y, "one",
                         ifelse(vision.df$BeakMid_x < vision.df$Box_x &  vision.df$BeakMid_y > vision.df$Box_y, "two",
                                ifelse(vision.df$BeakMid_x > vision.df$Box_x &  vision.df$BeakMid_y < vision.df$Box_y, "three",
                                       ifelse(vision.df$BeakMid_x > vision.df$Box_x &  vision.df$BeakMid_y > vision.df$Box_y, "four", NA))))

# MARKS BEGIN/END OF BOUTS (defined as no more that 2 seconds of no frames between points)
vision.df$mark <- ifelse(vision.df$deltaS >= 2 | lag(vision.df$ID) != vision.df$ID, "Start",
                         ifelse(lead(vision.df$ID) != vision.df$ID | lead(vision.df$deltaS) >= 2, "End",
                                ifelse(vision.df$deltaS < 2 & lead(vision.df$deltaS) < 2, "continue","na")))
vision.df$mark <- as.factor(vision.df$mark)  # corrects variable type
vision.df$mark <- vision.df$mark %>% replace_na("Start")  # corrects first value in dataframe from "na" to "Start"

first <- vision.df[(vision.df$mark=="Start"),]  # pulls out start frames
first <- first[order(first$ID, first$frame),]
# labels them as bouts
first <- first %>% 
  group_by(ID) %>%
  mutate(boutnum = 1:n())

second <- vision.df[(vision.df$mark == "continue" | vision.df$mark=="End"),]  # pulls out the other frames
second[ , 'boutnum'] = NA  # creates empty column for binding
# rejoins all fixations
vision.df <- rbind(first,second)
vision.df <- vision.df[order(vision.df$ID, vision.df$frame),] # orders the dataframe
vision.df <- vision.df %>% fill(boutnum) # carries the bout number throughout the bout

# calculates total movement ONLY during bouts 
vision.df$deltaB <- ifelse(vision.df$boutnum != lag(vision.df$boutnum), 0, vision.df$deltaB) #eliminates distance values from the bird leaving/returing to view
vision.df$deltaB <- vision.df$deltaB %>% replace_na(0)
# adds total movement to main data frame
vision.df <- vision.df %>%
  group_by(ID) %>% 
  mutate(Total.Move = sum(deltaB))
rm(first,second)

# keep only relevant variables
vision.df <- subset(vision.df, select=c(ID, Population, BoxType, Date, trialweight, food.dep, frame, eye, visualfield, Angle, Angle180, distcm, distance, twodist, 
                                        area, boutnum, Total.Time, Total.Move))


#####FIXATION CODE####################

# code written by ST
# ID must be numeric at this point
firstRun <- TRUE
ids <- c(0)
fixationStarts <- c(0)
fixationEnds <- c(0)
fixationLengths <- c(0)

# fixation criteria: at least six frames (100ms) in which the head angle does not change > 5 degrees from start angle
for (frameNumber in 1:nrow(fixprep)) {
  # pick current row from data
  df <- fixprep[frameNumber,]
  
  # one-time setup on first iteration
  if (firstRun) {
    frameCount <- 0
    id <- 0
    gap <- 0
    previousGap <- 0
    referenceAngle <- df$Angle
    referenceFrame <- df$frame
    firstRun <- FALSE
  }
  
  framesElapsed <- df$frame - referenceFrame
  gap <- framesElapsed - frameCount
  
  # end search when a new ID starts
  if (id != df$ID) {
    frameCount <- -1
    gap <- 0
    referenceAngle <- df$Angle
    referenceFrame <- df$frame
    id <- df$ID
  }
  
  # end search when two gaps are in a row
  else if (gap >= 3) {
    frameCount <- -1
    gap <- 0
    referenceAngle <- df$Angle
    referenceFrame <- df$frame
  }
  
  else {
    # check if fixation has ended
    if (abs(df$Angle - referenceAngle) > 5) {
      # add to results if enough frames have passed
      if (framesElapsed >= 6) {
        ids <- append(ids, id)
        fixationStarts <- append(fixationStarts, referenceFrame)
        fixationEnds <- append(fixationEnds, df$frame)
        fixationLengths <- append(fixationLengths, framesElapsed)
      }
      
      # end search after fixation ends
      frameCount <- -1
      gap <- 0
      referenceAngle <- df$Angle
      referenceFrame <- df$frame
    }
  }
  
  # count parsed frames for gap-finding
  frameCount <- frameCount + 1
}

# creates dataframe of produced fixations
res <- data.frame(ID = ids, Start = fixationStarts, End = fixationEnds, Length = fixationLengths)
res$ID <- as.factor(res$ID) # corrects variable type
res = res[-1,] # removes first row of zero values
colnames(res)[2] <- "frame" # rename for merging

# creates dataframe with all columns, by fixation
fixations.df <- merge(res, vision.df, by=c("ID", "frame"))
fixations.df$duration <- (fixations.df$Length / 60) * 1000 # transforms frames to milliseconds
fixations.df <- fixations.df[order(fixations.df$ID, fixations.df$frame),]  # orders by ID and Frame

rm(fixprep, res)

#####BEHAVIOR DATA PREP (BORIS output)#####

behavior.df <- read.csv("allboris.csv", na.strings = c("","na"), header=TRUE) # opens file

# select necessary columns
behavior.df <- subset(behavior.df, select = c(Subject, Box.Type, Behavioral.category, Start..s.))
# removes ID 3175, whose trial was only recorded for 10 minutes due to camera issue
behavior.df <- behavior.df[!(behavior.df$Subject == "3175"),]
# removes 3227 & 3155, who were never on screen [do not appear in raw DLC data]
behavior.df <- behavior.df[!(behavior.df$Subject == "3227" | behavior.df$Subject == "3155"),]
# removes five more, who were on screen for less than a minute
behavior.df <- behavior.df[!(behavior.df$Subject == "3152" | behavior.df$Subject == "3190" | behavior.df$Subject == "3223" | behavior.df$Subject == "3141"),]

# formats variables
colnames(behavior.df)[1] = "ID"
behavior.df$ID <- as.factor(behavior.df$ID)
colnames(behavior.df)[2] = "BoxType"
behavior.df$BoxType <- as.factor(behavior.df$BoxType)
behavior.df <- behavior.df %>% mutate(BoxType = recode_factor(BoxType, " None" = "Open Control", " Control" = "Blocked Control", " Lift" = "Lift", " Push" = "Push", " Slide" = "Slide"))
behavior.df$BoxType <- factor(behavior.df$BoxType, levels =c("Blocked Control", "Open Control", "Lift", "Push", "Slide"))
colnames(behavior.df)[3] = "Behavior"
behavior.df$Behavior <- as.factor(behavior.df$Behavior)
colnames(behavior.df)[4] = "Event.Time"
behavior.df <- behavior.df[!(behavior.df$Behavior == "Not defined"),]  # caused by notes added during coding
# eliminates the top beak touches that directly relate to a solve
# original coding marked a touch event with the solve event, but to eliminate the redundancy it is altered here
behavior.df$overlap <- ifelse(behavior.df$Behavior == "Top Beak Touch" & lead(behavior.df$Behavior) == "Box Solve" & (lead(behavior.df$Event.Time) - behavior.df$Event.Time) < 2, 0,
                          ifelse(behavior.df$Behavior == "Top Beak Touch" & lag(behavior.df$Behavior) == "Box Solve" & (behavior.df$Event.Time - lag(behavior.df$Event.Time)) < 2, 0, 1))
behavior.df <- behavior.df[!(behavior.df$overlap == 0),]
                                
behavior.df <- subset(behavior.df, select = -c(overlap))

#####BEHAVIOR CODE#####

# count of each event by behavior and ID
behav.df <- behavior.df %>%
  group_by(ID, Behavior, .drop=FALSE) %>%
  summarise(count=n())
# creates data frame with time of each behavior's first event, by ID
latency.df <- aggregate(Event.Time ~ ID + Behavior, min, data = behavior.df, na.action=NULL)
latency.df <- merge(behav.df, latency.df, by=c("ID", "Behavior"), all=TRUE)  # combines latencies with count
latency.df <- latency.df[!(latency.df$Behavior == "Not defined"),]
# changes no-occurrence behavior latencies to 1800 sec (right censored)
latency.df <- latency.df %>%
  mutate(Event.Time = replace_na(Event.Time, 1800))
rm(behav.df)

start.df <- latency.df[(latency.df$Behavior == "Start"),] # gets the starting frame for each trial (when box was revealed)
start.df <- subset(start.df, select=c(ID, Event.Time))  # keeps only ID and time
colnames(start.df)[2] = "Start.Time"  # renames to Start.Time

approach.df <- aggregate(frame ~ ID + distance, min, data = vision.df)  # first frame in each distance category
approach.df <- subset(approach.df, distance == "00to10")  # defining latency to approach as first time within 10cm of box
approach.df <- merge(start.df, approach.df, by="ID")  
approach.df$raw.Approach <- approach.df$frame / 60  # converts frames into seconds
approach.df$Lat.Approach <- approach.df$raw.Approach - approach.df$Start.Time  # adjusts time by start of trial
# changes negatives to zero (ie, bird was nearby before start of trial)
approach.df$Lat.Approach <- ifelse(approach.df$Lat.Approach < 0, 0, approach.df$Lat.Approach)
approach.df <- subset(approach.df, select=-c(distance, frame))  # eliminates unnecessary columns

# dataframe of the first time the bird entered each area (quadrant) of the arena
latexp.df <- aggregate(frame ~ ID + area, min, data = vision.df)
# the latency to explore the entire arena -- the first time in the last quadrant  
latexp.df <- aggregate(frame ~ ID, max, data = latexp.df)
latexp.df$raw.Explore <- latexp.df$frame / 60  # converts to seconds
latexp.df <- subset(latexp.df, select=-frame)  # removes frame column

latpca.df <- merge(approach.df, latexp.df, by="ID")
latpca.df$Lat.Explore <- latpca.df$raw.Explore - latpca.df$Start.Time  # adjusts time by start of trial
latpca.df <- subset(latpca.df, select = c(ID, Lat.Explore, Lat.Approach))  # keeps only latency to explore & approach
rm(latexp.df, approach.df)

latency.df <- merge(latency.df, latpca.df, by="ID")  # adds latencies to approach/explore to all latencies/totals
latency.df <- merge(latency.df, start.df, by="ID")  # adds start time
latency.df <- latency.df[!(latency.df$Behavior == "Start"),] # removes start events (now exist as Start.Time)

latency.df$Latency <- latency.df$Event.Time - latency.df$Start.Time  # adjusts time by start of trial
latency.df$Latency <- ifelse(latency.df$count == 0, 1800, latency.df$Latency) # redoes right censor time
latency.df <- latency.df[order(latency.df$ID),]  # orders by ID

# orders behaviors for easier plotting
latency.df$Behavior <- factor(latency.df$Behavior, levels = c("Top Foot Touch", "Bottom Beak Touch", "Top Beak Touch", "Box Solve"))
# creates binary variable for if the behavior was observed
latency.df$Pres.Behav <- ifelse(latency.df$count > 0, "Yes", "No")

# reintroduces BoxType, based on ID
latency.df <- latency.df %>%  mutate(BoxType = case_when(ID == "3222" | ID == "3240" | ID== "3164" | ID == "3226" | ID == "3227" | ID == "3211" | ID == "3208" | ID == "3160" | ID == "3147" | ID == "3224" | ID == "3155" | ID == "3151" | ID == "3150" ~ "Blocked Control", 
                                                     ID == "3126" | ID == "3152" | ID=="3157" | ID == "3145" | ID == "3154" | ID == "3171" | ID == "3182" | ID == "3166" | ID == "3167" | ID == "3172" | ID == "3174" | ID == "3190" | ID == "3187" ~ "Open Control",
                                                     ID == "3217" | ID == "3127" | ID == "3131" | ID== "3159" | ID== "3161" | ID == "3156" | ID == "3236" | ID == "3237" | ID == "3168" | ID == "3179" | ID == "3183" | ID =="3215" | ID =="3239" ~ "Lift",
                                                     ID == "3207" | ID == "3134" | ID== "3206" | ID== "3135" | ID== "3158" | ID== "3141" | ID =="3146" | ID == "3238" | ID == "3173" | ID == "3175" | ID == "3233" | ID == "3229" | ID == "3189" ~ "Push",
                                                     ID == "3223" | ID == "3143" | ID == "3144" | ID== "3149" | ID== "3136" | ID== "3142" | ID =="3148" | ID == "3169" | ID == "3181" | ID == "3177" | ID == "3176" | ID == "3184" | ID == "3232" ~ "Slide",
                                                     TRUE ~ NA_character_))

#####SOLVING CODE#####

# removes other behaviors, focus on solving
solve.df <- latency.df[(latency.df$Behavior == "Box Solve"),]
solve.df <- na.omit(solve.df)
# creates a solve dataframe for easy addition to other dataframes
solve.df <- subset(solve.df, select = c(ID, Pres.Behav, Latency))
colnames(solve.df)[2] <- "Opened"  # renames Pres.Behav
colnames(solve.df)[3] <- "Solve.Time"  # renames Event.Time

# creates binary variable for use in survival curves--1 is solve, 0 is no solve
solve.df$event <- ifelse(solve.df$Opened == "Yes", 1, 0)
latency.df <- merge(solve.df, latency.df, by="ID")  # adds Opened, event, and Solve.Time to data frame
# sets BoxType as a factor and organizes the levels
latency.df$BoxType <- as.factor(latency.df$BoxType)
latency.df$BoxType <- factor(latency.df$BoxType, levels =c("Blocked Control", "Open Control", "Lift", "Push", "Slide"))
# removes events of "box solve", now represented by "Solve.Time"
latency.df <- latency.df[!(latency.df$Behavior == "Box Solve"),]


#####DISTANCE AND VISUAL FIELDS -- PROPORTION####

# outputs the proportion of fixations based on two variables -- distance and visual field
prop.fix <- fixations.df %>%
  group_by(distance, visualfield, .drop=FALSE) %>% #add ID
  summarise(count = n()) %>%
  mutate(Observed = count / sum(count))

# adds expected proportions, at chance levels (given the size of each vf) for plotting
prop.fix <- prop.fix %>% mutate(Expected = case_when(visualfield == "Blind Spot" ~ (0.17777777777778), visualfield == "Binocular" ~ (0.072222222222222),
                                                     visualfield == "Left Front Periphery" | visualfield == "Right Front Periphery" ~ (0.108333333333333),
                                                     visualfield == "Left Fovea" | visualfield == "Right Fovea" ~ (0.0583333333333333333),
                                                     visualfield == "Left Rear Periphery A" | visualfield == "Right Rear Periphery A" ~ (0.102777777777778),
                                                     visualfield == "Left Rear Periphery B" | visualfield == "Right Rear Periphery B" ~ (0.105555555555556)))
# subsets frequency by distance for statistical analysis
prop00.df <- subset(prop.fix, distance == "00to10")
prop10.df <- subset(prop.fix, distance == "10to20")
prop20.df <- subset(prop.fix, distance == "20to30")

# chi squared to test against NULL HYPOTHESIS that head fixations in each VISUAL FIELD occur at random (0-10 cm)
a0 <- chisq.test(x=prop00.df$count, p = prop00.df$Expected) # simulate.p.value = TRUE
a0 # view
prop00.df$stdres <- a0$stdres # adds standardized residuals to original data frame for plotting
# chi squared to test against NULL HYPOTHESIS that head fixations in each VISUAL FIELD occur at random (10-20 cm)
a1 <- chisq.test(x=prop10.df$count, p = prop10.df$Expected) # simulate.p.value = TRUE
a1 # view
prop10.df$stdres <- a1$stdres # adds standardized residuals to original data frame for plotting
# chi squared to test against NULL HYPOTHESIS that head fixations in each VISUAL FIELD occur at random (20-30 cm)
a2 <- chisq.test(x=prop20.df$count, p = prop20.df$Expected) # simulate.p.value = TRUE
a2 # view
prop20.df$stdres <- a2$stdres # adds standardized residuals to original data frame for plotting

# arranges dataframe as needed -- one level of categorical distance at a time
zero.df <- melt(prop00.df, id.vars = c("visualfield", "distance", "stdres"), measure.vars = c("Observed", "Expected"))
colnames(zero.df)[4] <- "Head Fixations"
# includes standardized residuals from earlier analysis
zero.df$stdres <- format(zero.df$stdres, digits = 1, nsmall = 1)
zero.df$stdres <- as.numeric(zero.df$stdres)
zero.df$visualfield <- as.factor(zero.df$visualfield)
zero.df$effect <- ifelse(abs(zero.df$stdres) > 2, "*", "")
pattern_guide <- guide_legend(override.aes = list(pattern_fill = 'white'))
# bar graph of PROPORTION of VISUAL FIELD use at 10-20 DISTANCE, with expected values
nearVF.plot <- ggplot(zero.df, aes(x=visualfield, y=value, fill = visualfield), width=0.8) + theme_classic(base_size =15) +   # sets variable and style/size
  ggtitle("c) 00 - 10 cm") + guides(color="none", fill="none") +   # titles plot and removes legends for color and fill (same as x-axis)
  scale_y_continuous(name = "Proportion of Head Fixations", limit = c(0,0.4)) +   # labels the y-axis, caps at 0.4
  scale_x_discrete(name ="Area of the Visual Field Directed at Box",labels = function(x) stringr::str_wrap(x, width = 10.5)) +   # labels the x-axis, wraps text of the visual fields
  geom_text(aes(label = effect), size=7, y= 0.3, fontface="bold", color="#333333") + # adds * to indicate significance (from zero.df)
  geom_text(aes(label = ifelse(`Head Fixations` == "Observed", stdres, "")), size=5, y= 0.35, fontface="bold", color="#333333") +   # adds the standardized residual values (from zero.df)
  geom_col_pattern(aes(fill = visualfield, pattern_density = `Head Fixations`), pattern = 'stripe', color="black", pattern_color = "black", 
                   pattern_fill = "white", position=position_dodge(width=0.9))   + # adds pattern for differentiating the observed and expected columns
  scale_pattern_density_manual(values = c("Observed" = 0, "Expected"=0.3))   + # density of the pattern above (no stripes=observed, stripes=expected)
  scale_fill_manual(values = c("Binocular" = "#990033", "Left Rear Periphery A" = "#66FF99", "Left Rear Periphery B" = "#66FF99",
                               "Right Rear Periphery A" = "#66FF99", "Right Rear Periphery B" = "#66FF99", "Left Front Periphery" = "#99CCFF", 
                               "Right Front Periphery" = "#99CCFF", "Left Fovea" = "#CCCC00", "Right Fovea" = "#CCCC00", "Blind Spot" = "#996699")) + # adds visual field colors for increased readability
  theme(legend.position = c(.85, 1), legend.direction = "horizontal", legend.title = element_blank(),   # moves the legend, removes the title
        plot.title = element_text(size = 18))   # increases plot title size
print(nearVF.plot)

# arranges dataframe as needed -- one level of categorical distance at a time
ten.df <- melt(prop10.df, id.vars = c("visualfield", "distance", "stdres"), measure.vars = c("Observed", "Expected"))
colnames(ten.df)[4] <- "Head Fixations"
# includes standardized residuals from earlier analysis
ten.df$visualfield <- as.factor(ten.df$visualfield)
ten.df$effect <- ifelse(abs(ten.df$stdres) > 2, "*", "")
ten.df$stdres <- as.numeric(ten.df$stdres)
ten.df$stdres <- format(ten.df$stdres, digits = 1, nsmall = 1)

# bar graph of PROPORTION of VISUAL FIELD use at 10-20 DISTANCE, with expected values
midVF.plot <- ggplot(ten.df, aes(x=visualfield, y=value, fill = visualfield), width=0.8) + theme_classic(base_size =15) +    # sets variable and style/size
  ggtitle("b) 10 - 20 cm") + guides(color="none", fill="none") +   # titles plot and removes legends for color and fill (same as x-axis)
  scale_y_continuous(name = "Proportion of Head Fixations", limit = c(0,0.4)) +   # labels the y-axis, caps at 0.4
  scale_x_discrete(name ="Area of the Visual Field Directed at Box",labels = function(x) stringr::str_wrap(x, width = 10.5)) +   # labels the x-axis, wraps text of the visual fields
  geom_text(aes(label = effect), size=7, y= 0.3, fontface="bold", color="#333333") + # adds * to indicate significance (from zero.df)
  geom_text(aes(label = ifelse(`Head Fixations` == "Observed", stdres, "")), size=5, y= 0.35, fontface="bold", color="#333333") +   # adds the standardized residual values (from zero.df)
  geom_col_pattern(aes(fill = visualfield, pattern_density = `Head Fixations`), pattern = 'stripe', color="black", pattern_color = "black", 
                   pattern_fill = "white", position=position_dodge(width=0.9)) +   # adds pattern for differentiating the observed and expected columns
  scale_pattern_density_manual(values = c("Observed" = 0, "Expected"=0.3)) +   # density of the pattern above (no stripes=observed, stripes=expected)
  scale_fill_manual(values = c("Binocular" = "#990033", "Left Rear Periphery A" = "#66FF99", "Left Rear Periphery B" = "#66FF99",
                               "Right Rear Periphery A" = "#66FF99", "Right Rear Periphery B" = "#66FF99", "Left Front Periphery" = "#99CCFF", 
                               "Right Front Periphery" = "#99CCFF", "Left Fovea" = "#CCCC00", "Right Fovea" = "#CCCC00", "Blind Spot" = "#996699")) +   # adds visual field colors for increased readability
  theme(legend.position = c(.85, 1), legend.direction = "horizontal", legend.title = element_blank(),   # moves the legend, removes the title
        plot.title = element_text(size = 18))   # increases plot title size
print(midVF.plot)

# arranges dataframe as needed -- one level of categorical distance at a time
twenty.df <- melt(prop20.df, id.vars = c("visualfield", "distance", "stdres"), measure.vars = c("Observed", "Expected"))
colnames(twenty.df)[4] <- "Head Fixations"
twenty.df$visualfield <- as.factor(twenty.df$visualfield)
twenty.df$effect <- ifelse(abs(twenty.df$stdres) > 2, "*", "")  # adds effect, based on whether the standardized residual is greater than 2.0
twenty.df$stdres <- format(twenty.df$stdres, digits = 1, nsmall = 1)  # keeps trailing zeros
# bar graph of PROPORTION of VISUAL FIELD use at 20-30 DISTANCE, with expected values
farVF.plot <- ggplot(twenty.df, aes(x=visualfield, y=value, fill = visualfield), width=0.8) +  theme_classic(base_size =15) +  # sets variable and style/size
  ggtitle("a) 20 - 30 cm") + guides(color="none", fill="none") +   # titles plot and removes legends for color and fill (same as x-axis)
  scale_y_continuous(name = "Proportion of Head Fixations", limit = c(0,0.4)) +   # labels the y-axis, caps at 0.5
  scale_x_discrete(name ="Area of the Visual Field Directed at Box",labels = function(x) stringr::str_wrap(x, width = 10.5)) +   # labels the x-axis, wraps text of the visual fields
  geom_text(aes(label = effect), size=7, y= 0.33, fontface="bold", color="#333333") +   # adds * to indicate significance (from zero.df)
  geom_text(aes(label = ifelse(`Head Fixations` == "Observed", stdres, "")), size=5, y= 0.37, fontface="bold", color="#333333") +   # adds the standardized residual values (from zero.df)
  geom_col_pattern(aes(fill = visualfield, pattern_density = `Head Fixations`), pattern = 'stripe', color="black", pattern_color = "black", 
                   pattern_fill = "white", position=position_dodge(width=0.9)) +   # adds pattern for differentiating the observed and expected columns
  scale_pattern_density_manual(values = c("Observed" = 0, "Expected"=0.3)) +   # density of the pattern above (no stripes=observed, stripes=expected)
  scale_fill_manual(values = c("Binocular" = "#990033", "Left Rear Periphery A" = "#66FF99", "Left Rear Periphery B" = "#66FF99",
                               "Right Rear Periphery A" = "#66FF99", "Right Rear Periphery B" = "#66FF99", "Left Front Periphery" = "#99CCFF", 
                               "Right Front Periphery" = "#99CCFF", "Left Fovea" = "#CCCC00", "Right Fovea" = "#CCCC00", "Blind Spot" = "#996699")) +   # adds visual field colors for increased readability
  theme(legend.position = c(.85, 1), legend.direction = "horizontal", legend.title = element_blank(),   # moves the legend, removes the title
        plot.title = element_text(size = 18))   # increases plot title size
print(farVF.plot)



#####DISTANCE AND HEAD ANGLES -- CONTINUOUS####

# FULL POPULATION
# model to look at head angle (continuous, 0-180, no left/right) to distance (continuous, cm)
value <- lme(Angle180 ~ distcm, random = ~distcm|ID, data=fixations.df) # multiple data points per ID, included as a random factor
summary(value)  # shows significant effect
anova(value)
# check residuals
par(mfrow = c(1, 2))
plot(residuals(value) ~ fitted(value), main = "residuals v.s. Fitted")
qqnorm(residuals(value))  # even spread, straight line > fits assumption

# BY INDIVIDUAL
# establishes list of IDs
vec <- unique(na.omit(fixations.df$ID))
# creates empty dataframes
df = data.frame()
df_d = data.frame()
# for loop to model HEAD FIXATION ANGLE against DISTANCE (continuous) for EACH INDIVIDUAL
for(h in vec) {
  d<- fixations.df[(fixations.df$ID == h),]  # selects one ID
  value <- lm(Angle180 ~ distcm, data=d)     # runs model
  list <- summary(value)
  df = data.frame(ID = h,
                  Intercept = value$coefficients[1],
                  distCoef = value$coefficients[2],
                  fstatistic = list$fstatistic[1],
                  dfn = list$fstatistic[2],
                  dfd = list$fstatistic[3],
                  Rsq = list$r.squared,
                  aRsq = list$adj.r.squared,
                  p.value = list$coefficients[2,4])  # pulls relevant data from output, into a dataframe
  df_d <- bind_rows(df_d, df)   # combines all individuals into one dataframe
}
# check the residuals
par(mfrow = c(1, 2))
plot(residuals(value) ~ fitted(value), main = "residuals v.s. Fitted")
qqnorm(residuals(value))  # even spread, straight line > fits assumption
# adds column denoting significance based on p-value (as a factor)
df_d$significance <- as.factor(ifelse(df_d$p.value <= 0.05, 'S', 'NS'))

# merges with fixation data, so all data points can be plotted
scatter <- merge(fixations.df, df_d, by="ID")
scatter$aRsq <- as.numeric(scatter$aRsq)
# continuous HEAD ANGLE by DISTANCE for every INDIVIDUAL
p.anglevdist <- ggplot(scatter, aes(x=distcm, y=Angle180, fill = aRsq, color = aRsq)) + facet_wrap(~ID, ncol=10) + #variables assigned
  geom_point(size = 2.5, shape=21, alpha = 0.7, stroke=0, color="black") + theme_classic(base_size = 15) + # geom_point and theme
  ylab("Head Fixation Angle") + xlab("Distance from Box (cm)") + # axes labelled
  scale_y_continuous(breaks=c(0, 90, 180)) + scale_x_continuous(breaks=c(0,15,30)) +  # setting axis breaks
  scale_fill_gradient(name = "R-Squared", low = "#CCFFCC", high = "#333300") + # defining aesthetics for color and fill
  stat_smooth(aes(y=Angle180), method="lm", linewidth=1.7, color="yellow") +  # regression line added to scatter plots
  geom_text(aes(label = ifelse(`significance` == "NS", "NS", ""), x=5, y=169), size=4, color="black") +
  theme(legend.position = c(0.85,0.05), axis.text.x = element_text(angle = 70, vjust = 0.7), strip.text = element_blank(),
        legend.title = element_text(size=10), legend.text = element_text(size = 10), legend.direction = "horizontal") # rotates x-axis text and removes facet labels (IDs)
print(p.anglevdist)

#####DURATION OF FIXATIONS####

# mixed ANOVA for fixation duration across DISTANCE (categorical) -- repeated measures with ID -- 
# loses 1 ID due to missing data
lm_durD <- aov_ez("ID", "duration", within= "distance", fun_aggregate = mean, data = fixations.df) 
summary(lm_durD) # results
lm_durD # results

# creating data frame for plotting
emD <- emmeans(lm_durD, "distance", error="within")  # gets the means
meansD.df <- summary(emD) # sets a data frame
pemD <- pairs(emD)  # pairwise comparisons
summary(pemD) # values
meansD.df <- meansD.df %>% mutate(distance = recode_factor(distance, "X00to10" = "0 - 10", "X10to20" = "10 - 20", "X20to30" = "20 - 30"))  # fixes factor name
meansD.df$signif <- ifelse(meansD.df$distance == "10 - 20", "a", 
                           ifelse(meansD.df$distance == "0 - 10", "ab",
                                  ifelse(meansD.df$distance == "20 - 30", "b", "na"))) # labels groups by pairwise significance

# MEAN DURATION in seconds by VISUAL FIELD, with error bars
durD.plot <- ggplot(meansD.df, aes(x=distance, y=emmean)) +  theme_classic(base_size=15) +  ggtitle("a)") +
  geom_errorbar(data = meansD.df, mapping=aes(x=distance, ymin=emmean-SE, ymax=emmean+SE), position=position_dodge(width=0.6), width=0.3) +   # goes first so it appears behind points
  geom_point(size=5) +
  scale_y_continuous(name = "Mean Duration of Head Fixations (ms +/- SE)", breaks=c(250, 300, 350, 400), limits=c(245,400)) +
  scale_x_discrete(name ="Distance from Box (cm)", labels = function(x) stringr::str_wrap(x, width = 8)) + 
  geom_text(aes(label = signif, y=emmean+50),fontface="bold", size=6)  # puts letters above points to show significance 
print(durD.plot)

# mixed ANOVA for fixation duration by VISUAL FIELD (categorical) -- repeated measures with ID -- 
# loses 1 ID due to missing data
lm_durv <- aov_ez("ID", "duration", within= "visualfield", fun_aggregate = mean, data = fixations.df)
summary(lm_durv)
lm_durv

# creating dataframe for plotting
emV <- emmeans(lm_durv, "visualfield", error="within")
meansV.df <- summary(emV)
pemV <- pairs(emV)
summary(pemV)
# adds spaces to visual field names, for plotting reasons
meansV.df <- meansV.df %>% mutate(visualfield = recode_factor(visualfield, Left.Rear.Periphery.B = "Left Rear Periphery B", Left.Rear.Periphery.A = "Left Rear Periphery A", Left.Fovea = "Left Fovea",
                                                              Left.Front.Periphery = "Left Front Periphery", Binocular = "Binocular", Right.Front.Periphery = "Right Front Periphery", Right.Fovea= "Right Fovea",
                                                              Right.Rear.Periphery.A = "Right Rear Periphery A", Right.Rear.Periphery.B = "Right Rear Periphery B", Blind.Spot = "Blind Spot"))

meansV.df$signif <- ifelse(meansV.df$visualfield == "Left Rear Periphery B" | meansV.df$visualfield == "Right Rear Periphery B" | meansV.df$visualfield == "Blind Spot", "d", 
                           ifelse(meansV.df$visualfield == "Right Fovea", "a",
                                  ifelse(meansV.df$visualfield == "Right Front Periphery", "ab",
                                         ifelse(meansV.df$visualfield == "Left Fovea", "bc","c"))))
                                         
# MEAN DURATION in seconds by VISUAL FIELD, with error bars
durvf.plot <- ggplot(meansV.df, aes(x=visualfield, y=emmean, fill=visualfield)) +  theme_classic(base_size=15) +  ggtitle("b)") +
  geom_errorbar(data = meansV.df, mapping=aes(x=visualfield, ymin=emmean-SE, ymax=emmean+SE), position=position_dodge(width=0.6), width=0.3) +   # goes first so it appears behind points
  geom_point(size=5, pch=21, color="black") + guides(fill="none") +
  scale_y_continuous(name = "Mean Duration of Head Fixations (ms +/- SE)", breaks=c(250, 300, 350, 400), limits=c(245,400)) +
  scale_x_discrete(name ="Area of the Visual Field Directed at Box", labels = function(x) stringr::str_wrap(x, width = 8)) + 
  geom_text(aes(label = signif, y=emmean+50),fontface="bold", size=6) +   # puts letters above points to show significance
  scale_fill_manual(values = c("Binocular" = "#990033", "Left Rear Periphery A" = "#66FF99", "Right Rear Periphery A" = "#66FF99", "Left Rear Periphery B" = "#66FF99",
                               "Right Rear Periphery B" = "#66FF99", "Left Front Periphery" = "#99CCFF", "Right Front Periphery" = "#99CCFF",
                               "Left Fovea" = "#CCCC00", "Right Fovea" = "#CCCC00", "Blind Spot" = "#996699")) 
print(durvf.plot)

#####ORDER OF VISUAL FIELD FIXATIONS####

# pulls out only the columns necessary for sequential visual field states
order.df <- subset(fixations.df, select = c("ID", "frame", "boutnum", "visualfield"))
order.df <- order.df[order(order.df$ID, order.df$frame),]
# assigns variables as needed
id <- as.character(order.df$ID)
id1 <- order.df %>% distinct(order.df$ID)
colnames(id1)[1] = "ID"
id <- as.vector(id1$ID)

# files start at 0, add 1 to avoid error in TraMineR
order.df$frame <- order.df$frame + 1
# set categorical variables
order.df$visualfield <- as.factor(order.df$visualfield)
# merges periphery A and B together
order.df <- order.df %>% mutate(visualfield = recode_factor(visualfield, `Left Rear Periphery B` = "Left Rear Periphery", 
                                                            `Left Rear Periphery A` = "Left Rear Periphery", `Left Fovea` = "Left Fovea", `Left Front Periphery` = "Left Front Periphery",
                                                            `Binocular` = "Binocular", `Right Front Periphery` = "Right Front Periphery", `Right Fovea` = "Right Fovea", `Right Rear Periphery A` = "Right Rear Periphery",
                                                            `Right Rear Periphery B` = "Right Rear Periphery", `Blind Spot` = "Blind Spot"))


# creates a seqelist object from EVENT sequence
seq.e <- seqecreate(data=order.df, timestamp=order.df$frame, id=order.df$ID,  # sets timestamp and ID
                    event=order.df$visualfield, end.event=NULL, tevent="transition",  # sets visual field as event, looking at transitions
                    use.labels = TRUE, weighted = NULL)

# gathers all viable subsequences 
seq.s <- seqefsub(seq.e, pmin.support = 0.10, # pmin.support = minimum *percentage* of sequences it appears in (10% = minimum 6 individuals)
                  constraint=seqeconstraint(max.gap = 60, # max.gap = maximum number of frames allowed between events to be in the same subsequence (60 frames = 1 second = 10 x the minimum fixation length)
                                            window.size = 180, # window.size = maximum amount of time in which a subsequence can occur in (180 frames = 3 seconds = up to 30 fixations possible)
                                            age.min = -1, age.max = -1, age.max.end = -1,
                                            count.method = "CDIST"), max.k = 10) #cdist = total count of occurrences
seq.n <- seqentrans(seq.s) # the number of transitions / events per subsequence
seq.ay <- seq.n[seq.n$data$ntrans>2]  # eliminates any subsequences that are only two fixations


seq.c <- seqeapplysub(seq.ay, method="count", rules=FALSE)  # to count the occurrence of each subsequence within each sequence (ID)
seq.c <- ifelse(seq.c < 20, NA, seq.c)  # eliminates any subsequences within an individual if it occurs less than 20 times
seq.c <- as.data.frame(seq.c)
seq.b <- cbind(id, seq.c)  # combines reduced subsequences with IDs

#creates empty dataframes
df = data.frame()
df_subs = data.frame()
# for loop to reorganize dataframe such that each subsequence for each individual is its own row, with count
for(h in id) {
  d<- seq.b[(seq.b$id == h),]
  df <- d %>%
    select_if(~ !(is.na(.)))
  df1 <- melt(df, id.vars = "id")
  df <- cbind(df1, ID=h)
  df_subs <- bind_rows(df_subs, df)
}

df_subs <- na.omit(df_subs)  # eliminates NA values
# total number of birds with each subsequence
tot.subjects <- df_subs %>%
  group_by(variable) %>%
  summarise(birds = n()) 
# total number of occurrences of each subsequence
tot.seq <- df_subs %>%
  group_by(variable) %>%
  mutate(total = sum(value))
tot.seq <- merge(tot.subjects, tot.seq, by="variable") # combines into dataset
tot.seq <- subset(tot.seq, select=c(variable, birds, total)) # keeps relevant factors
tot.seq <- distinct(tot.seq) # eliminates duplicate rows
sum(tot.seq$total) 

# separates subsequence string into separate columns
df_totalF <- separate(df_subs, variable, into=c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth"), sep="-", extra="merge")
# strings are uneven -- replace NA values with "End", notifying the end of the subsequence
df_totalF <- df_totalF %>%
  mutate(Fourth = replace_na(Fourth, "End")) %>%
  mutate(Fifth = replace_na(Fifth, "End")) %>%
  mutate(Sixth = replace_na(Sixth, "End"))%>%
  mutate(Seventh = replace_na(Seventh, "End")) %>%
  mutate(Eighth = replace_na(Eighth, "End")) %>%
  mutate(Ninth = replace_na(Ninth, "End")) %>%
  mutate(Tenth = replace_na(Tenth, "End"))

# sets the frequency as numeric
df_totalF$value <- as.numeric(df_totalF$value)
# eliminates unnecessary / redundant columns
subF <- subset(df_totalF, select = -c(ID))
# labels each subsequence individually, so the order can be maintained
subF$sub <-  1:nrow(subF)


# gets count of each visual field first in subsequence
hori <- subF %>%
  group_by(First) %>%
  mutate(count = value)
hori <- subset(hori, select=c(id, sub, First, count))
hori$fixation = "First"
colnames(hori)[3] <- "visualfield"
# gets count of each visual field in subsequence relative to previous visual field
add <- subF %>%
  group_by(First, Second) %>%
  mutate(count = value)
add <- subset(add, select=c(id, sub, Second, count))
add$fixation = "Second"
colnames(add)[3] <- "visualfield"
hori <- rbind(hori, add)
# gets count of each visual field in subsequence relative to previous visual field
add <- subF %>%
  group_by(First, Second, Third) %>%
  mutate(count = value)
add <- subset(add, select=c(id, sub, Third, count))
add$fixation = "Third"
colnames(add)[3] <- "visualfield"
hori <- rbind(hori, add)
# gets count of each visual field in subsequence relative to previous visual field
add <- subF %>%
  group_by(First, Second, Third, Fourth) %>%
  mutate(count = value)
add <- subset(add, select=c(id, sub, Fourth, count))
add$fixation = "Fourth"
colnames(add)[3] <- "visualfield"
hori <- rbind(hori, add)
# gets count of each visual field in subsequence relative to previous visual field
add <- subF %>%
  group_by(First, Second, Third, Fourth, Fifth) %>%
  mutate(count = value)
add <- subset(add, select=c(id, sub, Fifth, count))
add$fixation = "Fifth"
colnames(add)[3] <- "visualfield"
hori <- rbind(hori, add)
# gets count of each visual field in subsequence relative to previous visual field
add <- subF %>%
  group_by(First, Second, Third, Fourth, Fifth, Sixth) %>%
  mutate(count = value)
add <- subset(add, select=c(id, sub, Sixth, count))
add$fixation = "Sixth"
colnames(add)[3] <- "visualfield"
hori <- rbind(hori, add)
# gets count of each visual field in subsequence relative to previous visual field
add <- subF %>%
  group_by(First, Second, Third, Fourth, Fifth, Sixth, Seventh) %>%
  mutate(count = value)
add <- subset(add, select=c(id, sub, Seventh, count))
add$fixation = "Seventh"
colnames(add)[3] <- "visualfield"
hori <- rbind(hori, add)
# gets count of each visual field in subsequence relative to previous visual field
add <- subF %>%
  group_by(First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth) %>%
  mutate(count = value)
add <- subset(add, select=c(id, sub, Eighth, count))
add$fixation = "Eighth"
colnames(add)[3] <- "visualfield"
hori <- rbind(hori, add)
# gets count of each visual field in subsequence relative to previous visual field
add <- subF %>%
  group_by(First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth, Ninth) %>%
  mutate(count = value)
add <- subset(add, select=c(id, sub, Ninth, count))
add$fixation = "Ninth"
colnames(add)[3] <- "visualfield"
hori <- rbind(hori, add)
# gets count of each visual field in subsequence relative to previous visual field
add <- subF %>%
  group_by(First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth, Ninth, Tenth) %>%
  mutate(count = value)
add <- subset(add, select=c(id, sub, Tenth, count))
add$fixation = "Tenth"
colnames(add)[3] <- "visualfield"
hori <- rbind(hori, add)

# recodes factors to eliminate parantheses
hori <- hori %>% mutate(visualfield = recode_factor(visualfield, `(Left Rear Periphery)` = "Left Rear Periphery", `(Left Fovea)` = "Left Fovea", 
                                                        `(Left Front Periphery)` = "Left Front Periphery", `(Binocular)` = "Binocular", `(Right Front Periphery)` = "Right Front Periphery",
                                                        `(Right Fovea)` = "Right Fovea", `(Right Rear Periphery)` = "Right Rear Periphery", `(Blind Spot)` = "Blind Spot", End = "End of Sequence"))
# reorders factors for plotting
hori$visualfield <- factor(hori$visualfield, levels = c(  "Right Rear Periphery", "Right Fovea", "Right Front Periphery","Binocular",
                                                             "Left Front Periphery", "Left Fovea","Left Rear Periphery", "Blind Spot","End of Sequence")) 
# reorders fixations by sequence
hori$fixation <- factor(hori$fixation, levels = c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth"))
hori <- na.omit(hori)
# sets frequency as numeric
hori$count <- as.numeric(hori$count)
# eliminates fixation numbers with very low counts
hori4 <- hori[!(hori$fixation == "Seventh" | hori$fixation == "Eighth" | hori$fixation == "Ninth" | hori$fixation == "Tenth"),]

# creates alluvial plot
allu.plot <- ggplot(hori4, aes(x = fixation, stratum = visualfield, alluvium = sub, y = count, fill=visualfield)) +  # sets data and variables
  theme_classic(base_size=16) + ylab("Total Fixations per Visual Field") + xlab("Fixation in Subsequence") + ggtitle("a)") + # labels and theme
  geom_flow(color="gray", linewidth=0.5) + geom_stratum(alpha = .5, color="black") + # sets strata details
  scale_fill_manual(values = c("Binocular" = "#990033", "Left Rear Periphery" = "#339900", "Right Rear Periphery" = "#33FF00", "Left Front Periphery" = "#003366",
                               "Right Front Periphery" = "#3399FF", "Left Fovea" = "#666600", "Right Fovea" = "#FFFF00", "Blind Spot" = "#996699", "End of Sequence" = "darkgray")) +  # sets colors
  theme(axis.text.y = element_blank(), axis.ticks.y.left = element_blank(), legend.title = element_blank())  # eliminates y-axis label, ticks, and the legend
print(allu.plot)


#####VISUAL EXPLORATION STRATEGY NEAR BOX####

fix.near <- fixations.df[(fixations.df$twodist == "near"),]

# dataframe for NEAR BOX (<15cm) data
pcaN.df <- subset(fix.near, select = c("ID", "Angle180", "duration"))
# adds columns for the MEAN HEAD ANGLE (out of 180), as well as TOTAL FREQUENCY of fixations
pcaN.df <- pcaN.df %>% group_by(ID) %>% mutate(N.m.angle = mean(Angle180)) %>% mutate(N.total = n())
pcaN.df <- subset(pcaN.df, select = -c(Angle180, duration))
# finds DEGREE of LEFT PREFERENCE (proportion of head fixations with left eye versus right eye)
side <- fix.near %>%
  group_by(ID, eye) %>%
  summarise(bias.fix = n())
side$ID <- as.factor(side$ID)
# eliminates fixations in binocular (both eyes) and blind spot (neither eye)
side <- side[!(side$eye == "Binocular" | side$eye == "Blind Spot"),]
# reorganizes data
side <- dcast(side, ID ~ eye, fun.aggregate=sum)
# calculates ratio of left fixations to right fixations
side$N.pref <- side$`Left Eye` / side$`Right Eye`
side <- subset(side, select=-c(`Left Eye`, `Right Eye`))
pcaN.df <- merge(pcaN.df, side, by="ID")
pcaN.df <- distinct(pcaN.df)

# set seed for reproducibility
set.seed(123)

# PCA analysis
pairs(pcaN.df[2:4])
# cluster analysis
scale(pcaN.df[2:4])
# elbox method to pick centers
fviz_nbclust(pcaN.df[2:4], kmeans, method = "wss")
# finds the kmeans
kmeans2 <- kmeans(pcaN.df[2:4], centers = 3, nstart = 100)
# plots the clusters
fviz_cluster(kmeans2, data = pcaN.df[2:4]) + theme_classic(base_size = 20) + theme(plot.title = element_blank())
# output
print(kmeans2)

# creates dataframe with the cluster and ID
stratN.df <- data.frame(pcaN.df$ID, kmeans2$cluster)
# renames columns
colnames(stratN.df)[1] = "ID"
colnames(stratN.df)[2] = "N.Strategy"
# sets strategy and ID as factors
stratN.df$N.Strategy <- as.factor(stratN.df$N.Strategy)
stratN.df$ID <- as.factor(stratN.df$ID)
stratN.df <- stratN.df %>% mutate(N.Strategy = recode_factor(N.Strategy, "1" = "Mid-N", `2` = "Low-N", `3` = "High-N"))
stratN.df$N.Strategy <- factor(stratN.df$N.Strategy, levels=c("Low-N", "Mid-N", "High-N"))
pcaN.df <- merge(stratN.df, pcaN.df, by="ID")
colnames(pcaN.df)[3] = "Head Angle"
colnames(pcaN.df)[4] = "Total"
colnames(pcaN.df)[5] = "Side Preference"

# run PCA analysis
pc <- prcomp(pcaN.df[,c(-1,-2)],
             center = TRUE,
             scale. = TRUE)
attributes(pc)
print(pc)
summary(pc)

# check for colinearity
pairs.panels(pc$x, gap = 0, pch=21,
             bg = c("red", "yellow", "blue")[pcaN.df$N.Strategy])

# plot PCA results
g <- ggbiplot::ggbiplot(pc, obs.scale = 1, var.scale = 1, groups = pcaN.df$N.Strategy, ellipse = TRUE, circle = FALSE,
              ellipse.prob = 0.68, varname.size=1, varname.adjust=20, point.size = 2) # size and adjust set so that labels don't appear (manual labels added below)
g <- g + theme_classic(base_size = 17)  + ggtitle("a) Head Fixation Strategy Near Box") + guides(fill="none") + 
  scale_fill_manual(values=c("Low-N" = "#FF9933", "Mid-N" = "#FF6600", "High-N" = "#993300")) +
  scale_color_manual(values=c("Low-N" = "#FF9933", "Mid-N" = "#FF6600", "High-N" = "#993300")) +
  theme(legend.direction = 'horizontal', legend.position = 'top', legend.title = element_blank(), legend.text = element_text(size=16))


pca.vars1<- as.data.frame(pc$rotation) # extract the data for the variables
labels <- data.frame(1:3) # creates empty data frame
labels$labels <- rownames(pca.vars1) # makes the contributing factors a column
labels$PC1 <- c(-0.4, 1.75, -2)
labels$PC2 <- c(1.75, 0, -1.25)  # coordinates for labels on plot (aesthetic)

g <- g + geom_label(labels, mapping = aes(x = PC1, y = PC2, label = labels), alpha=0.8, size=6)
print(g)

# check eigenvalues
get_eigenvalue(pc)
var<-get_pca_var(pc)
var$contrib  # each variable's contribution to the PCs

#####VISUAL EXPLORATION STRATEGY FAR FROM BOX####

fix.far <- fixations.df[(fixations.df$twodist == "far"),]
# subsets only necessary data
pcaF.df <- subset(fix.far, select = c("ID", "Angle180"))
# adds columns for the mean and standard deviation of the distance of fixations, as well as total count of fixations
pcaF.df <- pcaF.df %>% group_by(ID) %>% mutate(F.m.angle = mean(Angle180)) %>% mutate(F.total = n())
pcaF.df <- subset(pcaF.df, select = -c(Angle180))
# finds bias in use of left eye versus right eye
side <- fix.far %>%
  group_by(ID, eye) %>%
  summarise(bias.fix = n())
side$ID <- as.factor(side$ID)
side <- side[!(side$eye == "Binocular" | side$eye == "Blind Spot"),]
side <- dcast(side, ID ~ eye, fun.aggregate=sum)
side$F.bias <- side$`Left Eye` / side$`Right Eye`
side <- subset(side, select=-c(`Left Eye`, `Right Eye`))
pcaF.df <- merge(pcaF.df, side, by="ID")
pcaF.df <- distinct(pcaF.df)

set.seed(123)
# PCA analysis
pairs(pcaF.df[2:4])
# cluster analysis
scale(pcaF.df[2:4])
# elbox method to pick centers
fviz_nbclust(pcaF.df[2:4], kmeans, method = "wss")
# finds the kmeans
kmeans2 <- kmeans(pcaF.df[2:4], centers = 3, nstart = 100)
# plots the clusters
fviz_cluster(kmeans2, data = pcaF.df[2:4]) + theme_classic(base_size = 20) + theme(plot.title = element_blank())
# output
print(kmeans2)

# creates dataframe with the cluster and ID
stratF.df <- data.frame(pcaF.df$ID, kmeans2$cluster)
# renames columns
colnames(stratF.df)[1] = "ID"
colnames(stratF.df)[2] = "F.Strategy"
# sets strategy and ID as factors
stratF.df$F.Strategy <- as.factor(stratF.df$F.Strategy)
stratF.df$ID <- as.factor(stratF.df$ID)
stratF.df <- stratF.df %>% mutate(F.Strategy = recode_factor(F.Strategy, "1" = "Mid-N", `2` = "Low-N", `3` = "High-N"))
stratF.df$F.Strategy <- factor(stratF.df$F.Strategy, levels=c("Low-N", "Mid-N", "High-N"))
pcaF.df <- merge(stratF.df, pcaF.df, by="ID")
colnames(pcaF.df)[3] = "Head Angle"
colnames(pcaF.df)[4] = "Total"
colnames(pcaF.df)[5] = "Side Preference"

# run PCA analysis
pc <- prcomp(pcaF.df[,c(-1,-2)],
             center = TRUE,
             scale. = TRUE)
attributes(pc)
print(pc)
summary(pc)

# check for colinearity
pairs.panels(pc$x,gap = 0,pch=21,
             bg = c("red", "yellow", "blue")[pcaF.df$F.Strategy])


# plot PCA results
g <- ggbiplot::ggbiplot(pc, obs.scale = 1, var.scale = 1, groups = pcaF.df$F.Strategy, ellipse = TRUE, circle = FALSE,
              ellipse.prob = 0.68, varname.size=, varname.adjust = 30, point.size = 2)
g <- g + theme_classic(base_size = 17) + ggtitle("b) Head Fixation Strategy Far from Box") + guides(fill="none") + 
  scale_x_continuous(limits = c(-2.8, 4.3)) +
  scale_fill_manual(values=c("Low-N" = "#99CCFF", "Mid-N" = "#0066FF", "High-N" = "#000099")) +
  scale_color_manual(values=c("Low-N" = "#99CCFF", "Mid-N" = "#0066FF", "High-N" = "#000099")) +
  theme(legend.direction = 'horizontal', legend.position = 'top', legend.title = element_blank(), legend.text = element_text(size=16))


#We also need to extract the data for the variable contributions to each of the pc axes
pca.vars1<- as.data.frame(pc$rotation)
labels <- data.frame(1:3)
labels$labels <- rownames(pca.vars1)
labels$PC1 <- c(-1.9, -1.5, 2)
labels$PC2 <- c(0.4, -1.4, -1.5)
# adds labels
g <- g + geom_label(labels, mapping = aes(x = PC1, y = PC2, label = labels), alpha=0.8, size=6)
print(g)

# check eigenvalues
get_eigenvalue(pc)
# each variables contributions to the dimensions (from eigenvalue)
var <- get_pca_var(pc)
var$contrib

#####COMBINED STRATEGIES####

# adds Strategies to same dataframe
all.strat <- merge(stratN.df, stratF.df, by="ID")
all.strat1 <- all.strat
colnames(all.strat1)[2] <- "Strategy Near Box"
colnames(all.strat1)[3] <- "Strategy Far from Box"
# creates cross table of individuals per strategy
tab <- tabular((`Strategy Near Box` + 1) ~ Format(digits = 2) *
    (`Strategy Far from Box` + 1),  data = all.strat1)
# creates cross-table for plotting
ft <- as_flextable(tab, col_keys = names(tab)) |> colformat_double()
# formatting the image
ft <- bold(ft, i = NULL, j = 1, bold = TRUE, part = "body") 
ft <- bold(ft, i = 1:2, j = NULL, bold = TRUE, part = "header")
ft <- italic(ft, i = 4, j = NULL, italic = TRUE, part = "body")
ft <- italic(ft, i = NULL, j = 5, italic = TRUE, part = "body")
ft <- italic(ft, i = 2, j = 5, italic = TRUE, part = "header")
ft <- color(ft, i= 1, j = 1, color = "#FF6600", part = "header")
ft <- color(ft, i=1:3, j=1, color = "#FF6600", part = "body")
ft <- color(ft, i= 1:2, j = 2:4, color = "blue", part = "header")
ft <- fontsize(ft, i = NULL, j = NULL, size = 13, part = "body")
ft <- fontsize(ft, i = 2, j = NULL, size = 13, part = "header")
ft <- fontsize(ft, i = 1, j = NULL, size = 15, part = "header")
ft <- width(ft, j=2:5, 1, unit="in")
print(ft)


#####EXPLORATION TENDENCY#####

pca2.df <- subset(vision.df, select = c(ID, Total.Move, Total.Time)) # pulls tendency measurements
pca2.df <- merge(latpca.df, pca2.df, by="ID") # combines with latencies
touch <- latency.df[(latency.df$Behavior == "Top Beak Touch" | latency.df$Behavior == "Bottom Beak Touch"),] # relevant touch behavior
touch <- subset(touch, select=c(ID, count))  # selects only the counts
# creates column of total beak touches to box
touch <- touch %>%
  group_by(ID) %>%
  mutate(Total.Touches = sum(count))%>%
  na.omit()
touch <- subset(touch, select=-count) # reduces dataframe
touch <- distinct(touch) # eliminates duplicate rows
pca2.df <- merge(pca2.df, touch, by="ID")  # combines with pca dataframe
pca2.df <- distinct(pca2.df) # eliminates duplicate rows

# set seed for reproducibility
set.seed(123)

# PCA analysis
pairs(pca2.df[2:6])
# cluster analysis
scale(pca2.df[2:6])
# elbox method to pick centers
fviz_nbclust(pca2.df[2:6], kmeans, method = "wss")
# finds the kmeans
kmeans2 <- kmeans(pca2.df[2:6], centers = 4, nstart = 25)
# plots the clusters
fviz_cluster(kmeans2, data = pca2.df[2:6]) + theme_classic(base_size = 20) + theme(plot.title = element_blank())
# output
print(kmeans2)

# creates dataframe with the cluster and ID
tend.df <- data.frame(pca2.df$ID, kmeans2$cluster)
# renames columns
colnames(tend.df)[1] = "ID"
colnames(tend.df)[2] = "Tendency"
# sets strategy and ID as factors
tend.df$Tendency <- as.factor(tend.df$Tendency)
tend.df$ID <- as.factor(tend.df$ID)
tend.df <- tend.df %>% mutate(Tendency = recode_factor(Tendency, "1" = "Low-Level", "2" = "Mid-Level", "3" = "High-Level", "4" = "Outlier"))
tend.df$Tendency <- factor(tend.df$Tendency, levels=c("Low-Level", "Mid-Level", "High-Level", "Outlier"))
all.tend <- merge(tend.df, pca2.df, by="ID")

# creates pca object
pc <- prcomp(all.tend[,c(-1,-2)], center = TRUE, scale. = TRUE)
attributes(pc)
print(pc)
summary(pc)

pairs.panels(pc$x,gap = 0, pch=21,
             bg = c("red", "yellow", "blue")[all.tend$Tendency])

# creates pca plot
g <- ggbiplot(pc, obs.scale = 1, var.scale = 1, groups = all.tend$Tendency, ellipse = TRUE, circle = FALSE, ellipse.prob = 0.68,
              varname.size=0, varname.adjust = 20, point.size = 2)
g <- g + theme_classic(base_size = 17) + ggtitle("Tendency to Explore") + guides(fill="none") +
  scale_fill_manual(values = c("Low-Level" = "#99FF33", "Mid-Level" = "#00CC00", "High-Level" = "#006633", "Outlier" = "black")) + 
  scale_color_manual(values = c("Low-Level" = "#99FF33", "Mid-Level" = "#00CC00", "High-Level" = "#006633", "Outlier" = "black")) +
  theme(legend.direction = 'horizontal', legend.position = 'top', legend.title = element_blank())

# pulls labels for components
pca.vars1<- as.data.frame(pc$rotation)
labels <- data.frame(1:5)
labels$labels <- rownames(pca.vars1)
# applies coordinates to labels (found manually)
# explore, approach, move, time, freq
labels$PC1 <- c(2.4, 2.1, -2.5, -2.5, -0.75)
labels$PC2 <- c(-0.5, 0.5, -0.1, -0.8, 2)

# adds labels to plot (labels produced by ggbiplot cannot be rotated)
g <- g + geom_label(labels, mapping = aes(x = PC1, y = PC2, label = labels), alpha=0.8, size=6) # adds labels to the arrows
print(g)

get_eigenvalue(pc) # generates eigenvalue
var <- get_pca_var(pc) # variables in pca
var$contrib # contribution of variables to each pc

#####SOLVING AS SURVIVORSHIP#####

# preps dataframe
surv.df <- subset(latency.df, select = c(ID, BoxType, Solve.Time, Opened, event))
surv.df <- distinct(surv.df) # 1 row per ID
surv.df <- merge(surv.df, all.strat, by = "ID")  # adds near/far fixation strategies
surv.df <- merge(surv.df, all.tend, by="ID")  # adds exploration tendency
surv.df$Solve.Time <- surv.df$Solve.Time / 60  # converts seconds to minutes

# checks for effect of BOX TYPE
coxB <- coxph(Surv(Solve.Time, event, type="right") ~ BoxType, data=surv.df) # cox proportional hazards
summary(coxB)  # view
survobj <- Surv(surv.df$Solve.Time, surv.df$event, type= "right")  # survival model
pairwise_survdiff(Surv(Solve.Time, event, type="right") ~ BoxType, data=surv.df)  # pairwise comparisons

fitB <- survfit(survobj~ BoxType, data=surv.df)
# creates data frame for adding sample sizes to plots
labels <- data.frame(1:5)
labels$group <- c("Blocked-Control", "Open-Control", "Lift", "Push", "Slide")  # categorical factor
labels$n <- c("n=11","n=11","n=13", "n=10", "n=12")  # sample sizes for each
labels$x <- c(28.5, 20.1, 18, 28, 28.5)  # coordinates for location on plot
labels$y <- c(0.97, 0.58, 0.4, 0.66, 0.1)  # coordinates for location on plot

autoplot(fitB, xlab="Solve Time in Minutes",ylab="Prop. Individuals That Have Not Solved", censor.shape = '*',
         censor.size = 10, surv.size = 2, conf.int = FALSE) + ggtitle("a) Box Type") +
  theme_classic(base_size=20) + scale_y_continuous(limits=c(0,1)) + 
  geom_label(labels, mapping = aes(x = x, y = y, label = n), alpha=0.8, size=5) +
  theme(legend.position = "top", legend.title = element_blank(), title = element_text(size = 14), axis.title = element_text(size = 16))

# this box type could not be opened
survwo.df <- surv.df[!(surv.df$BoxType == "Blocked Control"),]
# survival model w/o blocked control
survobj <- Surv(survwo.df$Solve.Time, survwo.df$event, type= "right")

# checks for effect of BOX TYPE
coxB <- coxph(Surv(Solve.Time, event, type="right") ~ BoxType, data=survwo.df) # cox proportional hazards
summary(coxB)  # view -- likelihood ratio test, wald test, logrank test


coxN <- coxph(Surv(Solve.Time, event, type="right") ~ N.Strategy, data=survwo.df)
summary(coxN)
pairwise_survdiff(Surv(Solve.Time, event, type="right") ~N.Strategy, data=survwo.df)
fitN <- survfit(survobj~ N.Strategy, data=survwo.df)
# adds n values to plot, next to each group's line
labels <- data.frame(1:3)
labels$group <- c("Low-N", "Mid-N", "High-N")
labels$n <- c("n=27","n=12","n=7")
labels$x <- c(28, 25, 14.5)
labels$y <- c(0.61, 0.35, 0.1)

autoplot(fitN, xlab="Solve Time in Minutes",ylab="Prop. Individuals That Have Not Solved", censor.shape = '*',
         censor.size = 10, surv.size = 2, conf.int = FALSE) +
  theme_classic(base_size=20) + scale_y_continuous(limits=c(0,1)) + ggtitle("c) Fixation Strategy Near Box") +
  geom_label(labels, mapping = aes(x = x, y = y, label = n), alpha=0.8, size=5) +
  scale_color_manual(values=c("Low-N" = "#FF9933", "Mid-N" = "#FF3300", "High-N" = "#663300")) +
  theme(legend.position = "top", legend.title = element_blank(), title = element_text(size = 14), axis.title = element_text(size = 16))

coxF <- coxph(Surv(Solve.Time, event, type="right") ~ F.Strategy, data=survwo.df)
summary(coxF)
fitF <- survfit(survobj~ F.Strategy, data=survwo.df)
# adds n values to plot, next to each group's line
labels <- data.frame(1:3)
labels$group <- c("Low-N", "Mid-N", "High-N")
labels$n <- c("n=19","n=18","n=9")
labels$x <- c(28, 20, 27)
labels$y <- c(0.65, 0.45, 0.28)

autoplot(fitF, xlab="Solve Time in Minutes",ylab="Prop. Individuals That Have Not Solved", censor.shape = '*',
         censor.size = 10, surv.size = 2, conf.int = FALSE) + ggtitle("d) Fixation Strategy Far from Box") +
  theme_classic(base_size=20) + scale_y_continuous(limits=c(0,1))  +
  geom_label(labels, mapping = aes(x = x, y = y, label = n), alpha=0.8, size=5) +
  scale_color_manual(values=c("Low-N" = "#99CCFF", "Mid-N" = "#0066FF", "High-N" = "#000066")) + 
  theme(legend.position = "top", legend.title = element_blank(), title = element_text(size = 14), axis.title = element_text(size = 16))


survwoo.df <- survwo.df[!(survwo.df$Tendency == "Outlier"),]
survobj <- Surv(survwoo.df$Solve.Time, survwoo.df$event, type= "right")
coxT <- coxph(Surv(Solve.Time, event, type="right") ~ Tendency, data=survwoo.df)
summary(coxT)
pairwise_survdiff(Surv(Solve.Time, event, type="right") ~Tendency, data=survwoo.df)
fitT <- survfit(Surv(Solve.Time, event, type="right")~ Tendency, data=survwo.df)
summary(fitT)
# adds n values to plot, next to each group's line
labels <- data.frame(1:4)
labels$group <- c("Low-Level", "Mid-Level", "High-Level", "Outlier")
labels$n <- c("n=13","n=18","n=14","n=1")
labels$x <- c(28, 28, 29, 13.2)
labels$y <- c(0.68, 0.45, 0.28, 0.15)
autoplot(fitT, xlab="Solve Time in Minutes",ylab="Prop. Individuals That Have Not Solved", censor.shape = '*',
         censor.size = 10, surv.size = 2, conf.int = FALSE) +
  theme_classic(base_size=20) + scale_y_continuous(limits=c(0,1))  + ggtitle("b) Exploration Tendency") +
  geom_label(labels, mapping = aes(x = x, y = y, label = n), alpha=0.8, size=5) +
  scale_color_manual(values = c("Low-Level" = "#99FF33", "Mid-Level" = "#339900", "High-Level" = "#003300", "Outlier" = "black")) + 
  theme(legend.position = "top", legend.title = element_blank(), title = element_text(size = 14), axis.title = element_text(size = 16))



#####SOLVING AS REGRESSION################

# checking relationships between continuous factors
pairs(cond.df[8:13])

full.df <- subset(cond.df, select=c(ID, BoxType, Population, Sex, Date,trialweight, food.dep))
# adds solve success to conditions
full.df <- merge(full.df, solve.df, by="ID")
# adds near fixation measures
full.df <- merge(full.df, pcaN.df, by="ID")
colnames(full.df)[12] = "N.HeadAngle"
colnames(full.df)[13] = "N.Fix"
colnames(full.df)[14] = "N.SidePref"
# adds far fixation measures
full.df <- merge(full.df, pcaF.df, by="ID")
colnames(full.df)[16] = "F.HeadAngle"
colnames(full.df)[17] = "F.Fix"
colnames(full.df)[18] = "F.SidePref"
# adds behavioral measures
full.df <- merge(full.df, pca2.df, by="ID")

# correlational analysis to test effect of food deprivation time on engagement
cor.test(full.df$Solve.Time, full.df$food.dep, method="pearson")
cor.test(full.df$Lat.Approach, full.df$food.dep, method="pearson")
cor.test(full.df$Total.Touches, full.df$food.dep, method="pearson")

# checking relationships between continuous factors
pairs(full.df[c(6,12:14,16:23)])

full.df <- full.df[!(full.df$BoxType == "Blocked Control"),]
# tobit glm
# visual exploration factors
b <- vglm(Solve.Time ~ N.Fix + N.HeadAngle + N.SidePref + F.Fix + F.HeadAngle + F.SidePref + trialweight, family=tobit(Upper = 1800), data=full.df)
summary(b)
# with only fixations and weight
m2 <- vglm(Solve.Time ~ N.Fix + F.Fix+ trialweight, tobit(Upper = 1800), data = full.df)
# check for impact of eliminated factors in second model (m2) -- > 0.05 does not significantly impact
(p <- pchisq(2 * (logLik(b) - logLik(m2)), df = 2, lower.tail = FALSE))
summary(m2)
# tendency factors
c <- vglm(Solve.Time ~ Total.Move + Lat.Approach + Lat.Explore + Total.Time + Total.Touches + trialweight, family=tobit(Upper = 1800), data=full.df)
summary(c)
# with significant/near significant only
n2 <- vglm(Solve.Time ~ Lat.Approach + Total.Touches + Total.Time + trialweight, tobit(Upper = 1800), data = full.df)
# check for impact of eliminated factors in second model (m2) -- > 0.05 does not significantly impact
(p <- pchisq(2 * (logLik(c) - logLik(n2)), df = 2, lower.tail = FALSE))
summary(n2)
# with significant factors from both vision and tendency
a <- vglm(Solve.Time ~ N.Fix + F.Fix + Lat.Approach + Total.Touches + trialweight, tobit(Upper = 1800), data = full.df)
# compare to tendency model
(p <- pchisq(2 * (logLik(c) - logLik(a)), df = 2, lower.tail = FALSE))
# compare to visual exploration model
(p <- pchisq(2 * (logLik(b) - logLik(a)), df = 2, lower.tail = FALSE))

summary(a) # data output
plotvglm(a) # plot

ctable <- coef(summary(a)) # creates table of coefficients
pvals <- 2 * pt(abs(ctable[, "z value"]), df.residual(b), lower.tail = FALSE) # creates table of pvalues
cbind(ctable, pvals) # combines columns

full.df <- na.omit(full.df) # eliminates NAs
full.df$yhat <- fitted(a)[,1] # adds variable to full.df
full.df$rr <- resid(a, type = "response") # calculates residuals (response)
full.df$rp <- resid(a, type = "pearson")[,1] # calculates residuals (pearson)

# check assumptions
par(mfcol = c(2, 3))
with(full.df, {
  plot(yhat, rr, main = "Fitted vs Residuals")
  qqnorm(rr)
  plot(yhat, rp, main = "Fitted vs Pearson Residuals")
  qqnorm(rp)
  plot(N.Fix, rp, main = "Actual vs Pearson Residuals")
  plot(N.Fix, yhat, main = "Actual vs Fitted")
})
# get r^2
(r <- with(full.df, cor(yhat, Solve.Time)))
r^2

# plots partial dependency for each variable in the model
plotvgam(a, newdata = full.df, residuals = NULL,
         rugplot = TRUE, se = TRUE, scale = 5000, raw = TRUE,
         offset.arg = 0, deriv.arg = 0, overlay = TRUE,
         type.residuals = c("deviance", "working", "pearson", "response"),
         plot.arg = TRUE, which.term = NULL, which.cf = NULL)

ggplot(full.df, aes(x=N.Fix, y=Solve.Time)) + geom_point() + geom_smooth(method="lm")
