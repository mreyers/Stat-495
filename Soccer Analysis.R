# Soccer analysis 

# Libraries
library(ggplot2)
library(plyr)
library(tidyverse)

# User pathways, until we find a better way
#path <- "C:/Users/mreyers/Downloads/soccer.csv"
#path <- "X:/Downloads Part 2/soccer.csv"

# Read in the data
soccer <- read.csv(path, as.is = TRUE, strip.white = TRUE, header = TRUE)

# Basic data check
head(soccer)
summary(soccer)

# Average amount of data we have to work with
missingCheck <- apply(soccer, MARGIN = 2, FUN = is.na)
counts <- ncol(soccer) - rowSums(missingCheck)
hist(counts)

# Same check but excluding the identifying data
missingNotID <- apply(soccer[, 8:dim(soccer)[2]], MARGIN = 2, FUN = is.na)
countsNotID <- ncol(soccer[, 8:dim(soccer)[2]]) - rowSums(missingNotID)
hist(countsNotID)

# Data by team
soccer$UsableData <- countsNotID
dataPlot <- ggplot(data = soccer, aes(x = UsableData, fill = Team))+
  geom_histogram(binwidth = 1)

# Add a column indicating whether a player eventually makes the WNT or has not yet 
madeTeam <- soccer %>% group_by(id) %>% select(Team, id) %>% summarize(mean = mean(Team == "WNT")) %>% mutate(madeTeam = (mean > 0)) %>% select(-mean)
soccer <- soccer %>% merge(madeTeam, all = TRUE, by = "id")

# Separate the data into those that are on WNT and those that are not
WNT <- soccer %>% filter(Team == "WNT")
Amateurs <- soccer %>% filter(Team != "WNT")
dim(setdiff(WNT, Amateurs)) # Shows that the separated sets are indeed mutually exclusive. Since this is equal to dim(WNT), sets are entirely exclusive

# Assemble a player profile for national team players based on multiple testing rounds
WNTPlayers <- ddply(WNT, .(id, Team, Position), summarize,
                    Age = mean(Age, na.rm = TRUE),
                    avg_Mass = mean(Mass, na.rm = TRUE),
                    avg_Height = mean(Height, na.rm = TRUE),
                    avg_X30.15 = mean(X30.15, na.rm = TRUE),
                    avg_X10m = mean(X10m, na.rm = TRUE),
                    avg_X30m = mean(X30m, na.rm = TRUE),
                    avg_X40m = mean(X40m, na.rm = TRUE),
                    avg_Max.Speed = mean(Max.Speed, na.rm = TRUE),
                    avg_ASR = mean(ASR, na.rm = TRUE),
                    avg_Squat.Jump = mean(Squat.Jump, na.rm = TRUE),
                    avg_Broad.Jump = mean(Broad.Jump, na.rm = TRUE),
                    avg_CMJ = mean(CMJ, na.rm = TRUE))
head(WNTPlayers)
dim(WNTPlayers) # Verify the discrepancies between the number of rows and the number of unique id's
length(unique(WNTPlayers$id)) # Might be a player that switched positions in testing

MeanAndDevWNT <- ddply(WNTPlayers, .(Position), summarize,
                         std_Mass = sd(avg_Mass, na.rm = TRUE),
                         avg_Mass = mean(avg_Mass, na.rm = TRUE),
                         std_Height = sd(avg_Height, na.rm = TRUE),
                         avg_Height = mean(avg_Height, na.rm = TRUE),
                         std_X30.15 = sd(avg_X30.15, na.rm = TRUE),
                         avg_X30.15 = mean(avg_X30.15, na.rm = TRUE),
                         std_X10m = sd(avg_X10m, na.rm = TRUE),
                         avg_X10m = mean(avg_X10m, na.rm = TRUE),
                         std_X30m = sd(avg_X30m, na.rm = TRUE),
                         avg_X30m = mean(avg_X30m, na.rm = TRUE),
                         std_X40m = sd(avg_X40m, na.rm = TRUE),
                         avg_X40m = mean(avg_X40m, na.rm = TRUE),
                         std_Max.Speed = sd(avg_Max.Speed, na.rm = TRUE),
                         avg_Max.Speed = mean(avg_Max.Speed, na.rm = TRUE),
                         std_ASR = sd(avg_ASR, na.rm = TRUE),
                         avg_ASR = mean(avg_ASR, na.rm = TRUE),
                         std_Squat.Jump = sd(avg_Squat.Jump, na.rm = TRUE),
                         avg_Squat.Jump = mean(avg_Squat.Jump, na.rm = TRUE),
                         std_Broad.Jump = sd(avg_Broad.Jump, na.rm = TRUE),
                         avg_Broad.Jump = mean(avg_Broad.Jump, na.rm = TRUE),
                         std_CMJ = sd(avg_CMJ, na.rm = TRUE),
                         avg_CMJ = mean(avg_CMJ, na.rm = TRUE)
                         )
MeanAndDevWNT # For some reason sd() only calculated non-NA values if done prior to the mean calculation

# Create plot to show mean and a basic interval to hit by position
  # Note that some skills are preferred to be out of the interval
    # E.g. if someone has a 30 time below avg - sd, that is phenomenal and should be worth points
    # As we go further, perhaps we think of a scoring metric as a deviation multiplier
MassPlot <- ggplot(data = MeanAndDevWNT, aes(x = Position, y = avg_Mass))+
  geom_point()+
  geom_errorbar(aes(ymin = avg_Mass - std_Mass, ymax = avg_Mass + std_Mass))

AgilityPlot <- ggplot(data = MeanAndDevWNT, aes(x = Position, y = avg_X30.15))+
  geom_point()+
  geom_errorbar(aes(ymin = avg_X30.15 - std_X30.15, ymax = avg_X30.15 + std_X30.15))

X40YardDash <- ggplot(data = MeanAndDevWNT, aes(x = Position, y = avg_X40m))+
  geom_point()+
  geom_errorbar(aes(ymin = avg_X40m - std_X40m, ymax = avg_X40m + std_X40m))

# Events where it is desirable to be below the average score: x30.15, x10m, x30m, x40m
# Events where it is desirable to be aboce the average score: height and mass (maybe, probably the interaction of height with speed), max speed,
#                                                             Squat jump, broad jump, CMJ (basically fluid vertical jump)
# Events where we are unsure what is desirable: ASR (Anaerobic Speed Reserve) is the difference between maximal anaerobic speed (sprint) and maximal aerobic speed (distance run)
# Expectation is that this is best used as an interaction term with 40m sprint time

# Create National Ranges from quantile information instead of just the above
  # Involves splitting data into frames for each position
  positions <- split(WNTPlayers, with(WNTPlayers, Position))
  names(positions)
  positionRanges <- list()
  for(i in 1:length(positions)){
    positionRanges[[i]] <- apply(subset(positions[[i]], select = -c(id, Team, Position)), MARGIN = 2, FUN = quantile, na.rm = TRUE)
  }
  names(positionRanges) <- names(positions)
  
  # positionRanges list now contains quantiles for WNT by position
# Goal is now to compare the amateurs with the team quantiles
# Something to consider later is positional scarcity (e.g. A team starts one CMF but often times 4 D, meaning the CMF may have to be more elite to make roster)
head(Amateurs)  

# Average Amateur performance by id as some players are tested multiple times
  # Might be worth averaging these in a weighted fashion, dependent on age. More recent results should probably bare more in terms of likelihood to make team
  # For now leave as normal average
AmateurUnique <- ddply(Amateurs, .(id, Position), summarize,
                       Age = mean(Age, na.rm = TRUE),
                       avg_Mass = mean(Mass, na.rm = TRUE),
                       avg_Height = mean(Height, na.rm = TRUE),
                       avg_X30.15 = mean(X30.15, na.rm = TRUE),
                       avg_X10m = mean(X10m, na.rm = TRUE),
                       avg_X30m = mean(X30m, na.rm = TRUE),
                       avg_X40m = mean(X40m, na.rm = TRUE),
                       avg_Max.Speed = mean(Max.Speed, na.rm = TRUE),
                       avg_ASR = mean(ASR, na.rm = TRUE),
                       avg_Squat.Jump = mean(Squat.Jump, na.rm = TRUE),
                       avg_Broad.Jump = mean(Broad.Jump, na.rm = TRUE),
                       avg_CMJ = mean(CMJ, na.rm = TRUE)
                       )
head(AmateurUnique)

AmateurUniqueNoPos <- ddply(Amateurs, .(id), summarize,
                            Age = mean(Age, na.rm = TRUE),
                            avg_Mass = mean(Mass, na.rm = TRUE),
                            avg_Height = mean(Height, na.rm = TRUE),
                            avg_X30.15 = mean(X30.15, na.rm = TRUE),
                            avg_X10m = mean(X10m, na.rm = TRUE),
                            avg_X30m = mean(X30m, na.rm = TRUE),
                            avg_X40m = mean(X40m, na.rm = TRUE),
                            avg_Max.Speed = mean(Max.Speed, na.rm = TRUE),
                            avg_ASR = mean(ASR, na.rm = TRUE),
                            avg_Squat.Jump = mean(Squat.Jump, na.rm = TRUE),
                            avg_Broad.Jump = mean(Broad.Jump, na.rm = TRUE),
                            avg_CMJ = mean(CMJ, na.rm = TRUE)
                            ) 
head(AmateurUniqueNoPos)

# Events that are timed have exclusion value of 100 (NA spot holder). Others have spot holder of 0.
AmateurBestPerformance <- ddply(Amateurs, .(id, Position), summarize,
                                Age = max(c(Age, 0), na.rm = TRUE),
                                Mass = max(c(Mass, 0), na.rm = TRUE),
                                Height = max(c(Height, 0), na.rm = TRUE),
                                X30.15 = min(c(X30.15, 100), na.rm = TRUE),
                                X10m = min(c(X10m, 100), na.rm = TRUE),
                                X30m = min(c(X30m, 100), na.rm = TRUE),
                                X40m = min(c(X40m, 100), na.rm = TRUE),
                                Max.Speed = max(c(Max.Speed, 0), na.rm = TRUE),
                                ASR = max(c(ASR, 0), na.rm = TRUE),
                                Squat.Jump = max(c(Squat.Jump, 0), na.rm = TRUE),
                                Broad.Jump = max(c(Broad.Jump, 0), na.rm = TRUE),
                                Max_CMJ = max(c(CMJ, 0), na.rm = TRUE),
                                Min_CMJ = min(c(CMJ, 100), na.rm = TRUE) 
                                )
head(AmateurBestPerformance)

# Since age is a component of these tests, would be interesting to include biomechanical predictions as to what a score for a 14 year old would
# be 5-10 years later (when they would actually be considered for the team)
  # Looks like too much is hereditary and is interfering with a nice equation formulation
# May also need to look into imputation techniques for missing categories

# Compare and score each player against the WNT metrics for each test
# For now use a 0-6 scale: 0 = below min, 1 = between min and first quartile, ..., 4 = between third quartile and max, 5 = beyond max
# Scale is reversed for events that desire low test scores (timed events) so that an overall high score indicates excellent testing
comparison <- function(playerRow){
  if(playerRow$Position[1] == "CF"){
    pos <- "FWD"
  }
  else{  
    pos <- playerRow$Position
  }
  # Currently neglects players who have switched position, to be fixed later
  WNTStandard <- positionRanges[[pos[1]]]
  playerRow <- playerRow[1,]
  score <- 0
  # Timed events
  timed <- playerRow %>% select(avg_X30.15, avg_X10m, avg_X30m, avg_X40m)
  for(i in names(timed)){
    if(!is.na(playerRow[, i])){
      if(playerRow[, i] < WNTStandard[1, i]){
        score <- score + 5
      }
      else if(playerRow[, i] < WNTStandard[2, i]){
        score <- score + 4
      }
      else if(playerRow[, i] < WNTStandard[3, i]){
        score <- score + 3
      }
      else if(playerRow[, i] < WNTStandard[4, i]){
        score <- score + 2
      }
      else if(playerRow[, i] < WNTStandard[5, i]){
        score <- score + 1
      }
    }
    
  }
  
  # Non-timed events
  untimed <- playerRow %>% select(avg_Mass, avg_Height, avg_Max.Speed, avg_Squat.Jump, avg_Broad.Jump)
  for(j in names(untimed)){
    
    if(!is.na(playerRow[, i])){
      if(playerRow[, i] > WNTStandard[5, i]){
        score <- score + 5
      }
      else if(playerRow[, i] > WNTStandard[4, i]){
        score <- score + 4
      }
      else if(playerRow[, i] > WNTStandard[3, i]){
        score <- score + 3
      }
      else if(playerRow[, i] > WNTStandard[2, i]){
        score <- score + 2
      }
      else if(playerRow[, i] > WNTStandard[1, i]){
        score <- score + 1
      }
    }
  }
  return(cbind(playerRow, score))
}

# The first player (id = 1) scores a 29 according to combine metrics, will design a baseline for WNT later
testAmateur <- comparison(AmateurUnique[AmateurUnique$id == 1,])
testAmateur

allAmateurScores <- list()
scores <- c()
for( k in unique(AmateurUnique$id)){
  allAmateurScores[[k]] <- comparison(AmateurUnique[AmateurUnique$id == k,])
  scores[k] <- allAmateurScores[[k]]$score
}

idAndScores <- data.frame(id = 1:max(AmateurUnique$id), scores) %>% filter(!is.na(scores))
idAndScores[order(-idAndScores$scores), ]
