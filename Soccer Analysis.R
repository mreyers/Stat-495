# Soccer analysis 

# Libraries
library(ggplot2)
library(plyr)
library(tidyverse)


# Read in the data
soccer <- read.csv("X:/Downloads Part 2/soccer.csv", as.is = TRUE, strip.white = TRUE, header = TRUE)

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

# Separate the data into those that are on WNT and those that are not
WNT <- soccer %>% filter(Team == "WNT")
Amateurs <- soccer %>% filter(Team != "WNT")
dim(setdiff(WNT, Amateurs)) # Shows that the separated sets are indeed mutually exclusive

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

MeanAndRangeWNT <- ddply(WNTPlayers, .(Position), summarize,
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
MeanAndRangeWNT # For some reason sd() only calculated non-NA values if done prior to the mean calculation

