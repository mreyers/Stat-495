# Brad and Matt desperately trying to make this work
library(tidyverse)

allPlayers <- read.csv("missingForest_Impute.csv") %>% unique()

Defenders <- c("FB", "GK", "CB")
Forwards <- c("MF", "FWD", "CMF", "CF")

PlayersDef <- allPlayers %>% filter(Position %in% Defenders)
PlayersOff <- allPlayers %>% filter(Position %in% Forwards)

valueMax <- function(event){
  maxEvent <- max(event)
  everyResult <- abs(event - maxEvent)
  return(everyResult)
}

valueMin <- function(event){
  minEvent <- min(event)
  everyResult <- abs(event - minEvent)
  return(everyResult)
}

minEvents <- c("X30.15", "X10m", "X30m", "X40m")
maxEvents <- c("Max.Speed", "ASR", "Squat.Jump", "Broad.Jump", "CMJ")

OffScoringDF <- PlayersOff[,1:5]
for(i in minEvents){
  OffScoringDF[, i] <- valueMin(PlayersOff[,i])
}
for(j in maxEvents){
  OffScoringDF[, j] <- valueMax(PlayersOff[,j])
}
OffScoringDF

# Do Defensive players later

# Consider scaled data set as well, we will use zscores
tempOffScale <- apply(PlayersOff[, 11:19], MARGIN = 2, FUN = function(x) ((x - mean(x))/sd(x)))
tempOffScale

OffScaleDF <- PlayersOff[, 1:5]
for(i in minEvents){
  OffScaleDF[,i] <- valueMin(tempOffScale[,i])
}
for(j in maxEvents){
  OffScaleDF[,j] <- valueMax(tempOffScale[,j])
}
OffScaleDF

# OffScaleDF and OffScoringDF have same info, first one is scaled other is untouched
speedEvents <- c("X30.15", "X10m", "X30m", "X40m", "Max.Speed")
powerEvents <- c("ASR", "Squat.Jump", "Broad.Jump", "CMJ")

speedScore <- OffScoringDF[, speedEvents] %>% rowSums
powerScore <- OffScoringDF[, powerEvents] %>% rowSums
OffScoringDF$speedScore <- speedScore
OffScoringDF$powerScore <- powerScore

speedScale <- OffScaleDF[, speedEvents] %>% rowSums
powerScale <- OffScaleDF[, powerEvents] %>% rowSums
OffScaleDF$speedScore <- speedScale
OffScaleDF$powerScore <- powerScale

# Barinder team identifier
OffScoringDF <- OffScoringDF %>% mutate(Pro = case_when(Team == "WNT"~ 1, TRUE ~ 0))
OffScaleDF <- OffScaleDF %>% mutate(Pro = case_when(Team == "WNT"~ 1, TRUE ~ 0))



# By fwd/def
p1 <- ggplot(data = OffScoringDF, aes(x = speedScore, fill = as.factor(Pro))) + geom_density(alpha = 0.25)
p2 <- ggplot(data = OffScoringDF, aes(x = powerScore, fill = as.factor(Pro))) + geom_density(alpha = 0.25)

# By position exact
ggplot(data = OffScaleDF, aes(x = speedScore, fill = as.factor(Pro))) + geom_density(alpha = 0.25)
ggplot(data = OffScaleDF, aes(x = powerScore, fill = as.factor(Pro))) + geom_density(alpha = 0.25)

# New Team ID
madeTeam <- OffScoringDF %>% group_by(id) %>% select(Team, id) %>% summarize(mean = mean(Team == "WNT")) %>% mutate(madeTeam = (mean > 0)) %>% select(-mean)
OffScoringDF <- OffScoringDF %>% merge(madeTeam, all = TRUE, by = "id")
madeTeam2 <- OffScaleDF %>% group_by(id) %>% select(Team, id) %>% summarize(mean = mean(Team == "WNT")) %>% mutate(madeTeam = (mean > 0)) %>% select(-mean)
OffScaleDF <- OffScaleDF %>% merge(madeTeam2, all = TRUE, by = "id")

# Plots again
p3 <-ggplot(data = OffScoringDF, aes(x = speedScore, fill = as.factor(madeTeam))) + geom_density(alpha = 0.25)
p4 <-ggplot(data = OffScoringDF, aes(x = powerScore, fill = as.factor(madeTeam))) + geom_density(alpha = 0.25)

p5 <-ggplot(data = OffScoringDF, aes(x = speedScore, fill = as.factor(madeTeam-Pro))) + geom_density(alpha = 0.25)
p6 <-ggplot(data = OffScoringDF, aes(x = powerScore, fill = as.factor(madeTeam-Pro))) + geom_density(alpha = 0.25)

p7 <-ggplot(data = OffScaleDF, aes(x = speedScore, fill = as.factor(madeTeam-Pro))) + geom_density(alpha = 0.25)
p8 <-ggplot(data = OffScaleDF, aes(x = powerScore, fill = as.factor(madeTeam-Pro))) + geom_density(alpha = 0.25)


# Notes on the above
# # It looks like AMateurs that eventually go pro must make a marked departure from Amateur and Pro level performance before advancing
# # Why does this matter?
  # Do pros get complacent?
  # Do excellent athletes have a better chance than those that are good but improve? -maybe not a good q
  # How do speed and power graphs interact?
  # Where does one need to improve most?
    # DOes it change by position?
# What about age?
  # Try to think of how to integrate age and Made-Pro
  # Or other ways to incorporate age
