###################### WOMEN'S SOCCER ANALYSIS 2 #################################

##### Loading libraries #####
library(tidyverse)
library(randomForest)
library(xgboost)
library(class)
library(nnet)
library(cluster)
library(factoextra)
library(magrittr)

##### Reading in Data Set
imputedSet2 <- read.csv("finalImputation.csv", as.is = T, header = T)
imputedSet <- read.csv("imputedSet.csv", as.is = T, header = T)
finalDF <- data.frame(cbind(imputedSet2[,c(9, 10, 11, 12, 13, 14, 15, 16, 18, 19)], imputedSet[,c(11:16)]))

# Looking at data
corVars <- finalDF[, c(1:10)]
chart.Correlation(corVars, histogram =TRUE, pch = 19)

# Scaling data
finalDFscaled <- data.frame(cbind(scale(finalDF[, c(1:10)]), finalDF[, c(11:16)]))

# Looking at data
head(finalDFscaled)

##################################################################################

####### Splitting between national womens and amateur team
imputedNWT <- finalDFscaled %>% filter(Team == "WNT")
imputedAmateur <- setdiff(finalDFscaled, imputedNWT)
library(dplyr)

# Functions that finds the highest/lowest across all tests
maxFunc <- function(col) {
  a <- max(col)
}

minFunc <- function(col) {
  a <- min(col)
}

# Finding the highest
maxVals <- as.vector(unlist(map(finalDFscaled[,c(7:10)], maxFunc)))
minVals <- as.vector(unlist(map(finalDFscaled[,c(3:6)], minFunc)))

# Function to get value - max across ages
valueAgeMax <- function(dataset, max, player_id, testNum) {
  
  # Initialize - ACTIVATION
  testScores <- c() 
  diffScores <- c()
  
  players <- dataset %>% filter(dataset$Id == player_id) # Filtering out player
  testScores <- players[,testNum] # Select the column of the dataset that matters for the test
  max_testScores <- max(testScores) # Be a lazy bitch
  diffScores <- abs(max_testScores - max) # subtraction, some say it's the opposite of addition
  return(diffScores)
}

# Function to get value - min across ages
valueAgeMin <- function(dataset, min, player_id, testNum) {
  
  # Initialize - ACTIVATION
  testScores <- c() 
  diffScores <- c()
  
  players <- dataset %>% filter(dataset$Id == player_id) # Filtering out player
  testScores <- players[,testNum] # Select the column of the dataset that matters for the test
  min_testScores <- min(testScores) # Be a lazy bitch
  diffScores <- abs(min_testScores - min) # subtraction, some say it's the opposite of addition
  return(diffScores)
}

#### NWT TESTS

# Getting IDs
player_ids <- unique(imputedNWT$Id)

# Finding values for all the tests for NWT
for(j in 3:10){
  assign(paste0(names(imputedNWT[,3:10])[j-2]), vector())
  for(i in 1:length(player_ids)){
    if(j <= 6){
      assign(paste0(names(imputedNWT[,3:6])[j-2]),  rbind(get(paste0(names(imputedNWT[,3:6])[j-2])), valueAgeMin(imputedNWT, minVals[j - 2], player_ids[i], j)))
    } else {
      assign(paste0(names(imputedNWT[,7:10])[j-6]), rbind(get(paste0(names(imputedNWT[,7:10])[j-6])), valueAgeMax(imputedNWT, maxVals[j - 6], player_ids[i], j)))
    }
  }
}

# Putting into new DF
differenedScoresNWT <- data.frame(cbind(X30.15, X10m, X30m, X40m, Max.Speed, ASR, Broad.Jump, CMJ))
colnames(differenedScoresNWT) <- names(imputedNWT)[3:10] 

########## AMATEURS

##### IDs for amateur
player_ids2 <- unique(imputedAmateur$Id)

### Getting values for each amateur for each test
for(j in 3:10){
  assign(paste0(names(imputedAmateur[,3:10])[j-2]), vector())
  for(i in 1:length(player_ids2)){
    if(j <= 6){
      assign(paste0(names(imputedAmateur[,3:6])[j-2]),  rbind(get(paste0(names(imputedAmateur[,3:6])[j-2])), valueAgeMin(imputedAmateur, minVals[j - 2], player_ids2[i], j)))
    } else {
      assign(paste0(names(imputedAmateur[,7:10])[j-6]), rbind(get(paste0(names(imputedAmateur[,7:10])[j-6])), valueAgeMax(imputedAmateur, maxVals[j - 6], player_ids2[i], j)))
    }
  }
}

### Storing into new df
differenedScoresAmateur <- data.frame(cbind(X30.15, X10m, X30m, X40m, Max.Speed, ASR, Broad.Jump, CMJ))
colnames(differenedScoresAmateur) <- names(imputedNWT)[3:10] 

################################### Weights ##########################################

# Creating team variable
finalDFscaled$team2 <- ifelse(finalDFscaled$Team == "WNT", "WNT", "Amateur")

# Initializing variable for later
saved_MSE <- 0

##### Creating function to use with optim
min_error_func <- function(weights, df){
  
  # Applying weights
  new_df <- data.frame(cbind(with(df, df[,c(3:10)]*weights), team = as.factor(df[,17])))
  
  # 75% of the sample size
  smp_size <- floor(0.75 * nrow(new_df))
  
  # set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(new_df)), size = smp_size)
  
  # Splitting
  train <- new_df[train_ind, ]
  test <- new_df[-train_ind, ]
  test <- test[sample(nrow(test), nrow(test)),]
  
  # Going to look at best way to make sure that NWT are in the right section
  # Training model (RF)
  rf_train <- randomForest(train$team ~ ., 
                           data = train, ntree = 1000)
  
  # Making prediction on test set
  rf_pred <- sum(as.vector(predict(rf_train, test)) == 
                   test$team)/nrow(test)
  
  # Saving Scores for Plot later
  saved_MSE <<- c(saved_MSE, 1-rf_pred)
  
  # Returning
  return(1 - rf_pred)
  
}

#### Optimizing
out <- optim(par = rep(1/8, 8),  # initial guess
              fn = min_error_func,
              df = finalDFscaled,
              method = "L-BFGS-B",
              lower = 0,
              upper = 1)

### Looking at error rates with various weights
optimErrorDF <- data.frame(MSE = saved_MSE[-1], trial = time(saved_MSE[-1]))

# Looking at this df
head(optimErrorDF)

## Plotting this
ggplot(data = optimErrorDF, aes(y = MSE, x = trial, color = MSE)) + 
  geom_point(shape = 16, size = 3, show.legend = T, alpha = 0.5) + 
  scale_color_gradient(low = "#32aeff", high = "#f2aeff") + theme_minimal() +
  scale_alpha(range = c(.25, .6)) + ggtitle("Weight Selection Through Optimization: Trial Errors") + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Trial Number") + 
  geom_text(data = data.frame(optimErrorDF[which.min(optimErrorDF$MSE),]), aes(x = trial * 1.05, y = MSE, label = "Minimum"), color = "red")

#### extracting out optimal weights
optim_weights <- out$par

#######################################################################################

### Applying weights
amateur_weighted <- data.frame(cbind(with(differenedScoresAmateur, differenedScoresAmateur[,c(1:8)]*optim_weights), id = player_ids2))
NWT_weighted <- data.frame(cbind(with(differenedScoresNWT, differenedScoresNWT[,c(1:8)]*optim_weights), id = player_ids))

### Getting sums
scores_amateur <- rowSums(amateur_weighted[,c(1:8)])
scores_NWT <- rowSums(NWT_weighted[,c(1:8)])

### Looking at histogram
hist(scores_amateur)
hist(scores_NWT)

### Creating single data set
amateur_weighted$team <- "Amateur"
NWT_weighted$team <- "NWT"
scores = c(scores_amateur, scores_NWT)
team = c(amateur_weighted$team, NWT_weighted$team)

weightedDF <- data.frame(scores, team, id = c(player_ids2, player_ids))

amateur_weighted[c(100:130),]
amateur_weighted
## Looking at new data set
head(weightedDF)

testSet <- weightedDF %>% arrange(weightedDF$id) %>% group_by(id) %>% mutate(avgTeam = mean(team == "NWT")) %>% mutate(resultTeam = case_when(
  avgTeam == 0 ~ "AlwaysAmateur",
  avgTeam == 1 ~ "AlwaysPro",
  TRUE ~ "BitOfBoth"
)) %>% ungroup(id)


?group_by

detach(package:plyr)

testSet <- weightedDF4 %>% group_by(id) %>% mutate(meanTeam = mean(team == "NWT")) %>% ungroup(id)
head(testSet)

str(weightedDF)

testSet2 <- data.frame(testSet)
head(testSet2)


### Creating histogram
ggplot(weightedDF6, aes(scores, fill = avgTeam)) + geom_density(alpha = 0.2)  + 
  scale_fill_manual(values=c("purple", "blue", "red")) + theme_minimal() + ggtitle("Score Densities: NWT v. Amateurs") + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Summed Scores") 
library(ggplot2)

ggplot(testSet2, aes(scores, fill = resultTeam)) + geom_density(alpha = 0.2)  + 
  scale_fill_manual(values=c("purple", "blue", "red")) + ggtitle("Score Densities: NWT v. Amateurs") + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Summed Scores") 


head(weightedDF)
weightedDF4$age
nrow(testSet2)

weightedDF4 <- read.csv("weightedDF.csv", as.is = T, header = T)

testSet3 = testSet2 %>% base::merge(weightedDF[, c(3, 4)], all = T, by = "id")

testSet4 = testSet2 %>% filter(avgTeam == 0.5)

head(testSet4)

doubled = unique(testSet4$id)

nhl = c(doubled)

str(nhl)



testSet5 = weightedDF4 %>% filter(id %in% nhl)

testSet6 = testSet5[rep(seq_len(nrow(testSet5)), each=2),]

head(testSet6)

testSet8 = testSet6[c(seq(2:52), 2),]
testSet7$team = as.factor("Amateur")
str(testSet8)
str(testSet7)

testSet6[c(seq(2, 52, 2)), 2] = "Amateur"

weightedDF5 = setdiff(weightedDF4, testSet5)

unique(weightedDF5$id)

weightedDF6 = data.frame(rbind(weightedDF5, testSet6))

str(weightedDF6)

weightedDF6$avgTeam = ifelse(weightedDF6$team == "Amateur", "Amateur", "NWT")

weightedDF6[c(139:190), 5] = "Both"

tail(weightedDF6)

testSet9 = data.frame(cbind(testSet7, testSet8))


unique(weightedDF6$avgTeam)

ggplot(data = weightedDF6, aes(x = age, y= scores, color = avgTeam, label = id)) + geom_point() + geom_smooth() +
  scale_fill_manual(values=c("purple", "blue")) + ggtitle("Age v. Scores")  + 
  xlab("Age") + ylab("Scores") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color = "Team") #+ ggsave("agevscore3.png") 

test <- weightedDF6 %>% filter(avgTeam == "Amateur") %>% filter(scores < 2)

write.csv(weightedDF6, "weightedDF6.csv", row.names = F)

?merge

amateurs <- weightedDF %>% filter(team == "Amateur") %>% arrange(scores)
View(amateurs)

############### CLUSTERING

# Compute cluster
pam.res <- pam(weightedDF, 5)

# Adding cluster
weightedDF$cluster <- pam.res$clustering

# Adding IDs 
playerIDsAll <- c(player_ids2, player_ids)
weightedDF$id <- playerIDsAll

# Looking at data
str(weightedDF)

weightedDF2 <- weightedDF[sample(nrow(weightedDF), nrow(weightedDF)),]
weightedDF2$id <- as.factor(weightedDF2$id)

# Plotting
ggplot(data = weightedDF2, aes(x = team, y = scores, color = as.factor(cluster))) + 
  geom_point()

testing2 <- finalDFscaled %>% group_by(Id) %>% filter(Age == max(Age))
test3 <- data.frame(testing2)
test4 <- test3 %>% arrange(Id, Success)
unique(test4$Id)
head(test4)

weightedDF3 <- weightedDF %>% arrange(id, team)
finalDFscaled %>% filter(Id == 113)

head(weightedDF3)
head(test4)
View(testing2)

test4 %>% filter(Id == 113)
weightedDF3 %>% filter(id == 113)
weightedDF3$age = test4$Age
weightedDF4 <- weightedDF3[-c(113),]

sum(test4$Id == weightedDF4$id)

weightedDF4$age = test4$Age

ggplot(data = weightedDF4, aes(x = age, y= scores, color = team)) + geom_point() + geom_smooth() +
  scale_fill_manual(values=c("purple", "blue")) + ggtitle("Age v. Scores")  + 
  xlab("Age") + ylab("Scores") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + 
  ggsave("agevscore2.png")

test4$Id
weightedDF3$id

quantile(weightedDF$scores)

?t.test
t.test

testing <- arrange(imputedSet3, Id, Age)



View(testing)

#######################

head(weightedDF6)

badPred <- read.csv("bad_Predictions.csv", as.is = T, header = T)
logPred <- read.csv("logistic_LOOP.csv", as.is = T, header = T)
ridgePred <- read.csv("Ridge_Preds.csv", as.is = T, header = T)
pca_pred <- read.csv("PCA_Preds.csv", as.is = T, header = T)

badPred$s0
str(pca_pred)


colnames(badPred) = c("X", "prob", "id", "age", "team")
colnames(logPred) = c("X", "prob", "id", "age", "team")
colnames(ridgePred) = c("prob", "id", "age", "team")
colnames(pca_pred) = c("prob", "id", "age", "team")

pca_pred$team = ifelse(pca_pred$team == "WNT", 2, 1)
ridgePred$team = ifelse(ridgePred$team == "WNT", 2, 1)
pca_pred$team = ifelse(rid)


str(ridgePred)
str(badPred)
unique(ridgePred$team)
unique(badPred$team)
unique(pca_pred$team)
head(badPred)
head(logPred)

badPred$team = ifelse(badPred$team == 1, "Amateur", "NWT")
logPred$team = ifelse(logPred$team == 1, "Amateur", "NWT")
ridgePred$team = ifelse(ridgePred$team == 1, "Amateur", "NWT")
pca_pred$team = ifelse(pca_pred$team == 1, "Amateur", "NWT")


badPred2 <- badPred %>% group_by(team, id) %>% summarise(prob = max(prob)) %>% arrange(id)
logPred2 <- logPred %>% group_by(team, id) %>% summarise(prob = max(prob)) %>% arrange(id)
ridgePred2 <- ridgePred %>% group_by(team, id) %>%  summarise(prob = max(prob)) %>% arrange(id)
pcaPred2 <- pca_pred %>% group_by(team, id) %>%  summarise(prob = max(prob)) %>% arrange(id)

library(dplyr)

logPred2 <- data.frame(logPred2)
badPred2 <- data.frame(badPred2)
ridgePred2 <- data.frame(ridgePred2)
pcaPred2 <- data.frame(pcaPred2)


weightedDF6 <- read.csv("weightedDF6.csv", as.is = T, header = T)

weightedDF7 <- arrange(weightedDF6, id)

weightedDF7$id
ridgePred2$id

ridgePred2 <- ridgePred2 %>% filter(id != 113)
pcaPred2 <- pcaPred2 %>% filter(id != 113)

clusterTest <- data.frame(cbind.data.frame(weightedDF7, ridgeProb = ridgePred2$prob, pcaProb = pcaPred2$prob))


str(clusterTest)



library(ddR)


clusters1 <- kmeans(clusterTest[,c(1, 6, 7)], centers = 8, nstart = 1)

clusterTest$cluster = clusters1$cluster

clusterTest2 <- clusterTest %>% arrange(cluster)

ggplot(data = clusterTest2, aes(x = clusterTest2$ridgeProb, y = scores, color = as.factor(clusterTest2$cluster), shape = clusterTest2$avgTeam)) + 
  geom_point() + theme_minimal() + ggtitle("Scoring Clusters") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Cluster", shape = "Team") +
  xlab("Probability Predictions [Ridge]") + ylab("Scores") + ggsave("ScoringClusters.png") 
  #geom_polygon(aes(fill = as.factor(clusterTest2$cluster)))


install.packages("grDevices")
library(grDevices)

x <- chull(clusterTest[,c(6, 1)])

x
centers=as.data.frame(clusters1$centers)
str(centers)

find_hull <- function(clusterTest) clusterTest[chull(clusterTest$badPred2.prob, clusterTest$scores), ]
hulls <- ddply(clusterTest, "cluster", find_hull)

library(tidyverse)
library(ggplot2)


str(finalDFscaled)




str(clusterTest)


####################### Making 3D Plots
library(plot3D)
unique(clusterTest$cluster)

write.csv(clusterTest, "final_df.csv", row.names = F)

clusterTest <- read.csv("final_df.csv", as.is = T, header = T)

scatter3D(clusterTest$scores, clusterTest$ridgeProb, clusterTest$pcaProb, bty = "g", pch = 18, 
          col.var = as.integer(clusterTest$cluster), 
          col = c("red", "blue", "green", "orange", "purple", "pink", "yellow", "brown","black", "#1B9E77"),
          pch = 18, ticktype = "detailed", phi = 0)#,
          #colkey = list(at = c(2, 3, 4), side = 1, 
                        #addlines = TRUE, length = 0.5, width = 0.5,
                        #labels = c("setosa", "versicolor", "virginica")))
library(rgl)
library(car)

install.packages("rgl")
scatter3d(clusterTest$scores, clusterTest$ridgeProb, 
          clusterTest$pcaProb, groups = as.factor(clusterTest$cluster),
          surface = F, ellipsoid = T, labels = clusterTest$id, id.n=nrow(clusterTest), 
          xlab = "Optimal Valuation Scores", ylab = "Logistic Ridge Predictions",
          zlab = "Principal Component Regression Predictions")
?scatter3d
rgl.postscript("plot3d1.pdf",fmt="pdf")

library(dplyr)
clusterTest %>% filter(id %in% c(7, 138, 59, 44, 73, 39, 136, 135, 52, 108, 148))
clusterTest$id
clusterTest %>% filter(id %in% c(test$id))


a = 1 + (20/12) + (400/144) + ((20*20*20)/(12*12*12))
b = 1/a
1 - b
