##### Women's Data Set Analysis #####

##### Loading libraries #####
library(tidyverse)
library(randomForest)
library(pcaMethods)
library(xgboost)
library(class)
library(nnet)
library(neuralnet)

##### Saving Data Set #####
write.csv(imputedSet, "imputedSet.csv", row.names = F)

##### Reading in Data Set
imputedSet <- read.csv("imputedSet.csv", as.is = T, header = T)
imputedSet2

str(imputedSet)
str(imputedSet2)

imputedSet3 <- data.frame(cbind(imputedSet2[,c(9, 10, 11, 12, 13, 14, 15, 16, 18, 19)], imputedSet[,c(11:16)]))

str(imputedSet3)
##### Normalizing data
#imputedSet <- data.frame(cbind(imputedSet[,1:2], scale(imputedSet[,3:10]), imputedSet[,11:16]))

str(imputedSet)

####### Splitting between national womens and amateur team
imputedNWT <- imputedSet %>% filter(Team == "WNT")
imputedAmateur <- setdiff(imputedSet, imputedNWT)

# Function that finds the highest/lowest across all tests
maxFunc <- function(col) {
  a <- max(col)
}

minFunc <- function(col) {
  a <- min(col)
}

# Finding the highest
maxVals <- as.vector(unlist(map(imputedSet[,c(7:10)], maxFunc)))
minVals <- as.vector(unlist(map(imputedSet[,c(3:6)], minFunc)))

which.max(imputedAmateur$CMJ)
imputedAmateur[201,]

# function to get value - max across ages
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
differenedScoresNWT <- data.frame(cbind(x30.15, metre10, metre30, metre40, Max.Speed, ASR, Broad.Jump, CMJ))
colnames(differenedScoresNWT) <- names(imputedNWT)[3:10] 

apply(differenedScoresNWT, MARGIN = 2, min)


########## AMATEURS

##### IDs for amateur
player_ids2 <- unique(imputedAmateur$Id)

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

differenedScoresAmateur <- data.frame(cbind(x30.15, metre10, metre30, metre40, Max.Speed, ASR, Broad.Jump, CMJ))
colnames(differenedScoresAmateur) <- names(imputedNWT)[3:10] 

apply(differenedScoresAmateur, MARGIN = 2, min)

which.min(differenedScoresAmateur$CMJ)
which.min(differenedScoresNWT$CMJ)
player_ids2[61]

imputedNWT %>% filter(Id == 7)
imputedAmateur %>% filter(Id == 102)

# imputedSet$team2 <- ifelse(imputedSet$Team == "WNT", 1, 0)

p <- cor(as.numeric(imputedSet$Success), imputedSet$x30.15, method = "kendall")
p <- cor(as.numeric(imputedSet$Success), imputedSet$ASR, method = "kendall")





player_ids

########################### WEIGHTING ############################

##### Putting the data sets together
head(differenedScoresAmateur)

differenedScoresAmateur$id <- player_ids2
differenedScoresAmateur$team <- as.factor("amateur")
differenedScoresNWT$id <- player_ids
differenedScoresNWT$team <- as.factor("NWT")

differencedDataSet <- data.frame(rbind(differenedScoresAmateur, differenedScoresNWT))

head(differencedDataSet)
str(differencedDataSet)

write.csv(differencedDataSet, "differencedDataSet.csv", row.names = F)

# First, split the data, so we can test with various weights
# 75% of the sample size
smp_size <- floor(0.75 * nrow(differencedDataSet))

# set the seed to make your partition reproductible
set.seed(13)
train_ind <- sample(seq_len(nrow(differencedDataSet)), size = smp_size)

# Splitting
train <- differencedDataSet[train_ind, ]
test <- differencedDataSet[-c(train_ind), ]
test <- test[sample(nrow(test), nrow(test)),]

# Going to look at best way to make sure that NWT are in the right section
# Training model (RF)
rf_train <- randomForest(train$team ~ ., 
                     data = train, ntree = 10000)

imputedSet


# Training model (RF)
glm1 <- glm(train$team ~ ., data = train, family=binomial)
summary(glm1)
# Making prediction on test set
glm_pred <- sum(as.vector(round(predict(glm1, test, type = "response"))) == 
                  (as.numeric(test$team) - 1))/nrow(test)

# Making prediction on test set
rf_pred <- sum(as.vector(predict(rf_train, test)) == 
                   test$team)/nrow(test)

# Looking at error rate
rf_pred

##### Creating function to use with optim
min_error_func <- function(weights, df){
  
  # Applying weights
  new_df <- data.frame(cbind(with(df, df[,c(1:8)]*weights), team = df[,c(10)]))
  
  # 75% of the sample size
  smp_size <- floor(0.75 * nrow(new_df))
  
  # set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(new_df)), size = smp_size)
  
  # Splitting
  train <- new_df[train_ind, ]
  test <- new_df[-train_ind, ]
  
  # Going to look at best way to make sure that NWT are in the right section
  # Training model (RF)
  rf_train <- randomForest(train$team ~ ., 
                           data = train, ntree = 1000)
  
  # Making prediction on test set
  rf_pred <- sum(as.vector(predict(rf_train, test)) == 
                   test$team)/nrow(test)
  
  return(1 - rf_pred)
  
}

out <- optim(par = rep(1 / 8, 8),  # initial guess
             fn = min_error_func,
             df = differencedDataSet,
             method = "L-BFGS-B",
             lower = 0,
             upper = 10)
out

optimal_weights <- c(0.1248435, 0.1373612, 0.1373612, 0.1373612, 0.1248435, 0.1373612, 0.1250000, 0.1373612)

min_error_func(optimal_weights, differencedDataSet)


###### XGBOOST
##### Creating function to use with optim
min_error_func2 <- function(weights, df){
  
  # Applying weights
  new_df <- data.frame(cbind(with(df, df[,c(1:8)]*weights), team = df[,c(10)]))
  
  # 75% of the sample size
  smp_size <- floor(0.75 * nrow(new_df))
  
  # set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(new_df)), size = smp_size)
  
  # Splitting
  train <- new_df[train_ind, ]
  test <- new_df[-train_ind, ]
  
  # Train Data Setup 
  dtrain = xgb.DMatrix(data.matrix(train), label = train$team)
  
  # Test Data Setup
  dtest = xgb.DMatrix(data.matrix(test), label = test$team)
  
  # eXtreme Gradient Boost
  xgb.fit <- xgboost(data = dtrain,
                     max_depth = 10, # maximum depth of tree
                     eval_metric = "mlogloss", # Only metric that seems to work.  Others don't make sense.
                     num_class = 3, # Number of response categories + 1
                     eta = 0.1, # Step size shrinkage
                     # nthread = 2, # Number of threads to be used (computer cores)
                     nround = 100, # Number of trees
                     nfold = 10, # Is the function cross validating on its own?
                     silent = 1,
                     objective = "multi:softmax") # Multiple classification
  
  # Predictions
  xgb.pred <- as.vector(predict(xgb.fit, newdata = dtest)) # Predictions saved as vector
  xgb.pred2 = sum((xgb.pred == as.numeric(test$team)))/nrow(test) # Vector of True/False for if 
  
  # Returning
  return(1 - xgb.pred2)
  
}

out2 <- optim(par = rep(1 / 9, 8),  # initial guess
             fn = min_error_func2,
             df = differencedDataSet,
             method = "L-BFGS-B",
             lower = 0,
             upper = 2)
out2

min_error_func2(c(1, 1, 1, 1, 1, 1, 1, 1), differencedDataSet)


min_error_func3 <- function(weights, df){
  
  # Applying weights
  new_df <- data.frame(cbind(with(df, df[,c(1:8)]*weights), team = df[,c(10)]))
  
  # 75% of the sample size
  smp_size <- floor(0.75 * nrow(new_df))
  
  # set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(new_df)), size = smp_size)
  
  # Splitting
  train <- new_df[train_ind, ]
  test <- new_df[-train_ind, ]
  
  ideal4 <- class.ind(train$team)
  
  gene_ANN2 <- nnet(train[,-c(9)], ideal4, size = 13, softmax = T, MaxNWts = 4714)
  
  test_pred2 <- predict(gene_ANN2, test[, -c(9)], type = "class")
  
  success_rate_nn_s <- sum(test_pred2 == test$team)/nrow(test)

  # Returning
  return(1 - success_rate_nn_s)
  
}


out3 <- optim(par = rep(1 / 8, 8),  # initial guess
              fn = min_error_func3,
              df = differencedDataSet,
              method = "L-BFGS-B",
              lower = 0,
              upper = 1)
out3


min_error_func4 <- function(weights, df){
  
  # Applying weights
  new_df <- data.frame(cbind(with(df, df[,c(1:8)]*weights), team = df[,c(10)]))
  
  # 75% of the sample size
  smp_size <- floor(0.75 * nrow(new_df))
  
  # set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(new_df)), size = smp_size)
  
  # Splitting
  train <- new_df[train_ind, ]
  test <- new_df[-train_ind, ]
  
  # Going to look at best way to make sure that NWT are in the right section
  # Training model (RF)
  glm1 <- glm(train$team ~ ., data = train, family=binomial)
  
  # Making prediction on test set
  glm_pred <- sum(as.vector(round(predict(glm1, test, type = "response"))) == 
                    (as.numeric(test$team) - 1))/nrow(test)
  
  return(1 - glm_pred)
  
}

out4 <- optim(par = rep(1 / 8, 8),  # initial guess
             fn = min_error_func4,
             df = differencedDataSet,
             method = "L-BFGS-B",
             lower = 0,
             upper = 1)
out4


############# TRYING SOMETHING ELSE

str(imputedSet)

imputedSet$team3 <- ifelse(imputedSet$Team == "WNT", "WNT", "Amateur")

##### Creating function to use with optim
min_error_func5 <- function(weights, df){
  
  # Applying weights
  new_df <- data.frame(cbind(with(df, df[,c(3:10)]*weights), team = as.factor(df[,18])))
  
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
  
  return(1 - rf_pred)
  
}

out5 <- optim(par = rep(1 / 8, 8),  # initial guess
             fn = min_error_func5,
             df = imputedSet,
             method = "L-BFGS-B",
             lower = 0,
             upper = 10)
out5

weights_5 <- out5$par

min_error_func5(weights_5, imputedSet)

str(differenedScoresAmateur)

amateur_weighted <- data.frame(cbind(with(differenedScoresAmateur, differenedScoresAmateur[,c(1:8)]*weights_5), id = player_ids2))
NWT_weighted <- data.frame(cbind(with(differenedScoresNWT, differenedScoresNWT[,c(1:8)]*weights_5), id = player_ids))

scores_amateur <- rowSums(amateur_weighted)
scores_NWT <- rowSums(NWT_weighted)

hist(scores_amateur)
hist(scores_NWT)

max(scores_amateur)
max(scores_NWT)

min(scores_amateur)
min(scores_NWT)

mean(scores_amateur)
mean(scores_NWT)

########### GARBAGE

# trying with a different imputation
imputedSet2 <- read.csv("missingForest_Impute.csv", as.is = T, header = T)
str(imputedSet2)

imputedSet2$Team2 <- ifelse(imputedSet2$Test.ID == "WNT", "WNT", "Amateur")

##### Creating function to use with optim
min_error_func6 <- function(weights, df){
  
  # Applying weights
  new_df <- data.frame(cbind(with(df, df[,c(11:19)]*weights), team = as.factor(df[,20])))
  
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
  
  return(1 - rf_pred)
  
}

out6 <- optim(par = rep(1 / 8, 8),  # initial guess
              fn = min_error_func6,
              df = imputedSet2,
              method = "L-BFGS-B",
              lower = 0,
              upper = 1)
out6

weights_6 <- out6$par

min_error_func6(weights_6, imputedSet2)

str(differenedScoresAmateur)

amateur_weighted <- data.frame(cbind(with(differenedScoresAmateur, differenedScoresAmateur[,c(1:8)]*weights_6), id = player_ids2))
NWT_weighted <- data.frame(cbind(with(differenedScoresNWT, differenedScoresNWT[,c(1:8)]*weights_6), id = player_ids))

scores_amateur <- rowSums(amateur_weighted)
scores_NWT <- rowSums(NWT_weighted)

hist(scores_amateur)
hist(scores_NWT)

max(scores_amateur)
max(scores_NWT)

min(scores_amateur)
min(scores_NWT)

mean(scores_amateur)
mean(scores_NWT)

##################################################################################

####### Splitting between national womens and amateur team
imputedNWT <- imputedSet3 %>% filter(Team == "WNT")
imputedAmateur <- setdiff(imputedSet3, imputedNWT)

# Function that finds the highest/lowest across all tests
maxFunc <- function(col) {
  a <- max(col)
}

minFunc <- function(col) {
  a <- min(col)
}

# Finding the highest
maxVals <- as.vector(unlist(map(imputedSet3[,c(7:10)], maxFunc)))
minVals <- as.vector(unlist(map(imputedSet3[,c(3:6)], minFunc)))

which.max(imputedAmateur$CMJ)
imputedAmateur[401,]

# function to get value - max across ages
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

str(imputedNWT)

nrow(imputedNWT)
length(player_ids)

# Putting into new DF
differenedScoresNWT <- data.frame(cbind(X30.15, X10m, X30m, X40m, Max.Speed, ASR, Broad.Jump, CMJ))
colnames(differenedScoresNWT) <- names(imputedNWT)[3:10] 

apply(differenedScoresNWT, MARGIN = 2, min)


########## AMATEURS

##### IDs for amateur
player_ids2 <- unique(imputedAmateur$Id)

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

differenedScoresAmateur <- data.frame(cbind(X30.15, X10m, X30m, X40m, Max.Speed, ASR, Broad.Jump, CMJ))
colnames(differenedScoresAmateur) <- names(imputedNWT)[3:10] 

apply(differenedScoresAmateur, MARGIN = 2, min)

which.min(differenedScoresAmateur$CMJ)
which.min(differenedScoresNWT$CMJ)
player_ids2[61]

imputedNWT %>% filter(Id == 7)
imputedAmateur %>% filter(Id == 102)


#######################################################################################


amateur_weighted2 <- data.frame(cbind(with(differenedScoresAmateur, differenedScoresAmateur[,c(1:8)]*weights_6), id = player_ids2))
NWT_weighted2 <- data.frame(cbind(with(differenedScoresNWT, differenedScoresNWT[,c(1:8)]*weights_6), id = player_ids))

scores_amateur <- rowSums(amateur_weighted2)
scores_NWT <- rowSums(NWT_weighted2)

hist(scores_amateur)
hist(scores_NWT)

newdf3 <- data.frame(rbind(amateur = scores_amateur, nwt = scores_NWT))

ggplot(data = data.frame(scores_amateur), aes(x = scores_amateur)) + 
  geom_histogram(binwidth = 10, alpha = 0.5, position = "identity", color = "blue")
  
ggplot(data = data.frame(scores_NWT), aes(x = scores_NWT)) + 
  geom_histogram(binwidth = 10, alpha = 0.5, position = "identity", color = "red")


max(scores_amateur)
max(scores_NWT)

min(scores_amateur)
min(scores_NWT)

mean(scores_amateur)
mean(scores_NWT)
