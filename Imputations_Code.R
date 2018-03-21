# Statistics 495: Advance Sports Analytics
# Simon Fraser University
# March 18, 2018
# Brad Smallwood, Matthew Reyers, Barinder Thind

# Imputations - Version 2

# Install Packages
install.packages("tidyverse")
install.packages("caret")
install.packages("RANN") # Need it for KNN Imputation
install.packages("caTools") # Use for splitting data
install.packages("mice")
install.packages("missForest") # The missing forest package

# Load Libraries
library(tidyverse)
library(caret)
library(RANN)
library(caTools)
library(mice)
library(missForest)

# Read in the data.  Same file you gave to us.
data <- read.csv("soccer (1).csv", header = TRUE)
View(data)
names(data)


# We chose to remove squat jump because it is highly correlated with CMJ (was something like 0.99).
# Furthermore, each of the values missing in CMJ were also missing in squat jump but several more observations were missing in squat jump.
# If we had not removed squat jump we would have approximately 10 observations that had complete data.
# By removing squat jump we have 103 observations that are complete and are able to properly test our imputations.

data %>% select(-Squat.Jump) -> data

# Imputation Algorithm 1:  missingForest

install.packages("missForest")
library(missForest)

# Separate out the observations with full data.
fullTests <- data[complete.cases(data),]

# Remove the first few columns that don't need to be imputed.  Don't want NAs from the next step on those columns.
fullTests2 <- fullTests[, -c(1:7)]

# Add NA values to the data.  20% of the data now has NAs.
train <- prodNA(fullTests2, noNA = 0.2)

# Add back on the first few columns now that NAs won't be produced on them as well. 
train2 <- cbind(fullTests[, c(1:7)], train)

# Basic Missing Forest with arbitrary number of trees.
imp.missForest <- missForest(train2, ntree = 501)

# Compare imputed values to the actual values.
tempCheck_missForest <- imp.missForest$ximp[,-c(1:7)] - fullTests2
summary(tempCheck_missForest) # Missing Forest Imputation Results.

# Create a function that will summarize the imputations.  
imputationSummary <- function(data){
summary(tempCheck) # See other imputations at the bottom. tempCheck from the Version 1 code.
  imputed <- list()
  for(i in 1:8){
    imputedHolder <- data[, i]
    imputed[[i]] <- imputedHolder[imputedHolder != 0] %>% quantile( na.rm = TRUE)
    print(summary(imputed))
  }
  return(imputed)
}

# The results of the various imputation methods we tried.
# See the imputation code for them at the bottom of this file.

imputationSummary(tempCheck_missForest) # Missing Forest
imputationSummary(tempCheck) # Mice
imputationSummary(check) # KNN

imputation_Results <- list(MissingFor_imp = imputationSummary(tempCheck_missForest),
                           mice_imp = imputationSummary(tempCheck),
                           KNN_imp = imputationSummary(check))



### Optimizing Missing Forest ###

#Set Seed
set.seed(12345)

# Same set up as above.
fullTests <- data[complete.cases(data),]
fullTests2 <- fullTests[, -c(1:7)] 

train <- prodNA(fullTests2, noNA = 0.2)
train2 <- cbind(fullTests[, c(1:7)], train)

# Initialize lists to hold results.
impResults_list <- list()
resultsVec <- vector()

# Various numbers of trees to do try
ntreeLength <- c(200, 300, 400, 500, 600, 700, 800, 900, 1000)

# Nested for loop to run all the missing forests and save the results.
for(i in 1:(ncol(train2)-1)){
  for(j in 1:length(ntreeLength)){
    #impResults_list[[(i-1)*length(ntreeLength) + j]] <- missForest(train2, ntree = ntreeLength[j], mtry = i)
    resultsVec[(i-1)*length(ntreeLength) + j] <- missForest(train2, ntree = ntreeLength[j], mtry = i)$OOBerror[1]
  }
}

# impResults_list[[909]]$OOBerror[1]
resultsVec

# Find the Missing Forest with the lowest Out of Bag Error.
plot(resultsVec)
which.min(resultsVec)
for(i in 1:16){
  for(j in 1:9){
    print(i)
    print(j)
    print((i-1)*length(ntreeLength) + j)
  }
}

# Optimal trees and mtry:  7 variables, 200 trees

imp.missForest_optimal <- missForest(train2, ntree = 200, mtry = 7)
imp.missForest_optimal$ximp

dim(fullTests2)
dim(imp.missForest_optimal$ximp[,-c(1:7)])
tempCheck_missForest2 <- imp.missForest_optimal$ximp[,-c(1:7)] - fullTests2

imputationSummary(tempCheck_missForest2)

# Final imputated data
write.csv(imp.missForest_optimal$ximp, "finalImputation.csv")


########################################################################################################################################

# Imputations - Version 1

# Separate the data into those that are on WNT and those that are not
WNT <- data %>% filter(Team == "WNT")
Amateurs <- data %>% filter(Team != "WNT")

# Average amount of data we have to work with
missingCheck <- apply(data, MARGIN = 2, FUN = is.na)
counts <- ncol(data) - rowSums(missingCheck)

# Same check but excluding the identifying data
missingNotID <- apply(data[, 8:dim(data)[2]], MARGIN = 2, FUN = is.na)
countsNotID <- ncol(data[, 8:dim(data)[2]]) - rowSums(missingNotID)
hist(countsNotID)

# Data by team
data$UsableData <- countsNotID

goodDataAmateur <- Amateurs[Amateurs$UsableData >= 9, ]
modified <- goodDataAmateur

# Test with 20 random NA values per column, randomly done over the set
for(i in 1:8){
  samp <- sample(1:dim(goodDataAmateur)[1], 20)
  modified[samp, i + 9] <- NA
}
summary(modified) # Each entry now has 20 missing values

# Do the imputation with mice
toImpAmateur <- modified[, 10:17]
miceImpAmateur <- mice(toImpAmateur)
test1 <- mice::complete(miceImpAmateur, action = 1)
test2 <- mice::complete(miceImpAmateur, action = 2)
test3 <- mice::complete(miceImpAmateur, action = 3)
test4 <- mice::complete(miceImpAmateur, action = 4)
test5 <- mice::complete(miceImpAmateur, action = 5)

amateurImputed <- data.frame()

for(i in 1:dim(test1)[1]){
  for(j in 1:dim(test1)[2]){
    amateurImputed[i,j] <- mean(test1[i, j],
                                test2[i, j],
                                test3[i, j],
                                test4[i, j],
                                test5[i, j])
  }
}
names(amateurImputed) <- names(toImpAmateur)
amateurImputed$id <- toImpAmateur$id
amateurImputed


# This is the varible that gets used above when we compare it to the Missing Forest
tempCheck <- amateurImputed - goodDataAmateur[, 10:17]


# Imputations - Version 1.5

# Caret Imputations

# Median Impution
imp.median <- predict(preProcess(data), method = c("medianImpute"), data)

# Knn Imputation
imp.knn <- predict(preProcess(data, method = c("knnImpute")), data)



