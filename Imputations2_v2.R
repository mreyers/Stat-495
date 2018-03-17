# Imputations
# Time to stop fucking around and figure out these imputations

# Install Packages
install.packages("tidyverse")
install.packages("caret")
install.packages("RANN") # Need it for KNN Imputation
install.packages("caTools") # Use for splitting data
install.packages("mice")


# Load Libraries
library(tidyverse)
library(caret)
library(RANN)
library(caTools)
library(mice)

data <- read.csv("soccer.csv", header = TRUE)
soccer <- read.csv("soccer.csv", header = TRUE)
View(data)
names(data)

data %>% select(-Squat.Jump) -> data

# Imputation Algorithm 1:  missingForest

install.packages("missForest")
library(missForest)

?missForest()

imp.missForest <- missForest(data)


write.csv(imp.missForest$ximp, "missingForest_Impute.csv")

View(imp.missForest$ximp)

imp.missForest$OOBerror



fullTests <- data[complete.cases(data),]
fullTests2 <- fullTests[, -c(1:7)]

train <- prodNA(fullTests2, noNA = 0.2)
train2 <- cbind(fullTests[, c(1:7)], train)

imp.missForest2 <- missForest(train2, ntree = 501)

dim(imp.missForest2$ximp)
dim(fullTests2)

tempCheck_missForest <- imp.missForest2$ximp[,-c(1:7)] - fullTests2
summary(tempCheck_missForest) # Missing Forest Imputation Results

imputationSummary <- function(data){
summary(tempCheck) # Matt Mice Results
  imputed <- list()
  for(i in 1:8){
    imputedHolder <- data[, i]
    imputed[[i]] <- imputedHolder[imputedHolder != 0] %>% quantile( na.rm = TRUE)
    print(summary(imputed))
  }
  return(imputed)
}

imputationSummary(tempCheck_missForest) # Missing Forest
imputationSummary(tempCheck) # Mice
imputationSummary(check) # KNN

imputation_Results <- list(MissingFor_imp = imputationSummary(tempCheck_missForest),
                           mice_imp = imputationSummary(tempCheck),
                           KNN_imp = imputationSummary(check))



##################################### Optimizing Missing Forest ######################################################

#Set Seed
set.seed(12345)

# Using These Datasets
fullTests <- data[complete.cases(data),]
fullTests2 <- fullTests[, -c(1:7)] # Remove useless 

train <- prodNA(fullTests2, noNA = 0.2)
train2 <- cbind(fullTests[, c(1:7)], train)


imp.missForest2 <- missForest(train2, ntree = 501)

impResults_list <- list()
resultsVec <- vector()
ntreeLength <- c(200, 300, 400, 500, 600, 700, 800, 900, 1000)
for(i in 1:(ncol(train2)-1)){
  for(j in 1:length(ntreeLength)){
    #impResults_list[[(i-1)*length(ntreeLength) + j]] <- missForest(train2, ntree = ntreeLength[j], mtry = i)
    resultsVec[(i-1)*length(ntreeLength) + j] <- missForest(train2, ntree = ntreeLength[j], mtry = i)$OOBerror[1]
  }
}

#impResults_list[[909]]$OOBerror[1]
resultsVec




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

imp.missForest_optimal <- missForest(data, ntree = 200, mtry = 7)
imp.missForest_optimal$ximp

tempCheck_missForest2 <- imp.missForest_optimal$ximp[,-c(1:7)] - fullTests2

imputationSummary(tempCheck_missForest2)

# I'm happy

write.csv(imp.missForest_optimal$ximp, "finalImputation.csv")

 
##################################################################################################################################
# Caret Imputations

# Median Impution
imp.median <- predict(preProcess(data, method = c("medianImpute")), data)

# Knn Imputation
imp.knn <- predict(preProcess(data, method = c("knnImpute")), data)
write.csv(imp.knn, "knn_Impute.csv")

# Another Random Forest imputation but from the version from the caret package.
imp.bag <- predict(preProcess(data, method = c("bagImpute")), data)
write.csv(imp.bag,"bag_Impute.csv")


######################################################################################################
# Stop the screwing around

# Regular data split into individual variables
data_mass <- data$Mass
data_height <- data$Height
data_x15.30 <- data$X30.15
data_10m <- data$X10m
data_30m <- data$X30m
data_40m <- data$X40m
data_max <- data$Max.Speed
data_ASR <- data$ASR
data_CMJ <- data$CMJ
data_broad <- data$Broad.Jump



# The observations that don't have NAs
actuals_full <- data[complete.cases(data), ] # The full dataset
actuals_mass <- data_mass[complete.cases(data_mass)]
actuals_height <- data_height[complete.cases(data_height)]
actuals_x15.30 <- data_x15.30[complete.cases(data_x15.30)]
actuals_10m <- data_10m[complete.cases(data_10m)]
actuals_30m <- data_30m[complete.cases(data_30m)]
actuals_40m <- data_40m[complete.cases(data_40m)]
actuals_max <- data_max[complete.cases(data_max)]
actuals_ASR <- data_ASR[complete.cases(data_ASR)]
actuals_CMJ <- data_CMJ[complete.cases(data_CMJ)]
actuals_broad <- data_broad[complete.cases(data_broad)]


actuals_list <- list(actuals_mass,
                     actuals_height,
                     actuals_x15.30,
                     actuals_10m,
                     actuals_30m,
                     actuals_40m,
                     actuals_max,
                     actuals_ASR,
                     actuals_CMJ,
                     actuals_broad)


check_prodNA <- prodNA(data.frame(actuals_list[1]), noNA = 0.2)
?prodNA()

# Function to run all of the imputations and compare them
compareImputations <- function(actuals){ # Takes a list
  
  results_df <- c()
  
  for(i in 1:length(actuals)){
    test <- data.frame(actuals[i])
    
    # Make a train set from vec (actual data)

    train = data.frame(prodNA(test, noNA = 0.2))
    
    #### Imputations ####
    
    # Median Imputation
    imp.median <- predict(preProcess(train, method = c("medianImpute")), train)
    
    # Mice Impuatation
    imp.knn <- predict(preProcess(train, method = c("knnImpute")), train)
    
    # Another Random Forest imputation but from the version from the caret package.
    imp.bag <- predict(preProcess(train, method = c("bagImpute")), train)
    
    # Compare to Test
    
    results.bag <-sum(abs(test - imp.bag))
    results.median <- sum(abs(test - imp.median))
    results.knn <- sum(abs(test - imp.knn))
    
    results_df[i] <- c(results.median, results.mice, results.bag)
  }
  return(results_df)
}

impCheck1 <- compareImputations(actuals_list)
debugonce(compareImputations)

barinderphile <- write.csv(impCheck1, "usuk_bb.csv")


#### LIFE SUCKS


data %>% 
  filter(Team == "WNT") -> data_WNT

data %>% 
  filter(Team != "WNT") -> data_Amateurs


imp.missForest_WNT <- missForest(data_WNT, ntree = 501)
imp.missForest

write.csv(imp.missForest$ximp, "missingForest_Impute.csv")


# Can't impute from regular data...must limit it and have a bit higher error to check the rest...



