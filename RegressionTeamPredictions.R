# Slow and Steady Approach

# Packages

install.packages("doParallel")
install.packages("foreach")
install.packages("sqldf")

# Libraries
library(glmnet)
library(caret)
library(doParallel)
library(foreach)
library(tidyverse)
library(sqldf)

# Read Data
players <- read.csv("missingForest_Impute.csv", header = TRUE)


names(players)
dim(players)
players <- players[,-c(1, 17)]

players$Team <- ifelse(players$Team == "WNT", "WNT", "NEX")

players$Team <- as.factor(players$Team)
str(players$Team)

unique(players$Position)

players$Position <- ifelse(players$Position == "FWD" | players$Position == "CMF" | players$Position == "CF", "FWD", "BCK")

# Logistic Ridge Regression
fit1 <- glm(Team ~ Age + 
              Position + 
              Mass + 
              Height + 
              X30.15 + 
              X10m + 
              X30m + 
              X40m + 
              Max.Speed + 
              ASR + 
              Broad.Jump + 
              CMJ, 
            data = players, family = binomial) 

str(fit1)
fit1
summary(fit1)
fit1$

names(players)
# Ridge Regression
# 75% of the sample size
smp_size <- floor(0.75 * nrow(players))

# set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(players)), size = smp_size)

# Splitting
train <- players[train_ind, ]
test <- players[-train_ind, ]


x1_train <- train[, -c(1, 2, 4, 5, 6, 7)]
x2_train <- as.matrix(x1_train)

glmmod <- glmnet(x2, y=as.factor(players$Team), alpha=0, family="binomial")
glmmod

plot(glmmod, xvar="lambda")

cv.glmmod <- cv.glmnet(x2_train, y=as.factor(train$Team), alpha=0, family = "binomial")
plot(cv.glmmod)

(best.lambda <- cv.glmmod$lambda.min)

best.ridgeModel <- glmnet(x2_train, y = as.factor(train$Team), alpha = 0, family = "binomial", lambda = best.lambda)


summary(best.ridgeModel)
best.ridgeModel$beta
best.ridgeModel$call


x1_test <- test[, -c(1, 2, 4, 5, 6, 7)]
x2_test <- as.matrix(x1_test)


(pred2 <- 1 - pred1)  

pred3 <- predict(best.ridgeModel, newx = x2_test, type = "response")
test


# Predictions

x3_full <- data.frame(cbind(Team = players$Team, x2_full))

results <- train(as.factor(Team) ~. , method = "glmnet", data = x3_full, trControl = trainControl(method = "LOOCV"))

  
# Leave One Out Prediction

pkgs <- c('doParallel', 'foreach')
lapply(pkgs, require, character.only = T)

registerDoParallel(cores = 2) 

# Model 1


glmmod1 <- glmnet(x2, y=as.factor(players$Team), alpha=0, family="binomial")

loop_data <- data.frame(cbind(Outcome = players$Team, x2))

glmmod1 <- append(glmmod1, list(loop_data))
str(glmmod1)
length(glmmod1)
names(glmmod1)[14]<-"data"
glmmod1$loop_data
    
loo_predict <- function(obj){ 
  library(glmnet)
  yhat <- foreach(i = 1:nrow(obj$data), .combine = rbind) %dopar% {
    predict(update(obj, data = obj$data[-i, ]), obj$data[i,], type = "response")
  }
  return(data.frame(result = yhat[, 1], row.names = NULL))
}  

u <- list()


cv.glmmod <- cv.glmnet(x2, y=as.factor(players$Team), alpha=0, family = "binomial")
best.lambda <- cv.glmmod$lambda.min

# Testing for Leave One Out Prediction 
for(i in 1:(nrow(x2) - 1)) {
  # 75% of the sample size
  # smp_size <- floor(0.75 * nrow(players))
  
  # set the seed to make your partition reproductible
  # train_ind <- sample(seq_len(nrow(x2)), size = smp_size)
  
  # Splitting
  # train <- x2[train_ind, ]
  # testset <- x2[-train_ind, ]
  # y_train <- players[train_ind, 2] # Second column for Team variable
  # y_test <- players[-train_ind, 2] 
  # x2_test <- as.matrix(testset) # Needs to be a matrix for glmnet to work
  
  cv.glmmod <- cv.glmnet(x2[-i,], y=as.factor(players$Team)[-i], alpha=0, family = "binomial")
  best.lambda <- cv.glmmod$lambda.min

  # Model fit with best lambda  
  glmmod1 <- glmnet(x2[-i,], y=as.factor(players$Team)[-i], alpha=0, family="binomial", lambda = best.lambda)
  test = predict(glmmod1, x2[i:(i+1),], type = "response")
  u[i] = test[[1]]
}

u2 <- as.vector(unlist(u))

glmmod2 <- glmnet(x2[-750,], y = as.factor(players$Team)[-750], alpha = 0, family = "binomial", lambda = best.lambda)
last_u = predict(glmmod2, x2[749:750, ], type = "response")

final_ridge_preds <- as.data.frame(append(u2, last_u[2]))
newSet2 <- data.frame(cbind(prob = 1 - final_ridge_preds, id = players$id, age = players$Age, team = players$Team))

write.csv(newSet2, "Ridge_Preds.csv", row.names = FALSE)


glmmod1 <- glmnet(x2[-1,], y=as.factor(players$Team)[-1], alpha=0, family="binomial", lambda = best.lambda)
test = predict(glmmod1, x2[1:(1+1),], type = "response")
u[1] = test[[1]]

n <- do.call(rbind, u)

glmmod2 <- glmnet(x2[-1,], y=as.factor(players$Team)[-1], alpha=0, family="binomial")
predict(glmmod2, x2[1:2,], type = "response")


bad_pred <- predict(best.ridgeModel, newx = x2_full[1:2,], type = "response") 
bad_pred[1]u <- 1:10


loop1 <- loo_predict(fit1)  


newSet2 <- data.frame(cbind(prob = 1 - loop1$result, id = players$id, age = players$Age, team = players$Team))

write.csv(newSet2, "logistic_LOOP.csv")


### Clean Data

str(newSet2)
names(newSet2) <- c("Prob", "id", "Age", "Team")

weightedDF <- read.csv("weightedDF.csv", header = TRUE)
str(weightedDF)


newSet3 <- sqldf("SELECT Prob
             FROM newSet2
             LEFT JOIN weightedDF USING(id)
             WHERE newSet2.Age == weightedDF.age")

str(newSet3)

finalDF <- cbind(newSet3, weightedDF)

write.csv(finalDF ,"Scores_Probs.csv", row.names = FALSE)


clusters3 <- kmeans(finalDF$scores, centers = 10, nstart = 1)
finalDF2 <- cbind(finalDF, clusters3$cluster)
names(finalDF2)[6] <- c("Cluster")


finalDF <- read.csv("Scores_Probs.csv", header = TRUE)
finalDF


ggplot(finalDF2) +
  geom_point(aes(x=Prob, y=scores, color=factor(Cluster)), size=3, shape = 20) +
  stat_ellipse(aes(x=Prob,y=scores,fill=factor(Cluster)), geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster")) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0.5,6.5))


# Adding on additonal information to the dataset

str(players)
str(finalDF)
players <- players[,-c(1)]

newSet4 <- sqldf("SELECT Mass, Height, Position
             FROM players
                 LEFT JOIN finalDF Using(id)
                 WHERE players.Age == finalDF.age")

newSet4

finalDF3 <- cbind(finalDF, newSet4)
str(finalDF3)

write.csv(finalDF3, "finalData_v1.csv", row.names = FALSE)
