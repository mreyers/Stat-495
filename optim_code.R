# Initializing variable for later
saved_MSE1520 <- 0


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
  saved_MSE1520 <<- c(saved_MSE1520, 1-rf_pred)
  
  # Returning
  return(1 - rf_pred)
  
}

###### Subsetting data set
finalDFscaled1520 <- filter(finalDFscaled, Age > 20)

#### Optimizing
out2 <- optim(par = rep(0, 8),  # initial guess
             fn = min_error_func,
             df = finalDFscaled1520,
             method = "L-BFGS-B",
             lower = 0,
             upper = 1)

### Looking at error rates with various weights
optimErrorDF <- data.frame(MSE = saved_MSE[-1], trial = time(saved_MSE[-1]))

optim_weights

out2$par

