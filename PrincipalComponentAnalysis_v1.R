# Principal Components Regression
# Statistics 495
# March 30th, 2018

# Library

library(stats)
library(pls)

# Read In Data
players <- read.csv("missingForest_Impute.csv", header = TRUE)
str(players)
players <- players[,-c(1)] # Remove first column.  Useless variable

# Change to binary player teams.
players$Team <- ifelse(players$Team == "WNT", "WNT", "NEX")

# Select fitness test variables, minus squat jump.
fitnessTests <- players[, c(10, 11, 12, 13, 14, 15, 17, 18)] 
str(fitnessTests)
  
# Run Principal Component Analysis
pca1 <- prcomp(~ X30.15 +
                 X10m +
                 X30m +
                 X40m +
                 Max.Speed +
                 ASR +
                 Broad.Jump +
                 CMJ,
               data = fitnessTests, scale. = TRUE)

pca1
summary(pca1)

# Construct Dataframe for plotting
pcaDF <- data.frame(pca1$rotation)
str(pcaDF)

pcaVect <- c(pcaDF[, 1],
             pcaDF[, 2],
             pcaDF[, 3],
             pcaDF[, 4],
             pcaDF[, 5],
             pcaDF[, 6],
             pcaDF[, 7],
             pcaDF[, 8])

pcaVect_Names <- c("30.15",
                   "10m",
                   "30m",
                   "40m",
                   "Max.Speed",
                   "ASR",
                   "Broad.Jump",
                   "CMJ")

pcaVect2 <- rep(pcaVect_Names, 8)

a <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")
b <- c(rep("PC1", 8), rep("PC2", 8), rep("PC3", 8), rep("PC4", 8), rep("PC5", 8), rep("PC6", 8),rep("PC7", 8),rep("PC8", 8))

pcaDF2 <- as.data.frame(cbind.data.frame(pca_value = as.numeric(as.character(pcaVect)), fitness_Test = pcaVect2, PCA = b))

str(pcaDF2)
head(pcaDF2)


# Barplot for each principal component
plot1 <- ggplot(data = pcaDF2, aes(x = fitness_Test, y = pca_value)) + geom_col() + facet_wrap(~PCA)



str(pca1$x)
values <- data.frame(pca1$x)[,c(1:5)]
values$Team = as.factor(players$Team)

str(values)

# Principal Component Regression

pca_fit1 <- glm(Team ~ PC1 + PC2 + PC3 + PC4 + PC5, data = values, family = "binomial")
pca_fit1
summary(pca_fit1)

# Leave One Out Predictions
u <- list()

# Testing for Leave One Out Prediction 
for(i in 1:(nrow(values) - 1)) {
  
  # n-1 model fits and predictions 
  pc_reg <- glm(Team ~ PC1 +
                 PC2 +
                 PC3 +
                 PC4 +
                 PC5
               , data = values[-i, ], family = "binomial")
  
  test = predict(pc_reg, values[i,], type = "response")
  u[i] = test[[1]]
}
u
# Last model fit and prediction
pc_reg_nth <- glm(Team ~ PC1 +
  PC2 +
  PC3 +
  PC4 +
  PC5, data = values[-750], family = "binomial")


last_u = predict(pc_reg_nth, values[749:750, ], type = "response")
last_u[2]

# Combine Results
u2 <- as.vector(unlist(u))
final_pca_preds <- as.data.frame(append(u2, last_u[2]))
newSet <- data.frame(cbind(prob = 1 - final_pca_preds, id = players$id, age = players$Age, team = players$Team))


# Write Final Dataframe for further processing
write.csv(newSet, "PCA_Preds.csv", row.names = FALSE)



