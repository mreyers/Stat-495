

# Read Data

finalData <- read.csv("finalImputation.csv", header = TRUE)
finalData <- finalData %>% merge(madeTeam, all = TRUE, by = "id")
finalData %>% mutate(Success = case_when(Team == "WNT" ~ 1, TRUE ~ 0)) -> finalData
names(finalData)

# Models

#Logistic Regression

fit1 <- glm(madeTeam ~ Mass + 
              Height + 
              X30.15 + 
              X10m + 
              X30m + 
              X40m + 
              Max.Speed + 
              ASR + 
              Broad.Jump + 
              CMJ,
              data = finalData, family = "binomial")

fit1
summary(fit1)

fit2 <- glm(Success ~ Mass + 
              Height + 
              X30.15 + 
              X10m + 
              X30m + 
              X40m + 
              Max.Speed + 
              ASR + 
              Broad.Jump + 
              CMJ,
              data = finalData, family = "binomial")

fit2
summary(fit2)
View(finalData)
