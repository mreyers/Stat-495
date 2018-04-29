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