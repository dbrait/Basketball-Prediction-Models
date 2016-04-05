library(dplyr)
library(e1071)

data <- read.csv("rpm_dataset.csv")

#add home feature and win/loss column
data <- mutate(data, home=1)
data$homeWin <- ifelse(data$home_team_score > data$visit_team_score, 1, 0)

years <- c(2008, 2009, 2010, 2011, 2012, 2013)
train = filter(data, game_year %in% c(2008, 2009, 2010, 2011))
test = filter(data, game_year == 2012)

xtest = test[,9:17]
ytest = test[,18]
xtrain = train[,9:17]x
ytrain = train[,18]

#naive bayes to get prob
model <- naiveBayes(xtrain, ytrain)
preds <- as.data.frame(predict(model, xtest, type=c("raw"), threshold = 0.001))
preds$class <- ifelse(preds[,2] > preds[,1], 1, 0)
preds <- cbind(preds, ytest)

#save dataset with correct probabilities
probs <- cbind(test, preds)[, c(2:8, 17:22)]

#rename home and away
names(probs)[names(probs) == "0"] <- "away_prob"
names(probs)[names(probs) == "1"] <- "home_prob"

#create dataset for im
num_seasons <- 1000
season_df <- data.frame(matrix(0, nrow=length(unique(probs$home_team)), ncol=num_seasons))
row.names(season_df) <- unique(probs$home_team)
colnames(season_df) = paste("season_", 1:1000, sep="")

#get unique match id's for given season
match_ids <- unique(probs$match_id)

#get random number for each row
random_outcomes <- runif(length(probs[,1]))
probs <- cbind(probs, random_outcomes)

#loop through each season
for(i in 1:1000){
  print(paste("season", i))
  
  #loop thrugh each match and prob of a season
  for(match in match_ids){
    #Generate uniform random number
    res <- runif(1)
    
    #pull relevant game
    game <- probs[probs$match_id == match,]
    #Using random number, assign winner of game to data frame
    if(res <= game$away_prob){
      #Iterate season data frame for away team
      season_df[as.character(game$visit_team), i] = season_df[as.character(game$visit_team), i] + 1
    } else{
      #iterate season data frame for home team
      season_df[as.character(game$home_team), i] = season_df[as.character(game$home_team), i] + 1
    }
  }
}

#check out results
means <- apply(season_df, 1, mean)
ses <- apply(season_df, 1, sd)

#save outcomes
write.csv(cbind(means, ses), "sim_2012.csv")