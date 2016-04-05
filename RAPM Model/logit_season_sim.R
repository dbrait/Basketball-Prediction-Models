library(dplyr)
library(e1071)

data <- read.csv("rpm_dataset.csv")

#add hoe feature and win/loss column
data <- mutate(data, home=1)
data$homeWin <- ifelse(data$home_team_score > data$visit_team_score, 1, 0)

#set up datasets
train = filter(data, game_year %in% c(2008, 2009, 2010, 2011, 2012))
test = filter(data, game_year == 2013)

xtest = test[,9:17]
ytest = test[,18]
xtrain = train[,9:17]
ytrain = train[,18]

#logit regression to get probabilities
mylogit <- glm(homeWin ~ RPM_weight.0 + ORPM_weight.0 + DRPM_weight.0 + PER_weight.0 +
                 RPM_weight.1 + ORPM_weight.1 + DRPM_weight.1 + PER_weight.1 + home, data=train,
               family="binomial")
logit_preds <- as.data.frame(predict(mylogit, newdata=xtest, type="response"))
logit_preds$class <- ifelse(logit_preds[,1] >= .5, 1, 0)
logit_preds <- cbind(logit_preds, ytest)
logit_preds$result <- abs(logit_preds[,2] - logit-preds[,3])
logit_probs <- cbind(test, logit_preds)[, c(2:8, 17:22)]
names(logit_probs)[10] = "home_prob"

#Create dataset for simulation
num_seasons <- 1000
season_df <- data.frame(matrix(0, nrow=length(unique(logit_probs$home_team)), ncol=num_seasons))
row.names(season_df) <- unique(logit_probs$home_team)
colnames(season_df) = paste("season_", 1:1000, sep="")

#unique match ids for given season
match_ids <- unique(logit_probs$match_id)

#loop through each season
for(i in 1:1000){
  print(paste("season", i))
  
  #Generate random outcomes
  random_outcomes <- runif(length(logit_probs[,1]))
  logit_probs <- cbind(logit_probs, random_outcomes)
  
  #Loop through each match ID
  for(match in match_ids){
    #using random number assign winner of game to data frame
    if(logit_probs[logit_probs$match_id == match,]$random_outcomes <= logit_probs[logit_probs$match_id == match,]$home_prob){
      #iterate season data frame for home team
      season_df[as.character(logit_probs[logit_probs$match_id == match,]$home_team), i] = season_df[as.character(logit_probs[logit_probs$match_id == match,]$home_team), i] + 1
    }
  }
  #Remove random outcomes
  logit_probs <- logit_probs[,-14]
}

#Check results
means <- apply(season_df, 1, mean)
ses <- apply(season_df, 1, sd)

#Save outcomes
write.csv(season_df, "sim_2013_logit_df.csv")
write.csv(cbind(means, ses), "sim_2013_logit.csv")