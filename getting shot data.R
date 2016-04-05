library(jsonlite)

#get player ID info
#need to get updated player list
player.info <- read.csv("players.csv", header = T)
playerID <- player.info$PLAYER_ID

#Initialize
shot.df <- data.frame()

#get data for each player with ID
ct <- 1
for (playerID in player.info$PLAYER_ID[1:490]){
  Name <- player.info$PLAYER_NAME[ct]
  shotURL <- paste("http://stats.nba.com/stats/playerdashptshotlog?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00*Location=&Month=0&OpponentTeamID=0&Outcome=&Period=0&PlayerID", playerID, "&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=", sep = "")
  
  shot.df <- rbind(shot.df, data.frame(matrix(unlist(shotData$resultSets[3]), ncol=19, byrow=F), playerName=Name))
  
  #optional step to check progress
  print(playerID)
  ct <- ct + 1
}

#colnames
colnames(shot.df) <- c(unlist(shotData$resultSets[2]), "playerName")

write.csv(shot.df, file = "shot.csv")