library(XML)

#set up connection to database
con <- dbConnect(drv="SQLite", dbname="nba_rRegression_chi/nba.db")
teams <- dbGetQuery(con, "SELECT * FROM teams")

#set up years
years <- 2013:2014

#initial dataframe
win_vector <- as.data.frame(matrix(0, nrow=0, ncol=3))

for(j in years){
  #set up url for extraction
  url <- paste("http://espn.go.com/nba/standings/_/year/", j, sep="")
  standings <- readHTMLTable(url[1], header=T)[[1]][c(2:9, 11:17, 20:27, 29:35), c(2:3)]
  colnames(standings) <- c("name", "wins")
  standings[,1] <- as.character(standings[,1])
  
    for(i in 1:length(standings[,1])){
      standings[i,1] <- gsub("x - ", "", standings[i,1])
      standings[i,1] <- gsub("y - ", "", standings[i,1])
      standings[i,1] <- gsub("z - ", "", standings[i,1])
    }
  
  standings <- standings[order(standings$name),]
  teams <- teams[order(teams$fullName),]
  team_wins <- cbind(standings, teams)
  team_wins$year <- j - 1
  team_wins <- team_wins[, c(2, 3, 5)]
  win_vector <- rbind(win_vector, team_wins)
}

rownames(win_vector) <- NULL
write.csv(win_vector, "team_wins.csv")