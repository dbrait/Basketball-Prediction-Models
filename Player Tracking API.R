library(RCurl)
library(jsonlite)
library(png)
library(plotrix)
library(dplyr)

#read player movement data for specific play for game
url = "http://stats.nba.com/stats/locations_getmoments/?eventid=308&gameid=0041400235"
x <- getURL(url)
the.data <- fromJSON(x)

#read full court image and rasterize in R
full.court <- "http://tcbanalytics.com/uploads/fullcourt.png"
the.court <- readPNG(full.court)
plot(0:94, xlim=c(0,94), ylim=c(50,0), type="n")
lim <- par()
rasterImage(the.court, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])


#read in home and away team player info
home.team <- the.data$home
away.team <- the.data$visitor

moments <- the.data$moments
headers = c("team_id", "player_id", "x_loc", "y_loc",
            "radius", "game_clock", "shot_clock", "quarter")

quarters <- unlist(lapply(moments, function(x) x[1]))
game.clock <- unlist(lapply(moments, function(x) x[3]))
shot.clock <- unlist(lapply(moments, function(x) x[4]))

#add quarter, game clock shot clock and info to each moment
moment.details <- lapply(moments, function(x) x[[6]])
x <- mapply(function(a,b,c,d) cbind(a,b,c,d), moment.details, game.clock, shot.clock, quarters)
all.movement<- do.call("rbind", x)
colnames(all.movement) <- headers
all.movement <- data.frame(all.movement)
all.movement <- all.movement[order(all.movement$game_clock),]

home.players <- home.team$players
away.players <- away.team$players
colnames(home.players)[3] <- "player_id"
colnames(away.players)[3] <- "player_id"

#add player name info to each movement moment
home.movements <- merge(home.players, all.movement, by="player_id")
away.movements <- merge(away.players, all.movement, by="player_id")
home.movements <- home.movements[order(home.movements$game_clock, decreasing = TRUE),]
away.movements <- away.movements[order(away.movements$game_clock, decreasing = TRUE),]
all.movements <- rbind(home.movements, away.movements)

#James Harden as example
james <- home.movements[which(home.movements$lastname == "Harden"),]
lines(james$x_loc, james$y_loc, type="b", col=cut(james$game_clock, breaks=3))


#function to calculate player distance traveled
travelDist <- function(xloc, yloc){
  diffx <- diff(xloc)
  diffy <- diff(yloc)
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a <- diffx2 + diffy2
  b <- sqrt(a)
  return (sum(b))
}

travelDist(james$x_loc, james$y_loc)

#calc distance traveled for each player
player.groups <- group_by(all.movements, player_id)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc, y_loc))
all.players <- rbind(home.players, away.players)
player.travel <- merge(all.players, dist.traveled.players, by="player_id")
arrange(player.travel, desc(totalDist))
