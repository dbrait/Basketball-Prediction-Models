
source("_functions.R")
library(plotly)

#Load data
wget https://github.com/neilmj/BasketballData/blob/master/2016.NBA.Raw.SportVU.Game.Logs/12.18.2015.DET.at.CHI.7z?raw=true


all.movements <- sportvu_convert_json("data/0021500391.json")
write.csv(all.movements, "all.movements_0021500391.csv", row.names=FALSE)

#Extract movement for one player
all.movements <- all.movements %>% dplyr::arrange(quarter,desc(game_clock), x_loc)
id6 <- all.movements[which(all.movements$event.id == 6),]
rose <- all.movements[which(all.movements$lastname == "Rose" & all.movements$event.id == 6),]
gasol <- all.movements[which(all.movements$lastname == "Gasol" & all.movements$event.id == 6),]

plot_ly(data=rose, x=x_loc, y=y_loc, mode="markers", color=cut(rose$game_clock, breaks=3)) %>%
  layout(xaxis=list(range=c(0,100)),
         yaxis=list(range=c(0,50)))

plot_ly(data=gasol, x=x_loc, y=y_loc, mode="markers") %>%
  layout(xaxis=list(range=c(0,100)),
         yaxis=list(range=c(0,50)))

#Extract distance for one player

travelDist(rose$x_loc, rose$y_loc)

travelDist(gasol$x_loc, gasol$y_loc)

#Extract distance for group of players for one event
seconds = max(gasol$game_clock) - min(gasol$game_clock)
speed = travelDist(gasol$x_loc, gasol$y_loc)/seconds

#Extract distance for group of players for one event
player.groups <- group_by(id6, lastname)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc,y_loc), playerid=max(player_id))
arrange(dist.traveled.players, desc(totalDist))

#Extract distance for group of players for entire game
deduped.data <- unique(all.movements[, 1:12])
player.groups <- group_by(deduped.data, lastname)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc, y_loc), playerid=max(player_id))
total <- arrange(dist.traveled.players, desc(totalDist))

#Extract distnace for one player for entire game
gasol <- deduped.data[which(deduped.data$lastname == "Gasol"),]
travelDist(gasol$x_loc, gasol$y_loc)

#distance matrix for one player with ball for one event
rose <- all.movements[which((all.movements$lastname == "Rose" | all.movements$lastname == "ball") & all.movements$event.id == 6),]
testrose <- rose %>% filter (lastname=="Rose") %>% select (x_loc, y_loc)
testball <- rose %>% filter (lastname=="ball") %>% select (x_loc, y_loc)
testrosel <- 1:nrow(testrose)
distsdf <- unlist(lapply(testrosel, function(x) {dist(rbind(testrose[x,], testball[x,]))}))
ball_distance <- rose %>% filter (lastname=="ball") %>% select (game_clock) %>% mutate(distance=distsdf)
plot_ly(data = ball_distance, x=game.clock, y=distsdf, mode="markers")

#distance matrix with function for one player with ball for one event
player_dist <- function(lastnameA, lastnameB, eventID){
  df <- all.movements[which((all.movements$lastname == lastnameA | all.movements$lastname == lastnameB) & all.movements$event.id == eventID),]
  dfA <- df %>% filter(lastname==lastnameA) %>% select (x_loc, y_loc)
  dfB <- df %>% filter(lastname==lastnameB) %>% select (x_loc, y_loc)
  df.l <- 1:nrow(dfA)
  distsdf <- unlist(lapply(df.l, function(x) {dist(rbind(dfA[x,], dfB[x,]))}))
  return(distsdf)
}

temp <- player_dist("Rose", "ball", 6)
plot_ly(data=ball_distance, x=game_clock, y=temp, mode="markers")

#Distance Matrix for one player with all other players for one event
pickplayer <- "ball"
pickeventID <- 6
players <- all.movements %>% filter(event.id==pickeventID) %>% select(lastname) %>% distinct(lastname)
bigdistance <- lapply(list(players$lastname)[[1]], function (x){ player_dist(pickplayer, x, pickeventID)})
bigdistancedf <- as.data.frame(do.call("cbind", bigdistance))
colnames(bigdistancedf) <- list(players$lastname)[[1]]

#get clock info
clockinfo <- get_game_clock("Ginobili", 303)
bigdistancedf$game_clock <- clockinfo$game_clock
head(bigdistancedf)

#plot with plotly not elegant but works
for(i in 1:(ncol(bigdistancedf)-1)){
  if(i==1){
    pString <- "p <- plot_ly(data=bigdistancedf, x=game_clock, y = bigdistancedf[,1], name=colnames(bigdistancedf[1]), mode= 'markers')"
  } else {
    pString <- paste(pString, " %>% add_trace(y =", eval(paste("bigdistancedf[,",i,"]", sep="")),", name=", eval(paste("colnames(bigdistancedf[", i, "])", sep="")), ")", sep="")
  }
}
eval(parse(text=pString))
print(p)

#distance matrix for all players for one event id
pickeventID <- 6
players_matrix <- player_dist_matrix(pickeventID)

#Lets get game clock in minutes/seconds
library(lubridate)
seconds_to_period(361.27)
#Add to main data frame
all.movements$game_clock_minutes <- as.character(seconds_to_period(all.movements$game_clock))