library(RCurl)

library(jsonlite)
library(dplyr)


source("_functions.R")

all.movements <- sportvu_convert_json("data/0021500431.json")

str(all.movements)

gameid = "0021500431"

pbp <- get_pbp(gameid)
head(pbp)

pbp <- pbp[-1,]
colnames(pbp)[2] <- c("event.id")

pbp <- pbp %>% select (event.id, EVENTMSGTYPE, EVENTMSGACTIONTYPE, SCORE)
pbp$event.id <- as.numeric(levels(pbp$event.id))[pbp$event.id]
all.movements <- merge(x = all.movements, y = pbp, by = "event.id", all.x=TRUE)

id303 <- all.movements[which(all.movements$event.id == 303),]
head(id303)

ginobili_make <- all.movements[which(all.movements$lastname == "Ginobili" & all.movements$EVENTMSGTYPE == 1),]
ginobili_miss <- all.movements[which(all.movements$lastname == "Ginobili" & all.movements$EVENTMSGTYPE == 2),]
ginobili_rebound <- all.movements[which(all.movements$lastname == "Ginobili" & all.movements$EVENTMSGTYPE == 4),]

travelDist(ginobili_make$x_loc, ginobili_make$y_loc)

#misses
travelDist(ginobili_miss$x_loc, ginobili_miss$y_loc)

#Rebounds
travelDist(ginobili_rebound$x_loc, ginobili_rebound$y_loc)

player_layup <- all.movements[which(all.movements$EVENTMSGACTIONTYPE == 5),]
player.groups <- group_by(player_layup, lastname)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc, y_loc), playerid=max(player_id))
arrange(dist.traveled.players, desc(totalDist))

player_layup <- all.movements[which(all.movements$EVENTMSGACTIONTYPE == 5 & all.movements$EVENTMSGTYPE == 1),]
player.groups <- group_by(player_layup, lastname)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc, y_loc), playerid = max(player_id))
arrange(dist.traveled.players, desc(totalDist))