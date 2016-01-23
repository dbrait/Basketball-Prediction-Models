library(rjson)
#Steph Curry shot data
playerID <- 201939
shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2014-15&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=", playerID, "&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2014-15&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showDetails=0&showShots=1&showZones=0", sep="")

#import form json
shotData <- fromJSON(file=shotURL, method="C")

#unlist shot data and save into data frame
shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=21, byrow=TRUE))

#shot data headers
colnames(shotDataf) <- shotData$resultSets[[1]][[2]]

#covert x and y coordinates into numeric
shotData$LOC_X <- as.numeric(as.character(shotData$LOC_X))
shotData$LOC_Y <- as.numeric(as.character(shotData$LOC_Y))
shotData$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))

View(shotDataf)

#simple plot using EVENT_TYPE
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
        geom_point(aes(colour=EVENT_TYPE))

library(grid)
library(jpeg)

#half court image
courtImg.URL <- "http://lookingforamerica.us/wp-content/uploads/2015/03/Nba-Basketball-Court-Dimensions.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1, "npc"), height=unit(1, "npc"))

#plot using nba court background and colour by shot zone
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(colour=SHOT_ZONE_BASIC, shape=EVENT_TYPE)) +
      xlim(-250, 250) +
      ylim(-50, 420)

#plot using ggplot and NBA court background image
ggplot(shotDataf, aes=(x=LOC_X, y=LOC_Y)) +
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(colour=SHOT_ZONE_BASIC, shape=EVENT_TYPE)) +
      xlim(250, -250) +
      ylim(-50, 420) +
      geom_rug(alpha=0.2) +
      coord_fixed() +
      ggtitle(paste("Shot Chart\n" unique(shotDataf$PLAYER_NAME), sep="")) +
      theme(line=element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(size=15, lineheight=0.9, face="bold"))

#adding player picture
library(grid)
library(gridExtra)
library(png)
library(RCurl)

#scrape player photo and save as raster object
playerImg.URL <- paste("http://stats.nba.com/media/players/132x132/", playerID, ".png", sep="")
playerImg <- rasterGrob(readPNG(getURLContent(playerImg.URL)),
                                                  width=unit(0.15, "npc"), height=unit(0.15, "npc"))
#plot using ggplot and nba court background
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour=EVENT_TYPE, alpha=0.8), size=3) +
  scale_color_manual(values= c("#008000", "#FF6437")) +
  guides(alpha=FALSE, size=FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  geom_rug(alpha=0.2) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), sep="")) +
  theme(line=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size=17, lineheight=1.2, face="bold"))

library(hexbin)

#plot shots using ggplot, hex bins, NBA court background image
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  annotation_custom(court, -250, 250, -52, 418) +
  stat_binhex(bins = 25, colour="gray", alpha=0.7) +
  scale_fill_gradientn(colours= c("yellow", "orange", "red")) +
  guides(alpha=FALSE, size=FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  geom_rug(alpha=0.2) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), sep=""))+
  theme(line= element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size=17, lineheight=1.2, face="bold"))

#add player photo and footnote to plot
pushViewport(viewport(x = unit(0.9, "npc"), y=unit(0.8, "npc")))
    print(grid.draw(playerImg), newpage=FALSE)
    grid.text(label="thedatagame.com.au", just="centre", vjust=50)

#exclude backcourt shots
shotDataS <- shotDataf[which(!shotDataf$SHOT_ZONE_BASIC=="Backcourt"),]

#summarise shot data
library(plyr)
shotS <- ddply(shotDataS, .(SHOT_ZONE_BASIC), summarize,
              SHOTS_ATTEMPTED = length(SHOT_MADE_FLAG),
              SHOTS_MADE = sum(as.numeric(as.character(SHOT_MADE_FLAG))),
              MLOC_X = mean(LOC_X),
              MLOC_Y = mean(LOC_Y))

#calculate shot zone accuracy and zone accuracy labels
shotS$SHOT_ACCURACY <- (shotS$SHOTS_MADE / shotS$SHOTS_ATTEMPTED)
shotS$SHOT_ACCURACY_LAB <- paste(as.character(round(100*shotS$SHOT_ACCURACY, 1)), "%", sep="")

#plot shot accuracy per zone
ggplot(shotS, aes(x=MLOC_X, y=MLOC_Y)) +
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour=SHOT_ZONE_BASIC, size=SHOT_ACCURACY, alpha=0.8), size=8) +
  geom_text(aes(colour = SHOT_ZONE_BASIC, label=SHOT_ACCURACY_LAB), vjust = -1.2, size=8) +
  guides(alpha=FALSE, size=FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  ggtitle(paste("Shot Accuracy\n", unique(shotDataf$PLAYER_NAME), sep="")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        plot.title = element_text(size=17, lineheight=1.2, face="bold"))

#add player photo and footnoate
pushViewport(viewport(x=unit(0.9, "npc"), y=unit(0.8, "npc")))
  print(grid.draw(playerImg), newpage=FALSE)
  grid.text(label="thedatagame.com.au", just="centre", vjust=50)

