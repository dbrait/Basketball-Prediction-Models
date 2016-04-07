devtools::install_github("abresler/nbastatR")

library(data.table)
library(dplyr)
library(formattable)
library(jsonlite)
library(lubridate)
library(magrittr)
library(purrr)
library(rbokeh)
library(stringr)
library(tidyr)

library(ggplot2)


library(nbastatR)
get_nba_days_scores("11/05/15")
get_nba_franchise_data(return_franchises = c("all"))

get_nba_players_ids(active_only = F)

get_nba_teams_seasons_roster(team = "Nets", include_coaches = F)

get_team_season_shot_data(team = "Nets", year_data=2015)


#sort through player iso stats
PlayerIso %>% filter(possesions > 100) %>% filter(pct.play_type > 15)

#Get team defensive stats for each table type
teamDefense <- get_nba_synergy_stats(include_offense=F, 
                                     include_defense=T, 
                                     type_table="team")


write.csv(teamDefense, "Team Defense.csv")

defCut <- get_nba_synergy_stats(table_name="Cut",
                                include_defense=T,
                                include_offense=F,
                                type_table="team")

defteamIsolation <- get_nba_synergy_stats(table_name="Isolation",
                                          include_offense=F,
                                          include_defense=T,
                                          type_table="team")

defteamOffScreen <- get_nba_synergy_stats(table_name="Off Screen",
                                          include_offense=F,
                                          include_defense=T,
                                          type_table="team")

defteamOReb <- get_nba_synergy_stats(table_name="Offensive Rebounds",
                                     include_offense=F,
                                     include_defense=T,
                                     type_table="team")

defTeamPostUp <- get_nba_synergy_stats(table_name="Post-Up",
                                       include_offense=F,
                                       include_defense=T,
                                       type_table="team")

defBallHandler <- get_nba_synergy_stats(table_name="Pick & Roll, Ball Handler",
                                        include_offense=F,
                                        include_defense=T,
                                        type_table="team")

defTeamHandoff <- get_nba_synergy_stats(table_name="Handoff",
                                        include_offense=F,
                                        include_defense=T,
                                        type_table="team")

defTeamMisc <- get_nba_synergy_stats(table_name="Miscellanous",
                                     include_offense=F,
                                     include_defense=T,
                                     type_table="team")

defTeamPickAndRollRollMan <- get_nba_synergy_stats(table_name="Pick & Roll, Roll Man",
                                                   include_offense=F,
                                                   include_defense=T,
                                                   type_table="team")

defTeamSpotUp <- get_nba_synergy_stats(table_name="Spot-Up",
                                       include_offense=F,
                                       include_defense=T,
                                       type_table="team")

defTeamTransition <- get_nba_synergy_stats(table_name="Transition",
                                           include_offense=F,
                                           include_defense=T,
                                           type_table="team")

#Individual Players Offensive Tendencies
playerIso <- get_nba_synergy_stats(table="Isolation",
                                   include_offense=T,
                                   include_defense=F,
                                   type_table="player")

playerPostUp <- get_nba_synergy_stats(table="Post-Up",
                                      include_offense=T,
                                      include_defense=F,
                                      type_table="player")

playerPickAndRollBallHandler <- get_nba_synergy_stats(table="Pick & Roll, Ball Handler",
                                                      include_offense=T,
                                                      include_defense=F,
                                                      type_table="player")

playerCut <- get_nba_synergy_stats(table="Cut",
                                   include_offense=T,
                                   include_defense=F,
                                   type_table="player")

playerHandoff <- get_nba_synergy_stats(table="Handoff",
                                       include_offense=T,
                                       include_defense=F,
                                       type_table="player")

playerMisc <- get_nba_synergy_stats(table="Miscellanous",
                                    include_offense=T,
                                    include_defense=F,
                                    type_table="player")

playerOffScreen <- get_nba_synergy_stats(table="Off Screen",
                                         include_offense=T,
                                         include_defense=F,
                                         type_table="player")

playerOReb <- get_nba_synergy_stats(table="Offensive Rebounds",
                                    include_offense=T,
                                    include_defense=F,
                                    type_table="player")

playerRollMan <- get_nba_synergy_stats(table="Pick & Roll, Roll Man",
                                                  include_offense=T,
                                                  include_defense=F,
                                                  type_table="player")

playerSpotUp <- get_nba_synergy_stats(table_name="Spot-Up",
                                      include_offense=T,
                                      include_defense=F,
                                      type_table="player")

playerTransition <- get_nba_synergy_stats(table="Transition",
                                          include_offense=T,
                                          include_defense=F,
                                          type_table="player")


#Tendencies
percPlays <- teamDefense %>% select(team, pct.play_type.cut, pct.play_type.handoff, pct.play_type.isolation, pct.play_type.misc, pct.play_type.offrebound,
                                    pct.play_type.offscreen, pct.play_type.postup, pct.play_type.prballhandler, pct.play_type.prrollman, pct.play_type.spotup,
                                    pct.play_type.transition)

write.csv(percPlays, "Percentage of Plays.csv")

#giving up ORebs
OffReb <- teamDefense %>% select(team, pct.play_type.offrebound, pts.offrebound, ppp.offrebound, possesions.per_game.offrebound, possesions.offrebound,
                                 pct.to.offrebound, pct.scored.offrebound, pct.efg.offrebound, fgm.per_game.offrebound, fga.per_game.offrebound)

#defense against transition
Transition <- teamDefense %>% select(team, pct.play_type.transition, pts.transition, ppp.transition, possesions.per_game.transition, possesions.transition,
                                     pct.to.transition, pct.scored.transition, pct.efg.transition, fgm.per_game.transition, fga.per_game.transition)


#team offense in transition
offTransition <- get_nba_synergy_stats(table_name="Transition",
                                       include_defense=F,
                                       include_offense=T,
                                       type_table="team")

#teams in the pick and roll offense
teamRollMan <- get_nba_synergy_stats(table_name="Pick & Roll, Ball Handler",
                                     include_offense=T,
                                     include_defense=F,
                                     type_table="team")

teamBallHandler <- get_nba_synergy_stats(table_name="Pick & Roll, Roll Man",
                                         include_offense=T,
                                         include_defense=F,
                                         type_table="team")

#teams isolation use
offIsolation <- get_nba_synergy_stats(table="Isolation",
                                      include_defense=F,
                                      include_offense=T,
                                      type_table="team")

#teams use of cuts
offCut <- get_nba_synergy_stats(table="Cut",
                                include_offense=T,
                                include_defense=F,
                                type_table="team")

#team off screen
offScreen <- get_nba_synergy_stats(table="Off Screen",
                                   include_offense=T,
                                   include_defense=F,
                                   type_table="team")

#teams OReb

offReb <- get_nba_synergy_stats(table="Offensive Rebounds",
                                include_offense=T,
                                include_defense=F,
                                type_table="team")


#SOMs
library(dplyr)
Efg <- teamDefense %>% select(pct.efg.cut, pct.efg.handoff, pct.efg.isolation, pct.efg.misc, pct.efg.offrebound,
                              pct.efg.offscreen, pct.efg.postup, pct.efg.prballhandler, pct.efg.prrollman, pct.efg.spotup,
                              pct.efg.transition)

library(kohonen)


som_grid <- somgrid(xdim = 200, ydim=200, topo="hexagonal")
som_model <- som(Efg,
                 grid=som_grid,
                 rlen=100,
                 alpha=c(0.05, 0.01),
                 keep.data=TRUE,
                 n.hood="circular")

#kmeans clustering
library(stats)

KMe <- kmeans(Efg, centers=10, iter.max=10, nstart=1)

Efg$hclust_assignments <- Efg %>% hclust(method="single")

ggplot(Efg, aes(x, y, color= hclust_assignments)) + geom_points() + labs(color="hclust assignments")