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

plot_nba_team_season_bokeh_shotchart(team = "Nets", year_roster = 2016,
                                     year_data = 2015, plot_hex = F,
                                     use_shot_zone_side = T,
                                     season_type = c("Pre Season"))
plot_nba_player_bokeh_shotchart(player = "Brook Lopez", plot_hex = F, vs_conference = "East",
                                year_season_end = 2015)


get_batch_player_gamelogs(year.season_start=2015, season_type="Regular")


get_nba_synergy_stats (table_name = "Transition", include_defense = T, include_offense = T, type_table = "team", return_message = T)


get_player_season_shot_data(player="Kyle Lowry")




get_year_draft_combine(combine_year = 2009, return_message=T)

get_all_draft_data(draft_years = 1960:2015)

PlayerIso <- get_nba_synergy_stats(table_name="Isolation",
                      include_defense=T,
                      include_offense=T,
                      type_table="player")

#sort through player iso stats
PlayerIso %>% filter(possesions > 100) %>% filter(pct.play_type > 15)



TeamTransition <- get_nba_synergy_stats(table_name="Transition",
                      include_defense=T,
                      include_offense=T,
                      type_table="team",
                      return_message=T)

SpotUp <- get_nba_synergy_stats(table_name="Spot-Up",
                                include_defense=T,
                                include_offense=T,
                                type_table="player")

PickandRoll <- get_nba_synergy_stats(table_name="Pick & Roll, Ball Handler",
                                     include_defense=T,
                                     include_offense=T,
                                     type_table="player")

OffScreen <- get_nba_synergy_stats(table_name="Pick & Roll, Roll Man",
                                   include_defense=T,
                                   include_offense=T,
                                   type_table="player")

OffensiveRebounds <- get_nba_synergy_stats(table_name="Offensive Rebounds",
                                           include_defense=T,
                                           include_offense=T,
                                           type_table="player")

HandOff <- get_nba_synergy_stats(table_name="Handoff",
                                 include_defense=T,
                                 include_offense=T,
                                 type_table="player")

OffensiveRebounds %>% filter(pct.play_type > 10)

#Breaking down team defense, create clustering and visualizations for each
teamDefense <- get_all_team_synergy_stats(include_defense=T,
                                   include_offense=F)

write.csv(teamDefense, "Team Defense.csv")

#PLay tendencies
percRollman <- teamDefense %>% select(team, pct.play_type.prrollman)
ptsRollman <- teamDefense %>% select(team, pts.per_game.prrollman)

ptsBallhandler <- teamDefense %>% select(team, pts.per_game.prballhandler)
percBallhandler <- teamDefense %>% select(team, pct.play_type.prballhandler)

percIso <- teamDefense %>% select(team, pct.play_type.isolation)


#Tendencies
percPlays <- teamDefense %>% select(team, pct.play_type.cut, pct.play_type.handoff, pct.play_type.isolation, pct.play_type.misc, pct.play_type.offrebound,
                                    pct.play_type.offscreen, pct.play_type.postup, pct.play_type.prballhandler, pct.play_type.prrollman, pct.play_type.spotup,
                                    pct.play_type.transition)

write.csv(percPlays, "Percentage of Plays.csv")

percPlays %>% filter(pct.play_type.prballhandler > 18.5)
percPlays %>% filter(pct.play_type.transition > 14)

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

playerTransition <- get_nba_synergy_stats(table_name="Transition",
                                          include_defense=F,
                                          include_offense=T,
                                          type_table="player")


#team offense spot up
offSpotup <- get_nba_synergy_stats(table_name="Spot-Up",
                                   include_defense=F,
                                   include_offense=T,
                                   type_table="team")

playerSpotup <- get_nba_synergy_stats(table_name="Spot-Up",
                                      include_defense=F,
                                      include_offense=T,
                                      type_table="player")

#team offense post up
offPostUp <- get_nba_synergy_stats(table="Post-Up",
                                   include_defense=F,
                                   include_offense=T,
                                   type_table="team")

playerPostUp <- get_nba_synergy_stats(table="Post-Up",
                                      include_defense=F,
                                      include_offense=T,
                                      type_table="player")

#team offense pick and rolls
offBallHandler <- get_nba_synergy_stats(table="Pick & Roll, Ball Handler",
                                        include_defense=F,
                                        include_offense=T,
                                        type_table="team")

playerBallHandler <- get_nba_synergy_stats(table="Pick & Roll, Ball Handler",
                                           include_defense=F,
                                           include_offense=T,
                                           type_table="player")

offRollMan <- get_nba_synergy_stats(table="Pick & Roll, Roll Man",
                                    include_defense=F,
                                    include_offense=T,
                                    type_table="team")

playerRollMan <- get_nba_synergy_stats(table="Pick & Roll, Roll Man",
                                       include_defense=F,
                                       include_offense=T,
                                       type_table="player")

#isolation
offIsolation <- get_nba_synergy_stats(table="Isolation",
                                      include_defense=F,
                                      include_offense=T,
                                      type_table="team")

playerIsolation <- get_nba_synergy_stats(table="Isolation",
                                         include_defense=F,
                                         include_offense=T,
                                         type_table="player")

defIsolation <- get_nba_synergy_stats(table="Isolation",
                                      include_defense=T,
                                      include_offense=F,
                                      type_table="player")

defteamIsolation <- get_nba_synergy_stats(table="Isolation",
                                          include_offense=F,
                                          include_defense=T,
                                          type_table="team")

#cuts
offCut <- get_nba_synergy_stats(table="Cut",
                                include_offense=T,
                                include_defense=F,
                                type_table="team")

summary(offCut)
heavyTeamCut <- offCut %>% arrange(fga.per_game)

playerCut <- get_nba_synergy_stats(table="Cut",
                                   include_offense=T,
                                   include_defense=F,
                                   type_table="player")
summary(playerCut)
heavyPlayerCut <- playerCut %>% arrange(desc(fga.per_game))


defCut <- get_nba_synergy_stats(table="Cut",
                                include_offense=F,
                                include_defense=T,
                                type_table="team")

summary(defCut)
heavyDefCut <- defCut %>% arrange(desc(fga.per_game))

#off screen
offScreen <- get_nba_synergy_stats(table="Off Screen",
                                   include_offense=T,
                                   include_defense=F,
                                   type_table="team")

summary(offScreen)

playerOffScreen <- get_nba_synergy_stats(table="Off Screen",
                                         include_offense=T,
                                         include_defense=F,
                                         type_table="player")

summary(playerOffScreen)
heavyOffScreen <- playerOffScreen %>% filter(fga > 50) %>% arrange(pct.efg)

defteamOffScreen <- get_nba_synergy_stats(table="Off Screen",
                                          include_offense=F,
                                          include_defense=T,
                                          type_table="team")

summary(defteamOffScreen)
defHeavyOffScreen <- defteamOffScreen %>% arrange(desc(fga))
defPercOffScreen <- defteamOffScreen %>% arrange(desc(pct.efg))

#OReb

offReb <- get_nba_synergy_stats(table="Offensive Rebounds",
                                include_offense=T,
                                include_defense=F,
                                type_table="team")

summary(offReb)
heavyTeamOffReb <- offReb %>% arrange(desc(fga.per_game))

playerOffReb <- get_nba_synergy_stats(table="Offensive Rebounds",
                                      include_offense=T,
                                      include_defense=F,
                                      type_table="player")

summary(playerOffReb)
heavyPlayerOffReb <- playerOffReb %>% arrange(desc(fga.per_game))

teamdefOReb <- get_nba_synergy_stats(table="Offensive Rebounds",
                                     include_offense=F,
                                     include_defense=T,
                                     type_table="team")
summary(teamdefOReb)
heavyDefOffReb <- teamdefOReb %>% arrange(desc(fga.per_game))

#work through team defense


#Individual Player breakdowns

TeamOffense <- get_nba_synergy_stats(table_name="Transition", "Offensive Rebounds", "Off Screen",
                                     include_defense=F,
                                     include_offense=T,
                                     type_table="team")
                                     



indTransitionOff <- get_nba_synergy_stats(table_name="Transition",
                                          include_offense=T,
                                          include_defense=F)


get_fanduel_bref_players()
get_headers_css_data()
get_bref_player_season_stat_table()
  


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