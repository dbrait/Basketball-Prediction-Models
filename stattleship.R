devtools::install_github("stattleship/stattleship-r")


library(curl)
library(stattleshipR)
set_token("d82699dc35fb5088a7be0cb5f9ee42c5")

league = "nba"
sport = "basketball"
ep = "players"
q_body = list()
players = ss_get_result(sport=sport, league=league, ep=ep, query=q_body, version=1, walk=TRUE)
players_df = do.call("rbind", lapply(players, function(x) x$players))

#get triple doubles this season
league = "nba"
sport = "basketball"
ep = "stats"
q_body = list(stat="triple_double", type="basketball_doubles_stat")
tripdubs = ss_get_result(sport=sport, league=league, ep=ep, query=q_body, version=1, walk=FALSE)
