#Set working directory
setwd("C:/Users/My Files/Desktop/Data Science Resources/Basketball Prediction Models/RAPM Model")

library(RSQLite)
library(plyr)
library(dplyr)
library(doBy)

con <- dbConnect(drv="SQLite", dbname="nba_rRegression_chi/nba.db")
alltables <- dbListTables(con)

gs <- dbGetQuery(con, "SELECT * FROM gameScore")
p <- dbGetQuery(con, "SELECT * FROM players")
rpm_table <- dbGetQuery(con, "SELECT * FROM RPM WHERE year_ = 2010 AND Tm = 'bkn'")

game_train <- dbGetQuery(con, "SELECT gameScore.match_id, gameScore.gameDate, gameScore.game_year,
                         gameScore.home_team, gameScore.home_team_score, gameScore.visit_team, gameScore.visit_team_score,
                         home, gameDetail.playerID, players.playerName, players.avg_MIN, players.avg_GP, rpm.RPM, rpm.DRPM, rpm.ORPM,
                         rpm.PER
                         FROM gameScore LEFT JOIN gameDetail ON gameScore.match_id = gameDetail.match_id
                         LEFT_JOIN players ON gameDetail.playerId = players.playerId
                         LEFT_JOIN rpm ON players.playerName=rpm.Player AND players.SEASON=rpm.year_ AND players.team = rpm.Tm
                         WHERE gameDetail.match_id = 311225018 AND players.SEASON = 2011")

#combine duplicated players
game_train <- summaryBy(RPM + DRPM + ORPM + PER + avg_MIN ~ match_id + gameDate +
                          game_year + home_team + home_team_score + visit_team +
                          visit_team_score + home + playerId + playerName, FUN=c(mean), data=game_train)

Assign Null values to 0 for RPM
game_train[c("PER.mean")][is.na(game_train[c("PER.mean")])] <- 15
game_train[c("RPM.mean", "DRPM.mean", "ORPM.mean")][is.na(game_train[c("RPM.mean", "DRPM.mean", "ORPM.mean")])] <- 0

#weighted average of offensive and defensive rpm for both teams
total_mins <- ddply(game_train, .(home), summarise, total_mins = sum(avg_MIN.mean))
game_train <- merge(game_train, total_mins, by.x="home", by.y="home")
game_train <- mutate(game_train, weighted_mins = avg_MIN.mean/total_mins)

#weighted summary stats for each team
weight_inputs <- summarise(group_by(game_train, home, match_id, gameDate,
                                    game_year, home_team, home_team_score, visit_team,
                                    visit_team_score),
                           RPM_weight = weighted.mean(RPM.mean, weighted_mins),
                           ORPM_weight = weighted.mean(ORPM.mean, weighted_mins),
                           DRPM_weight = weighted.mean(DRPM.mean, weighted_mins),
                           PER_weight = weighted.mean(PER.mean, weighted_mins))

#combine weighted game stats into first row
final_game <- reshape(weight_inputs, timevar="home", idvar=c("match_id", "gameDate",
                                                             "game_year", "home_team", "home_team_score", "visit_team",
                                                             "visit_team_score"), direction="wide")

full_game_summary <- dbGetQuery(con, "SELECT gameScore.match_id, gameScore.gameDate, gameScore.game_year,
                                gameScore.home_team, gameScore.home_team_score, gameScore.visit_team, gameScore.visit_team_score,
                                home, gameDetail.playerID, players.playerName FROM gameScore LEFT JOIN gameDetail
                                ON gameScore.match_id = gameDetail.match_id LEFT JOIN
                                players ON gameDetail.playerId = players.playerId
                                WHERE gameScore.game_year = players.SEASON
                                AND gameDetail.netPoints is not null")

match_ids <- unique(full_game_summary$match_id)
unique(filter(full_game_summary, match_id == 311225018)$game_year)

mid <- 311225018
year <- 2011

dbGetQuery(con, springf("SELECT gameScore.match_id, gameScore.gameDate, gameScore.game_year,
                        gameScore.home_team, gameScore.home_team_score, gameScore.visit_team,
                        gameScore.visit_team_score,
                        home, gameDetail.playerID, players.playerName, players.avg_MIN, players.avg_GP,
                        rpm.RPM, rpm.DRPM, rpm.ORPM,
                        rpm.PER
                        FROM gameScore LEFT JOIN gameDetail ON gameScore.match_id = gameDetail.match_id
                        LEFT JOIN players ON gameDetail.playerId = players.playerId
                        LEFT JOIN rpm ON players.playerName = rpm.Player AND players.SEASON=rpm.year_ AND
                        players.team = rpm.Tm
                        WHERE gameDetail.match_id = %s AND players.SEASON = %s", mid, year))


#creating empty data frame
df <- data.frame(matrix(0, nrow=length(match_ids), ncol=length(final_game)))

count = 0

for(m in match_ids){
  count = count + 1
  year = unique(filer(full_game_summary, match_id == m)$game_year) - 1
  print(year)
  print(m)
  print(count)
  
  #get info from correct match_id and year
  gt <- dbGetQuery(con, sprintf("SELECT gameScore.match_id, gameScore.gameDate, gameScore.game_year,
                                gameScore.home_team, gameScore.home_team_score, gameScore.visit_team, gameScore.visit_team_score,
                                home, gameDetail.playerID, players.playerName, players.avg_MIN, players.avg_GP, rpm.RPM, rpm.DRPM, rpm.ORPM,
                                rpm.PER
                                FROM gameScore LEFT JOIN gameDetail ON gameScore.match_id = gameDetail.match_id
                                LEFT JOIN players ON gameDetail.playerId = players.playerId
                                LEFT JOIN rpm ON players.playerName = rpm.Player AND players.SEASON = rpm.year_ AND players.team = rpm.Tm
                                WHERE gameDetail.match_id = %s AND players.SEASON = %s", m, year))
  
  game_train <- summaryBy(RPM + DRPM + ORPM + PER + avg_MIN ~ match_id + gameDate +
                            game_year + home_team + home_team_score + visit_team +
                            visit_team_score + home + playerId + playerName, FUN=c(mean), data=gt)
  
  game_train[c("RPM.mean", "DRPM.mean", "ORPM.mean")][is.na(game_train[c("RPM.mean", "DRPM.mean", "ORPM.mean")])] <- 0
  
  
  #Set missing pers to 15
  game_train[c("PER.mean")][is.na(game_train[c("PER.mean")])] <- 15
  
  
  #Construc weighted average of offensive and defensive rpms for both teams
  total_mins <- ddply(game_train, .(home), summarise, total_mins = sum(avg_MIN.mean))
  game_train <- merge(game_train, total_mins, by.x="home", by.y="home")
  game_train <- mutate(game_train, weighted_mins = avg_MIN.mean/total_mins)
  
  #weighted summary stats for each team
  weight_inputs <- summarise(group_by(game_train, home, match_id, gameDate,
                                      game_year, home_team, home_team_score, visit_team,
                                      visit_team_score),
                             RPM_weight = weighted.mean(RPM.mean, weighted_mins),
                             ORPM_weight = weighted.mean(ORPM.mean, weighted_mins),
                             DRPM_weight = weighted.mean(DRPM.mean, weighted_mins),
                             PER_weight = weighted.mean(PER.mean, weighted_mins))
  
  #reshape weighted game stats into first row
  final_game <- reshape(weight_inputs, timevar="home",
                        idvar = c("match_id", "gameDate",
                                  "game_year", "home_team", "home_team_score", "visit_team",
                                  "visit_team_score"), direction="wide")
  df[count,] = final_game
}

colnames(df) = names(final_game)
write.csv(df, "rpm_dataset.csv")