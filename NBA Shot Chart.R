packages <-
  c("dplyr", "magrittr", "jsonlite", "ggExtra", "viridis",
    "tidyr", "stringr", "png", "grid", "ggplot2", "stringr")
options(warn = -1)
lapply(packages, library, character.only = TRUE)

plot_player_shot_chart <- function(player,
                                   year_season_end=2015) {
  year_season_start <-
    year_season_end - 1
  
  id.season <-
    year_season_start %>%
    paste(year_season_end %>% substr(start=3, stop=4),
          sep = "-")
  
  players.url <-
    "http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=0&LeagueID=00&Season=2015-16"
  
  players.data <-
    players.url %>%
    fromJSON(simplifyDataFrame=TRUE)
  
  players <-
    players.data$resultSets$rowSet %>%
    data.frame %>%
    tbl_df
  
  names(players) <-
    players.data$resultSets$headers %>%
    unlist %>%
    tolower()
  
  players %>%
    seperate(
      display_last_comma_first,
      sep = "\\,",
      into = c("name.last", "name.first")
    ) %>%
    rename(id.player = person_id) %>%
    mutate(
      name.first = name.first %>% gsub("[^A-Z a-z]", "", .),
      name.player = ifelse(name.first %>% is.na, name.last, paste(name.first %>% str_trim, name.last %>% str_trim)),
      id.player = id.player %>% as.numeric,
      is.active_player = rosterstatus %>% str_detect("0")
      ) %>%
    select(name.player, everything())
  
  id.player <-
    players %>%
    dplyr::filter(name.player == player) %>%
    .$id.player
  
  base_url <-
    "http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS="
  
  stem.2 <-
    "&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID="
  
  stem.3 <-
    "&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=" %>%
    paste0(id.season, "&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=AdvancedshowDetails=0&showShots=1&showZones=0")
  
  shot_data_url <-
    base_url %>%
    paste0(id.season, stem.2, id.player, stem.3)
  
  data <-
    shot_data_url %>%
    fromJSON(simplifyDataFrame=TRUE)
  
  data.shots <-
    data$resultSets$rowSet %>%
    .[1] %>%
    data.frame %>%
    tbl_df
  
  names(data.shots) <-
    data$resultSets$headers %>%
    .[1] %>%
    unlist %>%
    str_to_lower()
  
  data.shots %<>%
    mutate_each(funs(as.numeric), matches("loc")) %>%
    mutate_each(funs(as.numeric), matches("remaining")) %>%
    mutate_each(funs(as.numeric), matches("id")) %>%
    mutate_each(funs(as.numeric), matches("distance")) %>%
    mutate(
      period = period %>% as.numeric,
      shot_attempted_flag = "1" %>% grepl(shot_attempted_flag),
      shot_made_flag = "1" %>% grepl(shot_made_flag)
    )
  
  url.player.photo <-
    "http://stats.nba.com/media/players/230x185/" %>%
    paste0(id.player, ".png")
  
  con <-
    url.player.photo %>%
    url(open = "rb")
  
  rawpng <-
    con %>%
    readBin(what = "raw", n = 50000)
  
  close(con)
  
  png1 <-
    rawpng %>%
    readPNG
  
  g <-
    png1 %>%
    rasterGrob(interpolate = TRUE,
               width = .75,
               height = .75)
  
  title <- 
    player %>%
    paste0(" Shot Chart\n", id.season, " Season")
  
  p <-
    data.shots %>%
    ggplot(aes(loc_x, loc_y)) +
    stat_density2d(
      geom = "tile",
      aes(fill = ..density.. ^ 0.25),
      alpha = .45,
      contour = FALSE
    ) +
    scale_fill_viridis(guide_legend(label=TRUE, title="Shot Density\n % FG Attempts")) +
    geom_point(size=.45,
               aes(colour=shot_made_flag, scatter= "jitter"),
               alpha=.55) +
    theme_bw() +
    scale_colour_manual(values = c("red", "black"), guide_legend(label=TRUE, title="Shot Made")) +
    
    theme(
      panel.background = element_rect("black"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      rect = element_blank(),
      legend.postion = "bottom",
      axis.text = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(size=10,
                                face="bold"),
      plot.margin = unit(c(.15, .75, .15, .75), "cm")
      ) +
    labs(y = NULL, x = NULL) + 
    annotation_custom(
      g,
      xmin = data.shots$loc_x %>% max * .7,
      xmax = data.shots$loc_x %>% max,
      ymin = data.shots$loc_y %>% max * .7,
      ymax = data.shots$loc_y %>% max * 1
    ) + ggplot2::annotate(
      "text",
      x = -170,
      y = data.shots$loc_y %>% max * .95,
      ) +
    ggplot2::annotate("text",
                      x = 0,
                      y = data.shots$loc_y %>% max * .95,
                      label= title)
  
  p <- 
    ggMarginal(p,
               type= c("density"),
               colour = "red",
               size= 10)
  p
}