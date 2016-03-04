-library(dplyr)


Rebounding_2013 <- read.csv("2013 Rebounding.csv")
Rebounding_2014 <- read.csv("2014 Rebounding.csv")
Rebounding_2015 <- read.csv("2015 Rebounding.csv")

summary(Rebounding_2013)

#avg shot distance that each rebounder gets
Avg_shot_dist <- Rebounding_2013 %>% group_by(player_id, rebounder) %>% summarise(avg = mean(shot_dist))
Avg_shot_dist %>% filter(avg > 16)

Avg_rebound_dist <- Rebounding_2013 %>% group_by(player_id, rebounder) %>% summarise(avg=mean(reb_dist))
Avg_rebound_dist %>% filter(avg > 10)



#2fg versus 3fg
three_point_leaders <- Rebounding_2013 %>% group_by(player_id, rebounder) %>% filter(shot_type == "missed 3FG") %>% summarise(count= n())
three_point_leaders %>% filter(count > 70)

two_point_leaders <- Rebounding_2013 %>% group_by(player_id, rebounder) %>% filter(shot_type == "missed 2FG") %>% summarise(count=n()) %>% filter(count > 20) %>% mutate("rebounds" = count)
two_point_leaders %>% filter(count > 200)

#percentage of rebounds that are 2fg
perc_two_point <- cbind(two_point_leaders, three_point_leaders, by="player_id")

#collecting rebounds off different shooters
shooter_reb_dist <- Rebounding_2013 %>% group_by(shooter) %>% summarise(avg=mean(reb_dist))
summary(shooter_reb_dist)
shooter_reb_dist %>% filter(avg > 8)

#whether the shot was contested or not

#reb dist
