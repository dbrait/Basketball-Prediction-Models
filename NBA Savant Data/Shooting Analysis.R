library(dplyr)
library(ggplot2)
#Nba shooting analysis with nba savant data

setwd("C:/Users/My Files/Desktop/Data Science Resources/Basketball Prediction Models/NBA Savant Data")
Shooting_2010 <- read.csv("2010 Shooting.csv")
summary(Shooting_2010)

Shooting_2011 <- read.csv("2011 Shooting.csv")
summary(Shooting_2011)

Shooting_2012 <- read.csv("2012 Shooting.csv")
summary(Shooting_2012)

Shooting_2013 <- read.csv("2013 Shooting.csv")
summary(Shooting_2013)

Shooting_2014 <- read.csv("2014 Shooting.csv")
summary(Shooting_2014)

Shooting_2015 <- read.csv("2015 Shooting.csv")
summary(Shooting_2015)

tally(shot_made_flag ~ shot_type, data= Shooting_2015, format="proportion")
tally(shot_made_flag ~ shot_type, data= Shooting_2014, format="proportion")
tally(shot_made_flag ~ shot_type, data = Shooting_2013, format="proportion")
tally(shot_made_flag ~ shot_type, data = Shooting_2012, format="proportion")
tally(shot_made_flag ~ shot_type, data = Shooting_2011, format="proportion")

tally(shot_made_flag ~ action_type, data = Shooting_2015, format="proportion")
tally(shot_made_flag ~ action_type, data = Shooting_2014, format="proportion")



#investigate team defense specific trends
Opp <- Shooting_2015 %>% group_by(opponent) %>% filter(shot_type == "2PT Field Goal") %>% summarise(count = n(),
                                                                                                    shot_dist = mean(shot_distance),
                                                                                                    def_dist = mean(defender_distance))

#3pt defense
Opp_Three <- Shooting_2015 %>% group_by(opponent) %>% filter(shot_type == "3PT Field Goal") %>% summarise(count = n(),
                                                                                                          shot_dist = mean(shot_distance),
                                                                                                          def_dist = mean(defender_distance))


#Catch and shoot
Catch_And_Shoot <- Shooting_2015 %>% group_by(opponent) %>% filter(touch_time < 1.1) %>% summarise(count = n())

#early shot clock
Early_Shot <- Shooting_2015 %>% group_by(opponent) %>% filter(shot_clock < 10) %>% summarise(count = n())
