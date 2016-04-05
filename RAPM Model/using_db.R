library(RSQLite)
library(plyr)
library(dplyr)

con <- dbConnect(drv="SQLite", dbname="nba.db")

alltables <- dbListTables(con)
check <- dbGetQuery(con, "SELECT * FROM gameYearPlayer")

data <- read.csv("regTable.csv")