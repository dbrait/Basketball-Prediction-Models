library(XML)

url <- "http://stats-for-the-nba.appspot.com/ratings/2014.html"
rpm_2013 <- readHTMLTable(url[1])[[1]]

write.csv(rpm_2013, "data_2013.csv")