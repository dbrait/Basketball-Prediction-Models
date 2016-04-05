library(XML)

url <- paste0("http://www.basketball-reference.com/players/", letters,"/")

#set years from which data is available
years <- 1950:2014

#set urls for data to get
totals_url <- paste0("http://www.basketball-reference.com/leagues/NBA_", years, "_totals.html")
advanced_url <- paste0("http://www.basketball-reference.com/leagues/NBA_", years, "_advanced.html")

#set length of tables to scrape from
totals_len <- length(totals_url)
advanced_len <- length(advanced_url)

#Initialized both totals and advanced tables
totals_table <- readHTMLTable(totals_url[1])[[1]]
totals_table$year <- 1950

advanced_table <- readHTMLTable(advanced_url[1])[[1]]
advanced_table$year <- 1950


#append together all of totals data
for (i in 2:totals_len){
  #create a temp table and append on variable that contains year
  temp_table <- readHTMLTable(totals_url[i])[[1]]
  temp_table$year <- i + 1949
  totals_table <- rbind(totals_table, temp_table)
}

#append together all advanced data
for (i in 2:advanced_len){
  #create temp table, and append on variable that contains year
  temp_table <- readHTMLTable(advanced_url[i])[[1]]
  temp_table$year <- i + 1949
  advanced_table <- rbind(advanced_table, temp_table)
}

#combine output of two tables and get rid of default rows 
all_table <- as.data.frame(subset(cbind(totals_table, advanced_table), Player != "Player"))
all_table_revised <- all_table[,!grep1(".1", names(all_table))]

#write polished data frame to csv
write.csv(all_table_revised, file="bball_ref_data.csv")