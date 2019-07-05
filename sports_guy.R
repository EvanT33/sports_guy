# This program pulls FiveThirtyEight MLB game probabilities, along
# with Vegas odds, and then identifies all bets each day with 
# an expected return over [3%]. Go hawks.

# load packages
library(rvest)
library(gdata)


sg <- function(go){

# FiveThirtyEight MLB game predictions
url <- 'https://projects.fivethirtyeight.com/2019-mlb-predictions/games/'
webpage <- read_html(url)


#################################################
############### Dates, Home, Away ###############
#################################################

# Using CSS selectors to scrape the dates section
date_html <- html_nodes(webpage,'.long')

# Converting the dates data to text
date <- html_text(date_html)


# Convert to dataframe, reformat
date <- as.data.frame(date)
date <- date[-1, ]
date <- as.data.frame(date)

# Remove bad
bad <- which(grepl("Chance of winning", date$date))
date <- date[-bad, ]
date <- as.data.frame(date)
data <- date
data$type <- ""
for (k in 0:(nrow(data)/3 - 1)) {
      data$type[1+3*k] <- "date"
      data$type[2+3*k] <- "away"
      data$type[3+3*k] <- "home"
}
data$gamedate <- ""
data$date <- as.character(data$date)
data$gamedate <- as.character(data$gamedate)
for (k in 0:(nrow(data)/3 - 1)) {
      data$gamedate[1+3*k] <- data$date[1+3*k]
}

for (k in 2:nrow(data)) {
      if (data$gamedate[k] == ""){
            data$gamedate[k] <- data$gamedate[k-1]
      } else {
            data$gamedate[k] <- data$gamedate[k]
      }
}

data <- data[-which(data$type == "date"), ]


####################################################
############### FiveThirtyEight Odds ###############
####################################################



# Using CSS selectors to scrape the WP section
winp_html <- html_nodes(webpage,'.win-prob')

# Converting the WP data to text
winp <- html_text(winp_html)


# Convert to dataframe, reformat
winp <- as.data.frame(winp)
winp <- winp[-1, ]
winp <- as.data.frame(winp)

# Remove bad
bad <- which(grepl("Win prob.Chance of winning", winp$winp))
winp <- winp[-bad, ]
winp <- as.data.frame(winp)




# Combined game info and win probs
data <- cbind(data, winp)
names(data)[1] <- "team"


# create game_id and clean data into correct formats
data$g_id <- ""
for (k in 1:nrow(data)) {
      data$g_id[k] <- ceiling(k/2)
}
cols <- c("gamedate", "g_id", "team", "winp")
data <- data[ ,cols]
data$winp <- gsub("%", "", data$winp)
data$winp <- as.numeric(data$winp)/100
today <- Sys.Date()
data$gamedate <- sapply(strsplit(data$gamedate, split=', ', fixed=TRUE), function(x) (x[2]))
data$gamedate <- as.numeric(sapply(strsplit(data$gamedate, split=' ', fixed=TRUE), function(x) (x[2])))
today <- as.numeric(substr(today, 9, 10))
data <- data[today == data$gamedate, ]
data <- data[!duplicated(data$team),]



###########################################################
############### Scrape and merge vegas odds ###############
###########################################################

# Vegas Insider MLB game predictions
url <- 'https://www.actionnetwork.com/mlb/live-odds'
webpage <- read_html(url)


# Using CSS selectors to scrape the Vegas section
vegas_html <- html_nodes(webpage,'.h-h3')
vegas_odds_html <- html_nodes(webpage,'.mainOdds__row')

# Converting the Vegas data to text
vegas <- html_text(vegas_html)
vegas_odds <- html_text(vegas_odds_html)

# Convert to dataframe, reformat
vegas <- as.data.frame(vegas)
vegas_odds <- as.data.frame(vegas_odds)
vegas_odds$vegas_odds <- substr(vegas_odds$vegas_odds, 5, 8)
vegas$vegas <- as.character(vegas$vegas)
vegas$keep <- ""
vegas$keep <- ifelse(nchar(vegas$vegas)>1, 1, 0)
vegas <- vegas[which(vegas$keep == 1), ]
vegas <- cbind(vegas$vegas, vegas_odds)
names(vegas)[1] <- "abbr"
vegas <- vegas[!duplicated(vegas$abbr),]


# team mappings
team <- c("Marlins", 	"Nationals", 	"Marlins", 	"Nationals", 	"Cubs", 	
          "Pirates", 	"Red Sox", 	"Blue Jays", 	"Yankees", 	"Mets", 	
          "Orioles", 	"Rays", 	"Brewers", 	"Reds", 	"Phillies", 	
          "Braves", 	"Angels", 	"Rangers", 	"Astros", 	"Rockies", 	
          "Tigers", 	"White Sox", 	"Indians", 	"Royals", 	"Twins", 	
          "Athletics", 	"Giants", 	"Padres", 	"Diamondbacks", 	
          "Dodgers", 	"Cardinals", 	"Mariners", 	"Tigers", 	"White Sox")
abbr <- c("MIA", 	"WSH", 	"MIA", 	"WSH", 	"CHC", 	"PIT", 	"BOS", 	"TOR", 	
          "NYY", 	"NYM", 	"BAL", 	"TB", 	"MIL", 	"CIN", 	"PHI", 	"ATL", 	
          "LAA", 	"TEX", 	"HOU", 	"COL", 	"DET", 	"CWS", 	"CLE", 	"KC", 	
          "MIN", 	"OAK", 	"SF", 	"SD", 	"ARI", 	"LAD", 	"STL", 	"SEA", 	
          "DET", 	"CWS")
team_map <- cbind(team, abbr)


# merge team abbreviations onto FiveThiryEight data
data <- merge(data, team_map, by = "team")

# merge vegas odds onto FiveThirtyEight data
data_master <- merge(data, vegas, by = "abbr")
data_master$g_id <- as.numeric(data_master$g_id)
data_master <- data_master[order(data_master$g_id),] 
data_master <- data_master[!duplicated(data_master),]
data_master$gamedate <- Sys.Date()
data_master$vegas_odds <- as.numeric(data_master$vegas_odds)


# remove unneeded datasets
keep(data_master, sure = TRUE)





###########################################
############### Do the math ###############
###########################################


data_master$risk <- 1
data_master$win <- ifelse(data_master$vegas_odds > 0, data_master$vegas_odds/100, abs(100/data_master$vegas_odds))
data_master$implied <- ifelse(data_master$vegas_odds<0, data_master$vegas_odds/(data_master$vegas_odds - 100), 100/(data_master$vegas_odds+100))
data_master$ev <- data_master$winp*data_master$win - (1-data_master$winp)*data_master$risk
data_master$top <- ifelse(data_master$ev > 0.06, 1, 0)
data_master <- data_master[order(-data_master$ev),] 
data_master <- data_master[which(data_master$ev > 0.03),]

data_print <- data_master[, c(1, 6, 11)]


print(Sys.Date())
print(data_print)

}







