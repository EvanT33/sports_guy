# This program pulls FiveThirtyEight MLB game probabilities, along
# with Vegas odds, and then identifies all bets each day with 
# an expected return over [3%]. Go hawks.

# load packages
library(rvest)
library(gdata)
library(lubridate)

# FiveThirtyEight MLB game predictions
url <- 'https://projects.fivethirtyeight.com/2020-nba-predictions/games/'
webpage <- read_html(url)


#################################################
############### Dates, Home, Away ###############
#################################################

# Using CSS selectors to scrape the dates section
date_html <- html_nodes(webpage,'.team, .chance, .h3')

# Converting the dates data to text
df <- html_text(date_html)


# Convert to dataframe, reformat
df <- as.data.frame(df)
df$df <- as.character(df$df)
df$date <- ""
for (k in 1:nrow(df)){
   df$date[k] <- ifelse(grepl(",",df$df[k]),df$df[k],0)
}
for (k in 2:nrow(df)){
   df$date[k] <- ifelse(df$date[k]=="0",df$date[k-1],df$date[k])
}

df$today <- day(Sys.Date())
df <- df[which(df$date != "0"),]
df$period <- regexpr('\\.', df$date)
df$dayn <- substr(df$date, df$period+2, nchar(df$date))
df <- df[which(df$today == df$dayn),]
row.names(df) <- 1:nrow(df)
df$keep <- 0
for (k in 0:(nrow(df)/7-1)){
   df$keep[7*k + 4] <- 1
   df$keep[7*k + 5] <- 1
   df$keep[7*k + 7] <- 1
   df$keep[7*k + 8] <- 1

}
df <- df[which(df$keep==1),]
df$winp <- ""
for (k in 1:nrow(df)-1){
   df$winp[k] <- df$df[k+1]
}
row.names(df) <- 1:nrow(df)
df$keep2 <- 0
for (k in 1:nrow(df)){
   df$keep2[k] <- ifelse((k%%2)==0,0,1)
}
df <- df[which(df$keep2==1),]
df <- df[,c(1,7)]
names(df) <- c("team", "winp")
row.names(df) <- 1:nrow(df)
df$gamedate <- Sys.Date()
df$winp <- gsub("%", "", df$winp)
df$winp <- as.numeric(df$winp)/100

###########################################################
############### Scrape and merge vegas odds ###############
###########################################################

# nba moneylines
url <- 'http://www.vegasinsider.com/nba/odds/las-vegas/money/'
webpage <- read_html(url)


# Using CSS selectors to scrape the Vegas section
vegas_html <- html_nodes(webpage,'.nowrap , .tabletext')


# Converting the Vegas data to text
vegas <- html_text(vegas_html)


# Convert to dataframe, reformat
vegas <- as.data.frame(vegas)
vegas$keep <- 0
vegas <- vegas[which(vegas$vegas != "Picks"),]
vegas$vegas <- trimws(vegas$vegas)
vegas <- vegas[which(vegas$vegas != ""),]
for (k in 0:(nrow(vegas)/11-1)){
   vegas$keep[11*k + 1] <- 1
   vegas$keep[11*k + 2] <- 1
   vegas$keep[11*k + 5] <- 1
}
vegas <- vegas[which(vegas$keep == 1),]
vegas <- as.data.frame(vegas)
row.names(vegas) <- 1:nrow(vegas)
vegas <- vegas[1:(nrow(df)/2*3),]
vegas$odds <- ""
vegas <- data.frame(lapply(vegas, as.character), stringsAsFactors=FALSE)
for (k in 0:(nrow(vegas)/3-1)){
   vegas$odds[3*k + 1] <- vegas$vegas[3*k + 3]
   vegas$odds[3*k + 2] <- vegas$vegas[3*k + 3]
}
vegas <- vegas[-which(vegas$odds == ""),]
row.names(vegas) <- 1:nrow(vegas)
vegas$odds <- trimws(vegas$odds)
vegas$pos = regexpr('-', vegas$odds)
vegas$odds <- substr(vegas$odds,2,nchar(vegas$odds))
vegas$pos = regexpr('-', vegas$odds)
vegas$pos2 = regexpr('\\+', vegas$odds)
vegas$pos3 <- max(vegas$pos, vegas$pos2)
vegas$pos4 <- nchar(vegas$odds)
vegas$odds2 <- ""
for (k in 1:nrow(vegas)){
   vegas$odds2[k] <- ifelse((k %% 2) == 0,
                            substr(vegas$odds[k],vegas$pos3[k],vegas$pos4[k]),
                            substr(vegas$odds[k],1,vegas$pos3[k] - 1))
}
vegas <- vegas[,c(1,8)]
names(vegas) <- c("city", "odds")
vegas <- vegas[!duplicated(vegas$city),]




# team mappings
team <- c("Pelicans",	"Raptors",	"Lakers",	"Clippers",	"Bulls",
          "Hornets",	"Cavaliers",	"Magic",	"Pistons",	"Pacers",	
          "Celtics",	"76ers",	"Grizzlies",	"Heat",	"Timberwolves",	
          "Nets",	"Knicks",	"Spurs",	"Wizards",	"Mavericks",
          "Thunder",	"Jazz",	"Nuggets",	"Trail Blazers",	"Kings",
          "Suns",	"Hawks",	"Bucks",	"Rockets",	"Warriors")
abbr <- c("NOP",	"TOR",	"LAL",	"LAC",	"CHI",	"CLT",	"CLE",	
          "ORL",	"DET",	"IND",	"BOS",	"PHI",	"MEM",	"MIA",	
          "MIN",	"BKN",	"NYK",	"SAS",	"WAS",	"DAL",	"OKC",	
          "UTA",	"DEN",	"POR",	"SAC",	"PHX",	"ATL",	"MIL",	
          "HOU",	"GSW")

city <- c("New Orleans",	"Toronto",	"L.A. Lakers",	"L.A. Clippers",	
          "Chicago",	"Charlotte",	"Cleveland",	"Orlando",	"Detroit",	
          "Indiana",	"Boston",	"Philadelphia",	"Memphis",	"Miami",	
          "Minnesota",	"Brooklyn",	"New York",	"San Antonio",	"Washington",
          "Dallas",	"Oklahoma City",	"Utah",	"Denver",	"Portland",	
          "Sacramento",	"Phoenix",	"Atlanta",	"Milwaukee",	"Houston",
          "Golden State")

team_map <- cbind(team, city, abbr)


# merge team abbreviations onto FiveThiryEight data (and Vegas data)
data <- merge(df, team_map, by = "team")


# merge vegas odds onto FiveThirtyEight data
data_master <- merge(data, vegas, by = "city", all.x = TRUE)
data_master$odds2 <- ""
for (k in 1:nrow(data_master)){
   data_master$odds2[k] <- ifelse(nchar(data_master$odds[k])>3.5,data_master$odds[k],0)
   data_master$odds2[k] <- ifelse(is.na(data_master$odds2[k]),0,data_master$odds2[k])
   
}
data_master <- data_master[,-6]
names(data_master) <- c("city", "team","winp","gamedate","abbr","vegas_odds")
data_master$gamedate <- Sys.Date()
data_master$vegas_odds <- as.numeric(data_master$vegas_odds)





###########################################
############### Do the math ###############
###########################################


data_master$risk <- 1
data_master$win <- ifelse(data_master$vegas_odds > 0, data_master$vegas_odds/100, abs(100/data_master$vegas_odds))
data_master$implied <- ifelse(data_master$vegas_odds<0, data_master$vegas_odds/(data_master$vegas_odds - 100), 100/(data_master$vegas_odds+100))
data_master$ev <- data_master$winp*data_master$win - (1-data_master$winp)*data_master$risk
data_master$top <- ifelse(data_master$ev > 0.1, "*", "")
data_master$top <- ifelse(data_master$ev > 0.2, "**", data_master$top)
data_master <- data_master[order(-data_master$ev),] 
data_master <- data_master[which(data_master$vegas_odds!=0),]
data_master <- data_master[which(data_master$ev > 0.03),]
data_print <- data_master[, c(5, 6, 11)]

### Clean data for tweet output
date <- data.frame(Sys.Date(),"","NBA")
empty <- data.frame("","","")
names(date) <- c("abbr", "vegas_odds", "top")
names(empty) <- c("abbr", "vegas_odds", "top")
date$abbr <- as.character(date$abbr)
data_print$vegas_odds <- as.character(data_print$vegas_odds)
data_print <- rbind(date, empty, data_print)
data_print$vegas_odds <- as.character(data_print$vegas_odds)
str(data_print)
for (k in 3:nrow(data_print)){
      data_print$vegas_odds[k] <- ifelse(substr(data_print$vegas_odds[k],1,1) == "-",
                                         data_print$vegas_odds[k], 
                                         paste("+",data_print$vegas_odds[k],sep = ""))
}

data_print <- rbind(data_print, empty)
data_print <- rbind(data_print, empty)
data_print$abbr[nrow(data_print)] <- "#FreePicks #NBA #SportsGuy"

# remove unneeded datasets
keep(data_master, data_print, sure = TRUE)


# save table
write.table(data_print, "output_nba.txt", row.names = F, col.names = F, quote = F)
