# This program pulls FiveThirtyEight MLB game probabilities, along
# with Vegas odds, and then identifies all bets each day with 
# an expected return over [3%]. Go hawks.

# load packages
library(rvest)
library(gdata)

# FiveThirtyEight MLB game predictions
url <- 'https://projects.fivethirtyeight.com/2019-nfl-predictions/games/'
webpage <- read_html(url)


#################################################
############### Dates, Home, Away ###############
#################################################

# Using CSS selectors to scrape data
chance_html <- html_nodes(webpage,'.chance')
team_html <- html_nodes(webpage,'.team')

# Converting data to text
chance <- html_text(chance_html)
team <- html_text(team_html)

# Convert to dataframe, reformat
chance <- as.data.frame(chance)
team <- as.data.frame(team)

# formatting
#team <- team[c(130:nrow(team)),]#added 20191120
team <- team[c(98:nrow(team)),] #added 20191218, week 16 when Saturday games start
# team <- team[-c(1:33),]#delete row above, unhide this row if 538 
#doesnt include the check boxes for playoff scenarios (ie, early in the season)
team <- as.data.frame(team)
team <- team[-c(33:nrow(team)),]
team <- as.data.frame(team)
chance <- chance[which(chance$chance != "Win prob."),]
chance <- as.data.frame(chance)
chance <- chance[-c(33:nrow(chance)),]
chance <- as.data.frame(chance)
df <- cbind(team,chance)
bye_week_test <- df[!duplicated(df$team),]
keeprows <- nrow(bye_week_test)
df <- df[c(1:keeprows),]



# team mappings
team <- c("L.A. Rams", "Seattle", "Arizona", "Cincinnati", "Atlanta", "Houston",
          "Baltimore", "Pittsburgh", "Buffalo", "Tennessee", "Chicago", "Oakland",
          "Jacksonville", "Carolina", "Minnesota", "N.Y. Giants", "New England",
          "Washington", "N.Y. Jets", "Philadelphia", "Tampa Bay", "New Orleans",
          "Denver", "L.A. Chargers", "Green Bay", "Dallas", "Indianapolis",
          "Kansas City", "Cleveland", "San Francisco", "Miami", "Detroit")
abbr <- c("LA", "SEA", "ARI", "CIN", "ATL", "HOU", "BAL", "PIT", "BUF",
          "TEN", "CHI", "OAK", "JAC", "CAR", "MIN", "NYG", "NE", "WAS",
          "NYJ", "PHI", "TB", "NO", "DEN", "LAC", "GB", "DAL", "IND", "KC",
          "CLE", "SF", "MIA", "DET")


# merge abbr
teamabbr <- cbind(team, abbr)
df$team <- trimws(df$team)
df <- merge(df, teamabbr, by="team", all.y =  FALSE, sort = FALSE)

###########################################################
############### Scrape and merge vegas odds ###############
###########################################################

# nfl moneylines
url <- 'http://www.vegasinsider.com/nfl/odds/las-vegas/money/'
webpage <- read_html(url)


# Using CSS selectors to scrape the Vegas section
vegas_html <- html_nodes(webpage,'.nowrap , .tabletext')


# Converting the Vegas data to text
vegas <- html_text(vegas_html)


# Convert to dataframe, reformat
vegas <- as.data.frame(vegas)
#vegas <- vegas[-which(vegas$vegas == "Picks"),] #commented out 12/26/19
vegas <- as.data.frame(vegas)
vegas$keep <- 0
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
names(vegas) <- c("team", "odds")
vegas <- vegas[!duplicated(vegas$team),]

#merge vegas odds onto df
df <- merge(df, vegas, by="team", all.x = TRUE, all.y = FALSE, sort = FALSE)
names(df) <- c("team", "winp", "abbr", "vegas_odds")
df$winp <- as.character(df$winp)
df$winp <- gsub("%", "", df$winp)
df$winp <- as.numeric(df$winp)
df$winp <- df$winp/100
df$vegas_odds <- as.numeric(df$vegas_odds)










###########################################
############### Do the math ###############
###########################################


df$risk <- 1
df$win <- ifelse(df$vegas_odds > 0, df$vegas_odds/100, abs(100/df$vegas_odds))
df$implied <- ifelse(df$vegas_odds<0, df$vegas_odds/(df$vegas_odds - 100), 100/(df$vegas_odds+100))
df$ev <- df$winp*df$win - (1-df$winp)*df$risk
df$top <- ifelse(df$ev > 0.1, "*", "")
df$top <- ifelse(df$ev > 0.2, "**", df$top)
df <- df[order(-df$ev),] 
df <- df[which(df$ev > 0.03),]
data_print <- df[, c(3, 4, 9)]

### Clean data for tweet output
date <- data.frame(Sys.Date(),"","NFL")
empty <- data.frame("","","")
names(date) <- c("abbr", "vegas_odds", "top")
names(empty) <- c("abbr", "vegas_odds", "top")
date$abbr <- as.character(date$abbr)
data_print$vegas_odds <- as.character(data_print$vegas_odds)
data_print <- rbind(date, empty, data_print)
#### Maybe here??? ####
data_print$vegas_odds <- as.character(data_print$vegas_odds)
str(data_print)
for (k in 3:nrow(data_print)){
   data_print$vegas_odds[k] <- ifelse(substr(data_print$vegas_odds[k],1,1) == "-",
                                      data_print$vegas_odds[k], 
                                      paste("+",data_print$vegas_odds[k],sep = ""))
}

data_print <- rbind(data_print, empty)
data_print <- rbind(data_print, empty)
data_print$abbr[nrow(data_print)] <- "#FreePicks #NFL #SportsGuy"

# remove unneeded datasets
keep(df, data_print, sure = TRUE)


# save table
write.table(data_print, "output_nfl.txt", row.names = F, col.names = F, quote = F)


