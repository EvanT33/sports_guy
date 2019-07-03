

# load packages
library(rvest)

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
  
### library(tidyr), spread()???


# reformat data
# limit to today's date
# pull in odds
# merge odds (probably need a key to standardize  team names)
# do the math
# limit to the good bets
# identify the bok
# automate sports_guy









