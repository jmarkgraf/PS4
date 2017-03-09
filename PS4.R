# Problem Set 4: Webscraping; Data Cleaning; Plotting
# Date: March 3, 2017
# Author: Jonas Markgraf
# ====================================================

# load packages
library(rvest) 
library(repmis)

# clean environment
rm(list = ls())

# set working directory
# set working directory
possibles <- c("~/Dropbox/Hertie School/(4) Applied Statistical Programming (WUSTL)/Repositories/PS4")
set_valid_wd(possibles)

# take the Wiki URL
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# scaping data
temp <- wikiURL %>% 
  read_html %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
  html_table()
election <- temp[[1]]

# clean data
## new column names 
names(election) <- c("elecID", "year", "winner_candidate", "winner_party",
                     "college_votes", "college_voteshare", "pop_voteshare",
                     "pop_marginperc", "pop_votes", "pop_marginabs", "loser_candidate",
                     "loser_party", "turnout")

## first 2 observations do not contain meaningful information
election <- election[-(1:2), ]  

## remove special character '[a]' (footnote) from data frame
election <- as.data.frame(sapply(election, gsub, pattern = "\\[a\\]", replacement = "")) 

# turn variables into correct format
## remove special characters and turn factors in numeric
ToNumeric <- function(var) {
  # function removes special characters from variables and turns them into numeric variables
  # remove special characters
  new_var <- gsub('[^0-9\\.]', '', var)
  # turn string into numeric
  new_var2 <- as.numeric(as.character(new_var))
  return(new_var2)
}

ToCharacter <- function(var) {
  # function turns variables into numeric
  # purpose: names and parties are treated as factors
  new_var <- as.character(var)
  return(new_var)
}

## apply function to adequate variables
election[ , c(1:2, 6:7, 9, 13)] <- apply(election[ , c(1:2, 6:7, 9, 13)], 2, ToNumeric)
election[ , c(3:4, 11:12)] <- apply(election[ , c(3:4, 11:12)], 2, ToCharacter)

# deal with special cases that contain not identified minus sign
## remove unwanted characters
election$pop_marginperc <- as.character(gsub("%", "", election$pop_marginperc))
election$pop_marginabs <- as.character(gsub(",", "", election$pop_marginabs))

## deal with minus sign
# election$pop_marginperc <- as.numeric(gsub("[^0-9]", "-", election$pop_marginperc))  # no solution how to deal with minus here
election$pop_marginabs <- as.numeric(gsub("[^0-9]", "-", election$pop_marginabs))  # this works

# VISUALIZATION -----------------------
# ==============

# PLOT 1: college vs. popular voteshare over time
pdf(file = "ElectionPlots.pdf")
par(mfrow(c(1,2)))
# college vs. popular voteshare over time
plot(election$year, election$college_voteshare, type = "p", ylim = c(25, 110),
     main = "College Election Vs. Popular Voteshare, 1824-2016",
     ylab = "Voteshare (%)", xlab = "Election Year")
points(election$year, election$pop_voteshare, pch = 3, col = "red")
legend("topleft", c("College", "Popular"), pch = c(1, 3), col = c("black", "red"), cex = 0.6)

# Comment: historically, in all elections was the electoral college voteshare of the elected president higher 
# than his popular voteshare


# PLOT 2: discrepancy between college and popular voteshare by party
with(election[election$winner_party == "Rep.", ], plot(year, college_voteshare - pop_voteshare,
                                                       main = "Discrepancy College and Popular Voteshare \nby Winning Party, 1824-2016",
                                                       ylab = "Difference (in %)", xlab = "Election Year", pch = 2, col = "red",
                                                       ylim = c(0, 60)))
with(election[election$winner_party == "Rep.", ], 
     abline(lm((college_voteshare - pop_voteshare) ~ year),
            col = "red"))
with(election[election$winner_party == "Dem.", ], points(year, college_voteshare - pop_voteshare,
                                                         pch = 0, col = "blue"))
with(election[election$winner_party == "Dem.", ], 
     abline(lm((college_voteshare - pop_voteshare) ~ year),
            col = "blue"))
legend("topleft", c("Republican", "Democrat"), pch = c(2, 0), col = c("red", "blue"), cex = 0.6)
dev.off()

## Comment: historically, no party benefitted more from discrepancy; 
## the last 5 elections had low discrepancies between college vote and popular vote

# Scraping electoral college votes by candidate ----------------------------------
# ============================================

# take the new URL
wiki2URL <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

# scaping data
temp2 <- wiki2URL %>%   # this does not work because columns have different lengths ("table has inconsistent number of columns")
  read_html %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%
  html_table()

# new package required
library(htmltab)

# scraping data
college <- as.data.frame(htmltab(wiki2URL, which = 3))

# extract new dataframe with electoral votes for winner and runner up


# clean data

## turn "Electoral Votes" into numeric and delete not wanted information after "/"
college$electoral <- gsub("/.*","", college[,"Electoral votes"])  # delete last part
college$electoral <- as.numeric(gsub("^\\s+|\\s+$", "", college$electoral))  # trim whitespace

## set up new data frame to export electoral votes for winner and runner up
college.export <- as.data.frame(unique(college$Year))
names(college.export)[1] <- "year"
college.export$year <- ToNumeric(college.export$year)

## identify electoral votes for winner 
college.export$college_winner <- tapply(college$electoral, INDEX = college$Year, FUN = max)

## identify electoral votes for runner up 
### write function to find second highest value
college.export$college_runnerup <- tapply(college$electoral, INDEX = college$Year, 
                                     FUN = function(i) sort(i, decreasing = TRUE)[2])

# merge data frames
full.elections <- merge(election, college.export, by = "year")

# now save data file
save(full.elections, file='US_Elections.Rdata')
