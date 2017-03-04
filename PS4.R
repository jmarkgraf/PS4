library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

# scaping data
temp <- wikiURL %>% 
  read_html %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
  html_table()
election <- temp[[1]]

# clean data
names(election)[1] <- "elecID"
names(election)[2] <- "year"
names(election)[3] <- "winner_candidate"
names(election)[4] <- "winner_party"
names(election)[5] <- "college_votes"
names(election)[6] <- "college_voteshare"
names(election)[7] <- "pop_voteshare"
names(election)[8] <- "pop_marginperc"
names(election)[9] <- "pop_votes"
names(election)[10] <- "pop_marginabs"
names(election)[11] <- "bestloser_candidate"
names(election)[12] <- "bestloser_party"
names(election)[13] <- "Turnout"

test <- election[-(1:2), ]  # delete first 2 observations
