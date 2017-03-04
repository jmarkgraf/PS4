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
names(election)[11] <- "loser_candidate"
names(election)[12] <- "loser_party"
names(election)[13] <- "turnout"

election <- election[-(1:2), ]  # delete first 2 observations

election <- as.data.frame(sapply(election, gsub, pattern = "\\[a\\]", replacement = ""))  # remove footnote

# turn variables into correct format
election$year <- as.numeric(as.character(election$year))
election$elecID <- as.numeric(as.character(election$elecID))
election$winner_candidate <- as.character(election$winner_candidate)
election$winner_party <- as.character(election$winner_party)

election$college_voteshare <- as.numeric(as.character(gsub("%", "", election$college_voteshare)))
election$pop_voteshare <- as.numeric(as.character(gsub("%", "", election$pop_voteshare)))
election$pop_marginperc <- as.character(gsub("%", "", election$pop_marginperc))
election$pop_marginperc <- as.numeric(gsub("[^0-9]", "-", election$pop_marginperc))# solve minus sign!!
election$pop_votes <- as.numeric(as.character(gsub(",", "", election$pop_votes)))
election$pop_marginabs <- as.character(gsub(",", "", election$pop_marginabs))
election$pop_marginabs <- as.numeric(gsub("[^0-9]", "-", election$pop_marginabs))

election$loser_candidate <- as.character(election$loser_candidate)
election$loser_party <- as.character(election$loser_party)

election$turnout <- as.numeric(as.character(gsub("%", "", election$turnout)))

# VISUALIZATION -----------------------
# ==============

# college vs. popular voteshare over time
plot(election$year, election$college_voteshare, type = "p", ylim = c(25, 110),
     main = "College Election Vs. Popular Voteshare, 1824-2016",
     ylab = "Voteshare (%)", xlab = "Election Year")
points(election$year, election$pop_voteshare, pch = 3, col = "red")
legend("topleft", c("College", "Popular"), pch = c(1, 3), col = c("black", "red"), cex = 0.6)

# Comment: in all elections was the electoral college voteshare of the elected president higher than the popular voteshare

# discrepancy between college and popular voteshare by party
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

# Comment: historically, no party benefitted more from discrepancy; the last 5 elections had low discrepancies.

