library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(stringr, warn.conflicts=FALSE, quietly=TRUE)
library(xml2, warn.conflicts=FALSE, quietly=TRUE)
library(rvest, warn.conflicts=FALSE, quietly=TRUE)
library(RColorBrewer, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
# survival analysis packages
library(survival, warn.conflicts=FALSE, quietly=TRUE)
library(survminer, warn.conflicts=FALSE, quietly=TRUE)
library(rms, warn.conflicts=FALSE, quietly=TRUE)
library(flexsurv, warn.conflicts=FALSE, quietly=TRUE)
library(e1071, warn.conflicts=FALSE, quietly=TRUE) # random stats functions
# cluster analyis packages
library(cluster, warn.conflicts=FALSE, quietly=TRUE)
library(factoextra, warn.conflicts=FALSE, quietly=TRUE)
library(purrr, warn.conflicts=FALSE, quietly=TRUE)

# --------------------------------------------
# Functions
# --------------------------------------------
# purpose: scrapes data from web page given html and css selector
# inputs: html (web page link), selector (css selector), name, team
# output: data frame of scraped table
scrape_player_data <-function(html, selector, name, team) {
  html_cast <- html_nodes(html, selector)
  cast <- html_table(html_cast[4], fill = TRUE)
  temp <- as.data.frame(cast)
  temp$name <- name
  temp$team <- team

    # remove unnecessary column names and data
  temp <- temp[-c(10, 14)]
  
  return (temp)
}

# purpose: cleans scraped data in order to perform quick joins
# inputs: temp, df of player data
# output: temp, df of cleansed player data
clean_player_data <-function(temp) {
  # rename column variables
  names(temp) <- c("runs", "minutes", "balls.faced", "fours", "sixes", "strike.rate", "position",
                      "dismissal", "inninng", "opposition", "ground", "date", "player", "team")

  # remove asterisk for 'not out'
  temp$runs <- gsub ("\\*","",temp$runs)
  
  # remove 'v ' in front of countries
  temp$opposition <- gsub ("\\v ","",temp$opposition)
  
  # create separate column for year (useful for cohort analysis)
  temp$year <- str_sub(temp$date, start = -4)
    
  # fix variable types and assign factors
  temp$runs <- as.numeric(temp$runs)
  temp$minutes <- as.numeric(temp$minutes)
  temp$balls.faced <- as.numeric(temp$balls.faced)
  temp$fours <- as.numeric(temp$fours)
  temp$sixes <- as.numeric(temp$sixes)
  temp$strike.rate <- as.numeric(temp$strike.rate)
  temp$position <- as.numeric(temp$position)
  temp$dismissal <- as.factor(temp$dismissal)
  temp$inninng <- as.numeric(temp$inninng)
  temp$opposition <- as.character(temp$opposition)
  temp$ground <- as.character(temp$ground)
  temp$date <- as.character(temp$date)
  temp$player <- as.character(temp$player)
  temp$team <- as.factor(temp$team)
  temp$year <- as.numeric(temp$year)
  sum(is.na(temp))
  temp <- na.omit(temp)
  sum(is.na(temp))
  
  return (temp)
  }


# purpose: traverse through assembled links to prepare players dataset
# inputs: cricinfo - df of player cricinfo ids, selector - of html dom node
# output: temp, df of all player data
load_player_data <-function(cricinfo, selector) {
  players <- data.frame()
  u1 = "http://stats.espncricinfo.com/ci/engine/player/"
  u3 = ".html?class=1;filter=advanced;orderby=balls_faced;template=results;type=batting;view=innings"

  for (i in seq_len(nrow(cricinfo))) {
    # concatenate html link
    u2 = as.character(cricinfo[i,4])
    link = paste (u1, u2, u3, sep = "")
    print(link)
    
    # scrape and clean player data from espn cricinfo
    html <- read_html(link)
    temp <- scrape_player_data(html, selector,as.character(cricinfo[i,1]),as.character(cricinfo[i,6]))
    temp <- clean_player_data(temp)
    players <- bind_rows(players, temp)
  }
  
  return (players)
}


# purpose: calculate restricted means of survival function of all players
# inputs: cricinfo - df of player cricinfo ids, players - df of all data
# output: rmeans, df of restricted means and median for each player
get_restricted_mean <-function(cricinfo, players) {
  rmeans <- data.frame()
  
  for (i in seq_len(nrow(cricinfo))) {
    name <- as.character(cricinfo[i,1])
    temp <- players %>% filter(player == name)
    
    # calculate KM survival fit
    sc.temp <- Surv(temp$balls.faced, temp$event)
    scfit.temp <- survfit(sc.temp ~ 1, type = "kaplan-meier")
    
    # remove rest. mean from table
    rmstat <- as.data.frame(summary(scfit.temp)$table)
    rmean <- rmstat[5,1]
    rmedian <- rmstat[7,1]
    rmeans <- bind_rows(rmeans, data.frame(name, rmean, rmedian))
  }
  
  return (rmeans)
  
}

# helper logical exclusion function
`%notin%` <- function(x,y) !(x %in% y) 

  
# --------------------------------------------
# Scraping
# --------------------------------------------
setwd("D:/Github/Data_Test_Cricket")

# load supplemental data for grounds and cricinfo html links
grounds <- tbl_df(read.csv("data/supp_grounds.csv", stringsAsFactors=FALSE))
cricinfo <- tbl_df(read.csv("data/supp_cricinfo.csv", stringsAsFactors=FALSE))

# players <- tbl_df(read.csv("data/players_backup.csv", stringsAsFactors=FALSE))
# scrape data directly from ESPN Cricinfo's StatsGuru website: http://stats.espncricinfo.com
# selector <- ".engineTable:nth-child(5) td "
selector <- "table"

# load all player data, 15913 rows of 15 variables
players <- load_player_data (cricinfo, selector)


# --------------------------------------------
# Wrangling
# --------------------------------------------

sum(is.na(players))

# identify censored observations that our lost from the sample, 
# in our context these our innings where the batsman remains 'not out'
# event (out) = 1, censored event (not out) = 0
players$event <- 1
players$event[players$dismissal == "not out"] <- 0

# add country of ground, join by 'ground'
players <- inner_join(players, grounds)

# add whether playing home (1) or away (0)
players$home <- 0
players$home [players$country == players$team] <- 1

# fix for Pakistani players who haven't been able to play at home since terror attacks, home = UAE
players$home[players$team == "Pakistan" & players$country == "UAE"] <- 1

# identify strike rates
players$goodsr <- as.integer(players$strike.rate > median(players$strike.rate))

# identify if batting in first or second innings of the team
players$first <- 2
players$first[players$inninng %in% c(1,2)] <- 1

# add cohort class, average and start year, join by 'player', 'team'
players <- inner_join(players, cricinfo)

# add variable for number of years played
players$years.played <- players$year - players$start

# remove unnecessary columns / variables
names(players)
players <- subset(players, select = -c(fours, sixes, date, ground, espnid))

# remove non national teams (ICC World XI)
players <- players %>% filter(opposition != "ICC World XI")

# remove data for any positions below 6, likely not enough balls to face in the test
players <- players %>% filter(position < 6)

# include variable for top teams by decade (according to historical test rankings), 1 for top teams
players$top.team <- 0
players$top.team[players$year < 1970] <- 1
players$top.team[players$year < 1970 & players$opposition == "India" ] <- 0
players$top.team[players$opposition == "West Indies" & players$year <= 1990] <- 1
players$top.team[players$opposition == "Pakistan" & players$year <= 1996 & players$year >= 1984] <- 1
players$top.team[players$opposition == "Pakistan" & players$year <= 2003 & players$year >= 1999] <- 1
players$top.team[players$opposition == "South Africa"] <- 1
players$top.team[players$opposition == "South Africa" & players$year <= 2009 & players$year >= 2006] <- 0
players$top.team[players$opposition == "Australia" & players$year <= 1977] <- 1
players$top.team[players$opposition == "Australia" & players$year >= 1993] <- 1
players$top.team[players$opposition == "New Zealand" & players$year <= 1986 & players$year >= 1980] <- 1
players$top.team[players$opposition == "England" & players$year <= 1971] <- 1
players$top.team[players$opposition == "England" & players$year <= 1982 & players$year >= 1977] <- 1
players$top.team[players$opposition == "England" & players$year <= 2005 & players$year >= 2004] <- 1
players$top.team[players$opposition == "England" & players$year >= 2009] <- 1
players$top.team[players$opposition == "India" & players$year >= 2006] <- 1
players$top.team[players$opposition == "India" & players$year >= 2006] <- 1
players$top.team[players$opposition == "Sri Lanka" & players$year <= 2009 & players$year >= 2002] <- 1


# save back up in case of server issues
# write.csv(players, file="players_backup.csv" , row.names = FALSE)

# --------------------------------------------
# Exploratory Analysis
# --------------------------------------------

# explore different cuts of the data to explore impact of variables
temp <- players %>% group_by(team, opposition) %>% summarise(mean=mean(runs), meanBf=mean(balls.faced), n = n())
temp <- players %>% group_by(player, opposition, home) %>% summarise(mean=mean(runs), meanBf=mean(balls.faced), n = n())
temp <- players %>% group_by(player, home) %>% summarise(mean=mean(runs), meanBf=mean(balls.faced), n = n())
temp <- players %>% group_by(top.team, home) %>% summarise(mean=mean(runs),  meanBf=mean(balls.faced), n = n())
temp <- players %>% group_by(first, home) %>% summarise(mean=mean(runs), meanBf=mean(balls.faced), n = n())
temp <- players %>% group_by(cohort, top.team) %>% summarise(mean=mean(runs),  meanBf=mean(balls.faced), n = n())
temp <- players %>% group_by(team, top.team) %>% summarise(mean=mean(runs),  meanBf=mean(balls.faced), n = n())
temp <- players %>% group_by(position, top.team) %>% summarise(mean=mean(runs),  meanBf=mean(balls.faced), n = n())
temp <- players %>% group_by(years.played) %>% summarise(mean=mean(runs),  meanBf=mean(balls.faced), n = n())
temp <- players %>% group_by(team, years.played) %>% summarise(mean=mean(runs),  meanBf=mean(balls.faced), n = n())
summary(players)
dim(players)
rm(temp)

# graph impact of key variables
hist(players$balls.faced) # distribution looks exponential
hist(players$runs)
hist(players$minutes)
plot(density(players$balls.faced))
plot(density(players$minutes))
plot(density(players$years.played))
table(players$country)
table(players$team)
table(players$home)
table(players$top.team)
table(players$cohort)
table(players$first)
table(players$event)

# explore relationships between variables
plot(players$balls.faced ~ players$position)
plot(players$balls.faced ~ players$minutes)
plot(players$balls.faced ~ players$runs)
plot(players$balls.faced ~ players$top.team)
plot(players$balls.faced ~ players$first)
plot(players$balls.faced ~ players$home)
plot(players$balls.faced ~ players$strike.rate)
plot(players$year ~ players$top.team)

# explore relationships between variables
cor.test(players$balls.faced, players$runs, method = "pearson")
cor.test(players$balls.faced, players$minutes, method = "pearson") # as expected highly correlated, interchangeable without loss
cor.test(players$balls.faced, players$year, method = "pearson")
cor.test(players$balls.faced, players$position, method = "pearson")
cor.test(players$balls.faced, players$goodsr, method = "pearson")

# --------------------------------------------
# Plot Base Survival Curves
# --------------------------------------------
tendulkar <- players %>% filter(player == "SR Tendulkar")
ganguly <- players %>% filter(player == "SC Ganguly")
sc.tendu <- Surv(tendulkar$balls.faced, tendulkar$event)
sc.ganguly <- Surv(ganguly$balls.faced, ganguly$event)
sc.all <- Surv(players$balls.faced, players$event)

# plot KM survival curves for Tendulkar
scfit.tendu <- survfit(sc.tendu ~ 1, type = "kaplan-meier")
# scfit.fh.tendu <- survfit(sc.tendu ~ 1, type = "fleming-harrington")
summary(scfit.tendu)
plot(scfit.tendu, xlab = "balls faced", ylab = "probability of survival", main = "Survival Curve for Tendulkar - KM Fit")

# make prettier plot
?ggsurvplot
ggsurvplot(scfit.tendu, 
           data = tendulkar,
           pval = TRUE,
           conf.int = TRUE,
           #fun = 'cumhaz',
           surv.median.line = "hv",
           risk.table = 'percentage',
           risk.table.pos = "in",
           ggtheme = theme_bw(),
           tables.theme = theme_bw(),
           palette = rainbow(1),
           conf.int.fill = rainbow(1),
           censor = FALSE,
           legend = "none",
           xlab = "balls faced",
           ylab = "probability of survival",
           title = "Survival Curve for Tendulkar - KM Fit"
           )

# plot KM survival curves for Ganguly
scfit.ganguly <- survfit(sc.ganguly ~ 1, type = "kaplan-meier")
# scfit.fh.ganguly <- survfit(sc.ganguly ~ 1, type = "fleming-harrington")
summary(scfit.ganguly)
plot(scfit.ganguly, xlab = "balls faced", ylab = "probability of survival", main = "Survival Curve for Ganguly - KM Fit")

# plot KM survival curves for all players
scfit.all <- survfit(sc.all ~ 1, type = "kaplan-meier")
summary(scfit.all)
plot(scfit.all, xlab = "balls faced", ylab = "probability of survival", main = "Survival Curve for All Players - KM Fit")

# create dataframe to compare just two players
players.two <- players %>% filter(player %in% c("SR Tendulkar", "SC Ganguly"))

# plot and compare KM survival curves for two players
group <- players.two$player
sc.two <- Surv(players.two$balls.faced, players.two$event)
scfit.two <- survfit(sc.two ~ group, type = "kaplan-meier")
plot(scfit.two, xlab = "balls faced", ylab = "probability of survival", main = "Survival Curve Comparison", col = c('black', 'red'))
N <- length(unique(group))
legend("topright", text.col = c('black', 'red'), legend=unique(group), col=1:N)

# make prettier plot
ggsurvplot(scfit.two, 
           data = players.two,
           pval = TRUE,
           conf.int = TRUE,
           #fun = 'cumhaz',
           surv.median.line = "hv",
           risk.table = 'percentage',
           risk.table.col = "strata",
           linetype = "strata",
           ggtheme = theme_bw(),
           tables.theme = theme_bw(),
           palette = rainbow(2),
           censor = FALSE,
           legend = "top",
           legend.labs = c("Tendulkar", "Ganguly"),
           xlab = "balls faced",
           ylab = "probability of survival",
           title = "Survival Curve for Tendulkar vs Ganguly - KM Fit"
)

# plot hazard function and cumulative event probability, proportion of event occurrence (getting out) as balls faced increase
plot(scfit.tendu, fun="cumhaz") # cum. hazard function, f(y) = -log(y)
plot(scfit.tendu, fun="event") # cum. event probability, f(y) = 1-y
plot(scfit.tendu, fun="cloglog") # log of cumulative event probability

# comparison of two players
plot(scfit.two, fun="cumhaz",  col = c('black', 'red'))
N <- length(unique(group))
legend("bottomright", text.col = c('black', 'red'), legend=unique(group), col=1:N)

plot(scfit.two, fun="event",  col = c('black', 'red'))
N <- length(unique(group))
legend("bottomright", text.col = c('black', 'red'), legend=unique(group), col=1:N)

plot(scfit.two, fun="cloglog",  col = c('black', 'red'))
N <- length(unique(group))
legend("bottomright", text.col = c('black', 'red'), legend=unique(group), col=1:N)


# --------------------------------------------
# Plot Survival Curves for Impact of Factors / Covariates
# --------------------------------------------
# Major variables to test:
#   1. home
#   2. first
#   3. top.team
#   4. position

# check converge of survival curves for each factor
# divergence or crossing over time, indicates likely violation of proportional hazards assumption
# 1. home vs. away tests
group <- tendulkar$home
scfit.tendu <- survfit(sc.tendu ~ group)
plot(scfit.tendu, fun="cloglog")

group <- players$home
scfit.all <- survfit(sc.all ~ group)
plot(scfit.all, fun="cloglog")

# 2. first vs. second innings
group <- players$first
scfit.all <- survfit(sc.all ~ group)
plot(scfit.all, fun="cloglog")

# 3. top team
group <- players$top.team
scfit.all <- survfit(sc.all ~ group)
plot(scfit.all, fun="cloglog")

# 4. position
group <- players$position
scfit.all <- survfit(sc.all ~ group)
plot(scfit.all, fun="cloglog")

# 1. home vs. away tests
# compare survival curves based on impact of variables
# determine impact of home vs away tests for Tendulkar
group <- tendulkar$home
scfit.tendu <- survfit(sc.tendu ~ group)
plot(scfit.tendu, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Home vs Away for Tendulkar", col = c('black', 'red'))
leg.txt <- c("Away", "Home")
legend("topright", leg.txt, text.col = c('black', 'red'), bty = 'n')

# determine impact of home vs away tests for Ganguly
group <- ganguly$home
scfit.ganguly <- survfit(sc.ganguly ~ group)
plot(scfit.ganguly, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Home vs Away for Ganguly", col = c('black', 'red'))
leg.txt <- c("Away", "Home")
legend("topright", leg.txt, text.col = c('black', 'red'), bty = 'n')

# determine impact of home vs away tests for all players
group <- players$home
scfit.all <- survfit(sc.all ~ group)
plot(scfit.all, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Home vs Away for All Players", col = c('black', 'red'))
leg.txt <- c("Away", "Home")
legend("topright", leg.txt, text.col = c('black', 'red'), bty = 'n')


# 2. first vs. second innings
# determine impact of batting innning
group <- tendulkar$inninng
scfit.tendu <- survfit(sc.tendu ~ group)
plot(scfit.tendu, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Inninngs", col = rainbow(4))
N <- length(unique(group))
legend("topright", text.col = rainbow(4), legend=unique(group), col=1:N)

# determine impact of batting in first vs second inning
group <- tendulkar$first
scfit.tendu <- survfit(sc.tendu ~ group)
plot(scfit.tendu, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Innings", col = c('black', 'red'))
leg.txt <- c("First", "Second")
legend("topright", leg.txt, text.col = c('black', 'red'), bty = 'n')

# make prettier plot
ggsurvplot(scfit.tendu, 
           data = tendulkar,
           pval = TRUE,
           conf.int = TRUE,
           #fun = 'cumhaz',
           surv.median.line = "hv",
           risk.table = 'percentage',
           risk.table.col = "strata",
           linetype = "strata",
           ggtheme = theme_bw(),
           tables.theme = theme_bw(),
           palette = rainbow(2),
           censor = FALSE,
           legend = "top",
           legend.labs = c("First", "Second"),
           ncensor.plot = TRUE, 
           xlab = "balls faced",
           ylab = "probability of survival",
           title = "Impact of Innings"
)

# for all players
group <- players$first
scfit.all <- survfit(sc.all ~ group)
plot(scfit.all, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Innings for all Players", col = c('black', 'red'))
leg.txt <- c("First", "Second")
legend("topright", leg.txt, text.col = c('black', 'red'), bty = 'n')


# 3. top team
# for Tendulkar
group <- tendulkar$top.team
scfit.tendu <- survfit(sc.tendu ~ group)
plot(scfit.tendu, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Top Team for Tendulkar", col = c('black', 'red'))
# N <- length(unique(group))
# legend("topright", text.col = c('black', 'red'), legend=unique(group), col=1:N)
leg.txt <- c("Not Top", "Top")
legend("topright", leg.txt, text.col = c('black', 'red'), bty = 'n')

# for Ganguly
group <- ganguly$top.team
scfit.ganguly <- survfit(sc.ganguly ~ group)
plot(scfit.ganguly, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Top Team for Ganguly", col = c('black', 'red'))
leg.txt <- c("Not Top", "Top")
legend("topright", leg.txt, text.col = c('black', 'red'), bty = 'n')

# for all players
group <- players$top.team
scfit.all <- survfit(sc.all ~ group)
plot(scfit.all, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Top Team for All Players", col = c('black', 'red'))
leg.txt <- c("Not Top", "Top")
legend("topright", leg.txt, text.col = c('black', 'red'), bty = 'n')


# 4. position
# for Tendulkar
group <- tendulkar$position
N <- length(unique(group))
scfit.tendu <- survfit(sc.tendu ~ group)
plot(scfit.tendu, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Top Team for Tendulkar", col = rainbow(N))
legend("topright", text.col = rainbow(N), legend=unique(group), col=1:N)

# for Ganguly
group <- ganguly$position
scfit.ganguly <- survfit(sc.ganguly ~ group)
N <- length(unique(group))
plot(scfit.ganguly, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Top Team for Ganguly", col = rainbow(N))
legend("topright", text.col = rainbow(N), legend=unique(group), col=1:N)

# for all players
group <- players$position
N <- length(unique(group))
scfit.all <- survfit(sc.all ~ group)
plot(scfit.all, xlab = "balls faced", ylab = "probability of survival", main = "Impact of Top Team for All Players", col = rainbow(N))
legend("topright", text.col = rainbow(N), legend=unique(group), col=1:N)


# --------------------------------------------
# Log Rank Test
# --------------------------------------------
# for univariate analysis (categorical variables)
#   1. home           (1 = home, 0 = away)
#   2. first          (1 = first innning, 2 = second inning)
#   3. top.team       (0 = not a top team, 1 = playing against a top team)

# this is a non-parametric test that tests the statistical significance
# of the different factors identified above 
# the null hypothesis is that the factor has no significant impact 
# (i.e. the survival curves are statistically identical)

# calculate survdiff, log-rank test for stat sig.
# rho=0 log-rank (Mantel-Haenszel test)
# rho=1 Peto & Peto modification of the Gehan-Wilcoxon test

# note: a problem with testing p-value for all players is that the sample size is effectively too large
# must test for smaller sample sizes or use log(-log) plot for earlier


# 1. home or away test
# for tendulkar
survdiff(sc.tendu ~ tendulkar$home, rho=0)
survdiff(sc.tendu ~ tendulkar$home, rho=1)
# for ganguly
survdiff(sc.ganguly ~ ganguly$home, rho=0)
survdiff(sc.ganguly ~ ganguly$home, rho=1)
# for all players
survdiff(sc.all ~ players$home, rho=0)
survdiff(sc.all ~ players$home, rho=1)

# 2. first or second innings
# for tendulkar
survdiff(sc.tendu ~ tendulkar$first, rho=0)
survdiff(sc.tendu ~ tendulkar$first, rho=1)
# for ganguly
survdiff(sc.ganguly ~ ganguly$first, rho=0)
survdiff(sc.ganguly ~ ganguly$first, rho=1)
# for all players
survdiff(sc.all ~ players$first, rho=0)
survdiff(sc.all ~ players$first, rho=1)

# 3. top team or not
# for tendulkar
survdiff(sc.tendu ~ tendulkar$top.team, rho=0)
survdiff(sc.tendu ~ tendulkar$top.team, rho=1)
# for ganguly
survdiff(sc.ganguly ~ ganguly$top.team, rho=0)
survdiff(sc.ganguly ~ ganguly$top.team, rho=1)
# for all players
survdiff(sc.all ~ players$top.team, rho=0)
survdiff(sc.all ~ players$top.team, rho=1)
# p values good for top.team


# plot survdiff

# 1. home or away test
# for tendulkar
group <- tendulkar$home
scfit.tendu <- survfit(sc.tendu ~ group)
survdiffplot(scfit.tendu)
# for all players
group <- players$home
scfit.all <- survfit(sc.all ~ group)
survdiffplot(scfit.all)
# significant till about 240 balls

# 2. first or second innings
# for all players
group <- players$first
scfit.all <- survfit(sc.all ~ group)
survdiffplot(scfit.all)
# significant till about 330 balls

# 3. top team or not
# for tendulkar
group <- tendulkar$top.team
scfit.tendu <- survfit(sc.tendu ~ group)
survdiffplot(scfit.tendu)
# for all players
group <- players$top.team
scfit.all <- survfit(sc.all ~ group)
survdiffplot(scfit.all)
# significant till about 350 balls

# plot survival curve fits for variables: home and top.team
fit <- survfit(sc.all ~ home + top.team, data = players)
ggsurv <- ggsurvplot(fit, fun = "cumhaz", conf.int = TRUE, ggtheme = theme_bw())
ggsurv$plot + theme_bw() + 
  theme (legend.position = "bottom") +
  facet_grid(. ~ home)

ggsurv$plot + theme_bw() + 
  theme (legend.position = "bottom") +
  facet_grid(. ~ top.team)

# plot survival curve fits for variables: top.team and first
fit <- survfit(sc.all ~ first + top.team, data = players)
ggsurv <- ggsurvplot(fit, fun = "cumhaz", conf.int = TRUE, ggtheme = theme_bw())
ggsurv$plot + theme_bw() + 
  theme (legend.position = "bottom") +
  facet_grid(. ~ first)

# plot survival curve fits for variables: home and first
fit <- survfit(sc.all ~ first + home, data = players)
ggsurv <- ggsurvplot(fit, fun = "cumhaz", conf.int = TRUE, ggtheme = theme_bw())
ggsurv$plot + theme_bw() + 
  theme (legend.position = "bottom") +
  facet_grid(. ~ home)

# factors to consider: home, top.team, first

# --------------------------------------------
# Cox Proportional Hazards Regression Model
# --------------------------------------------
# for multivariate analysis
#   1. home
#   2. top.team
#   3. years.played
#   4. position
#   5. first
# the cox model uses the hazard ratio of two hazard functions and is hence a 
# proportional hazard model that assumes the ratio does not change with time
# verify hazard proportionality assumption (residuals not correlated w time)
# cox.zph correlates the corresponding set of scaled Schoenfeld residuals with time
# there should be a non-significant relationship between residuals and time

# Schoenfeld residuals to check the proportional hazards assumption
# cox.zph() correlates the corresponding set of scaled Schoenfeld residuals with time

# note: a problem with testing p-value for all players is that the sample size is effectively too large
# must test for smaller sample sizes or use log(-log) plot for earlier

# univariate analysis
# 1. home or away test
# tendulkar
coxfit.home <- coxph(sc.tendu ~ tendulkar$home)
summary(coxfit.home)
cox.zph(coxfit.home)
plot(cox.zph(coxfit.home))
ggcoxzph(cox.zph(coxfit.home))
# assume proportional hazards (i.e. not correlated with time/balls faced)
# for all players
coxfit.home <- coxph(sc.all ~ players$home)
summary(coxfit.home)
cox.zph(coxfit.home)
plot(cox.zph(coxfit.home))
ggcoxzph(cox.zph(coxfit.home))
# assume proportional hazards (i.e. not correlated with time/balls faced)
# hazard reduced by a factor of 0.94, i.e. batsman perform better at home than away
# p=value significant, null hypothesis rejected

# 2. top team or not
# tendulkar
coxfit.top.team <- coxph(sc.tendu ~ tendulkar$top.team)
summary(coxfit.top.team)
cox.zph(coxfit.top.team)
plot(cox.zph(coxfit.top.team))
ggcoxzph(cox.zph(coxfit.top.team))
# assume proportional hazards (i.e. not correlated with time/balls faced)
# for all players
coxfit.top.team <- coxph(sc.all ~ players$top.team)
summary(coxfit.top.team)
cox.zph(coxfit.top.team)
plot(cox.zph(coxfit.top.team))
ggcoxzph(cox.zph(coxfit.top.team))
# assume proportional hazards (i.e. not correlated with time/balls faced)
# hazard increased by a factor of 1.08, i.e. playing top teams is harder for batsman
# p=value significant, null hypothesis rejected

# 3. years playing
# tendulkar
coxfit.years.played <- coxph(sc.tendu ~ tendulkar$years.played)
summary(coxfit.years.played)
cox.zph(coxfit.years.played)
plot(cox.zph(coxfit.years.played))
ggcoxzph(cox.zph(coxfit.years.played))
# assume proportional hazards (i.e. not correlated with time/balls faced)
# for all players
coxfit.years.played <- coxph(sc.all ~ players$years.played)
summary(coxfit.years.played)
cox.zph(coxfit.years.played)
plot(cox.zph(coxfit.years.played))
ggcoxzph(cox.zph(coxfit.years.played))
# assume proportional hazards (i.e. not correlated with time/balls faced)
# hazard reduced by a factor of 0.99, not a significant factor

# 4. position
# tendulkar
coxfit.position <- coxph(sc.tendu ~ tendulkar$position)
summary(coxfit.position)
cox.zph(coxfit.position)
plot(cox.zph(coxfit.position))
ggcoxzph(cox.zph(coxfit.position))
# assume proportional hazards (i.e. not correlated with time/balls faced)
# for all players
coxfit.position <- coxph(sc.all ~ players$position)
summary(coxfit.position)
cox.zph(coxfit.position)
plot(cox.zph(coxfit.position))
ggcoxzph(cox.zph(coxfit.position))
# hazard reduced by a factor of 0.99, not a significant factor

# 5. test for first vs second innings
# tendulkar
coxfit.first <- coxph(sc.tendu ~ tendulkar$first)
summary(coxfit.first)
cox.zph(coxfit.first)
plot(cox.zph(coxfit.first))
ggcoxzph(cox.zph(coxfit.first))
# for all players
coxfit.first <- coxph(sc.all ~ players$first)
summary(coxfit.first)
cox.zph(coxfit.first)
plot(cox.zph(coxfit.first))
ggcoxzph(cox.zph(coxfit.first))
# proportional hazards assumption violated (i.e. hazard correlated with time/balls faced)
# variable: first - no good, residuals autocorrelated


# multivariate analysis - impact of two or more variables
# examine residuals from all three variables
coxfit <- coxph(sc.all ~ players$home + players$top.team + players$years.played) 
summary(coxfit)
cox.zph(coxfit)
# years played not significant in the overall model as well

# examine for home and top.team
coxfit <- coxph(sc.tendu ~ tendulkar$home + tendulkar$top.team)
summary(coxfit)
cox.zph(coxfit)
plot(cox.zph(coxfit))
ggcoxzph(cox.zph(coxfit))
# all players
coxfit <- coxph(sc.all ~ players$home + players$top.team)
summary(coxfit)
cox.zph(coxfit)
plot(cox.zph(coxfit))
ggcoxzph(cox.zph(coxfit))
# p-values are significant and so null hypothesis (all hazard betas are 0) can be rejected

# plot effect of all variables
fit <- survfit(coxfit , data = players)
ggsurvplot(fit, conf.int = TRUE, censor = FALSE, ggtheme = theme_bw())


# # reduce size of dataset (for ease of handling)
# players.sm <- players %>% filter(player == "SR Tendulkar")
# sc.small <- Surv(players.sm$balls.faced, players.sm$event)
# coxfit.sm <- coxph(sc.small ~ players.sm$home + players.sm$top.team)
# 
# # visualize impact of each variable
# temp.home <- with(players.sm, data.frame(home = c(0, 1), top.team = c(1, 1)))
# temp.tt <- with(players.sm, data.frame(home = c(0, 0), top.team = c(0, 1)))
# 
# # plot effect of each variable
# coxfit.home <- survfit(coxfit.sm, data = players.sm, newdata = temp.home)
# ggsurvplot(coxfit.home, conf.int = TRUE, ggtheme = theme_bw())
# coxfit.tt <- survfit(coxfit.sm, data = players.sm, newdata = temp.tt)
# ggsurvplot(coxfit.tt, conf.int = TRUE, ggtheme = theme_bw())


# Martingale residual to assess outliers and nonlinearity for continous variables (none in this case)
coxfit <- coxph(sc.all ~ players$home + players$top.team)
ggcoxdiagnostics(coxfit, type = "dfbeta", linear.predictions = FALSE)
# no major outliers discovered
ggcoxdiagnostics(coxfit, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
# pattern of deviance residuals (normalized martingale) symmetric around zero

# reduce r session ram
rm(fit)
rm(sc.ganguly)
rm(sc.tendu)
rm(sc.two)
rm(scfit.two)
rm(scfit.ganguly)
rm(scfit.tendu)
rm(scfit.all)
rm(sc.all)
rm(coxfit)
rm(coxfit.first)
rm(coxfit.home)
rm(coxfit.top.team)
rm(coxfit.years.played)
rm(rest.stats)
rm(players.two)
rm(ganguly)
rm(tendulkar)

# findings
# ---------
# relevant factors to analyze for clustering are: 
# 1. batsman playing home or away tests (home)
# 2. batsman playing top team or not (top.team)
# the following factors were deemed insufficient: first, years.played, position


# --------------------------------------------
# Identify Likely Parametric Distribution and Plot Regression Fit
# --------------------------------------------
# run regression to fit distribution
fit_exp <- survreg(sc.all ~ 1, dist = "exponential")
summary(fit_exp)
fit_weibull <- survreg(sc.all ~ 1, dist="weibull")
summary(fit_weibull)
fit_gauss <- survreg(sc.all ~ 1, dist="gaussian")
summary(fit_gauss)
fit_logistic <- survreg(sc.all ~ 1, dist="logistic")
summary(fit_logistic)
fit_lognormal <- survreg(sc.all ~ 1, dist="lognormal")
summary(fit_lognormal)
fit_loglogistic <- survreg(sc.all ~ 1, dist="loglogistic")
summary(fit_loglogistic)

# try distributions in flexsurv package
fit_exp <- flexsurvreg(sc.all ~ 1, dist="exp")
plot(fit_exp)
fit_weibull <- flexsurvreg(sc.all ~ 1, dist="weibull")
plot(fit_weibull)
fit_gamma <- flexsurvreg(sc.all ~ 1, dist="gamma")
plot(fit_gamma)
fit_gengamma <- flexsurvreg(sc.all ~ 1, dist="gengamma")
plot(fit_gengamma)
fit_genf <- flexsurvreg(sc.all ~ 1, dist="genf")
plot(fit_genf)
fit_lognormal <- flexsurvreg(sc.all ~ 1, dist="lnorm")
plot(fit_lognormal)

# Check log-likelihood to compare fits
fit_exp$loglik
fit_weibull$loglik
fit_gamma$loglik
fit_gengamma$loglik
fit_lognormal$loglik
# exponential distribution looks best

# Check AIC to compare fits
# Akaike information criterion (AIC) is an estimator of the relative quality of models
fit_exp$AIC
fit_weibull$AIC
fit_gamma$AIC
fit_gengamma$AIC
fit_lognormal$AIC
# confirms choice of exponential

# probability plots to test for distribution
probplot(players$balls.faced)
probplot(players$balls.faced, "qunif")
probplot(players$balls.faced, "qexp")
probplot(players$balls.faced, "qnorm")
probplot(players$balls.faced, "qweibull", shape = 1)
probplot(players$balls.faced, "qlnorm")
probplot(players$balls.faced, "qgamma", shape = 1)

# confirms choice of exponential, PH assumption valid furthermore as exp(x) is memoryless


# --------------------------------------------
# Cluster Analysis
# --------------------------------------------
# prepare dataset of players for k-means clustering
# based on above analysis there are two dimensions / factors to use:
# 1. home - batsman performance in home vs away tests
# 2. top.team - batsman performance vs top teams
# use the metric of balls.faced normalized by strike rate and player average

# check restricted means, this reveals the expected 
# median for when the survival probability is at 50%
rest.stats <- get_restricted_mean(cricinfo, players)
names(rest.stats)[1] <- "player"
players <- inner_join(players, rest.stats)
plot(players$rmean ~ players$rmedian) 

# prepare dataset for clustering based on above analysis
names(players)

# spread key factors: top team for key column variables
players.cluster <- subset(players, select = c(runs, balls.faced, player, team, opposition, year, event, home, avg, cohort, top.team, strike.rate))
players.cluster <- players.cluster %>% group_by(player, top.team) %>% summarise(runs=sum(runs), bats = n(), outs = sum(event), meanBF=mean(balls.faced))
players.cluster$average <- players.cluster$runs / players.cluster$outs
tmp <- players.cluster %>% filter(bats <= 20 | runs < 1000)
# spread mean balls faced
meanBF <- subset(players.cluster, select = c(player, top.team, meanBF))
meanBF <- spread(meanBF, top.team, meanBF)
names(meanBF) <- c("player", "nt.meanBF", "tt.meanBF")
# spread average
average <- subset(players.cluster, select = c(player, top.team, average))
average <- spread(average, top.team, average)
names(average) <- c("player", "nt.average", "tt.average")
final.tt <- inner_join(meanBF, average)

# spread key factors: home and away for key column variables
players.cluster <- subset(players, select = c(runs, balls.faced, player, team, opposition, year, event, home, avg, cohort, top.team, strike.rate))
players.cluster <- players.cluster %>% group_by(player, home) %>% summarise(runs=sum(runs), bats = n(), outs = sum(event), meanBF=mean(balls.faced))
players.cluster$average <- players.cluster$runs / players.cluster$outs
tmp2 <- players.cluster %>% filter(bats <= 20 | runs < 1000)
tmp3 <- full_join(tmp, tmp2)
# spread mean balls faced
meanBF <- subset(players.cluster, select = c(player, home, meanBF))
meanBF <- spread(meanBF, home, meanBF)
names(meanBF) <- c("player", "away.meanBF", "home.meanBF")
# spread average
average <- subset(players.cluster, select = c(player, home, average))
average <- spread(average, home, average)
names(average) <- c("player", "away.average", "home.average")
final.ha <- inner_join(meanBF, average)


# create final dataset for cluster analysis
final <- inner_join(final.tt, final.ha, by = "player")

# remove players with insufficient data for comparison
tmp <- as.data.frame(unique(tmp3$player))
names(tmp) <- "remove"
final <- subset(final, !(player %in% tmp$remove))

# create and add subset of additional data, avg, rmean and rmedian
tmp2 <- subset(players, select = c(player, avg, cohort))
tmp2 <- tmp2[!duplicated(tmp2), ]
final <- inner_join(final, tmp2, by = "player")

# calculate average based on data
players.cluster <- subset(players, select = c(runs, player, event))
players.cluster <- players.cluster %>% group_by(player) %>% summarise(runs=sum(runs), outs = sum(event))
players.cluster$calcavg <- players.cluster$runs / players.cluster$outs
players.cluster <- subset(players.cluster, select = c(player, calcavg))
final <- inner_join(final, players.cluster, by = "player")

# remove any NA rows
final <- na.omit(final)

# two metrics are created to compare the two factors
# 1. a measure of consistency
# 2. a measure of performance in tough conditions (i.e. against top teams and in away conditions)
# cap good performances in easy conditions
final <- final %>% mutate(nt.lower = ifelse(nt.average > calcavg, calcavg, nt.average))
final$topteam <- final$tt.average - final$nt.lower
final <- final %>% mutate(home.lower = ifelse(home.average > calcavg, calcavg, home.average))
final$home <- final$away.average - final$home.lower
# calculate standard deviation of home and top team averages
final$sigma <- apply(final[,c(5,13,8,15)], 1, sd)
# calculate metric 1 for consistency
final$metric1 <- final$avg * (1 - final$sigma / final$calcavg)
# calculate metric 2 for performance
final$metric2 <- final$avg * (1 + (final$topteam + final$home - final$sigma)/ final$calcavg)

# prepare subset for cluster analysis
names(final)
final <- subset(final, select = c(player, metric1, metric2, cohort))

# normalize the dataset
final$metric1 <- scale(final$metric1)
final$metric2 <- scale(final$metric2)

# save back up for cluster analysis
# write.csv(final, file="cluster_backup.csv", row.names = FALSE)

# rename row names to player names
tmp <- final$player
final <- subset(final, select = -c(player, cohort))
row.names(final) <- tmp
scaled.final <- final
cov(scaled.final <- scale(final))

# measure distance (i.e. similarlity between players)
distance <- get_dist(scaled.final, method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# create clusters
k <- kmeans(scaled.final, centers = 6, nstart = 25)
str(k)
fviz_cluster(k, data = scaled.final)

# check cluster quality and optimized number of clusters for dataset
set.seed(123)

# check optimal number of clusters, using within-cluster sum of squares
fviz_nbclust(scaled.final, kmeans, method = "wss")
# check quality of clusters
fviz_nbclust(scaled.final, kmeans, method = "silhouette")
# compute gap statistic
gap_stat <- clusGap(scaled.final, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)
