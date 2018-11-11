library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
library(scales, warn.conflicts=FALSE, quietly=TRUE)
library(shiny, warn.conflicts=FALSE, quietly=TRUE)
library(plotly, warn.conflicts=FALSE, quietly=TRUE)
library(htmlwidgets, warn.conflicts=FALSE, quietly=TRUE)
library(devtools, warn.conflicts=FALSE, quietly=TRUE)
library(streamgraph, warn.conflicts=FALSE, quietly=TRUE)

# devtools::install_github("hrbrmstr/streamgraph")
# devtools::install_github("ramnathv/rCharts")

# api key credententials (moved to .Rprofile)
Sys.setenv("plotly_username"="thekotecha")

# --------------------------------------------
# Loading
# --------------------------------------------
setwd("D:/Github/Data_US_Campaign_Finance/data")

# data from Vital Statistics study - https://www.brookings.edu/research/vital-statistics-on-congressdata-on-the-u-s-congress-a-joint-effort-from-brookings-and-the-american-enterprise-institute/
# government
df_house_maj <- tbl_df(read.csv("vsc2_elections_house_majority.csv", stringsAsFactors=FALSE))
df_senate_maj <- tbl_df(read.csv("vsc2_elections_senate_majority.csv", stringsAsFactors=FALSE))
df_swing_seats <- tbl_df(read.csv("supp_elections_swing_seats.csv", stringsAsFactors=FALSE))
# politics
df_costs <- tbl_df(read.csv("vsc3_campfin_costs.csv", stringsAsFactors=FALSE))
df_house_incum <- tbl_df(read.csv("vsc3_campfin_house_incumbents.csv", stringsAsFactors=FALSE))
df_house_open <- tbl_df(read.csv("vsc3_campfin_house_open.csv", stringsAsFactors=FALSE))
df_senate_incum <- tbl_df(read.csv("vsc3_campfin_senate_incumbents.csv", stringsAsFactors=FALSE))
df_senate_open <- tbl_df(read.csv("vsc3_campfin_senate_open.csv", stringsAsFactors=FALSE))
df_pac <- tbl_df(read.csv("vsc3_campfin_pac_indep.csv", stringsAsFactors=FALSE))
# media
df_polar_house <- tbl_df(read.csv("vsc8_polarization_house.csv", stringsAsFactors=FALSE))
df_polar_senate <- tbl_df(read.csv("vsc8_polarization_senate.csv", stringsAsFactors=FALSE))
df_media_trust <- tbl_df(read.csv("supp_media_trust.csv", stringsAsFactors=FALSE))

# Graphs (see story outline *.txt):
# YES: 
# - 60% district for house and senate
# - graph of swing voters
# - rising cost of election
# - comparison of challenger vs incumbent cost based for house
# - comparison of challenger vs incumbent cost based for senate
# - rising pac contributions with historical supreme court cases timeline
# - rising idealogical polarization in house and senate
# - heatmap for media trust by new sources
# MAYBE / NO: 
# - comparison of open seats cost for house
# - comparison of open seats cost for senate
# - (NEW PROJECT IDEA) cosine similarity / KNN analysis for pac spending vs house member voting history


# --------------------------------------------
# Wrangling & Analysis
# --------------------------------------------

# create new column to separate out starting year for senate session
df_senate_maj <- separate(df_senate_maj,"period",c("year","end.year"),sep=" - ")

# convert columns to neccessary data type
df_senate_maj$year <- as.integer(df_senate_maj$year)
df_house_maj$incumbents[df_house_maj$incumbents =="391a"] <- 391
df_house_maj$incumbents <- as.integer(df_house_maj$incumbents)

# rename columns
names(df_senate_maj)[6] <- "percentage"

# create new column to identify house vs senate district data
df_house_maj$chamber <- "house"
df_senate_maj$chamber <- "senate"

# remove unnecessary columns
df_senate_maj <- subset(df_senate_maj, select=-c(end.year,south,north))

# combine senate and house datasets for majority districts
df_majority <- full_join(df_house_maj, df_senate_maj)

# create factors
df_majority$chamber <- factor(df_majority$chamber)

# fix percentages
df_majority$percentage <- df_majority$percentage / 100

# gather row headings into column variable by type of victory in district
df_swing_seats <- gather(df_swing_seats, "type", "total", 2:4, factor_key=TRUE)

# remove unnecessary columns for nominal figures
df_costs <- subset(df_costs, select=-c(house.nominal, senate.nominal))

# rename columns
names(df_costs)[2] <- "house"
names(df_costs)[3] <- "senate"

# gather row headings into column variable for chamber
df_costs <- gather(df_costs, "chamber", "cost", 2:3, factor_key=TRUE)
df_costs$cost <- as.integer(gsub("*,*", "", df_costs$cost))

# convert cost to millions
df_costs$cost <- df_costs$cost / 1000000

# for House races
# gather all years into columns to vectorize the table
df_house_incum <- gather (df_house_incum, "year", "cost", -c(result,type1,type2))

# filter for just the rows on cost
df_house_incum <- filter(df_house_incum, grepl("Cost",type2))
df_house_incum$cost <- as.integer(gsub("*,*", "", df_house_incum$cost))

# fix year column to remove extra 'X'
df_house_incum$year <- as.integer(gsub("X*", "", df_house_incum$year))

# rename columns
names(df_house_incum)[2] <- "candidate"

# create factors
df_house_incum$result <- factor(df_house_incum$result)
df_house_incum$candidate <- factor(df_house_incum$candidate)

# filter for just cost for incumbents in house elections by total and then remove column
df_house_incum <- df_house_incum %>% filter(type2 == "Total Cost")
df_house_incum <- subset(df_house_incum, select=-c(type2))

# create separate dataframes for type of victory
df_house_incum_win60plus <- df_house_incum %>% filter(result == "Incumbent won with 60% or more")
df_house_incum_win <- df_house_incum %>% filter(result == "Incumbent won with <60%")
df_house_incum_loss <- df_house_incum %>% filter(result == "Incumbent was defeated")
df_house_incum_win60plus <- subset(df_house_incum_win60plus, select=-c(result))
df_house_incum_win <- subset(df_house_incum_win, select=-c(result))
df_house_incum_loss <- subset(df_house_incum_loss, select=-c(result))

# spread out incumbent and challengers column
df_house_incum_win60plus <- spread (df_house_incum_win60plus, candidate, cost)
df_house_incum_win <- spread (df_house_incum_win, candidate, cost)
df_house_incum_loss <- spread (df_house_incum_loss, candidate, cost)

# create new column for differential of election spending
df_house_incum_win60plus$diffw60 <- round(df_house_incum_win60plus$Incumbents / df_house_incum_win60plus$Challengers, 2)
df_house_incum_win$diffw <- round(df_house_incum_win$Incumbents / df_house_incum_win$Challengers, 2)
df_house_incum_loss$diffl <- round(df_house_incum_loss$Incumbents / df_house_incum_loss$Challengers, 2)

# create new dataset for all differentials
df_house_incumbents <- inner_join(df_house_incum_win60plus,df_house_incum_win, b = "year")
df_house_incumbents <- inner_join(df_house_incumbents,df_house_incum_loss, b = "year")
df_house_incumbents <- subset(df_house_incumbents, select = c(year, diffw60,diffw,diffl))

# for Senate races
# gather all years into columns to vectorize the table
df_senate_incum <- gather (df_senate_incum, "year", "cost", -c(result,type1,type2))

# filter for just the rows on cost
df_senate_incum <- filter(df_senate_incum, grepl("Cost",type2))
df_senate_incum$cost[is.na(df_senate_incum$cost)] <- 0
df_senate_incum$cost <- as.integer(gsub("*,*", "", df_senate_incum$cost))
df_senate_incum$cost[is.na(df_senate_incum$cost)] <- 0

# fix year column to remove extra 'X'
df_senate_incum$year <- as.integer(gsub("X*", "", df_senate_incum$year))

# rename columns
names(df_senate_incum)[2] <- "candidate"

# create factors
df_senate_incum$result <- factor(df_senate_incum$result)
df_senate_incum$candidate <- factor(df_senate_incum$candidate)

# filter for just cost for incumbents in house elections by total and then remove column
df_senate_incum <- df_senate_incum %>% filter(type2 == "Total Cost")
df_senate_incum <- subset(df_senate_incum, select=-c(type2))

# create separate dataframes for type of victory
df_senate_incum_win60plus <- df_senate_incum %>% filter(result == "Incumbent won with 60% or more")
df_senate_incum_win <- df_senate_incum %>% filter(result == "Incumbent won with <60%")
df_senate_incum_loss <- df_senate_incum %>% filter(result == "Incumbent was defeated")
df_senate_incum_win60plus <- subset(df_senate_incum_win60plus, select=-c(result))
df_senate_incum_win <- subset(df_senate_incum_win, select=-c(result))
df_senate_incum_loss <- subset(df_senate_incum_loss, select=-c(result))

# spread out incumbent and challengers column
df_senate_incum_win60plus <- spread (df_senate_incum_win60plus, candidate, cost)
df_senate_incum_win <- spread (df_senate_incum_win, candidate, cost)
df_senate_incum_loss <- spread (df_senate_incum_loss, candidate, cost)

# create new column for differential of election spending
df_senate_incum_win60plus$diffw60 <- round(df_senate_incum_win60plus$Incumbents / df_senate_incum_win60plus$Challengers, 2)
df_senate_incum_win$diffw <- round(df_senate_incum_win$Incumbents / df_senate_incum_win$Challengers, 2)
df_senate_incum_loss$diffl <- round(df_senate_incum_loss$Incumbents / df_senate_incum_loss$Challengers, 2)

# create new dataset for all differentials
df_senate_incumbents <- inner_join(df_senate_incum_win60plus,df_senate_incum_win, b = "year")
df_senate_incumbents <- inner_join(df_senate_incumbents,df_senate_incum_loss, b = "year")
df_senate_incumbents <- subset(df_senate_incumbents, select = c(year, diffw60,diffw,diffl))

# convert text to numbers
df_pac$for.dems <- as.integer(gsub("*,*", "", df_pac$for.dems))
df_pac$for.repub <- as.integer(gsub("*,*", "", df_pac$for.repub))
df_pac$against.dems <- as.integer(gsub("*,*", "", df_pac$against.dems))
df_pac$against.repub <- as.integer(gsub("*,*", "", df_pac$against.repub))
df_pac$total <- as.integer(gsub("*,*", "", df_pac$total))

# remove extra whitespace
df_pac$congress <- gsub("* *", "", df_pac$congress)

# create factors
df_pac$congress <- factor(df_pac$congress)

# convert cost figures for graphic purposes
df_pac$against.dems = df_pac$against.dems * -1
df_pac$against.repub = df_pac$against.repub * -1

# remove extra columns
df_pac <- subset(df_pac, select=-total)

# create separate data frames for house and senate data
df_pac_house <- df_pac %>% filter(congress == "House")
df_pac_senate <- df_pac %>% filter(congress == "Senate")

# # rename columns for House
# names(df_pac_house)[3] <- "house.for.dems"
# names(df_pac_house)[4] <- "house.against.dems"
# names(df_pac_house)[5] <- "house.for.rep"
# names(df_pac_house)[6] <- "house.against.rep"
# 
# # rename columns for Senate
# names(df_pac_senate)[3] <- "sen.for.dems"
# names(df_pac_senate)[4] <- "sen.against.dems"
# names(df_pac_senate)[5] <- "sen.for.rep"
# names(df_pac_senate)[6] <- "sen.against.rep"
# 
# # create new dataframes for democrats and republicans
# df_pac_comb <- full_join(df_pac_senate, df_pac_house, by = 'year')
# df_pac_dems <- subset(df_pac_comb, select = c(year, sen.for.dems, sen.against.dems, house.for.dems, house.against.dems))
# df_pac_rep <- subset(df_pac_comb, select = c(year, sen.for.rep, sen.against.rep, house.for.rep, house.against.rep))

# remove unnecessary columns
df_polar_house <- subset(df_polar_house, select = -c(Nonsouthern.Democrats, Southern.Democrats))
df_polar_senate <- subset(df_polar_senate, select = -c(Nonsouthern.Democrats, Southern.Democrats))

# gather columns
df_polar_house <- gather(df_polar_house, "type", "total", 3:5, factor_key=TRUE)
df_polar_senate <- gather(df_polar_senate, "type", "total", 3:5, factor_key=TRUE)                         

# remove unnecessary columns
df_media_trust <- subset(df_media_trust, select = -Overall)

# rename columns for ease of use
names(df_media_trust)[2] <- "cons.lib"
names(df_media_trust)[3] <- "most.lib"
names(df_media_trust)[4] <- "mixed"
names(df_media_trust)[5] <- "most.con"
names(df_media_trust)[6] <- "cons.con"

# convert to integer
df_media_trust$cons.lib <- as.integer(gsub("%", "", df_media_trust$cons.lib))
df_media_trust$most.lib <- as.integer(gsub("%", "", df_media_trust$most.lib))
df_media_trust$mixed <- as.integer(gsub("%", "", df_media_trust$mixed))
df_media_trust$most.con <- as.integer(gsub("%", "", df_media_trust$most.con))
df_media_trust$cons.con <- as.integer(gsub("%", "", df_media_trust$cons.con))

# calculate total percentage of respondents that have a trust / distrust opinion
df_media_trust$total <- rowSums(df_media_trust[2:6])

# calculate percentages
df_media_trust$cons.lib <- round(df_media_trust$cons.lib / df_media_trust$total, 2)
df_media_trust$most.lib <- round(df_media_trust$most.lib / df_media_trust$total, 2)
df_media_trust$mixed <- round(df_media_trust$mixed / df_media_trust$total, 2)
df_media_trust$most.con <- round(df_media_trust$most.con / df_media_trust$total, 2)
df_media_trust$cons.con <- round(df_media_trust$cons.con / df_media_trust$total, 2)

# update total column
df_media_trust$total <- rowSums(df_media_trust[2:6])

# fix differential due to rounding error
df_media_trust$diff <- 1 - df_media_trust$total
df_media_trust$mixed <- df_media_trust$mixed + df_media_trust$diff
df_media_trust$total <- rowSums(df_media_trust[2:6])

# set variable to order bar graph by
df_media_trust$order <- rowSums(df_media_trust[2:4])

# remove unnecessary columns
df_media_trust <- subset(df_media_trust, select = -c(total, diff))

# rename columns for legend purposes
names(df_media_trust)[2] <- "Consistently Liberal"
names(df_media_trust)[3] <- "Mostly Liberal"
names(df_media_trust)[4] <- "Mixed"
names(df_media_trust)[5] <- "Mostly Conservative"
names(df_media_trust)[6] <- "Consistently Conservative"

# gather columns
df_media_trust <- gather(df_media_trust, "type", "total", 2:6, factor_key=TRUE)

# to remove zero labels during graphing bar graph
df_media_trust$total[df_media_trust$total == 0] <- NA


# --------------------------------------------
# Graphing
# --------------------------------------------

# Styling
# theme constants for graphing
HOUSE_C1 <- "#32b200" # green
SENATE_C1 <- "#a100ff" # purple
HOUSE_C2 <- "#56ff3f" # light green
SENATE_C2 <- "#d284ff" # light purple
HOUSE_C3 <- "#0b6000" # dark green
SENATE_C3 <- "#5a008e" # dark purple
DEMS_C1 <- 'rgba(0, 107, 201, 0.75)' # dark blue
REPS_C1 <- 'rgba(244, 0, 0, 0.75)' # dark red
DEMS_C2 <- 'rgba(201, 229, 255, 0.75)' # light blue
REPS_C2 <- 'rgba(255, 201, 201, 0.75)' # light red
DEMS_C3 <- "#54aaf9" # blue
REPS_C3 <- "#ff5b5b" # red

# set axis styling
f1 <- list(
  family = "serif",
  color = "black", 
  size = 10
)
ax <- list(
  title = "",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f1, 
  showgrid = FALSE
)
ay <- list(
  title = "$ in millions",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f1, 
  showgrid = FALSE
)
ay2 <- list(
  title = "",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f1, 
  showgrid = FALSE
)

# set title annotations for subplots
title1 <- list(
  x = 0.5,
  y = 1,
  text = "Incumbents win with >=60%",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)
title2 <- list(
  x = 0.5,
  y = 1,
  text = "Incumbents win with <60%",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)
title3 <- list(
  x = 0.5,
  y = 1,
  text = "Incumbents lose",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)
title4 <- list(
  x = 0.5,
  y = 1,
  text = "House",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)
title5 <- list(
  x = 0.5,
  y = 1,
  text = "Senate",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)


# Plots
# Plot - Districts Won with 60% of Major Party Vote in Both Chambers, 1944 - 2012
p <- ggplot(data = df_majority, aes(x = year, y = percentage, colour = chamber)) + 
  geom_point(size = 2, stroke = 1, shape = 21) +
  geom_smooth(method = 'lm', span = 5, se = FALSE) + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1940, 2012), breaks = seq(1940,2012,10)) + 
  scale_y_continuous (labels = percent, limits = c(0.2, 1), breaks = seq(0.2,1,.2)) + 
  labs (x=NULL, y=NULL) + 
  scale_colour_manual(values = c(HOUSE_C1,SENATE_C1)) + 
  annotate("text", x = 1995, y = 0.85, label = "House", colour = HOUSE_C1, size = 4, fontface = 'bold') + 
  annotate("text", x = 1965, y = 0.35, label = "Senate", colour = SENATE_C1, size = 4, fontface = 'bold') 
ggplotly(p)
link <- plotly_POST(p, filename = "g1")

# Plot - Swing Districts in the House, 1998 - 2014
p <- ggplot(data = df_swing_seats, aes(x = year, y = total, colour = type)) + 
  geom_line(size = 2, linetype = 'solid') + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1998, 2014), breaks = seq(1998,2014,4)) + 
  scale_y_continuous (limits = c(80, 200), breaks = seq(80,200,20)) + 
  labs (x=NULL, y="# of districts") + 
  scale_colour_manual(values = c(DEMS_C3, REPS_C3, 'grey')) + 
  annotate("text", x = 2006, y = 187, label = "Republicans win by >5%", colour = REPS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 2006, y = 155, label = "Democrats win by >5%", colour = DEMS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 2004, y = 100, label = "Swing seats", colour = 'grey', size = 4, fontface = 'bold')
ggplotly(p)
link <- plotly_POST(p, filename = "g2")

# Plot - Cost of Running for Senate and House (inflation adjusted based on 2012 CPI), 1986-2012
p <- ggplot(data = df_costs, aes(x = year, y = cost, colour = chamber)) + 
  geom_line(size = 2, linetype = 'solid') + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1986, 2012), breaks = seq(1986,2012,4)) + 
  scale_y_continuous (labels = dollar, limits = c(0, 11), breaks = seq(0,11,1)) + 
  labs (x=NULL, y="$ in millions") + 
  scale_colour_manual(values = c(HOUSE_C1,SENATE_C1)) + 
  annotate("text", x = 1994, y = 1.5, label = "House", colour = HOUSE_C1, size = 4, fontface = 'bold') + 
  annotate("text", x = 1996, y = 7, label = "Senate", colour = SENATE_C1, size = 4, fontface = 'bold') 
ggplotly(p)
link <- plotly_POST(p, filename = "g3")
#layout(gg, dragmode = "pan")
#rangeslider(gg)

# Plot - Comparison of spending by House incumbents and challengers based on election outcomes, 1980 - 2012
# p <- ggplot(data = df_house_incum_total, aes(x = year, y = cost, fill = candidate)) +
#   geom_area()
# gg <- ggplotly(p)
# p <- streamgraph(data = df_house_incum, key = "candidate", value = "cost", date = "year", offset = "zero")

# Outcome: House incumbent wins with 60%+
p1 <- plot_ly(df_house_incum_win60plus, x = ~year, y = ~Incumbents, name = 'House Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = HOUSE_C2) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = HOUSE_C1)  %>%
  layout(xaxis = ax, yaxis = ay, legend = list(x = 0, y = 0.95, font = list(family = 'serif', size = 11)), 
         annotations = title1)
# Outcome: House incumbent wins with <60%
p2 <- plot_ly(df_house_incum_win, x = ~year, y = ~Incumbents, name = 'Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = HOUSE_C2, showlegend = FALSE) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = HOUSE_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title2)
# Outcome: House incumbent loses
p3 <- plot_ly(df_house_incum_loss, x = ~year, y = ~Incumbents, name = 'Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = HOUSE_C2, showlegend = FALSE) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = HOUSE_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title3)
p <- subplot(p1, p2, p3, titleY = TRUE)
link <- plotly_POST(p, filename = "g4")

# Plot - Comparison of spending by Senate incumbents and challengers based on election outcomes, 1980 - 2012
# Outcome: Senate incumbent wins with 60%+
p1 <- plot_ly(df_senate_incum_win60plus, x = ~year, y = ~Incumbents, name = 'Senate Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = SENATE_C2) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = SENATE_C1)  %>%
  layout(xaxis = ax, yaxis = ay, legend = list(x = 0, y = 0.95, font = list(family = 'serif', size = 11)), 
         annotations = title1)
# Outcome: House incumbent wins with <60%
p2 <- plot_ly(df_senate_incum_win, x = ~year, y = ~Incumbents, name = 'Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = SENATE_C2, showlegend = FALSE) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = SENATE_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title2)
# Outcome: House incumbent loses
p3 <- plot_ly(df_senate_incum_loss, x = ~year, y = ~Incumbents, name = 'Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = SENATE_C2, showlegend = FALSE) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = SENATE_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title3)
p <- subplot(p1, p2, p3, titleY = TRUE)
link <- plotly_POST(p, filename = "g5")

# Plot - House and Senate spending differentials by election outcomes, 1980 - 2012
p1 <- plot_ly(df_house_incumbents, x = ~year, y = ~diffw60, name = 'Win >=60%', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = HOUSE_C2) %>%
  add_trace(y = ~diffw, name = 'Win <60%', fillcolor = HOUSE_C1)  %>%
  add_trace(y = ~diffl, name = 'Lose', fillcolor = HOUSE_C3)  %>%
    layout(xaxis = ax, yaxis = ay, legend = list(x = 0, y = 0.95, font = list(family = 'serif', size = 11)), 
           annotations = title4)
p2 <- plot_ly(df_senate_incumbents, x = ~year, y = ~diffw60, name = 'Win >=60%', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = SENATE_C2) %>%
  add_trace(y = ~diffw, name = 'Win <60%', fillcolor = SENATE_C1)  %>%
  add_trace(y = ~diffl, name = 'Lose', fillcolor = SENATE_C3)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title5)
p <- subplot(p1, p2, titleY = TRUE)
link <- plotly_POST(p, filename = "g6")

# Plot - Comparison of House and Senate independent expenditure spending by PACs for / against Democrats and Republicans, 1978-2012
p1 <- plot_ly(df_pac_house, x = ~year, y = ~for.repub, name = 'For Republicans', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = REPS_C2) %>%
  add_trace(y = ~against.repub, name = 'Against Republicans', fillcolor = REPS_C1)  %>%
  add_trace(y = ~for.dems, name = 'For Democrats', fillcolor = DEMS_C2)  %>%
  add_trace(y = ~against.dems, name = 'Against Democrats', fillcolor = DEMS_C1)  %>%
  layout(xaxis = ax, yaxis = ay, legend = list(x = 0.01, y = 0.1, font = list(family = 'serif', size = 11)), 
         annotations = title4)
p2 <- plot_ly(df_pac_senate, x = ~year, y = ~for.repub, name = 'For Republicans', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = REPS_C2, showlegend = FALSE) %>%
  add_trace(y = ~against.repub, name = 'Against Republicans', fillcolor = REPS_C1)  %>%
  add_trace(y = ~for.dems, name = 'For Democrats', fillcolor = DEMS_C2)  %>%
  add_trace(y = ~against.dems, name = 'Against Democrats', fillcolor = DEMS_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title5)
p <- subplot(p1, p2, titleY = TRUE)
link <- plotly_POST(p, filename = "g7")
#rangeslider(gg)


# Plot - Senate spending streamgraph
# df_pac_senate_stream <- subset(df_pac_senate, select=-congress)
# df_pac_senate_stream <- gather(df_pac_senate_stream, "type", "total", 2:5, factor_key=TRUE)
# df_pac_senate_stream %>%
#   streamgraph("type", "total", "year", interpolate="cardinal") %>%
#   sg_axis_x(1, "year", "%Y") %>%
#   sg_fill_brewer("PuOr")

# Plot - Ideological polarization in House and Senate, 1947 - 2013
p <- ggplot(data = df_polar_house, aes(x = Year, y = total, colour = type)) + 
  geom_line(size = 2, linetype = 'solid') + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1945, 2015), breaks = seq(1945,2015,10)) + 
  scale_y_continuous (limits = c(-1, 1), breaks = seq(-1,1,0.25)) + 
  labs (x=NULL, y="avg. idealogical position") +
  scale_colour_manual(values = c(HOUSE_C1, DEMS_C3, REPS_C3)) + 
  annotate("text", x = 1960, y = 0.4, label = "Republicans", colour = REPS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 1975, y = -0.4, label = "Democrats", colour = DEMS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 2002, y = -0.2, label = "Entire House", colour = HOUSE_C1, size = 4, fontface = 'bold')
p1 <- ggplotly(p)
p <- ggplot(data = df_polar_senate, aes(x = Year, y = total, colour = type)) + 
  geom_line(size = 2, linetype = 'solid') + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1945, 2015), breaks = seq(1945,2015,10)) + 
  scale_y_continuous (limits = c(-1, 1), breaks = seq(-1,1,0.25)) + 
  labs (x=NULL, y=NULL) +
  scale_colour_manual(values = c(SENATE_C1, DEMS_C3, REPS_C3)) + 
  annotate("text", x = 1960, y = 0.4, label = "Republicans", colour = REPS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 1975, y = -0.45, label = "Democrats", colour = DEMS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 2000, y = 0.15, label = "Entire Senate", colour = SENATE_C1, size = 4, fontface = 'bold')
p2 <- ggplotly(p)
p <- subplot(p1, p2, titleY = TRUE)
link <- plotly_POST(p, filename = "g8")

# Plot - Media trust by news sources and ideological position, 2014
p <- ggplot(data = df_media_trust, aes(reorder(Source, order), total)) + 
  geom_bar(stat = "identity", aes(fill = type), position = "fill") + 
  geom_text(size = 2, position = position_stack(vjust = 0.5), aes(label = paste0(round(total*100,1),"%")), colour = "white") + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "right", 
    legend.title = element_blank(),
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  scale_y_continuous (labels = percent, limits = c(0, 1.1), breaks = seq(0,1.1,0.2)) + 
  labs (x=NULL, y=NULL) +
  scale_fill_manual(values=c(DEMS_C1, DEMS_C3, "grey75", REPS_C3, REPS_C1)) +
  coord_flip()
p <- ggplotly(p)
link <- plotly_POST(p, filename = "g9")

# Plotly version (no good)
# p <- plot_ly(df_media_trust, x = ~df_media_trust$Consistently.liberal, y = ~df_media_trust$Source, type = 'bar', orientation = 'h',
#              marker = list(color = 'rgba(38, 24, 74, 0.8)',
#                            line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
#   add_trace(x = ~df_media_trust$Mostly.liberal, marker = list(color = 'rgba(71, 58, 131, 0.8)')) %>%
#   add_trace(x = ~df_media_trust$Mixed, marker = list(color = 'rgba(122, 120, 168, 0.8)')) %>%
#   add_trace(x = ~df_media_trust$Mostly.conservative, marker = list(color = 'rgba(164, 163, 204, 0.85)')) %>%
#   add_trace(x = ~df_media_trust$Consistently.conservative, marker = list(color = 'rgba(190, 192, 213, 1)')) %>%
#   layout(xaxis = list(title = "",
#                       showgrid = FALSE,
#                       showline = FALSE,
#                       showticklabels = FALSE,
#                       zeroline = FALSE,
#                       domain = c(0.15, 1)),
#          yaxis = list(title = "",
#                       showgrid = FALSE,
#                       showline = FALSE,
#                       showticklabels = FALSE,
#                       zeroline = FALSE),
#          barmode = 'stack',
#          paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
#          margin = list(l = 120, r = 10, t = 140, b = 80),
#          showlegend = FALSE) %>%
#   # labeling the y-axis
#   add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = df_media_trust$Source,
#                   xanchor = 'right',
#                   text = df_media_trust$Source,
#                   font = list(family = 'Arial', size = 12,
#                               color = 'rgb(67, 67, 67)'),
#                   showarrow = FALSE, align = 'right') %>%
#   # labeling the top legend
#   add_annotations(xref = 'x', yref = 'paper',
#                   x = c(25, 75, 125, 175, 225),
#                   y = 1.15,
#                   text = c('A', 'B', 'C', 'D', 'E'),
#                   font = list(family = 'Arial', size = 12,
#                               color = 'rgb(67, 67, 67)'),
#                   showarrow = FALSE)