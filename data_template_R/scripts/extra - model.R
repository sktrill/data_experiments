# scrape data
# include twitter handles
# perform LDA
# load to shiny app
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(xml2)
library(rvest)
library(RColorBrewer)
library(ggplot2)
library(twitteR)

library(MASS)
library(class)
library(ISLR)



# Setup Twitter API credentials (removed)


setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw <- twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 1e4, since = '2016-11-08', retryOnRateLimit = 1e3)
d <- twitteR::twListToDF(tw)

tw <- twitteR::friendships('@thekotecha', retryOnRateLimit = 1e3)

tw <- twitteR::getUser('@thekotecha', retryOnRateLimit = 1e3)
d <- twitteR::twListToDF(tw)

tw <- twitteR::getUser('@statwonk', retryOnRateLimit = 1e3)
tw$getFriendIDs(n = NULL)

tw <- friendships(screen_names = '@statwonk', retryOnRateLimit = 1e3)
tw$getProfileImageUrl()
tw$getFollowersCount()
tw$getFriendsCount()
tw$getFriendsCount()
tw$getStatusesCount()
tw$getFriendIDs()

# contours

x=seq(-pi ,pi ,length =50)

y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour (x,y,f,nlevels =45, add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)

image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa ,theta =30)
persp(x,y,fa ,theta =30, phi =20)
persp(x,y,fa ,theta =30, phi =70)
persp(x,y,fa ,theta =30, phi =40)



# --------------------------------------------
# Classification - Log Reg, LDA, QDA, Clustering
# --------------------------------------------

# log reg

cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
glm.fits <- glm(Direction∼Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
             data=Smarket ,family =binomial )

summary(glm.fits)

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)


glm.pred = rep ("Down" ,1250)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction)

mean(glm.pred == Direction)

train = (Year < 2005)
Smarket.2005 = Smarket [!train ,]
dim(Smarket.2005)
Direction.2005 = Direction [!train]

glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
             data = Smarket ,family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")

glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fits = glm(Direction ~ Lag1 + Lag2 ,data=Smarket, family = binomial,
               subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106/(106+76)



# LDA
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit , Smarket.2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1] >=.5)
sum(lda.pred$posterior[,1] <.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

sum(lda.pred$posterior[,1] > .9)



# qda
qda.fit = qda(Direction ~ Lag1 + Lag2 , data = Smarket, subset = train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)



# knn
train.X = cbind(Lag1, Lag2)[train, ]
test.X = cbind(Lag1, Lag2)[!train, ]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
(83+43) /252

knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred== Direction.2005)




# as of Mar-18

# --------------------------------------------
# Scraping / Loading Data
# --------------------------------------------
# based on page structure as of Feb-18
selector <- ".email, h4, .title"

# read html page from Zapier's About page (with pictures of all team members)
html <- read_html("https://zapier.com/about/")

# extract nodes based on css selector
html_cast <- html_nodes(html, selector)
cast_text <- html_text(html_cast)
cast_img <- html_nodes(html, "img") %>% html_attr("src")
# 121 employee as of Feb-18

# zapier - combined data frame of all Zapier employees with images
# convert employee list to dataframe for reshaping
zapier <- as.data.frame(cast_text)

# number of employees, 121
n <- nrow(zapier)/3

# list (1, 1, 1, 2, 2, 2...) to identify sets of rows for spread fcn
list <- rep(seq_len(n), each = 3)
zapier$ID <- list

# spread name, title and email of all employees from scraped data
list <- rep(c("name", "title", "email"), n)
zapier <- cbind(zapier,list)
zapier <- spread(zapier, list, cast_text)

# add list of images
cast_img <- cast_img[-c(1)]
zapier <- cbind(zapier, cast_img)

# remove ID column
zapier <- zapier[,-1]

# save zapier file
write.csv(zapier, file="data/zap_out.csv" , row.names = FALSE)

# load zapier file with twitter handles
# the twitter handles were added manually (there was no quicker way to hack this)
zapier <- tbl_df(read.csv("data/zap_in.csv", stringsAsFactors = FALSE))

# remove all employees without twitter accounts
zapier <- zapier %>% filter(twitter != "")



# --------------------------------------------
# Decision Trees and Random Forests
# --------------------------------------------


train <- fullset %>% 
  filter(trainYN == 1)
test <- fullset %>% 
  filter(trainYN == 0)

# prediction #1 - using decision trees
# build decision tree
my_tree <- rpart(wageHike ~ MinWage + Housing + family + Food + Medical + ChildCare + union + leg + gov + rtw, data = train, method = "class")

# visualize the decision tree using plot() and text()
plot(my_tree)
text(my_tree)

# plot fancy tree
fancyRpartPlot(my_tree, f)

# make prediction using the test set
prediction_tree <- predict(my_tree, test, type = "class")

# create data frame with two columns state and wageHike
solution_tree <- data.frame(state = test$region, wageHike = prediction_tree)

# create summary by state
by_state <- group_by(solution_tree, state)
solution_tree <- summarise(by_state,  
                           decTree = getMode(wageHike))


# prediction #2 - using random forrest
# clean up factor issues for random forrest function
train$gov <- factor(train$gov)
train$leg <- factor(train$leg)
train$rtw <- factor(train$rtw)
test$gov <- factor(test$gov)
test$leg <- factor(test$leg)
test$rtw <- factor(test$rtw)

# get forrest prediction and plot accuracy and gini coeff. charts for features
forrest <- randomForest(as.factor(wageHike) ~  MinWage + Housing + family + Food + Medical + ChildCare + union + leg + gov + rtw, data=train, importance=TRUE, ntree=2000)
varImpPlot(forrest)

# make prediction using the complete dataset
prediction_forrest <- predict(forrest, test)

# create data frame with two columns per kaggle rules: PassengerId & Survived
solution_forrest <- data.frame(state = test$region, wageHike = prediction_forrest)

# create summary by state
by_state <- group_by(solution_forrest, state)
solution_forrest <- summarise(by_state,  
                              ranForrest = getMode(wageHike))


# prediction #3 - using party package (forrest of inference trees)
set.seed (400)
party <- cforest(as.factor(wageHike) ~  MinWage + Housing + family + Food + Medical + ChildCare + union + leg + gov + rtw, data = train, controls = cforest_unbiased(ntree=2000, mtry=3))

# make prediction using the complete dataset
prediction_inf_forrest <- predict(party, test, OOB=TRUE, type="response")

# create data frame with two columns per kaggle rules: PassengerId & Survived
solution_inf_forrest <- data.frame(state = test$region, wageHike = prediction_inf_forrest)

# create summary by state
by_state <- group_by(solution_inf_forrest, state)
solution_inf_forrest <- summarise(by_state,  
                                  infForrest = getMode(wageHike))

# create a dataframe joining all three prediction models
solution <- inner_join(solution_tree, solution_forrest, by = "state")
solution <- inner_join(solution, solution_inf_forrest, by = "state")




