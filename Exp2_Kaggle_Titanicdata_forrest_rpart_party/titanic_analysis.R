library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
library(rpart, warn.conflicts=FALSE, quietly=TRUE)
library(randomForest, warn.conflicts=FALSE, quietly=TRUE)
library(party, warn.conflicts=FALSE, quietly=TRUE)

# load packages to create a fancy version of the decision tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# load raw data from Kaggle csv files
setwd("D:/Github/Experiments_Data_R_Py/Exp2_Kaggle_Titanicdata_forrest_rpart_party")
train <- tbl_df(read.csv("train.csv",stringsAsFactors=FALSE))
test <- tbl_df(read.csv("test.csv",stringsAsFactors=FALSE))
# load clearned up titanic file (titanic_clean.csv)
train_two <- tbl_df(read.csv("titanic_clean.csv",stringsAsFactors=FALSE))

# check variables / features
str(train)
str(test)

# percentage of passengers that survived
prop.table(table(train$Survived))

# percentage of passengers that survived by gender (row-wise proportions)
prop.table(table(train$Sex, train$Survived), margin=1)

# add new column to indicate '1' if passenger was a child (age < 18)
train$Child <- 0
train$Child[is.na(train$Age)] <- NA
train$Child[!is.na(train$Age) & train$Age < 18] <- 1

# percentage of passengers that survived by age (child)
prop.table(table(train$Child,train$Survived), margin = 1)

# build percentage aggregrate of sex and gender
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x){sum(x)/length(x)})

# build percentage aggregrate of passenger class, sex and gender
aggregate(Survived ~ Pclass + Child + Sex, data = train, FUN = function(x){sum(x)/length(x)})
# result clearly shows a correlation of survival to passenger class, sex and age

# prediction #1 - using decision trees
# build decision tree
my_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# visualize the decision tree using plot() and text()
plot(my_tree)
text(my_tree)

# plot fancy tree
fancyRpartPlot(my_tree, f)

# make prediction using the test set
prediction_tree <- predict(my_tree, train_two, type = "class")

# create data frame with two columns per kaggle rules: PassengerId & Survived
kaggle_solution_tree <- data.frame(PassengerId = train_two$Name, Survived = prediction_tree)

# csv solution file
write.csv(kaggle_solution_tree, file="kaggle_soln_dectree.csv" , row.names = FALSE)

# feature engineering (trying extra features of title and family-size)
# train_two <- train
# train_two$family_size <- train_two$SibSp + train_two$Parch + 1
# my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train_two, method = "class")

# prediction #2 - using random forrest
# clean up fare and factor issues for random forrest function
train_two$Fare[is.na(train_two$Fare)] <- 0;
train_two$Sex <- factor(train_two$Sex)
train_two$Embarked <- factor(train_two$Embarked)

# create family_size and title features
train_two$family_size <- train_two$SibSp + train_two$Parch + 1

# get forrest prediction and plot accuracy and gini coeff. charts for features
forrest <- randomForest(as.factor(Survived) ~  Pclass + Sex + Age + Fare + SibSp + Parch + family_size + Embarked, data=train_two, importance=TRUE, ntree=2000)
varImpPlot(forrest)

# make prediction using the complete dataset
prediction_forrest <- predict(forrest, train_two)

# create data frame with two columns per kaggle rules: PassengerId & Survived
kaggle_solution_forrest <- data.frame(PassengerId = train_two$Name, Survived = prediction_forrest)

# csv solution file
write.csv(kaggle_solution_forrest, file="kaggle_soln_forrest.csv" , row.names = FALSE)

# prediction #3 - using party package (forrest of inference trees)
set.seed (400)
party <- cforest(as.factor(Survived) ~  Pclass + Sex + Age + Fare + SibSp + Parch + family_size + Embarked, data=train_two, controls=cforest_unbiased(ntree=2000, mtry=3))

# make prediction using the complete dataset
prediction_inf_forrest <- predict(party, train_two, OOB=TRUE, type="response")

# create data frame with two columns per kaggle rules: PassengerId & Survived
kaggle_solution_inf_forrest <- data.frame(PassengerId = train_two$Name, Survived = prediction_inf_forrest)

# csv solution file
write.csv(kaggle_solution_inf_forrest, file="kaggle_soln_inf_forrest.csv" , row.names = FALSE)
