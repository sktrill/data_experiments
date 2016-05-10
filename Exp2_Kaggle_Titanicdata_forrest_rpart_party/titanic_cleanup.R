library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)

# load raw data from csv file
setwd("D:/Github/Experiments_Data_R_Py/Exp2_Kaggle_Titanicdata_forrest_rpart_party")
titanic <- tbl_df(read.csv("titanic_original.csv",stringsAsFactors=FALSE))

# fix known missing value for passenger that embarked at Southampton
titanic$embarked[titanic$embarked == ""] <- "S"

# fill 'NA' values in age with the mean of known values
val <- mean(titanic$age, na.rm = TRUE)
titanic$age[is.na(titanic$age)] <- val
# could also use median or mean + normal distribution (i.e. rnorm)

# fill in dummy value for passengers that did not make it to life boats :(
titanic$boat[titanic$boat == ""] <- "NA"

# add new column to indicate whether cabin number exists
titanic <- titanic %>% mutate(has_cabin_number = cabin != "")

# create 'refine_clean.csv' file
write.table(titanic,file="titanic_clean.csv",sep = ",", row.names=FALSE,qmethod="escape", quote=TRUE)
