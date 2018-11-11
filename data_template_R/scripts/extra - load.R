# Load required packages
library(readxl)
library(tidyverse)


################ IMPORTING DATA ################ 

# Reading in CSV files
read_csv("data/mydata.csv")

# Read in and save data
mydata <- read_csv("data/mydata.csv")

# reading in an Excel file
excel_sheets("data/mydata.xlsx")
read_excel("data/mydata.xlsx", sheet = "PICK_ME_FIRST!")


#############
# YOUR TURN #
#############



# dimension of data
data(mtcars)
nrow(mtcars)
ncol(mtcars)
dim(mtcars)

# what are the variables
names(mtcars)
glimpse(mtcars)
View(mtcars)

# missing values
data(airquality)
is.na(airquality)
sum(is.na(airquality))
colSums(is.na(airquality))
clean_data <- na.omit(airquality)

################ DATA STRUCTURES ################ 

# Creating vectors
c("Learning", "to", "create", "character", "vectors")
c(3, 2, 10, 55)
c(TRUE, FALSE, FALSE, FALSE, TRUE)
6:15
15.5:6.75

# Indexing vectors
v1 <- 1:10
v1[4]
v1[4:7]
v1[c(4, 3, 4)]
v1[v1 > 6]

# different summaries of vectors
length(v1)
summary(v1)
mean(v1)
median(v1)
v1 > 5


#############
# YOUR TURN #
#############



# Creating matrices
set.seed(123)
v1 <- sample(1:10, 25, replace = TRUE)
m1 <- matrix(v1, nrow = 5)
m1

# Indexing matrices
m1[1, 3]
m1[ , 1:3]
m1[1:3,  ]

# Summaries of matrices
summary(m1)
mean(m1)
mean(m[1,])
rowMeans(m1)
colMeans(m1)
rowSums(m1)
colSums(m1)
m > .5
sum(m > .5)
which(m > .5)
m[m > .5]


#############
# YOUR TURN #
#############



# Data frames
raw_data <- read_csv("data/CustomerData.csv")


# indexing data frames
raw_data[, 4]
raw_data[, "Gender"]
raw_data[, 1:3]
raw_data[, c("CustomerID", "Region", "TownSize")]
raw_data[1, ]

# check out the first 6 rows with:
head(raw_data)




# Lists
model <- lm(mpg ~ wt, data = mtcars)
summary(model)
names(model)
str(model)

# Indexing lists
model[“residuals”]
model[[“residuals”]]
model$residuals
