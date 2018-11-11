#explore
--------------------------------------------

head(rivers)    # peek at the data set
# 735 320 325 392 524 450

length(rivers)  # how many rivers were measured?
# 141
summary(rivers) # what are some summary statistics?
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  135.0   310.0   425.0   591.2   680.0  3710.0

# make a stem-and-leaf plot (a histogram-like data visualization)
stem(rivers)
stem(log(rivers))

# make a histogram:
hist(rivers, col="#333333", border="white", breaks=25) # play around with these parameters
hist(log(rivers), col="#333333", border="white", breaks=25)

# Draw from a standard Gaussian 9 times
rnorm(9)

length(c("Call","me","Ishmael")) # 3
# You can do regex operations on character vectors:
substr("Fortuna multis dat nimis, nulli satis.", 9, 15) # "multis "
gsub('u', 'ø', "Fortuna multis dat nimis, nulli satis.") # "Fortøna møltis dat nimis, nølli satis."
# R has several built-in character vectors:
letters
# =>
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"
month.abb # "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

# FACTORS
# The factor class is for categorical data
# Factors can be ordered (like childrens' grade levels) or unordered (like gender)
factor(c("female", "female", "male", NA, "female"))
#  female female male   <NA>   female
# Levels: female male
# The "levels" are the values the categorical data can take
# Note that missing data does not enter the levels
levels(factor(c("male", "male", "female", NA, "female"))) # "female" "male"
# If a factor vector has length 1, its levels will have length 1, too
length(factor("male")) # 1
length(levels(factor("male"))) # 1
# Factors are commonly seen in data frames, a data structure we will cover later
data(infert) # "Infertility after Spontaneous and Induced Abortion"
levels(infert$education) # "0-5yrs"  "6-11yrs" "12+ yrs"

# LOOPS
# We've got for loops
for (i in 1:4) {
  print(i)
}
# We've got while loops
a <- 10
while (a > 4) {
  cat(a, "...", sep = "")
  a <- a - 1
}
# Keep in mind that for and while loops run slowly in R
# Operations on entire vectors (i.e. a whole row, a whole column)
# or apply()-type functions (we'll discuss later) are preferred

# IF/ELSE
# Again, pretty standard
if (4 > 3) {
  print("4 is greater than 3")
} else {
  print("4 is not greater than 3")
}

# FUNCTIONS
# Defined like so:
jiggle <- function(x) {
  x = x + rnorm(1, sd=.1) #add in a bit of (controlled) noise
  return(x)
}
# Called like any other R function:
jiggle(5)   # 5±ε. After set.seed(2716057), jiggle(5)==5.005043

vec <- c(8, 9, 10, 11)
# We can also search for the indices of specific components,
which(vec %% 2 == 0)    # 1 3
# grab just the first or last few entries in the vector,
head(vec, 1)    # 8
tail(vec, 2)    # 10 11
# or figure out if a certain value is in the vector
any(vec == 10) # TRUE
# and R has many built-in functions to summarize vectors
mean(vec)   # 9.5
var(vec)    # 1.666667
sd(vec)     # 1.290994
max(vec)    # 11
min(vec)    # 8
sum(vec)    # 38
# Some more nice built-ins:
5:15    # 5  6  7  8  9 10 11 12 13 14 15
seq(from=0, to=31337, by=1337)

# There are many twisty ways to subset data frames, all subtly unalike
students$year   # 3  2  2  1  0 -1
students[,2]    # 3  2  2  1  0 -1
students[,"year"]   # 3  2  2  1  0 -1

mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# Use apply(X, MARGIN, FUN) to apply function FUN to a matrix X
# over rows (MAR = 1) or columns (MAR = 2)
# That is, R does FUN to each row (or column) of X, much faster than a
# for or while loop would do
apply(mat, MAR = 2, jiggle)
# =>
#      [,1] [,2]
# [1,]    3   15
# [2,]    7   19
# [3,]   11   23
# Other functions: ?lapply, ?sapply


#load
--------------------------------------------

# "pets.csv" is a file on the internet
# (but it could just as easily be a file on your own computer)
require(RCurl)
pets <- read.csv(textConnection(getURL("http://learnxinyminutes.com/docs/pets.csv")))
pets
head(pets, 2) # first two rows
tail(pets, 1) # last row

# To save a data frame or matrix as a .csv file
write.csv(pets, "pets2.csv") # to make a new .csv file


#model
--------------------------------------------

# Linear regression!
linearModel <- lm(price  ~ time, data = list1)
summary(linearModel)$coefficients 

# GENERAL LINEAR MODELS
# Logistic regression
set.seed(1)
list1$success = rbinom(length(list1$time), 1, .5) # random binary
glModel <- glm(success  ~ time, data = list1, 
               family=binomial(link="logit"))
glModel

# BUILT-IN PLOTTING FUNCTIONS
# Scatterplots!
plot(list1$time, list1$price, main = "fake data")
# Plot regression line on existing plot
abline(linearModel, col = "red")
# Get a variety of nice diagnostics
plot(linearModel)
# Histograms!
hist(rpois(n = 10000, lambda = 5), col = "thistle")
# Barplots!
barplot(c(1,4,5,1,2), names.arg = c("red","blue","purple","green","yellow"))



v1 <- c(1,2,3,4,5)
v2 <- c(6,7,8,9,10)
v3 <- c(11,12,13,14,15)
v4 <- c(16,17,18,19,20)
cbind(v1,v2,v3,v4)
rbind(v1,v2,v3,v4)

or, 

v <- seq(from=1,to=20,by=1)
matrix(v, nrow=4, ncol=5, byrow = TRUE)


matrix20 <- matrix(v, nrow=4, ncol=5, byrow=TRUE)
colnames(matrix20) <- c("Col1","Col2","Col3","Col4","Col5")
rownames(matrix20) <- c("Row1","Row2","Row3","Row4")

length(v1)
nrow(matrix20)
ncol(matrix20)

dataset <- read.table("C:\\Datasets\\haberman.csv", header=FALSE, sep=",")
dataset <- read.csv("C:\\Datasets\\haberman.csv", header=FALSE)



norm_vec <- rnorm(n=10, mean=5, sd=2)
exp_vec <- rexp(n=100, rate=3)
pois_vec <- rpois(n=50, lambda=6)
unif_vec <- runif(n=20, min=1, max=9)
bin_vec <- rbinom(n=20, size=1000, prob=0.7)

sample(v, size=25, replace=FALSE)
set.seed(100) # to generate the same random vector


# mean, var, sd, min, max, sum, abs, sqrt
# rowSum, colSum - to find the row and column sums for a matrix
# cor, cov - correlation and covariance of a matrix


for (i in 1:10){ # for (i in c(1:3,5:7))
  if (i %% 2 == 0){
    cat(paste(i, "is even.\n", sep=" ")) # use paste to concatenate strings
    }
  }


# use which to pick out the indices of elements in a vector that satisfy a certain property:
which(v >= 0) # indices of nonnegative elements of v
v[which(v >= 0)] # nonnegative elements of v



# to put all three plots in a 1 × 3 matrix, use par(mfrow=c(1,3))
# to put each plot in its own window, use win.graph() to create new windows.
plot(dataset[,1], dataset[,3], main="Scatterplot", xlab="Age", ylab="Number of Nodes", pch=20)
hist(dataset[,2], main="Histogram", xlab="Year", ylab="Count")
boxplot(dataset[,1], main="Boxplot", xlab="Age")

# linear regression
lm_model <- lm(y ~ x1 + x2, data=as.data.frame(cbind(y,x1,x2)))
summary(lm_model)
lm_model$coefficients

predicted_values <- predict(lm_model, newdata=as.data.frame(cbind(x1_test, x2_test)))

# logistic regression
glm_mod <-glm(y ~ x1+x2, family=binomial(link="logit"), data=as.data.frame(cbind(y,x1,x2)))

# k-means clustering (m = # of clusters)
kmeans_model <- kmeans(x=dataset, centers=m)

# knn
knn_model <- knn(train=X_train, test=X_test, cl=as.factor(labels), k=K)

# naive bayes, part of e1071 package
nB_model <- naiveBayes(y ~ x1 + x2, data=as.data.frame(cbind(y,x1,x2)))

# decision trees, part of rpart package
cart_model <- rpart(y ~ x1 + x2, data=as.data.frame(cbind(y,x1,x2)), method="class")



#plot
--------------------------------------------
  
  
# to download the data directly:
gapminder <- read.csv("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder-FiveYearData.csv")

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.5)

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.5) +
  scale_x_log10()

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.5) +
  # clean the x-axis breaks
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000),
                limits = c(1, 120000))

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  # add scatter points
  geom_point(alpha = 0.5) +
  # log-scale the x-axis
  scale_x_log10() +
  # change labels
  labs(title = "GDP versus life expectancy in 2007",
       x = "GDP per capita (log scale)",
       y = "Life expectancy",
       size = "Popoulation",
       color = "Continent")


ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  # add scatter points
  geom_point(alpha = 0.5) +
  # log-scale the x-axis
  scale_x_log10() +
  # change labels
  labs(title = "GDP versus life expectancy in 2007",
       x = "GDP per capita (log scale)",
       y = "Life expectancy",
       size = "Popoulation (millions)",
       color = "Continent") +
  # change the size scale
  scale_size(range = c(0.1, 10),
             breaks = 1000000 * c(250, 500, 750, 1000, 1250),
             labels = c("250", "500", "750", "1000", "1250")) 


ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  # add scatter points
  geom_point(alpha = 0.5) +
  # log-scale the x-axis
  scale_x_log10() +
  # change labels
  labs(title = "GDP versus life expectancy in 2007",
       x = "GDP per capita (log scale)",
       y = "Life expectancy",
       size = "Popoulation (millions)",
       color = "Continent") +
  # change the size scale
  scale_size(range = c(0.1, 10),
             breaks = 1000000 * c(250, 500, 750, 1000, 1250),
             labels = c("250", "500", "750", "1000", "1250")) +
  # add faceting
  facet_wrap(~continent)


p <- ggplot(gapminder_2007) +
  # add scatter points
  geom_point(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop),
             alpha = 0.5) +
  # add some text annotations for the very large countries
  geom_text(aes(x = gdpPercap, y = lifeExp + 3, label = country),
            color = "grey50",
            data = filter(gapminder_2007, pop > 1000000000 | country %in% c("Nigeria", "United States"))) +
  # clean the axes names and breaks
  scale_x_log10(limits = c(200, 60000)) +
  # change labels
  labs(title = "GDP versus life expectancy in 2007",
       x = "GDP per capita (log scale)",
       y = "Life expectancy",
       size = "Popoulation",
       color = "Continent") +
  # change the size scale
  scale_size(range = c(0.1, 10),
             # remove size legend
             guide = "none") +
  # add a nicer theme
  theme_classic() +
  # place legend at top and grey axis lines
  theme(legend.position = "top",
        axis.line = element_line(color = "grey85"),
        axis.ticks = element_line(color = "grey85"))

# save the plot
ggsave("beautiful_plot.png", p, dpi = 300, width = 6, height = 4)



