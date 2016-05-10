library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
library(gridExtra, warn.conflicts=FALSE, quietly=TRUE) 
library(GGally, warn.conflicts=FALSE, quietly=TRUE)
library(RColorBrewer, warn.conflicts=FALSE, quietly=TRUE)
library(memisc, warn.conflicts=FALSE, quietly=TRUE)
library(MASS, warn.conflicts=FALSE, quietly=TRUE)
library(lattice, warn.conflicts=FALSE, quietly=TRUE)

setwd("D:/Github/Experiments_Data_R_Py/Exp4_Diamonds_ggplot2_ggally_gridextra")

# experiments in R graphing packages using the diamonds database
summary(diamonds)
summary(diamonds$carat)
summary(diamonds$price)

# histogram of diamond carat by cut
qplot(x = carat, data = diamonds, geom="histogram", binwidth = 0.2) + 
  facet_wrap(~cut, ncol=2) + 
  ggsave("caratcount_by_cut.png")

# histogram of diamond price by cut
ggplot(data = diamonds, aes(x = price)) + 
  geom_histogram(binwidth = 0.1, aes(fill = ..count..)) + 
  scale_x_log10() + 
  scale_fill_gradient("count", low = "green", high = "red") +
  facet_wrap(~cut, ncol = 2) + 
  ggsave("pricecount_by_cut.png")

# density plot of diamond price by cut
ggplot(data = diamonds, aes(x = price, fill = cut)) + 
  geom_density(alpha = 0.2) + 
  scale_x_log10() + 
  xlab('Price') + 
  ylab('Count') +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 2)) +
  facet_grid(cut ~ .) + 
  ggsave("pricedensity_by_cut.png")

# diamond price vs carat with means and quantile
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point(alpha = 1/10, 
             position = position_jitter(h = 2), 
             color = 'orange') + 
  coord_trans(y = "log10") + 
  geom_line(stat = 'summary', fun.y = mean) + 
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), 
            linetype = 2, color = 'Red') + 
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), 
            linetype = 2, color = 'Blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), 
            linetype = 2, color = 'Black') + 
  ggsave("price_by_carat_quantiles.png")

# diamond price by carat for each cut
ggplot(data = diamonds, aes(x = 1 * round(carat/1), y = price/carat)) + 
  geom_line(aes(color = cut), stat = 'summary', fun.y = mean) +
  ggsave("mean_price_for_carat_cut.png")

# without carat rounding
ggplot(data = diamonds, aes(x = carat, y = price/carat)) + 
  geom_smooth(aes(color = cut)) +
  geom_line(aes(color = cut), stat = 'summary', fun.y = mean)

# calculate correlation between price and carat
cor.test(diamonds$carat, diamonds$price, method = 'pearson')

# diamond price by carat for upto 95% quantile along with linear regression
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() + 
  xlim(0, quantile(diamonds$carat, 0.95)) + 
  ylim(0, quantile(diamonds$price, 0.95)) + 
  geom_smooth(method='lm', color = 'red') +
  ggsave("price_by_carat_linear_regression.png")

# diamond price by carat for upto 95% quantile without linear regression
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() + 
  xlim(0, quantile(diamonds$carat, 0.95)) + 
  ylim(0, quantile(diamonds$price, 0.95)) + 
  geom_smooth(color = 'red') +
  ggsave("price_by_carat.png")

# diamond price by carat for each clarity rating along with linear regression
ggplot(data = diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point(fill = '#F79420', shape = 21, 
             alpha = 1/2, 
             size = 1, 
             position = 'jitter') +
  scale_y_log10(breaks = c(350, 1000, 5000, 10000, 15000)) + 
  xlim(0, quantile(diamonds$carat, 0.95)) + 
  #ylim(0, quantile(diamonds$price, 0.95)) + 
  scale_color_brewer(type = 'div', 
                     guide = guide_legend(title = 'Clarity', reverse = TRUE, 
                                          override.aes= list(alpha = 1, size = 2))) +
  ggtitle('Price (log10) by Carat') + 
  geom_smooth(method = 'lm') + 
  ggsave("price_by_carat_for_clarity.png")

# calculate linear model
m1 <- lm (I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)

# explore the model
summary(m1)
anova(m1)
fitted(m1)
coef(m1)
residuals(m1)
head(sort(table(diamonds$carat), decreasing = T))

# run estimate based on the model
df <- data.frame(carat = 1.00, cut = "Very Good",color="I",clarity="VS1")
modelEst <- predict (m5, df, interval = "prediction", level = 0.95)
exp(modelEst)