# Load required packages
library(tidyverse)

# Data we're using
mpg


#############
# YOUR TURN #
#############



# Creating our canvas
ggplot(data = mpg, aes(x = displ, y = hwy))


# Using geoms to plot our data
## histogram
ggplot(data = mpg, aes(x = hwy)) +
  geom_histogram()

## frequency polygram
ggplot(data = mpg, aes(x = hwy)) +
  geom_freqpoly()

## density chart
ggplot(data = mpg, aes(x = hwy)) +
  geom_density()

## bar chart
ggplot(data = mpg, aes(x = class)) +
  geom_bar()

## scatter plot
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point()

## box plot
ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_boxplot()

## violin plot
ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_violin()


#############
# YOUR TURN #
#############



# Non-mapping aesthetics
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue", size = 2, shape = 17, alpha = .5)

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

# Mapping aesthetics
ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()


#############
# YOUR TURN #
#############



# facets for small multiples
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)


#############
# YOUR TURN #
#############



# Adding titles
ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_boxplot() +
  ggtitle("Displacement vs Highway MPG”,
          subtitle = “Data from 1999 & 2008")

# Axis scales
ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .25) +
  scale_x_log10()

# Axis titles and labels
ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .25)  +
  scale_y_continuous(name = "Median Sales Price", labels = scales::dollar) +
  scale_x_log10(name = "Total Sales Volume", labels = scales::comma)

# Putting it all together
ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .25) +
  scale_y_continuous(name = "Median Sales Price", labels = scales::dollar) +
  scale_x_log10(name = "Total Sales Volume", labels = scales::comma) +
  ggtitle("Texas Housing Sales",
          subtitle = "Sales data from 2000-2010 provided by the TAMU real estate center")

# Overplotting
ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .25)  +
  scale_x_log10() +
  geom_smooth()

ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .25)  +
  scale_x_log10() +
  geom_smooth(method = "lm")

# Layering and facetting
ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .25)  +
  scale_x_log10() +
  geom_smooth(method = "lm") +
  facet_wrap(~ month)

#############
# YOUR TURN #
#############



#