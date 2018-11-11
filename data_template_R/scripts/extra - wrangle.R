# Load required packages
library(nycflights13)
library(tidyverse)

# check out the data
flights

#############
# YOUR TURN #
#############



# Filtering
filter(flights, month == 1)
filter(flights, month == 1, day == 1)
filter(flights, month == 1, day == 1, dep_delay > 0)

filter(flights, month == 12, day == 25)

# logical tests
12 == 12
12 <= c(12, 11)
12 %in% c(12, 11, 8)
x <- c(12, NA, 11, NA, 8)
is.na(x)

# multiple logical tests
12 == 12 & 12 < 14
12 == 12 & 12 < 10
12 == 12 | 12 < 10
any(12 == 12, 12 < 10)
all(12 == 12, 12 < 10)

# multiple logical tests inside filter()
filter(flights, month == 12, day == 25)
filter(flights, month == 12 & day == 25)

filter(flights, month == 11 | month == 12)
filter(flights, month %in% c(11, 12))

#############
# YOUR TURN #
#############



# selecting variables
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

# selecting variables with naming patterns
select(flights, ends_with("time"))
select(flights, c(carrier, ends_with("time"), contains("delay")))

# change variable placement
select(flights, time_hour, air_time, everything())

# rename a variable
rename(flights, ANNOYING = dep_delay)

#############
# YOUR TURN #
#############


# Ordering data
arrange(flights, dep_delay)
arrange(flights, dep_delay, arr_delay)
arrange(flights, desc(dep_delay))



#############
# YOUR TURN #
#############


# let's reduce our data
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
flights_sml

# create new variables with mutate()
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       gain_per_hour = gain / hours)

# just keep the variables you created with transmute
transmute(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       gain_per_hour = gain / hours)


# many ways to use functions inside mutate & transmute...for example:
transmute(flights,
          normalized_delay = dep_delay / mean(dep_delay, na.rm = TRUE))

transmute(flights,
          log_air_time = log2(air_time),
          exp_delay = exp(dep_delay))

transmute(flights,
          dep_delay = dep_delay,
          lag_delay = lag(dep_delay),
          sum_delay = cumsum(dep_delay))


#############
# YOUR TURN #
#############


# compute summary statistics with summarise
summarise(flights, dep_delay_mean = mean(dep_delay, na.rm = TRUE))

summarise(flights,
          dep_delay_mean = mean(dep_delay, na.rm = TRUE),
          dep_delay_sd = sd(dep_delay, na.rm = TRUE))

summarise(flights,
          dep_delay_mean = mean(dep_delay, na.rm = TRUE),
          dep_delay_sd = sd(dep_delay, na.rm = TRUE),
          n = n())

# grouped summary statistics
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

#############
# YOUR TURN #
#############




# This is inefficient!
sub_cust <- select(customer, CustomerID, Region, Gender, Age, HHIncome, CardSpendMonth)
by_gdr_rgn <- group_by(sub_cust, Gender, Region)
avg_gdr_rgn <- summarize(by_gdr_rgn, Avg_spend = mean(CardSpendMonth, na.rm = TRUE))
arrange(avg_gdr_rgn, desc(Avg_spend))

# Use the pipe operator (%>%) to make more efficient
customer %>%
  select(CustomerID, Region, Gender, Age, HHIncome, CardSpendMonth) %>%
  group_by(Gender, Region) %>%
  summarize(Avg_spend = mean(CardSpendMonth, na.rm = TRUE)) %>%
  arrange(desc(Avg_spend))

#############
# YOUR TURN #
#############






