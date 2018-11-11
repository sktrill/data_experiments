renameColumns(): an optional part of my workflow that changes the column names of each of my columns so that I can actually understand what they mean.

convertMissing(): a function which converts missing values to NA

convertClass: a function which sets factor variables to factors, sets character variables to characters, etc



library(dplyr)
renameColumns <- function(data) {
  data <- data %>% select(id = CASEID,
                          year = YEAR,
                          # number of attempted contacts
                          num_contacts = NUMCONTACTS_CPS8,
                          state = STATEFIP,
                          household_size = HH_SIZE,
                          family_income = FAMINCOME,
                          num_children = HH_NUMKIDS,
                          num_adults = HH_NUMADULTS,
                          age = AGE,
                          sex = SEX,
                          time_spent_working = ACT_WORK)
  return(data)
}


time_use <- renameColumns(time_use_orig)
head(time_use)


# identify how many times informative missingness occurs for each variable
informative_missing <- sapply(time_use, 
                              function(x) sum(x %in% c(996, 997, 998)))
# print out only the non-zero values
informative_missing[informative_missing != 0]

# print out the maximum of each column
sapply(time_use, max)



# Helper function for identifying missing values equal to 99, 999, etc
equalFun <- function(x, value) {
  x == value
}

# Helper function for identifying if the max of a variable is equal to 99, ...
maxFun <- function(x, value) {
  max(x, na.rm = T) == value
}


convertMissing <- function(data) {
  # convert missing values to NA
  data <- data %>%
    # mutate all missing values coded as 99 to NA
    mutate_if(function(x) maxFun(x, 99), 
              funs(if_else(equalFun(., 99), NA_integer_, .))) %>%
    # mutate all missing values coded as 999 to NA
    mutate_if(function(x) maxFun(x, 999), 
              funs(if_else(equalFun(., 999), NA_integer_, .))) %>%
    # mutate all missing values coded as 997 to NA
    mutate_if(function(x) maxFun(x, 997), 
              funs(if_else(equalFun(., 997), NA_integer_, .)))
  return(data)    
}

if_else(equalFun(., 99), NA_integer_, .) can be read aloud as “If the value is equal to 99, convert it to a NA of integer type, otherwise do nothing” (the . serves as a placeholder for the data, like x in function(x)).


convertClass <- function(data, path_to_codes = "../data/") {
  # convert id to a factor
  data <- data %>% mutate(id = as.factor(id))
  # loop through each of the factor variables and convert to meaningful
  # factor then add to data frame
  for (variable in c("state", "occupation_industry", "occupation_category",
                     "education_level", "race", "marital_status", "sex",
                     "employment_status", "family_income")) {
    # identify the path to the code file
    path <- paste0(path_to_codes, variable, ".txt")
    # read in the code file
    codes <- read.table(path, sep = "\t")
    # remove the second column (the entries are separated by two \t's)
    codes <- codes[, -2]
    # convert the column names
    colnames(codes) <- c(variable, paste0(variable, "_name"))
    # add the code to the original data frame
    data <- left_join(data, codes, by = variable)
    # remove old variable and replace with new variable
    data[, variable] <- data[, paste0(variable, "_name")]
    data <- data[, !(colnames(data) %in% paste0(variable, "_name"))]
  }
  return(data)
}




# filename: clean.R

# Main function for data cleaning stage
cleanData <- function(data) {
  # rename each of the columns to be human-readable
  # ignore some of the useless columns (such as alternative ID columns)
  data <- renameColumns(data)
  # convert missing data to NA
  data <- convertMissing(data)
  # convert integers to meaningful factors
  data <- convertClass(data)
  return(data)
}

# rename each of the columns to be human-readable
renameColumns <- function(data) {
  data <- data %>% select(id = CASEID,
                          year = YEAR,
                          # number of attempted contacts
                          num_contacts = NUMCONTACTS_CPS8,
                          state = STATEFIP,
                          household_size = HH_SIZE,
                          family_income = FAMINCOME,
                          num_children = HH_NUMKIDS,
                          num_adults = HH_NUMADULTS,
                          age = AGE,
                          sex = SEX,
                          race = RACE,
                          marital_status = MARST,
                          education_level = EDUC,
                          employment_status = EMPSTAT,
                          occupation_category = OCC2,
                          occupation_industry = IND2,
                          employed_full_time = FULLPART,
                          hours_usually_worked = UHRSWORKT,
                          weekly_earning = EARNWEEK,
                          paid_hourly = PAIDHOUR,
                          hourly_wage = HOURWAGE,
                          hours_worked_hourly_rate = HRSATRATE,
                          time_spent_caring_household = ACT_CAREHH,
                          time_spent_caring_non_household = ACT_CARENHH,
                          time_spent_education = ACT_EDUC,
                          time_spent_eating = ACT_FOOD,
                          time_spent_gov = ACT_GOVSERV,
                          time_spent_household_activities = ACT_HHACT,
                          time_spent_household_services = ACT_HHSERV,
                          time_spent_personal_care = ACT_PCARE,
                          time_spent_phone = ACT_PHONE,
                          time_spent_personal_care_services = ACT_PROFSERV,
                          time_spent_shopping = ACT_PURCH,
                          time_spent_religion = ACT_RELIG,
                          time_spent_leisure = ACT_SOCIAL,
                          time_spent_sports = ACT_SPORTS,
                          time_spent_travelling = ACT_TRAVEL,
                          time_spent_volunteer = ACT_VOL,
                          time_spent_working = ACT_WORK)
  return(data)    
}

# identify missing values equal to 99, 999, etc
equalFun <- function(x, value) {
  x == value
}

# identify if the max of a variable is equal to 99, 999, etc
maxFun <- function(x, value) {
  max(x, na.rm = T) == value
}

# convert weird missing values to NA
convertMissing <- function(data) {
  # convert missing values to NA
  data <- data %>%
    # mutate all missing values coded as 99 to NA
    mutate_if(function(x) maxFun(x, 99), 
              funs(if_else(equalFun(., 99), NA_integer_, .))) %>%
    # mutate all missing values coded as 999 to NA
    mutate_if(function(x) maxFun(x, 999), 
              funs(if_else(equalFun(., 999), NA_integer_, .))) %>%
    # mutate all missing values coded as 9999 to NA
    mutate_if(function(x) maxFun(x, 9999), 
              funs(if_else(equalFun(., 9999), NA_integer_, .))) %>%
    # mutate all missing values coded as 999.99 to NA
    mutate_if(function(x) maxFun(x, 999.99), 
              funs(if_else(equalFun(., 999.99), NA_real_, .))) %>%
    # mutate all missing values coded as 99999.99 to NA
    mutate_if(function(x) maxFun(x, 99999.99), 
              funs(if_else(equalFun(., 99999.99), NA_real_, .))) %>%
    # mutate all missing values coded as 998 to NA
    mutate_if(function(x) maxFun(x, 998), 
              funs(if_else(equalFun(., 998), NA_integer_, .))) %>%
    # mutate all missing values coded as 997 to NA
    mutate_if(function(x) maxFun(x, 997), 
              funs(if_else(equalFun(., 997), NA_integer_, .))) %>%
    # mutate all missing values coded as 9995 to NA
    mutate_if(function(x) maxFun(x, 9995), 
              funs(if_else(equalFun(., 9995), NA_integer_, .))) 
  return(data)
}

# change numerics to meaningful factors
convertClass <- function(data, path_to_codes = "../data/") {
  # convert id to a factor
  data <- data %>% mutate(id = as.factor(id))
  # loop through each of the factor variables
  for (variable in c("state", "occupation_industry", "occupation_category",
                     "education_level", "race", "marital_status", "sex",
                     "employment_status", "family_income")) {
    # identify the path to the code file
    path <- paste0(path_to_codes, variable, ".txt")
    # read in the code file
    codes <- read.table(path, sep = "\t")
    # remove the second column (the entries are separated by two \t's)
    codes <- codes[, -2]
    # convert the column names
    colnames(codes) <- c(variable, paste0(variable, "_name"))
    # add the code to the original data frame
    data <- left_join(data, codes, by = variable)
    # remove old variable and replace with new variable
    data[, variable] <- data[, paste0(variable, "_name")]
    data <- data[, !(colnames(data) %in% paste0(variable, "_name"))]
  }
  return(data)
}


