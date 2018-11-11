# a function to load in the data
loadData <- function(path_to_data = "../data/atus_00002.csv.gz") {
  # open zipped file for reading
  unz <- gzfile(path_to_data)
  # load in the data
  read.csv(unz)
}

time_use_orig <- loadData()

dim(time_use_orig)
head(time_use_orig)



