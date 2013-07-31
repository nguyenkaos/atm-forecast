##
# Fetches data contained within various file formats.
##

library("Hmisc")

#
# Fetches data contained within a CSV.
#
fetchCSV <- function(path) {
  data <- read.csv(path)
  data$date <- as.Date(data$date, format="%m/%d/%Y")
  return(data)
}

#
# Fetches a specific dataset by name.  If the R data set
# does not exist, the data is loaded from the SAS data set.
#
fetchR <- function(name) {
  data <- readRDS(file.path("lib", paste(name, ".rds", sep="")))
  return(data)
}

#
# Loads all of the SAS data sets and also persists them as
# R data sets for future use.
#
fetchSAS <- function(name) {
  
  # the SAS executable is used to read SAS data sets
  sasprog <- file.path(Sys.getenv("sashome"), "sas.exe")
  data <- sas.get("lib", name, sasprog=sasprog, formats=FALSE)
  
  # save the data as an R data set for later use
  saveRDS(data, file.path( paste(name, ".rds", sep="")))
  
  return(data)
}









