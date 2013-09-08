library("Hmisc")

############################################################################
# Loads a SAS data set and then saves it off as an R data frame for
# later use.  This function must be run on a machine with SAS installed.
############################################################################
fetchSAS <- function(name) {
  
  # the SAS executable is used to read SAS data sets
  sasprog <- file.path(Sys.getenv("sashome"), "sas.exe")
  data <- sas.get("lib", name, sasprog=sasprog, formats=FALSE)
  
  # save the data as an R data set for later use
  saveRDS(data, file.path( paste(name, ".rds", sep="")))
  
  return(data)
}









