

library (data.table)
library (gdata)

# Download the following files from the SAS Farm and export them to 
# CSV. Then update the following paths to indicate where they exist
# on the local computer.
#
#  /gpfs_nonhsm/cto/bma/atm_trans/gasper/ooc.sas7bdat
#  /gpfs_nonhsm/cto/bma/atm_trans/gasper/dbf.sas7bdat
#
ooc.path <- "C:/Users//j918358//Desktop//ooc.csv"
dbf.path <- "C:/Users//j918358//Desktop//dbf.csv"

# out of cash
ooc <- fread(ooc.path)
setnames(ooc, make.names(tolower(names(ooc)), allow_=F, ))
ooc <- ooc [, list(atm, type = "Out of Cash", start.time, time.to.resolve) ]
ooc [, start.time := as.Date(start.time, "%d%b%Y")]
ooc [, time.to.resolve := as.numeric(time.to.resolve)]

# deposit bin full
dbf <- fread(dbf.path)
setnames(dbf, make.names(tolower(names(dbf)), allow_=F, ))
dbf <- dbf [, list(atm, type = "Deposit Bin Full", start.time, time.to.resolve)]
dbf [, start.time := as.Date(start.time, "%d%b%Y")]
dbf [, time.to.resolve := as.numeric(time.to.resolve)]

# merge both fault types
faults <- rbindlist(list(ooc, dbf))
setkeyv (faults, c("atm"))
keep(faults, sure = T)

# export the faults data
saveRDS(faults, "../..//resources/faults.rds", compress = "bzip2")
