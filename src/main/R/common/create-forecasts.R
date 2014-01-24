
library ("data.table")

#
# create the withdrawals forecast data set from the SAS original
#
input <- "C:/Users/j918358/Desktop/forecast_withdrawals.csv"
forecast <- fread (input)
forecast [, `:=` (
    trandate  = as.Date (trandate, "%d%b%y"),
    usage     = as.numeric (gsub ("[$,]", "", usage)),
    p_usg_adj = as.numeric (gsub ("[$,]", "", p_usg_adj)),
    lb        = as.numeric(lb),
    ub        = as.numeric(ub)
)]

setnames (forecast, "usage", "cash")
setnames (forecast, "p_usg_adj", "cash.hat")
setnames (forecast, "lb", "lower.bound.99")
setnames (forecast, "ub", "upper.bound.99")
setkeyv (forecast, c("atm","trandate"))

saveRDS (forecast, "../../resources/forecast-withdrawals.rds", compress = T)

#
# create the deposits forecast data set from the SAS original
#
input <- "C:/Users/j918358/Desktop/forecast_deposits.csv"
forecast <- fread (input)
forecast [, `:=` (
    trandate  = as.Date (trandate, "%d%b%y"),
    billcnt   = as.numeric (billcnt),
    p_dep_adj = as.numeric (gsub ("[$,]", "", p_dep_adj)),
    lb        = as.numeric(lb),
    ub        = as.numeric(ub)
)]

setnames (forecast, "billcnt", "bills")
setnames (forecast, "p_dep_adj", "bills.hat")
setnames (forecast, "lb", "lower.bound.99")
setnames (forecast, "ub", "upper.bound.99")
setkeyv (forecast, c("atm","trandate"))

saveRDS (forecast, "../../resources/forecast-deposits.rds", compress = T)

