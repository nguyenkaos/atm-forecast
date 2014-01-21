
library ("lubridate")
library ("data.table")

# 
# there are 128 (2^7) possible schedules
# TODO - missing every 2 weeks, once per week schedules; A, B week
# TODO - missing every 4 weeks, once per week schedules; A, B, C, D weeks
#
schedules <- function (atms) {
    x <- expand.grid (
        "atms" = atms,
        "1"    = 0:1,  # Sunday
        "2"    = 0:1, 
        "3"    = 0:1, 
        "4"    = 0:1, 
        "5"    = 0:1, 
        "6"    = 0:1, 
        "7"    = 0:1)
}

#
# is today a scheduled service day?
#
service <- function(date, schedule) {
        
    
}



# faults.rates = []
# for each *iteration* do ->
#     atm.bal = f(bin.capacity)
# 
#     faults = 0
#     for each *day* do ->
#         vol.before = f(supplier arrival time)
#     
#         if *day* not in *schedule* then
#             vol.before = 1
#         end
#         
#         peak = atm.bal + vol.before * f(forecast)
#         atm.bal = atm.bal + f(forecast)
#         
#         if peak > f(bin.capacity) then
#             faults = faults + 1
#         end
#     end
#     
#     fault.rate = fault / count(days)
#     faults.rates [fault.rate]
# end

simulate.schedule <- function (atms,
                                period.start, 
                               period.length, 
                               period.end = period.start + period.length,
                               usage, 
                               schedule, 
                               volume.before.service, ) {
    
    # progress through each day in the simulation period
    foreach (date = period.start:period.end) %do% {
        
        
        
        
        
    }
}






