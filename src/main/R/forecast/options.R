

# defines the options/arguments
library("optparse")

getOptions <- function() {
    
    # define the valid command line options
    all_options <- list(
        make_option(c("--forecastOut"),
                    help="How far out to forecast in days [default: %default]",
                    default=120),
        make_option(c("--splitAt"),
                    help="Date at which to split data into training vs test [default: %default]",
                    default="2013-08-15"),
        make_option(c("-l", "--logLevel"),
                    help="Level of logging [default: %default]",
                    default="INFO"),  
        make_option(c("--subset"),
                    help="An expression to identify a subset of ATMs to forecast [default: %default (all)]",
                    default="T==T"),
        make_option(c("--parallel"), 
                    action="store_true",
                    help="Run with a parallel backend enabled",
                    default=F),
        make_option(c("-d", "--dataDir"),
                    help="Directory containing the data files [default: %default]",
                    default="../../resources"),
        make_option(c("--historyFile"), 
                    help="RDS file containing the ATM history [default: %default]",
                    default="withdrawals-micro.rds"),
        make_option(c("--eventsFile"), 
                    help="CSV file containing events data [default: %default]",
                    default="events.csv")
    )
    
    opts <- parse_args(OptionParser(option_list=all_options))
}

