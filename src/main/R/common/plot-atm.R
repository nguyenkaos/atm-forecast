
#############################################################################################
# Plots the actual versus forecasted usage pattern of an ATM.
#############################################################################################
plot.atm <- function(data, 
                     atm.name=as.character(sample(unique(data$atm), 1))) {
    
    # grab the ATM's data
    atm.usage <- subset(data, atm==atm.name)
    
    # the main title
    location.desc <- as.character(unique(atm.usage$location.desc)) 
    main <- paste(atm.name, location.desc, sep=" - ")
    
    # calculate the expected ylim
    ymax <- max(atm.usage$usage, atm.usage$usageHat)
    
    # plot actual usage vs forecast
    plot(atm.usage$trandate, atm.usage$usage, 
         col="blue", type="b", main=main, sub=sub, xlab="", ylab="Usage", ylim=c(0,ymax))
    lines(atm.usage$trandate, atm.usage$usageHat, col="red", type="b")
    
    # traffic light each day based on the mape
    apply(atm.usage, 1, function(u) {
        
        mape <- as.numeric(u['mape'])
        color <- if(mape <= 0.05) { 
            "green" 
        } else if(mape > 0.05 && mape <= 0.10) { 
            "yellow" 
        } else if(mape > 0.10 && mape <= 0.20) { 
            "orange" 
        } else { 
            "red" 
        }
        
        points(as.Date(u['trandate']), as.numeric(u['usage']), col=color, pch=16)
    })
}
