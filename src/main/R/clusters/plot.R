# plot the time series for comparison
withd <- melt(withd, id=c("atm","trandate","group"))
ggplot(withd, aes(x=trandate, y=value, linestyle=group, colour=group, group=atm)) + geom_line()