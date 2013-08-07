library("ggplot2")

cash <- readRDS("work/cash.rds")

# paydays
cash$payday <- as.factor(cash$payday)
levels(cash$payday) <- c("pay","pay","pay","post","post","pre","pre")
qplot(payday, usage, data=cash, geom=c("boxplot", "jitter"), 
      fill=payday, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon")

# dayOfWeek
qplot(usage, data=cash, geom="density", fill=dayOfWeek, alpha=I(.5), 
      main="Distribution of Paydays", xlab="Miles Per Gallon", 
      ylab="Density")
