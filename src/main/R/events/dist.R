

profiles <- readRDS("../../resources//profiles.rds")
profiles <- subset(profiles, select=c(atm, latitude, longitude))


library(geosphere)
atm1 <- profiles[1,]
atm2 <- profiles[2,]
distm(c(atm1$longitude,atm2$latitude), c(atm2$longitude,atm2$latitude))

matrix(profiles$longitude, profiles$latitude, ncol=2, byrow=T)

m <- as.matrix(profiles[,c("longitude","latitude")])
d <- distm(m)

