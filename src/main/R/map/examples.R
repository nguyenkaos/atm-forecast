
source ("maps.R")


profiles <- readRDS("../../resources/profiles.rds")
profiles <- data.table (profiles, key = "atm")

# prep the data
setnames (profiles, "latitude", "lat")
setnames (profiles, "longitude", "lon")
profiles <- profiles[, list(atm, lon, lat, zipcode, billcount.past.2wk, withdrawals.past.2wk), ]

# Midwest ATMs based around Chicago
map.color ("Chicago, IL", profiles, zoom = 7)
map.color ("Chicago, IL", profiles, zoom = 14)

map.size ("Chicago, IL", profiles, zoom = 14)
map.size ("Chicago, IL", profiles, zoom = 12)
