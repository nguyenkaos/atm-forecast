
library (ggmap)
library (data.table)

#
# find geographic details of a location based on a search term
#
geoCode <- function (address) {
    
    url <- function(address, root = "http://maps.google.com/maps/api/geocode/") {
        URLencode (paste (root, "json", "?address=", address, "&sensor=", "false", sep = ""))
    }
    
    location <- NULL
    
    # build the request
    request <- getURL( url (address))
    response <- fromJSON (request, simplify = FALSE)
    
    # if the request was successful
    if (response$status=="OK") {
        
        # extract the result
        result <- response$results[[1]]
        address.clean <- result$formatted_address
        
        # extract the location details
        geo <- result$geometry
        lon <- geo$location$lng
        lat <- geo$location$lat
        type <- geo$location_type
        
        location <- list(lon = lon, lat = lat, type = type, address = address.clean)
    } 
    
    return (location)
}

#
# plot a set of points that fit within the map (bad description)
#
plotAtms <- function (address, zoom = "auto", points, alpha = 0.67, size = 10) {
    
    # start with initial geographic locale
    location <- geoCode (address)
    map.data <- get_map (location = c(location$lon, location$lat), zoom = zoom, maptype = 'roadmap')
    map <- ggmap (map.data)
    
    # find the range of lon/lat defined within the map
    lon.min <- min (map$data$lon)
    lon.max <- max (map$data$lon)
    lat.min <- min (map$data$lat)
    lat.max <- max (map$data$lat)
    
    # find all points within the borders of the map
    in.map <- points [ longitude > lon.min & longitude < lon.max & latitude > lat.min & latitude < lat.max ]
    
    # plot those points that fit within the map
    map + 
        geom_point(data = in.map, 
                   aes(x = longitude, y = latitude, colour = withdrawals.past.2wk), alpha = alpha, size = size) + 
        scale_colour_gradient (low = "blue", high = "red")
}

# grab the profile data for the fleet
profiles <- readRDS("../../resources/profiles.rds")
profiles <- data.table(profiles, key = "atm")

plotAtms ("Chicago, IL", 2, profiles)
