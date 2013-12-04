
library (ggmap)
library (data.table)

#
# find geographic details of a location based on a search term
#
geocode <- function (address) {
    
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
        geo  <- result$geometry
        location <- list(lon     = geo$location$lng, 
                         lat     = geo$location$lat, 
                         type    = geo$location_type, 
                         address = address.clean)
    } else {
        stop ("unable to find address: %s", response)
        
    }
    
    return (location)
}

#
# plots points of interest (poi) within a map.  the only requirement is that
# poi must contain a "lon" and "lat" column.
#
map.color <- function (address, poi, zoom = "auto", alpha = 0.67, size = 10) {
    
    # start with initial geographic locale
    location <- geocode (address)
    map.data <- get_map (location = c(location$lon, location$lat), zoom = zoom, maptype = 'roadmap')
    map.base <- ggmap (map.data, extent = "panel")
    
    # find the range of lon/lat visible within the map
    lon.min <- min (map.base$data$lon)
    lon.max <- max (map.base$data$lon)
    lat.min <- min (map.base$data$lat)
    lat.max <- max (map.base$data$lat)
    
    # find all points within the borders of the map
    poi.in.map <- poi [ lon > lon.min & lon < lon.max & lat > lat.min & lat < lat.max ]
    
    # plot those points that fit within the map
    map.base + 
        geom_point(data = poi.in.map, aes(x = lon, y = lat, colour = withdrawals.past.2wk), alpha = alpha, size = size) + 
        scale_colour_gradient (low = "blue", high = "red")
}

#
# plots points of interest (poi) within a map.  the only requirement is that
# poi must contain a "lon" and "lat" column.
#
map.size <- function (address, poi, zoom = "auto", size.range = c(0,20)) {
    
    # start with initial geographic locale
    location <- geocode (address)
    map.data <- get_map (location = c(location$lon, location$lat), zoom = zoom, maptype = 'roadmap')
    map.base <- ggmap (map.data, extent = "panel")
    
    # find the range of lon/lat visible within the map
    lon.min <- min (map.base$data$lon)
    lon.max <- max (map.base$data$lon)
    lat.min <- min (map.base$data$lat)
    lat.max <- max (map.base$data$lat)
    
    # find all points within the borders of the map
    poi.in.map <- poi [ lon > lon.min & lon < lon.max & lat > lat.min & lat < lat.max ]
    
    # plot those points that fit within the map
    map.base + 
        geom_point (data = poi.in.map, aes(x      = lon, 
                                           y      = lat, 
                                           colour = withdrawals.past.2wk, 
                                           size   = withdrawals.past.2wk,
                                           alpha  = withdrawals.past.2wk), alpha = 0.5) + 
        scale_colour_gradient (low = "blue", high = "red") + 
        scale_size_continuous (range = size.range)
}

#
# plots points of interest (poi) within a map.  the only requirement is that
# poi must contain a "lon" and "lat" column.
#
map.zip <- function (address, poi, zoom = "auto", size.range = c(0,20)) {
    
    # start with initial geographic locale
    location <- geocode (address)
    map.data <- get_map (location = c(location$lon, location$lat), zoom = zoom, maptype = 'roadmap')
    map.base <- ggmap (map.data, extent = "panel")
    
    # find the range of lon/lat visible within the map
    lon.min <- min (map.base$data$lon)
    lon.max <- max (map.base$data$lon)
    lat.min <- min (map.base$data$lat)
    lat.max <- max (map.base$data$lat)
    
    # find all points within the borders of the map
    poi.in.map <- poi [ lon > lon.min & lon < lon.max & lat > lat.min & lat < lat.max ]
    
    # plot those points that fit within the map
    map.base + 
        geom_point (data = poi.in.map, aes (x      = lon, 
                                            y      = lat, 
                                            colour = withdrawals.past.2wk, 
                                            size   = withdrawals.past.2wk,
                                            alpha  = withdrawals.past.2wk), alpha = 0.5) + 
        scale_colour_gradient (low = "blue", high = "red") + 
        scale_size_continuous (range = size.range)
}

map.faults <- function (address, poi, zoom = "auto", size.range = c(0,20)) {
    
    # start with initial geographic locale
    location <- geocode (address)
    loginfo("'%s' -> '%s'", address, location)
    
    map.data <- get_map (location = c(location$lon, location$lat), zoom = zoom, maptype = 'roadmap')
    map.base <- ggmap (map.data, extent = "panel")
    
    # find the range of lon/lat visible within the map
    lon.min <- min (map.base$data$lon)
    lon.max <- max (map.base$data$lon)
    lat.min <- min (map.base$data$lat)
    lat.max <- max (map.base$data$lat)
    
    # find all points within the borders of the map
    poi.in.map <- poi [ lon > lon.min & lon < lon.max & lat > lat.min & lat < lat.max ]
    
    # plot those points that fit within the map
    map.base + 
        geom_point (data = poi.in.map, aes (x      = lon, 
                                            y      = lat, 
                                            colour = fault.per.usage, 
                                            size   = fault.per.usage), alpha = 0.5) + 
        scale_colour_gradient (low = "blue", high = "red") + 
        scale_size_continuous (range = size.range)
}


