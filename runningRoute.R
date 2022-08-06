library(sf)
library(tidyverse)
library(tidygeocoder)
library(mapboxapi)
library(hereR)
library(osrm)
library(combinat)
library(igraph)

# Punch in addresses and names of locations

address <- c("Trail Tree Park, Deerfield, IL",
             "6 Greenbriar Dr, Deerfield, IL")

name <- c("Trail Tree Park",
          "Briarwood Park")

locations <- cbind(name,address)
locations <- as.data.frame(locations)

# geocode the two addresses & transform to {sf} data structure
data <- tidygeocoder::geo(locations[,2], method = "osm") %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

data <- data %>% inner_join(locations)

edges <- data.frame(origin_nm = character(0),
                    origin_addr = character(0),
                    origin_geo = character(0),
                   destination_nm = character(0),
                   destination_addr = character(0),
                   destination_geo = character(0),
                   distance = numeric(0))

counter = 1

# Now, create a function that creates a table of every possible 'edge' between nine schools
for (row in 1:(nrow(data)-1)){
  for (col in (row+1):nrow(data)){
    if (setequal(data[row,],data[col,]) == FALSE){
    start_addr = data$address[row]
    end_addr = data$address[col]
    route = mapboxapi::mb_directions(origin=start_addr,destination=end_addr,profile="walking")
    start_nm = data$name[row]
    end_nm = data$name[col]
    start_geo = data$geometry[row]
    end_geo = data$geometry[col]
    edges[counter,] = c(start_nm,start_addr,start_geo,end_nm,end_addr,end_geo,route$distance)
    counter = counter + 1
    }
  }
} 


start <- data$name[1] # start
routes <- permn(data$name) # list of every possible route

# Create adjacency matrix

adj_matrix <- get.adjacency(graph_from_data_frame(
  edges %>% select(origin_nm,destination_nm,distance),
  directed = FALSE),
  attr="distance",sparse = FALSE)

# Calculate the distance of a route

distRoute <- function(adjmat, route) {
  d <- 0
  for(i in 2:nrow(adjmat)) {
    d <- d + adjmat[route[i-1],route[i]]
  }
  return(d)
}

best_route <- NULL
min_d <- Inf

for(i in 1:length(routes)) {
  
  route <- routes[[i]]
  route <- c(route,route[1])
  
  # distance
  new_d <- distRoute(adj_matrix,route)
  
  # update distance and plot
  if(new_d < min_d) {
    min_d <- new_d
    best_route <- route
  }
}


# Create the list of addresses for the route
route_table <- as.data.frame(best_route) %>%
  rename(name = best_route) %>%
  inner_join(data) %>% 
  select(address,geometry)

# geocode the listed addresses

mroute <- mapboxapi::mb_directions(input_data = route_table[2],
                                   profile = "walking")

summary(mroute)


library(leaflet)


leaflet(data = data) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(label = ~address) %>% 
  addPolylines(data = mroute,
               label = "Mapbox engine",
               color = "blue") 