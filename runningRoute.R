library(sf)
library(tidyverse)
library(tidygeocoder)
library(mapboxapi)
library(hereR)
library(osrm)
library(combinat)
library(igraph)
library(janitor)
library(usethis)

# Punch in addresses and names of locations

address <- c("375 Carlisle Ave, Deerfield, IL",
             "6 Greenbriar Dr, Deerfield, IL",
             "200 Deerfield Rd, Deerfield, IL",
             "900 Clay Ct, Deerfield, IL",
             "836 Jewett Park Dr, Deerfield, IL",
             "845 North Ave, Deerfield, IL",
             "396 Cumnor Ct, Deerfield IL",
             "319 Pine St, Deerfield IL",
             "1425 Wilmot Rd, Deerfield IL",
             "375 Elm St, Deerfield IL")

name <- c("Trail Tree Park",
          "Briarwood Park",
          "Deerspring Park",
          "Maplewood Park",
          "Jewett Park",
          "John Blumberg Tot Lot",
          "Cumnor Court Park",
          "Keller Park",
          "Woodland Park",
          "Brickyards Park")

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

# Now, create a function that creates a table of every possible 'edge' between locations
for (row in 1:(nrow(data)-1)){
  for (col in (row+1):nrow(data)){
    if (setequal(data[row,],data[col,]) == FALSE){
    start_addr = data$address[row]
    end_addr = data$address[col]
#    print(start_addr)
#    print(end_addr)
    route = mapboxapi::mb_directions(origin=start_addr,destination=end_addr,profile="walking")
#    print(route$distance)
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

# First attempt will be to brute force the result
library(rbenchmark)
benchmark("bruteforce" = {
for(i in 1:length(routes)) {
  
  route <- routes[[i]]
  route <- c(route,route[1])
  
  # distance
  new_d <- distRoute(adj_matrix,route)
  
#  print(i)
#  print(route)
#  print(new_d)
  
  # update distance and plot
  if(new_d < min_d) {
    min_d <- new_d
    best_route <- route
  }
 # print(routes[[i]])
 # print(route)
}
}, replications = 10,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))

# First Method - Create a Table for the New Route by Joining to Original SF object
# route_table <- as.data.frame(best_route) %>%
#   rename(name = best_route) %>%
#   inner_join(data) %>% 
#   select(address,geometry)

# The problem now is that if you this this then the text "POINT" has been added to the table. It won't work when you try to 
# generate a map. What's the solution?

# Instead of joining, iterate through the sf object and create a new row for each name in the best route list

# first create an empty sf object with the same fields by taking the original one and pulling out the rows
stop_num = 1
final_route = data %>% filter(is.null(name))

# Then iterate through the best route, creating a new row for each location
for (item in best_route){
  final_route[stop_num,] = data %>% filter(name == item)
  stop_num = stop_num + 1 # R doesn't have a built-in increment function like C++ or Python
}

# geocode the listed addresses

mroute <- mapboxapi::mb_directions(input_data = final_route[1],
                                   profile = "walking")

summary(mroute)

library(leaflet)

leaflet(data = data) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(label = ~address) %>% 
  addPolylines(data = mroute,
               label = "Mapbox engine",
               color = "blue") 