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

# Create an empty data frame to hold all the relevant information regarding the edges between each two points.
edges <- data.frame(origin_nm = character(0),
                    origin_addr = character(0),
                    origin_geo = character(0),
                   destination_nm = character(0),
                   destination_addr = character(0),
                   destination_geo = character(0),
                   distance = numeric(0))

counter = 1

# Now, iterate through the table of locations to fill in the table of every possible 'edge' between locations
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

# Create adjacency matrix which shows the distance between each set of two locations

adj_matrix <- get.adjacency(graph_from_data_frame(
  edges %>% select(origin_nm,destination_nm,distance),
  directed = FALSE),
  attr="distance",sparse = FALSE)

# Create a function to calculate the length of any route

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

brute_force_tsp <- function(adj_matrix, start_point = 1) {
  # Get all locations except start point
  locations <- 1:nrow(adj_matrix)
  
  # Generate all possible routes using permutations
  routes <- permn(locations)
  
  best_route <- NULL
  min_distance <- Inf
  
  # Check each possible route
  for(i in 1:length(routes)) {
    route <- routes[[i]]
    # Add return to start
    route <- c(route, route[1])
    
    # Calculate distance
    new_distance <- distRoute(adj_matrix, route)
    
    # Update if better route found
    if(new_distance < min_distance) {
      min_distance <- new_distance
      best_route <- route
    }
  }
  
  return(list(route = best_route, distance = min_distance))
}

# Now let's try a slightly different methodology, Nearest Neighbor.
nearest_neighbor_tsp <- function(adj_matrix, start_point) {
  n <- nrow(adj_matrix)
  unvisited <- 1:n
  route <- start_point
  unvisited <- unvisited[unvisited != start_point]
  
  while(length(unvisited) > 0) {
    current <- route[length(route)]
    
    # Find nearest unvisited point
    distances <- adj_matrix[current, unvisited]
    next_point <- unvisited[which.min(distances)]
    route <- c(route, next_point)
    unvisited <- unvisited[unvisited != next_point]
  }
  
  # Return to start
  route <- c(route, start_point)
  return(route)
}

# Now le'ts try the two opt method.
two_opt_swap <- function(route, i, k) {
  new_route <- route
  new_route[i:k] <- rev(route[i:k])
  return(new_route)
}

two_opt <- function(adj_matrix, initial_route) {
  improvement <- TRUE
  best_distance <- distRoute(adj_matrix, initial_route)
  best_route <- initial_route
  
  while(improvement) {
    improvement <- FALSE
    for(i in 2:(length(initial_route)-2)) {
      for(k in (i+1):(length(initial_route)-1)) {
        new_route <- two_opt_swap(best_route, i, k)
        new_distance <- distRoute(adj_matrix, new_route)
        if(new_distance < best_distance) {
          best_distance <- new_distance
          best_route <- new_route
          improvement <- TRUE
        }
      }
    }
  }
  return(best_route)
}

# Next, let's use the existing TSP package that exists within R and see how well it does
library(TSP)

solve_with_tsp_package <- function(adj_matrix, method = "nearest_insertion") {
  tsp <- TSP(adj_matrix)
  tour <- solve_TSP(tsp, method = method)
  route <- c(labels(tour), labels(tour)[1])  # Add return to start
  return(route)
}

# Try different methods
methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion", 
             "arbitrary_insertion", "nn", "repetitive_nn", "two_opt")

results <- lapply(methods, function(m) {
  route <- solve_with_tsp_package(adj_matrix, method = m)
  distance <- distRoute(adj_matrix, route)
  return(list(method = m, route = route, distance = distance))
})

# Finally, let's try it with simulated annealing. This is the most complex version of the model.
simulated_annealing_tsp <- function(adj_matrix, initial_temp = 100, cooling_rate = 0.95, 
                                    max_iterations = 1000) {
  current_route <- c(1:nrow(adj_matrix), 1)  # Start with sequential route
  current_distance <- distRoute(adj_matrix, current_route)
  best_route <- current_route
  best_distance <- current_distance
  temp <- initial_temp
  
  for(i in 1:max_iterations) {
    # Generate neighbor by swapping two random cities
    idx <- sample(2:(length(current_route)-1), 2)
    new_route <- current_route
    new_route[idx[1]:idx[2]] <- rev(new_route[idx[1]:idx[2]])
    new_distance <- distRoute(adj_matrix, new_route)
    
    # Accept if better or probabilistically if worse
    delta <- new_distance - current_distance
    if(delta < 0 || exp(-delta/temp) > runif(1)) {
      current_route <- new_route
      current_distance <- new_distance
      if(current_distance < best_distance) {
        best_route <- current_route
        best_distance <- current_distance
      }
    }
    temp <- temp * cooling_rate
  }
  return(list(route = best_route, distance = best_distance))
}

# Benchmark different approaches
library(rbenchmark)

benchmark(
  "brute_force" = {
    result <- brute_force_tsp(adj_matrix)
  },
  "nearest_neighbor" = {
    route <- nearest_neighbor_tsp(adj_matrix, 1)
  },
  "two_opt" = {
    initial_route <- nearest_neighbor_tsp(adj_matrix, 1)
    route <- two_opt(adj_matrix, initial_route)
  },
  "tsp_package" = {
    route <- solve_with_tsp_package(adj_matrix, "nearest_insertion")
  },
  "simulated_annealing" = {
    result <- simulated_annealing_tsp(adj_matrix)
  },
  replications = 5,
  columns = c("test", "elapsed", "relative", "user.self", "sys.self")
)


# Now let's compare the different algorithms and see how long the routes end up being for each
compare_algorithms <- function(adj_matrix) {
  # Run each algorithm
  bf_result <- brute_force_tsp(adj_matrix)
  nn_route <- nearest_neighbor_tsp(adj_matrix, 1)
  two_opt_route <- two_opt(adj_matrix, nearest_neighbor_tsp(adj_matrix, 1))
  tsp_route <- solve_with_tsp_package(adj_matrix, "nearest_insertion")
  sa_result <- simulated_annealing_tsp(adj_matrix)
  
  # Compare results
  results <- data.frame(
    Algorithm = c("Brute Force", "Nearest Neighbor", "Two-opt", "TSP Package", "Simulated Annealing"),
    Distance = c(
      bf_result$distance,
      distRoute(adj_matrix, nn_route),
      distRoute(adj_matrix, two_opt_route),
      distRoute(adj_matrix, tsp_route),
      sa_result$distance
    )
  )
  
  # Add relative performance
  results$RelativeToOptimal <- results$Distance / min(results$Distance)
  
  return(results)
}

# Usage:
results <- compare_algorithms(adj_matrix)
print(results)

create_route_coordinates <- function(route, original_data) {
  # Create empty sf object with same structure as original data
  formatted_route = original_data %>% filter(is.null(name))
  
  # Populate with locations in route order
  for (i in seq_along(route)) {
    # Get location name from original data based on index in route
    location_name <- original_data$name[route[i]]
    # Add that location's data to our new route object
    formatted_route[i,] = original_data %>% filter(name == location_name)
  }
  
  return(formatted_route)
}

# geocode the listed addresses for the best route

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

# Geocode them for the Two-opt route
two_opt_route <- two_opt(adj_matrix, nearest_neighbor_tsp(adj_matrix, 1))
two_opt_coordinates <- create_route_coordinates(two_opt_route,data)

mroute2 <- mapboxapi::mb_directions(input_data = two_opt_coordinates,
                                   profile = "walking")

summary(mroute2)

library(leaflet)

leaflet(data = data) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(label = ~address) %>% 
  addPolylines(data = mroute,
               label = "Brute Force Route",
               color = "blue",
               weight = 2) %>%
  addPolylines(data = mroute2,
               label = "Two Opt Route",
               color = "red",
               weight = 2)