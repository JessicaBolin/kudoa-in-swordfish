# Extract distance (km) to closest seamount or guyout (i.e., >1000m in elevation)
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)

# Blue Habitats: https://bluehabitats.org

# Dependencies ------------------------------------------------------------

library(sf)
library(tictoc)
library(tidyverse)

fishing <- readRDS("spatialmetadata_alltrips.RDS") #not available
fishing <- fishing %>% filter(Trip !=2) # Nuisance trip (no data recorded)

source("functions/MinDist_PointToPoly.R") # Function to calculate min distance to feature

# Shapefile of ETBF guyouts and seamounts pre-created via Blue Habitats
try <- st_read("shapefiles/Bathy") 


# Distances between all shots and polygons -------------------------------

points.df <- data.frame("x" = fishing$Longitude, "y" = fishing$Latitude)
points_df <- split(points.df, seq(nrow(points.df))) 
new <- furrr::future_map(.x = points_df, .f = hair)
fishing$featuredist_km <- as.vector(unlist(new))


# Split points into trip by trip ------------------------------------------

#Average distance to nearest feature (Blue Habitats) for each trip
average_dist <- fishing %>% 
  group_by(Trip, Vessel) %>% 
  summarise(SeamountDistKM = mean(featuredist_km), 
            SeamountDistKM_sd = sd(featuredist_km),
            Company = company %>% unique) %>% 
  data.frame()

# Need average distances for trips 1, 4, 6, 7 as these have multiple boats


# Function to fix multiple boat issue ----------------------------------------

multiple <- subset(fishing, Trip == 1 | Trip == 4 | Trip == 6 | Trip == 7)

fix_multiple <- function(x) {
  
  trip1 <- subset(multiple, Trip == x)
  coordsey <- cbind(trip1$Longitude, trip1$Latitude) %>% as.data.frame
  distys <- list()
  
  for (i in 1:nrow(coordsey)) {
    points.sf <- st_as_sf(coordsey[i,], coords = c("V1", "V2")) 
    st_crs(points.sf) <- st_crs(4326) 
    disty <- raster::which.min(st_distance(try, points.sf)) 
    distys[i] <- as.vector(st_distance(try, points.sf)[disty]/1000) 
  }
  
  distyss <- unlist(distys)
  average_dist[average_dist$Trip == x, "SeamountDistKM"] <<- distyss %>% mean
  average_dist[average_dist$Trip == x, "Vessel"] <<- NA
  average_dist[average_dist$Trip == x, "SeamountDistKM_sd"] <<- distyss %>% sd
} 


# Run function ------------------------------------------------------------

fix_multiple(1)
fix_multiple(4)
fix_multiple(6)
fix_multiple(7)

average_dist <- average_dist[-c(2,4,7,9:11),] #remove duplicates

#Outcome = df of trip vessel combos with average distance to seamount (mean and sd)

saveRDS(average_dist, "cleaned_data/env/DistToSeamount_Domain_ALL.RDS")
# not made available in repo