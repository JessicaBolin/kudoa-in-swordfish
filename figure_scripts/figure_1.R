# Figure 1 of fishing and sampling effort in the ETBF
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)
# Updated Feb 2024

# Adapted from: https://urbandatapalette.com/post/2021-08-tessellation-sf/

# This script creates plots of:

# 1. Swordfish (number of sword caught per bin)
# 2. Swordfish (number of sword sampled per bin)
# Plots are then imported to Adobe illustrator for tidying up. 

# Dependencies ------------------------------------------------------------

library(tidyverse)
library(raster)
library(tmap)
library(sf)
library(sp)
library(terra)

source("functions/make_hex_grid.R")

eez <- st_read("shapefiles/Common shapefiles/australiaeez.shp")
norfolk <- st_read("shapefiles/Common shapefiles/norfolkeez.shp")
both <- st_union(eez, norfolk)[0]
stray <- st_read("shapefiles/East_coast_detailed/australia_east_coast.shp")
seamount <- st_read("shapefiles/Bathy/Seamounts_Guyouts_ETBF.shp")[0]
soloeez <- shapefile("shapefiles/Common shapefiles/solomonislandeez.shp")  
pngeez <- shapefile("shapefiles/Common shapefiles/pngeez.shp") 
nz <- shapefile("shapefiles/Common shapefiles/newzealand_dateline_eez.shp") 
nc <- shapefile("shapefiles/Common shapefiles/newcaledoniaeez.shp") 
croppedseamount <- st_intersection(seamount, both)


##############################################################################
# Panel A - swordfish per bin -------------------------------------------------
##############################################################################

# AFMA data not publicly available 
dat <- read.csv("afma_data.csv")

# Remove data in cells with < 5 boats ------------------------------------------

# Remove data with no Trip IDs
nrow(dat[is.na(dat$tripid),]) 
nas <- dat[is.na(dat$tripid),]
newdat <- anti_join(dat, nas)
newdat$lat_grid <- floor(newdat$lat)
newdat$lon_grid <- floor(newdat$lon)

# Group by grid cells and count unique tripid values
grid_counts <- newdat %>%
  group_by(lat_grid, lon_grid) %>%
  summarise(tripid_count = n_distinct(tripid)) 

# Create a raster with the grid counts
raster_resolution <- 1  # 1-degree resolution
raster_extent <- extent(floor(range(newdat$lon)), floor(range(newdat$lat)))
raster_template <- raster(ncols = ncol(grid_counts),
                          nrows = nrow(grid_counts),
                          resolution = raster_resolution,
                          crs = CRS("+proj=longlat +datum=WGS84"))

# Convert the grid count data to a raster
raster_tripid_count <- rasterize(grid_counts[, c("lon_grid", "lat_grid")],
                                 raster_template,
                                 field = grid_counts$tripid_count,
                                 fun = "first")

# Convert to terra object and crop to extent
raster_tripid_count <- raster_tripid_count %>% 
  rast %>% 
  crop(., c(140, 170, -45, -10))

# Plot the raster. This depicts cells with number of trips
plot(raster_tripid_count, main = "Unique tripid Count Raster")

# Replace cells with count less than 5 with NA
threshold <- 5
raster_tripid_count[raster_tripid_count < threshold] <- NA

# Plot the modified raster
plot(raster_tripid_count, main = "Unique tripid Count Raster (Threshold >= 5)")
maps::map("world", add = T)

# Identify rows to keep based on the raster
rows_to_keep <- !is.na(getValues(raster_tripid_count %>% raster))
newdat_filtered <- newdat[rows_to_keep, ]


# Create df for hexagonal grid --------------------------------------------

# Make points spatial
df <- dplyr::select(newdat_filtered, "lat", "lon", "swdno")
names(df) <- c("y", "x", "swdno")

# Get number of swordfish caught per grid cell
test <- df; empty <- list()
for (i in 1:nrow(test)) {
  n = test$swdno[i]
  dfy <- test[i, ]
  empty[[i]] <- dfy[rep(seq_len(nrow(dfy)), each = n),]
  print(i)
}

test2 <- do.call(rbind, empty)

# Turn df into spatial object
dff <- test2 %>% 
  st_as_sf(coords = c("x", "y"), 
           crs = 4326, 
           remove = F)


# Create hexagonal grid ---------------------------------------------------

# Turn points into hexagonal grid
area_honeycomb_grid <- st_make_grid(dff, 
                                    c(1,1),  #1 degree resolution 
                                    what = "polygons", 
                                    square = FALSE) %>% 
  st_sf() %>% 
  mutate(grid_id = 1:length(lengths(.))) #Add grid_id field

# Add a field for sword counts 
area_honeycomb_grid$swd <- lengths(st_intersects(area_honeycomb_grid, dff)) 
fishnet_count <- filter(area_honeycomb_grid, swd > 0) #remove cells with 0 sword

# Sanity check - check total number of sword are the same 
fishnet_count$swd %>% sum; sum(df$swdno) #confirmed same


# Plot --------------------------------------------------------------------

map_swd <-  tm_shape(fishnet_count["swd"], 
                     bbox = c(145, 168, -44, -11)) +
  tm_fill(col ="swd",
          palette = viridis::mako(255),
          style = "cont",
          title = "Swordfish caught",
          legend.is.portrait=F,
          n = 6) +
  tm_graticules(ticks = T, lwd = 0.5, n.x = 6, n.y = 6, 
                col = "transparent", labels.size = 1) +
  tm_layout(legend.frame = F, 
            fontfamily = "Arial Narrow",
            legend.outside = T,
            inner.margins = c(0.01, 0.14, 0.01, 0.01)) +
  tm_shape(fishnet_count["swd"]) +
  tm_fill(col ="swd",
          palette = viridis::mako(255),
          style = "cont",
          legend.show = F) +
  tm_shape(croppedseamount) +
  tm_polygons(alpha = 0.001, border.col = "grey", lwd = 0.7) +
  tm_shape(nc) +
  tm_polygons(col = "white", lty = 2, lwd = 1, border.col = "grey") +
  tm_shape(nz) +
  tm_polygons(col = "white", lty = 2, lwd = 1, border.col = "grey") +
  tm_shape(soloeez) +
  tm_polygons(col = "white", lty = 2, lwd = 1, border.col = "grey") +
  tm_shape(pngeez) +
  tm_polygons(col = "white", lty = 2, lwd = 1, border.col = "grey") +
  tm_shape(both) +
  tm_polygons(alpha = 0.001, lty = 1, lwd = 1, border.col = "black") +
  tm_shape(stray) +
  tm_polygons(lwd = 0.5) 

map_swd


###########################################################################
# Panel b - total swordfish sampled -----------------------------------------
###########################################################################


dat <- readRDS("prev_clean.RDS") #not available in repo
dat$X <- NULL
dat <- dat[!dat$Trip == 46,] #not used in analysis
daty <- dat[, c("Trip", "Fish", "operation", "overall", "meanlon", "meanlat")]

# Number of infected/non-infected swordfish per operation
fis <- daty %>% 
  group_by(operation, overall) %>% 
  count() %>%
  data.frame()

# Get associated trip coords
coords <- daty %>% 
  group_by(operation, meanlon, meanlat) %>% 
  count() %>%
  dplyr::select(-n) %>% 
  data.frame()

# Merge
all <- merge(fis, coords, by = c("operation"))

# Number/percent sword infected
sampled <- sum(all$n) 
infected <- sum(subset(all, overall == 1)$n) 
percentinfected <- infected/sampled * 100 

# Empty df
overallinfected2 <- data.frame(operation = NA, 
                               fishsampled = NA, 
                               fishinfected = NA, 
                               percentinfected_overall = NA,
                               meanlon = NA,
                               meanlat = NA)
overallinfected2$operation <- overallinfected2$operation %>% 
  as.character

# Fill df with assoc. infection stats
for (i in 1:length(unique(all$operation))) { 
  
  n <- subset(all, operation == unique(all$operation)[i]) 
  sampled <- sum(n$n) 
  infected <- sum(subset(n, overall == 1)$n) 
  percentinfected <- infected/sampled * 100 
  overallinfected2[i, c(2:6)] <- c(sampled, 
                                   infected,
                                   round(percentinfected, 2),
                                   n$meanlon[1],
                                   n$meanlat[1])
  overallinfected2[i, "operation"] <- n$operation[1] %>% as.character()
}

df <- dplyr::select(overallinfected2, "meanlat", "meanlon", "fishsampled")
names(df) <- c("y", "x", "fishsampled")
test <- df
empty <- list()

for (i in 1:nrow(test)) {
  n = test$fishsampled[i]
  dfy <- test[i, ]
  empty[[i]] <- dfy[rep(seq_len(nrow(dfy)), each = n),]
  print(i)
}

test2 <- do.call(rbind, empty)

dff <- test2 %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = F)

area_honeycomb_grid <- st_make_grid(dff, 
                                    c(1,1), 
                                    what = "polygons", 
                                    square = FALSE)

honeycomb_grid_sf <- st_sf(area_honeycomb_grid) %>% 
  mutate(grid_id = 1:length(lengths(area_honeycomb_grid))) 
honeycomb_grid_sf$swd <- lengths(st_intersects(honeycomb_grid_sf, dff))  
fishnet_count <- filter(honeycomb_grid_sf, swd > 0)
fishnet_count$swd %>% sum; sum(df$fishsampled) #confirmed same

sampled_sword <- tm_shape(fishnet_count["swd"]) +
  tm_fill(col ="swd",
          palette = viridis::mako(255),
          style = "cont",
          title = "Swordfish caught",
          legend.is.portrait = F,
          n = 10) +
  tm_graticules(ticks = T, lwd = 0.5, n.x = 6, n.y = 6, 
                col = "transparent", labels.size = 1) +
  tm_layout(legend.frame = F, 
            fontfamily = "Arial Narrow",
            legend.outside = T,
            inner.margins = c(0.01, 0.14, 0.01, 0.01)) +
  tm_shape(fishnet_count["swd"]) +
  tm_fill(col ="swd",
          palette = viridis::mako(255),
          style = "cont",
          legend.show = F) +
  tm_shape(croppedseamount) +
  tm_polygons(alpha = 0.01, border.col = "grey", lwd = 0.7) +
  tm_shape(nc) +
  tm_polygons(col = "white", lty = 2, lwd = 1, border.col = "grey") +
  tm_shape(both) +
  tm_polygons(alpha = 0.001, lty = 1, lwd = 1, border.col = "black") +
  tm_shape(stray) +
  tm_polygons(lwd = 0.5) 

sampled_sword
