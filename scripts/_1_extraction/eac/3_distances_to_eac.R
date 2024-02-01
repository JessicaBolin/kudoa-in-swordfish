# Prepare rasters of distance to EAC and extract distances (Part II)
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)
# Last updated: 2024-02-01

# Steps
# 3. Read in rasters corresponding to shots and extract distances
# 4. Compute average dist for each trip/vessel combination

# Best method for PCA: SST, SSH, V, Speed
# All steps correspond to this directory: /EAC

# Libraries and dependencies ---------------------------------------------------------------

# Libraries
library(raster)
library(tidyverse)
library(lubridate)
library(terra)

setwd("/EAC") #path to where /EAC is stored

# Env data
area <- extent(c(xmin = 140.125, xmax = 170.125, ymin = -45, ymax = -8)) #extent 
dir = "3a_finalrasters"

meta <- readRDS("spatialmetadata_alltrips.RDS")
# not available in repo


# Step 3: Extract distances ------------------------------------------------------------

# Company 1 ---------------------------------------------------------------

# Company 1 metadata data
comp <- meta %>% 
  filter(company == "REDACTED") %>%  # REDACTED
  plyr::ddply(c("Trip", "Vessel")) 

# Trip vessel combos
tv <- comp %>% 
  group_by(Trip, Vessel) %>% 
  dplyr::count() %>% 
  dplyr::select(-n) %>% data.frame

# Empty list
all.boats <- list()

for (p in 1:nrow(tv)) { #for each trip/vessel combo
  
  datee <- comp[comp$Trip == tv$Trip[p] & comp$Vessel == tv$Vessel[p],] 
  outputs <- list()
  
  for (k in 1:nrow(datee)) {  #for each date in the fishing trip
    
    # Read in raster
    yearmonth <- datee[k, "Date"]
    if (yearmonth == "2020-12-31") { yearmonth = "2020-12-30" }
    fileys <- list.files(dir, pattern = ".grd$")[grep(yearmonth, 
                                                      list.files(dir, pattern = ".grd$"))]
    pc1.map <- raster(paste0(dir, "/", fileys))
    
    # For each row in PC1 raster: (o) extract PC1 cell values (ii) get the 
    # corresponding longitudes,
    # (iii) indentify cells not within the a priori threshold of the EAC core (152-155.5˚),
    # (iv) overwrite those cells with NA and (v) put the NAs back into the raster
    for (f in 1:nrow(pc1.map)) { 
      rows <- f
      values <- pc1.map[rows,]
      longtest <- xFromCol(pc1.map, col = 1:ncol(pc1.map))
      outerthreshold <- 155.5
      KILL <- which(longtest[] > outerthreshold)
      values[KILL] <- NA
      pc1.map[f,] <- values
    }
    
    #values of 'new' PC1 raster
    pc1_newvals <- pc1.map@data@values
    #   plot(pc1.map)
    
    # Identify the cell that has the maximum value of PC1 within the a priori longitudinal
    # threshold, in the row corresponding the shot of interest's latitude
    pcdat <- setNames(data.frame(cbind(coordinates(pc1.map), pc1_newvals)), 
                      c("lon", "lat", "PC1"))
    lats <- unique(pcdat$lat)
    a <- datee$Latitude[k] 
    datee$close.lats[k] <- lats[which(abs(lats - a) == min(abs(lats-a)))][1]
    pcdat2 <- pcdat[pcdat$lat %in% datee$close.lats , ] 
    it <- subset(pcdat2, lat == datee$close.lats[k]) %>% na.omit
    max.point <- it[it$PC1 == max(it$PC1),]  
    names(max.point) <- c("max.lon", "shot.lat", "PC1")
    max.pointy <- cbind(max.point, datee[k,]) 
    
    # Distance from shot to core of the EAC (max value of PC1), in km.
    edgedist <- geosphere::distm(c(max.pointy$max.lon, 
                                   max.pointy$shot.lat), 
                                 c(max.pointy$Longitude, 
                                   max.pointy$shot.lat))
    max.pointy$distcore <- edgedist/1000 
    outputs[[k]] <- max.pointy 
    
    print(paste0(p, "_", yearmonth))
  
  }
  
  all.boat <- do.call("rbind", outputs)  
  all.boats[[p]] <- all.boat
  
}

all_company1 <- do.call("rbind", all.boats)



# Company 2 ---------------------------------------------------------------

comp <- meta %>% 
  filter(company == "REDACTED") %>% #REDACTED
  plyr::ddply(c("Trip", "Vessel")) 

tv <- comp %>% 
  group_by(Trip, Vessel) %>% 
  dplyr::count() %>% 
  dplyr::select(-n) %>% data.frame

all.boats <- list()

for (p in unique(tv$Trip)) { 
  
  datee <- comp[comp$Trip == p,]
  outputs <- list()
  
  for (k in 1:nrow(datee)) {  
    
    yearmonth <- datee[k, "Date"]
    if (yearmonth == "2020-12-31") { yearmonth = "2020-12-30" }
    fileys <- list.files(dir, pattern = ".grd$")[grep(yearmonth, 
                                                      list.files(dir, pattern = ".grd$"))]
    pc1.map <- raster(paste0(dir, "/", fileys))
    
    for (f in 1:nrow(pc1.map)) { 
      rows <- f
      values <- pc1.map[rows,] 
      longtest <- xFromCol(pc1.map, col = 1:ncol(pc1.map)) 
      # innerthreshold <- 152
      outerthreshold <- 155.5
      KILL <- which(longtest[] > outerthreshold) 
      values[KILL] <- NA  
      pc1.map[f,] <- values 
    }
    
    pc1_newvals <- pc1.map@data@values 
    pcdat <- setNames(data.frame(cbind(coordinates(pc1.map), pc1_newvals)), 
                      c("lon", "lat", "PC1"))
    lats <- unique(pcdat$lat) 
    a <- datee$Latitude[k] 
    datee$close.lats[k] <- lats[which(abs(lats - a) == min(abs(lats-a)))][1] 
    pcdat2 <- pcdat[pcdat$lat %in% datee$close.lats, ] 
    it <- subset(pcdat2, lat == datee$close.lats[k]) %>% na.omit()
    max.point <- it[it$PC1 == max(it$PC1),] 
    names(max.point) <- c("max.lon", "shot.lat", "PC1") 
    max.pointy <- cbind(max.point, datee[k,]) 
    
    edgedist <- geosphere::distm(c(max.pointy$max.lon, 
                                   max.pointy$shot.lat), 
                                 c(max.pointy$Longitude, 
                                   max.pointy$shot.lat))
    max.pointy$distcore <- edgedist/1000
    outputs[[k]] <- max.pointy 
    
    print(paste0(p, "_", yearmonth))
    
  }
  
  all <- do.call("rbind", outputs)
  all.boats[[p]] <- all 
  
}

all_company2 <- do.call("rbind", all.boats)



# Step 4: Convert to trip/vessel combo-----------------------------

company2 <- all_company2 %>% 
  group_by(Trip) %>% 
  dplyr::summarise(dist_core = mean(distcore), 
                   sd_dist_core = sd(distcore)) %>% 
  dplyr::mutate(company = "REDACTED", Vessel = NA) %>%  #REDACTED
  as.data.frame

company1 <- all_company1 %>% 
  group_by(Trip, Vessel) %>% 
  dplyr::summarise(dist_core = mean(distcore), 
                   sd_dist_core = sd(distcore)) %>%
  dplyr::mutate(company = "REDACTED") %>% #REDACTED
  as.data.frame

total <- rbind(company1, company2)

mean(total$dist_core) #[1] 340.1483
sd(total$dist_core) #[1] 207.6555
median(total$dist_core) #[1] 341.0634
range(total$dist_core) #[1]  47.75904 762.07585

saveRDS(total, "distances_to_core_best_method.RDS") #not in repo
