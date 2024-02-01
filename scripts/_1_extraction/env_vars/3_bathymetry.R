# Extract bathymetry (depth and sd_depth) from GEBCO
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)

# Dependencies ------------------------------------------------------------

library(sf)
library(terra)
library(tidyverse)
library(data.table)
library(exactextractr)
library(lubridate)

meta <- readRDS("spatialmetadata_alltrips.RDS")
bathy <- raster::raster("gebco_2021_n-5.0_s-50.0_w135.0_e175.0.nc") #GEBCO .nc file (too big for repo)
area <- raster::extent(142, 168, -35, -11)
b <- raster::crop(bathy, area) # ETBF extent
bc <- raster::aggregate(b, 60) #0.25 resolution


# Company 1 ---------------------------------------------------------------

partner = "REDACTED" #REDACTED

comp <- meta %>% 
  filter(company == partner) %>% 
  plyr::ddply(c("Trip", "Vessel")) 

tv <- comp %>% 
  group_by(Trip, Vessel) %>% 
  count() %>% 
  dplyr::select(-n) %>% data.frame

for (p in 1:nrow(tv)) { #for each trip/vessel combo
  
  datee <- comp[comp$Trip == tv$Trip[p] & comp$Vessel == tv$Vessel[p],]
  biglist <- list()
  
  for (k in 1:nrow(datee)) {
    
    testy <- data.frame(x = datee[k, "Longitude"], y = datee[k, "Latitude"]) %>% 
      st_as_sf(coords = c("x", "y")) %>% 
      st_combine() %>% 
      st_buffer(0.1, sp = T) 
    st_crs(testy) <- 4326 
    st <- exact_extract(bc, y = testy, include_cell = T, include_xy = T) 
    
    st[[1]]$coverage_fraction <- st[[1]]$coverage_fraction * 100 
    delete <- list() 
    for (f in 1:length(st[[1]]$coverage_fraction)) { 
      if (st[[1]]$coverage_fraction[f] < 1) { 
        delete[[f]] <- row.names(st[[1]][f,])} } 
    destroy <- delete %>% unlist() %>% as.integer
    if (length(destroy) !=0) { 
      keep <- st[[1]][-destroy,]$value 
    } else { keep <- st[[1]]$value }
    
    biglist[[k]] <- keep
    
  } #k for loop 
  
  bb <- unlist(biglist)
  
  comp[comp$Trip == tv$Trip[p] & comp$Vessel == tv$Vessel[p], 
       paste0("mean_", "bathy")] <- mean(bb)
  comp[comp$Trip == tv$Trip[p] & comp$Vessel == tv$Vessel[p], 
       paste0("sd_", "bathy")] <- sd(bb)
  
  print(paste0(p,"/", nrow(tv))) 
  
} #for loop for p 

company1_bathy <- comp


# Company 2 ---------------------------------------------------------------

partner = "REDACTED" # REDACTED

comp <- meta %>% 
  filter(company == partner) %>% 
  plyr::ddply(c("Trip", "Vessel")) 

tv <- comp %>% 
  group_by(Trip, Vessel) %>% 
  count() %>% 
  dplyr::select(-n) %>% data.frame

for (p in unique(tv$Trip)) { #for each trip/vessel combo
  
  datee <- comp[comp$Trip == p,]
  biglist <- list()
  
  for (k in 1:nrow(datee)) {
    
    testy <- data.frame(x = datee[k, "Longitude"], y = datee[k, "Latitude"]) %>% 
      st_as_sf(coords = c("x", "y")) %>% 
      st_combine() %>% 
      st_buffer(0.1, sp = T) 
    st_crs(testy) <- 4326 
    
    st <- exact_extract(b, y = testy, include_cell = T, include_xy = T) 
    st[[1]]$coverage_fraction <- st[[1]]$coverage_fraction * 100 
    delete <- list() 
    for (f in 1:length(st[[1]]$coverage_fraction)) { 
      if (st[[1]]$coverage_fraction[f] < 1) { 
        delete[[f]] <- row.names(st[[1]][f,]) } } 
    
    destroy <- delete %>% unlist() %>% as.integer
    
    if (length(destroy) !=0) { 
      keep <- st[[1]][-destroy,]$value 
    } else {keep <- st[[1]]$value}
    
    biglist[[k]] <- keep
    
  } #k for loop (each date of tv)
  
  bb <- unlist(biglist)
  
  comp[comp$Trip == p, paste0("mean_", "bathy")] <- mean(bb)
  comp[comp$Trip == p, paste0("sd_", "bathy")] <- sd(bb)
  
  print(paste0(p,"/", length(unique(tv$Trip)))) 
} #for loop for p (row of trip/vessel combo)

company2_bathy <- comp
  

# Merge and calculate mean/sd per trip------------------------------------------

total <- rbind(company1_bathy, company2_bathy)

#Below companies redacted in code
company2_overall <- total %>% 
  filter(company == "REDACTED") %>%  #REDACTED
  group_by(Trip) %>% 
  dplyr::summarise(overallmeanBATHY = mean_bathy %>% unique, 
                   overallsdBATHY = sd_bathy %>% unique) %>% 
  data.frame() %>% 
  mutate(company = "REDACTED") #REDACTED

company1_overall <- total %>% 
  filter(company == "REDACTED") %>% #REDACTED
  group_by(Trip, Vessel) %>% 
  dplyr::summarise(overallmeanBATHY = mean_bathy %>% unique, 
                   overallsdBATHY = sd_bathy %>% unique) %>%
  data.frame() %>% 
  mutate(company = "REDACTED") #REDACTED

tt_bathy <- full_join(company1_overall, company2_overall)
tt_bathy <- tt_bathy[order(tt_bathy$Trip),]

saveRDS(tt_bathy, "cleaned_data/env/overall_BATHY.RDS")
# not made available in repo
