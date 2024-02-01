# Extracting chlorophyll-a from NASA: MODIS
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)

# The below code:
# (i)   Extracts chlorophyll from Aqua MODIS
# (ii)  Extracts variable from a buffered circle 0.2˚ in diameter, to account for longline drift. 
# (iii) Extracts variable at monthly temporal resolution to avoid cloud contamination


# Libraries ---------------------------------------------------------------

library(sf)
library(terra)
library(tidyverse)
library(data.table)
library(exactextractr) 
library(lubridate)

# Data and dependencies --------------------------------------------------------------------

meta <- readRDS("spatialmetadata_alltrips.RDS") 
diri = "Volumes/modis" #path where MODIS .nc files are stored 
xmin = 142
xmax = 168
ymin = -35
ymax = -11

# get sequences of dates for each file
list.files(diri) -> filey

# Extract metadata from each filename, since dates are in day of year, not normal dates
lookup <- data.frame(file = NA, start = NA, end = NA, month = NA, year = NA)

for (e in 1:length(filey)) {
  
  first <- substr(filey[e], 6,8)
  firstyr <- substr(filey[e], 2,5)
  last <- substr(filey[e], 13,15)
  endyr <- substr(filey[e], 9,12)
  firstdt <- as.Date(first %>% as.numeric, 
                     origin = paste0(firstyr, "-01-01") %>% as.Date -1)
  enddt <- as.Date(last %>% as.numeric, 
                   origin = paste0(endyr, "-01-01") %>% as.Date -1)
  lookup[e,]$file = e
  lookup[e,]$start = firstdt %>% as.character()
  lookup[e,]$end = enddt %>% as.character()
  lookup[e,]$month = enddt %>% month()
  lookup[e,]$year = enddt %>% year()
  
}



# Company 1 ---------------------------------------------------------------

partner = "REDACTED" #<REDACTED>

comp <- meta %>% 
  filter(company == partner) %>% 
  plyr::ddply(c("Trip", "Vessel")) 

area <- raster::extent(xmin, xmax, ymin, ymax) 

tv <- comp %>% 
  group_by(Trip, Vessel) %>% 
  count() %>% 
  dplyr::select(-n) %>% data.frame

for (p in 1:nrow(tv)) { #for each trip/vessel combo
  
  #extract dates and trip metadata for trip/vessel combo
  datee <- comp[comp$Trip == tv$Trip[p] & comp$Vessel == tv$Vessel[p],]
  biglist <- list()
  
  for (k in 1:nrow(datee)) {  # For each date, extract chl-a
    
    latlat <- datee[k, "Latitude"]
    testy <- data.frame(x = datee[k, "Longitude"], y = datee[k, "Latitude"]) %>% 
      st_as_sf(coords = c("x", "y")) %>% 
      st_combine() %>% 
      st_buffer(0.1, sp = T) 
    st_crs(testy) <- 4326 
    
    newdate4 <- datee$Date[k]
    yrdate <- year(newdate4)
    doy <- yday(newdate4)
    moy <- month(newdate4)
    
    # Get file corresponding to date of fishing shot
    nn <- subset(lookup, year == yrdate)
    mm <- subset(nn, month == moy)
    rowy <- mm$file
    thefinalfile <- filey[rowy]
    
    # Read in raster, apply running mean to smooth out residual cloud contamination
    rr <- raster::raster(paste0(diri, "/", thefinalfile), varname = "chlor_a") 
    st <- terra::crop(rr, area) %>% 
      aggregate(6) %>%  # 0.25˚ as per ACCESS-S2
      focal(w = matrix(1, ncol =3, nrow =3), fun = mean, NAonly = T, na.rm=T) %>% 
      exact_extract(y = testy) 
    
    # Only include cells in mean/sd calculation that contribute more than 1% of their 
    # area to the buffered polygon
    st[[1]]$coverage_fraction <- st[[1]]$coverage_fraction * 100
    if (is.nan(st[[1]]$value[1])) {st[[1]]$value[1] <- st[[1]]$value[2]}
    delete <- list() 
    for (f in 1:length(st[[1]]$coverage_fraction)) { 
      if (st[[1]]$coverage_fraction[f] < 1) { 
        delete[[f]] <- row.names(st[[1]][f,]) }} 
    destroy <- delete %>% unlist() %>% as.integer
    if (length(destroy) !=0) { 
      keep <- st[[1]][-destroy,]$value 
    } else { keep <- st[[1]]$value }
    
    biglist[[k]] <- keep
    
  } #k for loop (each date of tv)
  
  bb <- unlist(biglist)
  comp[comp$Trip == tv$Trip[p] & comp$Vessel == tv$Vessel[p], 
       paste0("mean_chla")] <- mean(bb)
  comp[comp$Trip == tv$Trip[p] & comp$Vessel == tv$Vessel[p], 
       paste0("sd_chla")] <- sd(bb)
  
  print(paste0(p,"/", nrow(tv))) #print iteration
  
} #for loop for p (row of trip/vessel combo)


all_company1 <- comp

#Latitude of haul for each trip
latty <- all_company1 %>% 
  group_by(Trip, Vessel, company) %>% 
  summarise(newlat = mean(Latitude)) %>% 
  as.data.frame

#extract unique rows - i.e., summarise for each trip
company1 <- all_company1 %>% 
  group_by(Trip, Vessel, company, FishingDuration, no_shots, mean_chla, 
           sd_chla) %>% 
  count %>% 
  data.frame %>% 
  dplyr::select(-n)

company1 <- merge(company1, latty)


# Company 2 ---------------------------------------------------------------------

partner = "REDACTED" #<REDACTED>

comp <- meta %>% 
  filter(company == partner) %>% 
  plyr::ddply(c("Trip", "Vessel")) 

area <- raster::extent(xmin, xmax, ymin, ymax) 

tv <- comp %>% 
  group_by(Trip, Vessel) %>% 
  count() %>% 
  dplyr::select(-n) %>% data.frame

for (p in unique(tv$Trip)) { 
  
  datee <- comp[comp$Trip == p,]
  biglist <- list()
  
  for (k in 1:nrow(datee)) {  
    
    testy <- data.frame(x = datee[k, "Longitude"], y = datee[k, "Latitude"]) %>% 
      st_as_sf(coords = c("x", "y")) %>% 
      st_combine() %>% 
      st_buffer(0.1, sp = T) 
    st_crs(testy) <- 4326 
    
    newdate4 <- datee$Date[k]
    yrdate <- year(newdate4)
    doy <- yday(newdate4)
    moy <- month(newdate4)
    
    if (min(newdate4) == max(newdate4)) {
      
      nn <- subset(lookup, year == yrdate)
      mm <- subset(nn, month == moy)
      rowy <- mm$file
      thefinalfile <- filey[rowy]
      
      rr <- raster::raster(paste0(diri, "/", thefinalfile), varname = "chlor_a") 
      st <- terra::crop(rr, area) %>% 
        aggregate(6) %>% 
        focal(w = matrix(1, ncol =3, nrow =3), fun = mean, NAonly = T, na.rm=T) %>% 
        exact_extract(y = testy) 
      st[[1]]$coverage_fraction <- st[[1]]$coverage_fraction * 100 
      delete <- list() } else { }
    
    for (f in 1:length(st[[1]]$coverage_fraction)) { 
      if (st[[1]]$coverage_fraction[f] < 1) { 
        delete[[f]] <- row.names(st[[1]][f,]) } } 
    destroy <- delete %>% unlist() %>% as.integer
    if (length(destroy) !=0) { 
      keep <- st[[1]][-destroy,]$value 
    } else { keep <- st[[1]]$value }
    
    biglist[[k]] <- keep
    
  } #k for loop (each date of tv)
  
  bb <- unlist(biglist)
  comp[comp$Trip == p, paste0("mean_chla")] <- mean(bb)
  comp[comp$Trip == p, paste0("sd_chla")] <- sd(bb)
  
  print(paste0(p,"/", nrow(tv))) #print iteration
  
} #for loop for p (row of trip/vessel combo)

#merge 
all_company2 <- comp

latty <- all_company2 %>% 
  group_by(Trip, company) %>% 
  summarise(newlat = mean(Latitude)) %>% 
  as.data.frame

#extract unique rows
company2 <- all_company2 %>% 
  group_by(Trip, Vessel, company, FishingDuration, no_shots, mean_chla, sd_chla) %>% 
  count %>% 
  data.frame %>% 
  dplyr::select(-n)

#For Trip 1, 4, 6, 7, remove duplicates
company2 <- company2[-c(2,4,7,9,10,11),] #manually removed rows

company2 <- merge(company2, latty)

#Bind both companies
all <- rbind(company1, company2)

# Write to dir ------------------------------------------------------------

saveRDS(all, "cleaned_data/env/env_surface_do_CHLA.RDS") 
# not made available in repo
