# Extract oceanographic data from ACCESS-S2 Reanalysis
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)

# Libraries ---------------------------------------------------------------

library(sf)
library(terra)
library(tidyverse)
library(data.table)
library(exactextractr) 
library(lubridate)

# Data --------------------------------------------------------------------

# Spatial metadata for each vessel/trip combination
meta <- readRDS("spatialmetadata_alltrips.RDS") #not in repo

regriddir <- "ACCESS-S/regridded/do"

# Company 1 ---------------------------------------------------------------

extractACCESS_Company1 <- function(partner = "REDACTED",
                                   var, # variable of interest
                                   diri = regriddir, # path to regridded ACCESS-S RA 
                                   xmin = 142,  
                                   xmax = 168,  
                                   ymin = -35,  
                                   ymax = -11,  
                                   timestep = "do") { 
  
  area <- raster::extent(xmin, xmax, ymin, ymax)
  
  comp <- meta %>% 
    filter(company == partner) %>% 
    plyr::ddply(c("Trip", "Vessel")) 
  
  tv_combo <- comp %>% 
    group_by(Trip, Vessel) %>% 
    count() %>% 
    dplyr::select(-n) %>% data.frame
  
  for (p in 1:nrow(tv_combo)) { 
    
    datee <- comp[comp$Trip == tv_combo$Trip[p] & comp$Vessel == tv_combo$Vessel[p],]
    biglist <- list()
    
    for (k in 1:nrow(datee)) {  
      
      latlat <- datee[k, "Latitude"]
      buff_poly <- data.frame(x = datee[k, "Longitude"], y = datee[k, "Latitude"]) %>% 
        st_as_sf(coords = c("x", "y")) %>% 
        st_combine() %>% 
        st_buffer(0.1, sp = T) 
      st_crs(buff_poly) <- 4326 
      
      newdate4 <- datee$Date[k]
      yrdate <- year(newdate4)
      doy <- yday(newdate4)
      
      r <- terra::rast(paste0(diri, "/", var, "/", 
                              timestep, "_", var, "_", yrdate, "_RG.nc")) 
      rr <- r[[doy]]
      st <- terra::crop(rr, area) %>% 
        exact_extract(y = buff_poly) 
      
      st[[1]]$coverage_fraction <- st[[1]]$coverage_fraction * 100 
      delete <- list() 
      
      for (f in 1:length(st[[1]]$coverage_fraction)) { 
        if (st[[1]]$coverage_fraction[f] < 1) { 
          delete[[f]] <- row.names(st[[1]][f,]) } } 
      destroy <- delete %>% unlist() %>% as.integer
      if (length(destroy) !=0) { 
        keep <- st[[1]][-destroy,]$value 
      } else { keep <- st[[1]]$value }
      
      biglist[[k]] <- keep
      
    } 
    
    bb <- unlist(biglist)
    comp[comp$Trip == tv_combo$Trip[p] & comp$Vessel == tv_combo$Vessel[p], 
         paste0("mean_", var)] <- mean(bb)
    comp[comp$Trip == tv_combo$Trip[p] & comp$Vessel == tv_combo$Vessel[p], 
         paste0("sd_", var)] <- sd(bb)
    
    print(paste0(p,"/", nrow(tv_combo))) 
    
  } 
  
  return(comp) 
  
} 


# Run function for Company #1----------------------------------------------------

company1_sst   <- extractACCESS_Company1(var = "sst", timestep = "do")
company1_sss   <- extractACCESS_Company1(var = "sss", timestep = "do")
company1_mld1  <- extractACCESS_Company1(var = "mld1", timestep = "do")
company1_hc300 <- extractACCESS_Company1(var = "hc300", timestep = "do")
company1_ssh   <- extractACCESS_Company1(var = "ssh_corrected", timestep = "do")
company1_v     <- extractACCESS_Company1(var = "v", timestep = "do")
company1_u     <- extractACCESS_Company1(var = "u", timestep = "do")
company1_eke   <- extractACCESS_Company1(var = "eke", timestep = "do")
company1_speed <- extractACCESS_Company1(var = "speed", timestep = "do")

# Merge
all_company1 <- left_join(company1_sst, company1_hc300) %>% 
  left_join(company1_mld1) %>% 
  left_join(company1_sss) %>% 
  left_join(company1_ssh) %>% 
  left_join(company1_u) %>% 
  left_join(company1_v) %>% 
  left_join(company1_eke) %>% 
  left_join(company1_speed)

# Mean latitude of haul for each trip
latty <- all_company1 %>% 
  group_by(Trip, Vessel, company) %>% 
  summarise(newlat = mean(Latitude)) %>% 
  as.data.frame

# Unique rows (i.e., summarise for each trip)
company1 <- all_company1 %>% 
  group_by(Trip, Vessel, company, FishingDuration, no_shots, mean_sst, 
           sd_sst, mean_hc300, sd_hc300, mean_mld1, sd_mld1, mean_sss, 
           sd_sss, mean_ssh_corrected, sd_ssh_corrected, mean_v, sd_v,
           mean_u, sd_u, mean_eke, sd_eke, mean_speed, sd_speed) %>% 
  count %>% 
  data.frame %>% 
  dplyr::select(-n)

# Merge latitudes and oceanographic/fishing data
company1 <- merge(company1, latty)


# Company 2 ---------------------------------------------------------------------

extractACCESS_Company2 <- function(partner = "REDACTED", 
                                   var, 
                                   diri = regriddir, #path to regridded ACCESS-S RA files 
                                   xmin = 142, 
                                   xmax = 168, 
                                   ymin = -35, 
                                   ymax = -11,
                                   timestep = "do") { 
  
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
      
      testy <- data.frame(x = datee[k, "Longitude"], 
                          y = datee[k, "Latitude"]) %>% 
        st_as_sf(coords = c("x", "y")) %>% 
        st_combine() %>% 
        st_buffer(0.1, sp = T)
      st_crs(testy) <- 4326 
      
      newdate4 <- datee$Date[k]
      yrdate <- year(newdate4)
      doy <- yday(newdate4)
      
      if (min(newdate4) == max(newdate4)) { 
        r <- terra::rast(paste0(diri, "/", var, "/", timestep, 
                                "_", var, "_", yrdate, "_RG.nc")) 
        rr <- r[[doy]]
        st <- terra::crop(rr, area) %>% 
          exact_extract(y = testy)
        
        st[[1]]$coverage_fraction <- st[[1]]$coverage_fraction * 100 
        delete <- list() 
      } else { } #else, do nothing. 
      
      for (f in 1:length(st[[1]]$coverage_fraction)) { 
        if (st[[1]]$coverage_fraction[f] < 1) { 
          delete[[f]] <- row.names(st[[1]][f,]) } } 
      destroy <- delete %>% unlist() %>% as.integer
      if (length(destroy) !=0) { 
        keep <- st[[1]][-destroy,]$value 
      } else { keep <- st[[1]]$value }
      
      biglist[[k]] <- keep
      
    } 
    
    bb <- unlist(biglist)
    comp[comp$Trip == p, paste0("mean_", var)] <- mean(bb)
    comp[comp$Trip == p, paste0("sd_", var)] <- sd(bb)
    print(paste0(p,"/", nrow(tv))) 
    
  } 
  
  return(comp) 
  
}

# Run function for Company #2----------------------------------------------------

# Extract
company2_sst   <- extractACCESS_Company2(var = "sst", timestep = "do")
company2_sss   <- extractACCESS_Company2(var = "sss", timestep = "do")
company2_mld1  <- extractACCESS_Company2(var = "mld1", timestep = "do")
company2_hc300 <- extractACCESS_Company2(var = "hc300", timestep = "do")
company2_ssh   <- extractACCESS_Company2(var = "ssh_corrected", timestep = "do")
company2_v     <- extractACCESS_Company2(var = "v", timestep = "do")
company2_u     <- extractACCESS_Company2(var = "u", timestep = "do")
company2_eke   <- extractACCESS_Company2(var = "eke", timestep = "do")
company2_speed <- extractACCESS_Company2(var = "speed", timestep = "do")

# Merge 
all_company2 <- left_join(company2_sst, company2_sss) %>% 
  left_join(company2_mld1) %>% 
  left_join(company2_hc300) %>% 
  left_join(company2_ssh) %>% 
  left_join(company2_u) %>% 
  left_join(company2_v) %>% 
  left_join(company2_eke) %>% 
  left_join(company2_speed)

# Mean latitude of haul locations for each sampling trip combination
latty <- all_company2 %>% 
  group_by(Trip, company) %>% 
  summarise(newlat = mean(Latitude)) %>% 
  as.data.frame

# Extract unique rows
company2 <- all_company2 %>% 
  group_by(Trip, Vessel, company, FishingDuration, no_shots, mean_sst, 
           sd_sst, mean_hc300, sd_hc300, mean_mld1, sd_mld1, mean_sss, 
           sd_sss, mean_ssh_corrected, sd_ssh_corrected, mean_v, sd_v,
           mean_u, sd_u, mean_eke, sd_eke, mean_speed, sd_speed) %>% 
  count %>% 
  data.frame %>% 
  dplyr::select(-n)

# For Trips 1, 4, 6, 7, remove duplicate rows (due to multiple vessels)
company2 <- company2[-c(2,4,7,9,10,11),] # manually removed rows

# Merge with latitudes
company2 <- merge(company2, latty)

# Bind both companies
all <- rbind(company1, company2)
all %>% head


# Write to dir ------------------------------------------------------------

saveRDS(all, "cleaned_data/env/dataenv_surface_do.RDS") 
# not made available in repo