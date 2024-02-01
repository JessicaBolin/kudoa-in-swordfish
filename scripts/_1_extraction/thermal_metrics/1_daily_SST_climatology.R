# Create a DAILY SST climatology using ACCESS-S2 RA SST
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)

# Dependencies ------------------------------------------------------------

library(lubridate)
library(sf)
library(sp)
library(terra)
library(raster)
library(tictoc)

# Fishing data
dat <- readRDS("spatialmetadata_alltrips.RDS")
dat <- dat[, c("Latitude", "Longitude", "Date", "Trip", "Vessel", "company")]

# Env data
oFold <- "ACCESS-S/regridded/do/sst" 
#path to where regridded ACCESS SST .nc files are stored
oFold1 <- dir(oFold, pattern = ".nc")

# Define climatology ------------------------------------------------------

#Climatology is 1981 to 2018, as per ACCESS-S hindcast
start <- as.Date("1981-01-01")
end <- as.Date("2018-12-31")
timeSeq <- seq(start, end, by = "day")
yrs <- unique(year(timeSeq))


# For loop ----------------------------------------------------------------

all_coords <- list()

tic(); for (i in 1:nrow(dat)) { #for each latlon
  
  datt <- dat[i,]
  pointy <- data.frame(x = datt["Longitude"], y = datt["Latitude"])
  datey <- datt$Date
  df_list <- list()
  if (datey == "2020-12-31") { datey = "2020-12-30" }
  
  # Metadata for shot of interest
  df <- data.frame(date = NA, 
                   temp = NA, 
                   lat = datt["Latitude"], 
                   lon = datt["Longitude"],
                   trip = datt["Trip"], 
                   vessel = datt["Vessel"], 
                   company = datt["company"])
  
  for (j in 1:length(unique(year(timeSeq)))) { #for each year
    
    # Prepare raster
    path <- paste0(oFold, "/", list.files(oFold)[j])
    rs <- rast(path)
    yr <- gsub("\\D", "", path) #remove all non-digits
    names(rs) <- gsub("sst_deptht=0.50576001_", "", names(rs))
    names(rs) <- as.Date(names(rs) %>% as.integer, 
                         origin = as.Date(paste0(yr, "-01-01"))-1)
    dy <- substr(datey, 5, 10)
    
    # Extract SST
    ind <- grep(dy, names(rs))
    val <- extract(rs[[ind]], pointy)
    df[, "temp"] <- as.numeric(val[2])
    df[, "date"] <- paste0(yr, dy)
    df_list <- rbind(df_list, df) 
    
  }
  
  all_coords[[i]] <- df_list
  print(paste0(i, "/", nrow(dat)))
  
}; toc() #27 minutes on 2018 Macbook Pro

saveRDS(all_coords, 
        "cleaned_data/env/ssts_latlons_daily_1981-2018.RDS")
# not available in repo


# Calculate Daily Climatology for each lat/lon ---------------------------------------------

dat2 <- dat
dat2$sst_clim <- NA
dat2$sst_clim_sd <- NA

for (i in 1:nrow(dat2)) {
  dat2$sst_clim[i] <- all_coords[[i]]$temp %>% mean
  dat2$sst_clim_sd[i] <- all_coords[[i]]$temp %>% sd
}


# Calculate average clim for each trip ------------------------------------

dat3 <- dat2 %>% 
  group_by(Trip, Vessel, company) %>% 
  summarise(sst_clim_do = mean(sst_clim),
            sst_clim_do_sd = sd(sst_clim)) %>% 
  as.data.frame()


# Average of Company 2 trips ----------------------------------------------------

problems <- c(1,4,6,7)

for (i in problems) { 
  dat3[dat3$company == "REDACTED" & #REDACTED
         dat3$Trip ==i, ]$sst_clim_do <- dat3[dat3$company == "REDACTED" & #REDACTED
                                                dat3$Trip ==i, ]$sst_clim_do %>% mean
  dat3[dat3$company == "REDACTED" & #REDACTED
         dat3$Trip ==i, ]$sst_clim_do_sd <- dat3[dat3$company == "REDACTED" &  #REDACTED
                                                   dat3$Trip ==i, ]$sst_clim_do_sd %>% mean
}

dat3[ -c(1,4,7,9,10,11),] -> dat4 #manually remove duplicate rows
dat4[dat4$company == "REDACTED" & dat4$Trip == 1,]$Vessel <- "REDACTED" #REDACTED

saveRDS(dat4, "cleaned_data/env/dat_sst_clim_latlons_daily_1981-2018.RDS")
# not available in repo
