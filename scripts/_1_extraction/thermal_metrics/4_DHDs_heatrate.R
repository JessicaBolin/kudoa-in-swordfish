# Extract degree heating days and heating rate from ACCESS-S2 RA
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)


# - Both metrics calculated similar to Maynard et al. 2008 re. LMST
# - Climatology defined here as 1981-2018, as per ACCESS-S2 hindcast period


# Dependencies ------------------------------------------------------------

### Libraries ###

library(tidyverse)
library(terra)
library(lubridate)

### Data ###

# Fishing metadata
dat <- readRDS("spatialmetadata_alltrips.RDS")
# not available in repo

dat <- dat[, c("Latitude", "Longitude", "Date", "Trip", "Vessel", "company")]

# ACCESS-S2 RA 
oFold <- "ACCESS-S/regridded/do/sst" 
#path to regridded ACCESS-S RA SST daily nc files
path <- paste0(oFold, "/", list.files(oFold))
oFold1 <- dir(oFold, pattern = ".nc")
head(oFold1)
# [1] "do_sst_1981_RG.nc" "do_sst_1982_RG.nc" "do_sst_1983_RG.nc" "do_sst_1984_RG.nc" "do_sst_1985_RG.nc"
# [6] "do_sst_1986_RG.nc"

# Step 1 ------------------------------------------------------------------

# For each lat lon (i.e., shot), get SSTs for previous three months
# Note - this takes ~45 min on 2018 Macbook Pro

all_coords <- list()

for (i in 1:nrow(dat)) {
  
  datt <- dat[i,]
  pointy <- data.frame(x = datt["Longitude"], y = datt["Latitude"])
  datey <- datt$Date
  df_list <- list()
  
  # Remove 31st December in study period to avoid problems
  if (datey == "2020-12-31") { 
    datey = "2020-12-30" }
  
  start <- as.Date(datey)-(7*12) # Start date 12 weeks prior
  end <- datey #End date is date of shot 
  twelveweeks <- seq(start %>% as.Date, 
                     datey %>% as.Date, by = 1) # daily sequence over 12 weeks
  
  # dataframe for 12 weeks-worth of SSTs for shot of interest
  df <- data.frame(date = NA, 
                   temp = NA, 
                   lat = datt["Latitude"], 
                   lon = datt["Longitude"],
                   trip = datt["Trip"], 
                   vessel = datt["Vessel"], 
                   company = datt["company"])
  
  for (j in 1:length(twelveweeks)) { #for each day in 12 week period
    
    #problems if December 31st, fix
    if (twelveweeks[j] == "2019-12-31") { twelveweeks[j] = "2019-12-30" }
    if (twelveweeks[j] == "2020-12-31") { twelveweeks[j] = "2020-12-30" }
    if (twelveweeks[j] == "2021-12-31") { twelveweeks[j] = "2021-12-30" }
    # Read in and prepare raster
    yr <- twelveweeks[j] %>% year
    filey <- path[grep(yr, path)]
    rs <- rast(filey)
    names(rs) <- gsub("sst_deptht=0.50576001_", "", names(rs))
    names(rs) <- as.Date(names(rs) %>% as.integer, 
                         origin = as.Date(paste0(yr, "-01-01"))-1)
    # Extract SST for day of interest (within 12 week period)
    rasty <- rs[[which(twelveweeks[j] == names(rs))]]
    val <- terra::extract(rasty, pointy)
    # Write SSTs to df
    df[, "temp"] <- as.numeric(val[2])
    df[, "date"] <- twelveweeks[j]
    df_list <- rbind(df_list, df)
    print(j)
    
  }
  
  all_coords[[i]] <- df_list 
  print(paste0(i, "/", nrow(dat)))
  
}

saveRDS(all_coords, "cleaned_data/ssts_latlons_daily_DHDs_12weeksprior.RDS")
# not available in repo


# Step 2 & 3 ------------------------------------------------------------------

# Step 2 - for each day (shot) in each df (trip), extract monthly climatology
# Step 3 - Calculate deviations (anomalies) for each day in three week block 

start <- as.Date("1981-01-01") # First day of clim
end <- as.Date("2018-12-31") # Last day of clim
timeSeq <- seq(start, end, by = "day")
yrs <- unique(year(timeSeq))

# Pre-prepared rasters of ACCESS-S2 climatologies and SST data
rr <- rast("cleaned_data/climatologies/month_climatology_ETBF_1981-2018_ACCESS-S-RA.grd") 
# Monthly SST climatology
rr_std <- rast("cleaned_data/climatologies/month_climatology_stdev_ETBF_1981-2018_ACCESS-S-RA.grd") # Monthly SST climatology (std. dev)
all_coords <- readRDS("cleaned_data/ssts_latlons_daily_DHDs_12weeksprior.RDS") 
# SSTs for previous 12 weeks for each shot

all_coords1 <- list()

for (i in 1:length(all_coords)) { #for each df
  
  datt <- all_coords[[i]]
  df_list <- list()
  
  for (k in 1:nrow(datt)) { #each lat lon in df
    
    pointy <- data.frame(x = datt["Longitude"] %>% unique, 
                         y = datt["Latitude"] %>% unique)
    datey <- datt$date[k]
    
    if (datey == "2019-12-31") { datey = "2019-12-30" }
    if (datey == "2020-12-31") { datey = "2020-12-30" }
    if (datey == "2021-12-31") { datey = "2021-12-30" }
    
    df <- data.frame(date = NA, 
                     mo_clim_temp = NA, 
                     mo_clim_temp_sd = NA,
                     temp = datt[k, "temp"], 
                     anom_mo = NA,
                     lat = datt[k, "Latitude"], 
                     lon = datt[k, "Longitude"],
                     trip = datt[k, "Trip"], 
                     vessel = datt[k, "Vessel"], 
                     company = datt[k, "company"])
    
    mthy <- month(datey)
    rastey <- rr[[mthy]]
    rastey_std <- rr_std[[mthy]]
    vals <- terra::extract(rastey, pointy)
    vals_sd <- terra::extract(rastey_std, pointy)
    df[, "mo_clim_temp"] <- as.numeric(vals[2])
    df[, "mo_clim_temp_sd"] <- as.numeric(vals_sd[2])
    df[, "date"] <- datey
    df$anom_mo <- df$temp - df$mo_clim_temp #calculate anomalies
    df_list <- rbind(df_list, df) #df with one row of temp for each year corresponding to day
    
  }
  
  all_coords1[[i]] <- df_list
  print(paste0(i, "/", length(all_coords)))
  
}


# Step 4 -----------------------------------------------------------------

# Calculate DHDs and heating rate for each lat/lon 

all_coords2 <- list()

for (i in 1:length(all_coords1)) {
  
  datt <- all_coords1[[i]]
  newdat <- datt[which(datt$anom_mo > 0), ] #positive values only
  dhd <- sum(newdat$anom_mo) # summed positive deviations of ˚C over past 12 weeks
  datt$dhd <- dhd
  intens <- dhd / nrow(newdat) #heating rate (˚C/day over 12 weeks)
  datt$heat_rate <- intens
  all_coords2[[i]] <- datt
  
} #i for loop


# Step 5 ------------------------------------------------------------------

# Reduce dataframe to trip/vessel 

dat2 <- dat

for (i in 1:nrow(dat2)) {
  
  dat2$dhd[i] <- all_coords2[[i]]$dhd %>% mean
  dat2$heat_rate[i] <- all_coords2[[i]]$heat_rate %>% mean
  dat2$mo_clim_temp[i] <- all_coords2[[i]]$mo_clim_temp %>% mean
  dat2$mo_clim_temp_sd[i] <- all_coords2[[i]]$mo_clim_temp_sd %>% mean
  dat2$anom_mo[i] <- all_coords2[[i]]$anom_mo %>% mean
  
  if (dat2$heat_rate[i] == "NaN") {
    dat2$heat_rate[i] <- 0 }
  
} # i for loop

dat3 <- dat2 %>% 
  group_by(Trip, Vessel, company) %>% 
  summarise(dhd = mean(dhd),
            heat_rate = mean(heat_rate),
            anom_mo = mean(anom_mo)) %>% 
  as.data.frame()

dat3[ -c(1,4,7,9,10,11),] -> dat4 #manually remove duplicate rows
dat4[dat4$company == "REDACTED" & dat4$Trip == 1,]$Vessel <- "REDACTED" # REDACTED

# Write to wd -------------------------------------------------------------

saveRDS(dat4, "cleaned_data/datasst_DHDs_heatingrate_NEW.RDS")
# not available in repo
