# Create a MONTHLY SST climatology using daily ACCESS-S2 RA SST
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)

# Dependencies ------------------------------------------------------------

library(raster)
library(tictoc)
library(purrr)
library(terra)

dat <- readRDS("spatialmetadata_alltrips.RDS") #not available in repo
dat <- dat[, c("Latitude", "Longitude", "Date", "Trip", "Vessel", "company")]

start <- as.Date("1981-01-01")
end <- as.Date("2018-12-31")
timeSeq <- seq(start, end, by = "day")
yrs <- unique(year(timeSeq))

oFold <- "/Volumes/OWC_STX_HDD/Volumes/ACCESS-S/regridded/do/sst" 
#path to where regridded daily SST .nc files

mth <- list()
for (i in 1:12) {
  mth[[i]] <- formatC(i, digits = 2, width = 2, flag = "0")
}
mthh <- unlist(mth)


# Create monthly rasters --------------------------------------------------

allmth <- list()
allmthsd <- list()

for (j in 1:length(mthh)) { #for each month
  
  path <- paste0(oFold, "/", list.files(oFold))
  path <- path[1:38] #don't include 2019 onwards
  rasty <- list()
  
  for (k in 1:length(path)) { #for each year
    
    rs <- rast(path[k])
    yr <- gsub("\\D", "", path[k])
    names(rs) <- gsub("sst_deptht=0.50576001_", "", names(rs))
    names(rs) <- as.Date(names(rs) %>% as.integer, 
                         origin = as.Date(paste0(yr, "-01-01"))-1)
    new <- rs[[which(month(names(rs)) == j)]]
    rasty[[k]] <- new
    
  } #year for loop
  
  all_rasts <- rast(rasty)
  all_rasts <- crop(all_rasts, ext(140, 179, -50, -10))
  monthlyclim <- mean(all_rasts)
  sdclim <- stdev(all_rasts)
  names(monthlyclim) <- paste0("month_", mthh[j])
  names(sdclim) <- paste0("month_", mthh[j])
  
  allmth[[j]] <- monthlyclim
  allmthsd[[j]] <- sdclim
  
}

writeRaster(rast(allmth), 
            "cleaned_data/climatologies/month_climatology_ETBF_1981-2018_ACCESS-S-RA.grd")
writeRaster(rast(allmthsd), 
            "cleaned_data/climatologies/month_climatology_stdev_ETBF_1981-2018_ACCESS-S-RA.grd")


# Extract assoc clim value from latlon of interest ------------------------

df_list <- list()

for (i in 1:nrow(dat)) { #for each latlon
  
  datt <- dat[i,]
  pointy <- data.frame(x = datt["Longitude"], y = datt["Latitude"])
  datey <- datt$Date
  
  df <- data.frame(date = datey, 
                   mon_clim = NA, 
                   mon_clim_sd = NA,
                   lat = datt["Latitude"], 
                   lon = datt["Longitude"],
                   trip = datt["Trip"], 
                   vessel = datt["Vessel"], 
                   company = datt["company"])
  
  month_rast <- month(datey)
  rasty <- allmth[[month_rast]]
  val <- extract(rasty, pointy)
  df[, "mon_clim"] <- as.numeric(val[2])
  rasty_sd <- allmthsd[[month_rast]]
  val <- extract(rasty_sd, pointy)
  df[, "mon_clim_sd"] <- as.numeric(val[2])
  df_list[[i]] <- df #df with one row of temp for each year corresponding to day
  print(paste0(i, "/", nrow(dat)))
  
}

nearly_all <- do.call("rbind", df_list)
 saveRDS(nearly_all, "cleaned_data/ssts_latlons_monthly_1981-2018.RDS")
# not available in repo


# Calculate average clim for each trip ------------------------------------

df2 <- nearly_all %>% 
  group_by(Trip, Vessel, company) %>% 
  summarise(mon_clim_sst = mean(mon_clim),
            mon_clim_sst_sd = mean(mon_clim_sd)) %>% 
  as.data.frame()


# Average of Company 2 trips ----------------------------------------------------

problems <- c(1,4,6,7)

for (i in problems) { 
  df2[df2$company == "REDACTED" &  #REDACTED
        df2$Trip ==i, ]$mon_clim_sst <- df2[df2$company == "REDACTED" &  #REDACTED
                                              df2$Trip ==i, ]$mon_clim_sst %>% mean
  df2[df2$company == "REDACTED" &  #REDACTED
        df2$Trip ==i, ]$mon_clim_sst_sd <- df2[df2$company == "REDACTED" & #REDACTED
                                                 df2$Trip ==i, ]$mon_clim_sst_sd %>% mean
}

df2[ -c(1,4,7,9,10,11),] -> df4 #manually remove duplicate rows
df4[df4$company == "REDACTED" & df4$Trip == 1,]$Vessel <-"REDACTED" #REDACTED


# Save to wd() ------------------------------------------------------------

saveRDS(df4, "cleaned_data/dat_sst_clim_latlons_monthly_1981-2018.RDS")
# not available in repo