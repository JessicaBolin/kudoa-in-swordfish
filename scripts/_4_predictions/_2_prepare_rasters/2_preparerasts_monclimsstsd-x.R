# Daily rasters of SST, V, MonthlyClimSSTSD, MLD and SSTa
# Part II - MonthlyClimSSTSD
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)


# Dependencies ------------------------------------------------------------

library(terra)
library(tidyverse)
library(lubridate)

out_dir <- "access_rasts_preds/mon_clim_sst_sd/"


# Outputs -----------------------------------------------------------------


### Monthly (1981-2018)  ####

mthclim <- rast("month_climatology_stdev_ETBF_1981-2018_ACCESS-S-RA.grd")
writeRaster(mthclim, paste0(out_dir, "SST_SD_mth_clim_1981-2018.grd"),
            overwrite = T)


### Seasonal (1981-2018) ####

# Seasonal average of the monthly SST clim SD
summer <- mthclim[[c(1,2,12)]] %>% mean
summer_sd <- mthclim[[c(1,2,12)]] %>% stdev
autumn <- mthclim[[c(3,4,5)]] %>% mean
autumn_sd <- mthclim[[c(3,4,5)]] %>% stdev
winter <- mthclim[[c(6,7,8)]] %>% mean
winter_sd <- mthclim[[c(6,7,8)]] %>% stdev
spring <- mthclim[[c(9,10,11)]] %>% mean
spring_sd <- mthclim[[c(9,10,11)]] %>% stdev

writeRaster(summer, 
            paste0(out_dir, "SST_SD_season_clim_1981-2018_SUMMER.grd"),
            overwrite = T)
writeRaster(summer_sd, 
            paste0(out_dir, "SST_SD_season_clim_1981-2018_SUMMER_SD.grd"),
            overwrite = T)
writeRaster(autumn, 
            paste0(out_dir, "/SST_SD_season_clim_1981-2018_AUTUMN.grd"),
            overwrite = T)
writeRaster(autumn_sd, 
            paste0(out_dir, "SST_SD_season_clim_1981-2018_AUTUMN_SD.grd"),
            overwrite = T)
writeRaster(winter, 
            paste0(out_dir, "SST_SD_season_clim_1981-2018_WINTER.grd"),
            overwrite = T)
writeRaster(winter_sd, 
            paste0(out_dir, "SST_SD_season_clim_1981-2018_WINTER_SD.grd"),
            overwrite = T)
writeRaster(spring, 
            paste0(out_dir, "SST_SD_season_clim_1981-2018_SPRING.grd"),
            overwrite = T)
writeRaster(spring_sd, 
            paste0(out_dir, "SST_SD_season_clim_1981-2018_SPRING_SD.grd"),
            overwrite = T)
