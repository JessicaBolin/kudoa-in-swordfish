# Daily rasters of SST, V, MonthlyClimSSTSD, MLD, and SSTa
# Part IV - V
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)


# Dependencies ------------------------------------------------------------

library(terra)
library(tidyverse)
library(lubridate)

# Daily V regridded ACCESS .nc files 
oFold <- "ACCESS-S/regridded/do/v"

# Where outputs will be stored
# daily
rast_dir_daily <- "access_rasts_preds/daily/V_"
# monthly averages
outty <- "access_rasts_preds/month_av"
# month by year averages
month_by_year_dir <- "access_rasts_preds/month_by_year/"
# seasonal averages
season_dir <- "access_rasts_preds/seasonal/"
# season by year averages
season_by_year_dir <- "access_rasts_preds/year_by_season/"



# Daily -------------------------------------------------------------------

oFold1 <- dir(oFold, pattern = ".nc")
oFold1 <- oFold1[39:length(oFold1)]
yrs <- c(2019, 2020, 2021, 2022) 
path <- paste0(oFold, "/", list.files(oFold))

for (i in yrs) { 
  
  filey <- path[grep(i, path)]
  rs <- rast(filey) 
  rs <- crop(rs, ext(140, 170, -50, -8))
  names(rs) <- gsub("v_depthv=0.50576001_", "", names(rs)) 
  names(rs) <- as.Date(names(rs) %>% as.integer, 
                       origin = as.Date(paste0(i, "-01-01"))-1)
  
  for (j in 1:nlyr(rs)) { 
    
    subbed <- rs[[j]]
    writeRaster(subbed, 
                paste0(rast_dir_daily, names(subbed), ".grd"), 
                overwrite = T)
    print(names(subbed)) 
    
  } 
} 


# Monthly  --------------------------------------------------------

mth_average <- function(yrs, mthy) {
  
  yrry <- function(var, yeary) {
    r1 <- rast(paste0(oFold, "/", "do_", var, "_", yeary, "_RG.nc"))
    r1 <- crop(r1, ext(140, 170, -50, -8))
    names(r1) <- gsub(paste0(var, "_depth", var, "=0.50576001_"),
                      "", names(r1)) 
    names(r1) <- as.Date(names(r1) %>% as.integer, 
                         origin = as.Date(paste0(yeary, "-01-01"))-1)
    new1 <- r1[[which(month(names(r1)) == mthy)]]
    av <- mean(new1)
    names(av) <- paste0(toupper(var), "_mean_", yeary, "_", mthy)
    mthlist[[i]] <<- av
  }
  
  for (i in 1:length(yrs)) {
    yrry(var = "v", yeary = yrs[i])
  }
  
}


### January  ####
#2020, 2021, 2022

mthlist <- list()
mth_average(yrs = c(2020, 2021, 2022), mthy = 1) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 1, ".grd"), 
            overwrite = T)

### February ####
#2020, 2021, 2022

mthlist <- list()
mth_average(yrs = c(2020, 2021, 2022), mthy = 2) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 2, ".grd"), 
            overwrite = T)

### March ####
#2020, 2021

mthlist <- list()
mth_average(yrs = c(2020, 2021), mthy = 3) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 3, ".grd"), 
            overwrite = T)


### April ####
#2020, 2021

mthlist <- list()
mth_average(yrs = c(2020, 2021), mthy = 4) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 4, ".grd"), 
            overwrite = T)

### May ####
#2020, 2021

mthlist <- list()
mth_average(yrs = c(2020, 2021), mthy = 5) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 5, ".grd"), 
            overwrite = T)


### June ####
#2020, 2021

mthlist <- list()
mth_average(yrs = c(2020, 2021), mthy = 6)
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 6, ".grd"), 
            overwrite = T)


### July ####
#2020, 2021

mthlist <- list()
mth_average(yrs = c(2020, 2021), mthy = 7) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 7, ".grd"), 
            overwrite = T)

### August ####
#2020, 2021

mthlist <- list()
mth_average(yrs = c(2020, 2021), mthy = 8) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 8, ".grd"), 
            overwrite = T)

### September ####
#2020, 2021

mthlist <- list()
mth_average(yrs = c(2020, 2021), mthy = 9) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 9, ".grd"), 
            overwrite = T)

### October ####
#2019, 2020, 2021

mthlist <- list()
mth_average(yrs = c(2019, 2020, 2021), mthy = 10) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 10, ".grd"), 
            overwrite = T)

### November ####
#2019, 2020, 2021

mthlist <- list()
mth_average(yrs = c(2019, 2020, 2021), mthy = 11) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 11, ".grd"), 
            overwrite = T)


### December ####
#2019, 2020, 2021

mthlist <- list()
mth_average(yrs = c(2019, 2020, 2021), mthy = 12) 
writeout <- rast(mthlist[1:length(mthlist)]) %>% mean
writeRaster(writeout, paste0(outty, "/", "V_mean_", 12, ".grd"), 
            overwrite = T)


# Monthly by year  --------------------------------------------------------

for (i in yrs) { 
  
  filey <- path[grep(i, path)]
  rs <- rast(filey) 
  rs <- crop(rs, ext(140, 170, -50, -8)) 
  names(rs) <- gsub("v_depthv=0.50576001_", "", names(rs)) 
  names(rs) <- as.Date(names(rs) %>% as.integer, 
                       origin = as.Date(paste0(i, "-01-01"))-1)
  
  for (j in 1:12) { #each month 
    
    new <- rs[[which(month(names(rs)) == j)]]
    av <- mean(new)
    mth <- month(names(new))[1]
    names(av) <- paste0("V_mean_", i, "_", mth)
    sd <- stdev(new)
    names(sd) <- paste0("V_sd_", i, "_", mth)
    writeRaster(av, 
                paste0(month_by_year_dir, 
                       names(av), ".grd"), 
                overwrite = T)
    writeRaster(sd, 
                paste0(month_by_year_dir, 
                       names(sd), ".grd"), 
                overwrite =  T)
    print(j)
  } 
  print(i)
} 



# Seasonal averages -------------------------------------------------------

# Spring
sep19 <- rast(paste0(month_by_year_dir, "V_mean_2019_9.grd"))
oct19 <- rast(paste0(month_by_year_dir, "V_mean_2019_10.grd"))
nov19 <- rast(paste0(month_by_year_dir, "V_mean_2019_11.grd"))
sep20 <- rast(paste0(month_by_year_dir, "V_mean_2020_9.grd"))
oct20 <- rast(paste0(month_by_year_dir, "V_mean_2020_10.grd"))
nov20 <- rast(paste0(month_by_year_dir, "V_mean_2020_11.grd"))
sep21 <- rast(paste0(month_by_year_dir, "V_mean_2021_9.grd"))
oct21 <- rast(paste0(month_by_year_dir, "V_mean_2021_10.grd"))
nov21 <- rast(paste0(month_by_year_dir, "V_mean_2021_11.grd"))
spring <- mean(sep19, oct19, nov19, 
               sep20, oct20, nov20,  
               sep21, oct21, nov21)
names(spring) <- "Vmean_spring_2019-2022"
spring_sd <- stdev(sep19, oct19, nov19, 
                   sep20, oct20, nov20, 
                   sep21, oct21, nov21)
names(spring_sd) <- "Vsd_spring_2019-2022"

# Winter
jun20 <- rast(paste0(month_by_year_dir, "V_mean_2020_6.grd"))
jul20 <- rast(paste0(month_by_year_dir, "V_mean_2020_7.grd"))
aug20 <- rast(paste0(month_by_year_dir, "V_mean_2020_8.grd"))
jun21 <- rast(paste0(month_by_year_dir, "V_mean_2021_6.grd"))
jul21 <- rast(paste0(month_by_year_dir, "V_mean_2021_7.grd"))
aug21 <- rast(paste0(month_by_year_dir, "V_mean_2021_8.grd"))
winter <- mean(jun20, jul20, aug20,  
               jun21, jul21, aug21)
names(winter) <- "Vmean_winter_2019-2022"
winter_sd <- stdev(jun20, jul20, aug20,  
                   jun21, jul21, aug21)
names(winter_sd) <- "Vsd_winter_2019-2022"

# Autumn
mar20 <- rast(paste0(month_by_year_dir, "V_mean_2020_3.grd"))
apr20 <- rast(paste0(month_by_year_dir, "V_mean_2020_4.grd"))
may20 <- rast(paste0(month_by_year_dir, "V_mean_2020_5.grd"))
mar21 <- rast(paste0(month_by_year_dir, "V_mean_2021_3.grd"))
apr21 <- rast(paste0(month_by_year_dir, "V_mean_2021_4.grd"))
may21 <- rast(paste0(month_by_year_dir, "V_mean_2021_5.grd"))
autumn <- mean(mar20, apr20, may20, 
               mar21, apr21, may21)
names(autumn) <- "Vmean_autumn_2019-2022"
autumn_sd <- stdev(mar20, apr20, may20, 
                   mar21, apr21, may21)
names(autumn_sd) <- "Vsd_autumn_2019-2022"

# Summer
dec19 <- rast(paste0(month_by_year_dir, "V_mean_2019_12.grd"))
jan20 <- rast(paste0(month_by_year_dir, "V_mean_2020_1.grd"))
feb20 <- rast(paste0(month_by_year_dir, "V_mean_2020_2.grd"))
dec20 <- rast(paste0(month_by_year_dir, "V_mean_2020_12.grd"))
jan21 <- rast(paste0(month_by_year_dir, "V_mean_2021_1.grd"))
feb21 <- rast(paste0(month_by_year_dir, "V_mean_2021_2.grd"))
dec21 <- rast(paste0(month_by_year_dir, "V_mean_2021_12.grd"))
jan22 <- rast(paste0(month_by_year_dir, "V_mean_2022_1.grd"))
feb22 <- rast(paste0(month_by_year_dir, "V_mean_2022_2.grd"))
summer <- mean(dec19, 
               jan20, feb20, dec20, 
               jan21, feb21, dec21, 
               jan22, feb22)
names(summer) <- "Vmean_summer_2019-2022"
summer_sd <- stdev(dec19, 
                   jan20, feb20, dec20, 
                   jan21, feb21, dec21, 
                   jan22, feb22)
names(summer_sd) <- "Vsd_summer_2019-2022"

### Write to wd

writeRaster(summer, paste0(season_dir, names(summer), ".grd"), 
            overwrite = T)
writeRaster(summer_sd, paste0(season_dir, names(summer_sd), ".grd"), 
            overwrite = T)
writeRaster(autumn, paste0(season_dir, names(autumn), ".grd"), 
            overwrite = T)
writeRaster(autumn_sd, paste0(season_dir, names(autumn_sd), ".grd"), 
            overwrite = T)
writeRaster(winter, paste0(season_dir, names(winter), ".grd"), 
            overwrite = T)
writeRaster(winter_sd, paste0(season_dir, names(winter_sd), ".grd"), 
            overwrite = T)
writeRaster(spring, paste0(season_dir, names(spring), ".grd"), 
            overwrite = T)
writeRaster(spring_sd, paste0(season_dir, names(spring_sd), ".grd"), 
            overwrite = T)


# Season by year ----------------------------------------------------------

### Spring 2019

spring19 <- mean(sep19, oct19, nov19)
names(spring19) <- "V_mean_2019_Spring"
writeRaster(spring19, 
            paste0(season_by_year_dir, names(spring19), ".grd"), overwrite = T)
spring19_sd <- stdev(sep19, oct19, nov19)
names(spring19_sd) <- "V_sd_2019_Spring"
writeRaster(spring19_sd, 
            paste0(season_by_year_dir, names(spring19_sd), ".grd"), overwrite = T)

### Summer 2019-20

summer19 <- mean(dec19, jan20, feb20)
names(summer19) <- "V_mean_2019-20_Summer"
writeRaster(summer19, 
            paste0(season_by_year_dir, names(summer19), ".grd"), overwrite = T)
summer19_sd <- stdev(dec19, jan20, feb20)
names(summer19_sd) <- "V_sd_2019_Spring"
writeRaster(summer19_sd, 
            paste0(season_by_year_dir, names(summer19_sd), ".grd"), overwrite = T)

### Autumn 2020

autumn20 <- mean(mar20, apr20, may20)
names(autumn20) <- "V_mean_2020_Autumn"
writeRaster(autumn20, 
            paste0(season_by_year_dir, names(autumn20), ".grd"), overwrite = T)
autumn20_sd <- stdev(mar20, apr20, may20)
names(autumn20_sd) <- "V_sd_2020_Autumn"
writeRaster(autumn20_sd, 
            paste0(season_by_year_dir, names(autumn20_sd), ".grd"), overwrite = T)

### Winter 2020

winter20 <- mean(jun20, jul20, aug20)
names(winter20) <- "V_mean_2020_Winter"
writeRaster(winter20, 
            paste0(season_by_year_dir, names(winter20), ".grd"), overwrite = T)
winter20_sd <- stdev(jun20, jul20, aug20)
names(winter20_sd) <- "V_sd_2020_Winter"
writeRaster(winter20_sd, 
            paste0(season_by_year_dir, names(winter20_sd), ".grd"), overwrite = T)

### Spring 2020

spring20 <- mean(sep20, oct20, nov20)
names(spring20) <- "V_mean_2020_Spring"
writeRaster(spring20, 
            paste0(season_by_year_dir, names(spring20), ".grd"), overwrite = T)
spring20_sd <- stdev(sep20, oct20, nov20)
names(spring20_sd) <- "V_sd_2020_Spring"
writeRaster(spring20_sd, 
            paste0(season_by_year_dir, names(spring20_sd), ".grd"), overwrite = T)

### Summer 2020-21

summer20 <- mean(dec20, jan21, feb21)
names(summer20) <- "V_mean_2020-21_Summer"
writeRaster(summer20, 
            paste0(season_by_year_dir, names(summer20), ".grd"), overwrite = T)
summer20_sd <- stdev(dec20, jan21, feb21)
names(summer20_sd) <- "V_sd_2020-21_Summer"
writeRaster(summer20_sd,
            paste0(season_by_year_dir, names(summer20_sd), ".grd"), overwrite = T)

### Autumn 2021

autumn21 <- mean(mar21, apr21, may21)
names(autumn21) <- "V_mean_2021_Autumn"
writeRaster(autumn21, 
            paste0(season_by_year_dir, names(autumn21), ".grd"), overwrite = T)
autumn21_sd <- stdev(mar21, apr21, may21)
names(autumn21_sd) <- "V_sd_2021_Autumn"
writeRaster(autumn21_sd, 
            paste0(season_by_year_dir, names(autumn21_sd), ".grd"), overwrite = T)

### Winter 2021

winter21 <- mean(jun21, jul21, aug21)
names(winter21) <- "V_mean_2021_Winter"
writeRaster(winter21, 
            paste0(season_by_year_dir, names(winter21), ".grd"), overwrite = T)
winter21_sd <- stdev(jun21, jul21, aug21)
names(winter21_sd) <- "V_sd_2021_Winter"
writeRaster(winter21_sd, 
            paste0(season_by_year_dir, names(winter21_sd), ".grd"), overwrite = T)

### Spring 2021

spring21 <- mean(sep21, oct21, nov21)
names(spring21) <- "V_mean_2021_Spring"
writeRaster(spring21, 
            paste0(season_by_year_dir, names(spring21), ".grd"), overwrite = T)
spring21_sd <- stdev(sep21, oct21, nov21)
names(spring21_sd) <- "V_sd_2021_Spring"
writeRaster(spring21_sd, 
            paste0(season_by_year_dir, names(spring21_sd), ".grd"), overwrite = T)

### Summer 2021-22

summer21 <- mean(dec21, jan22, feb22)
names(summer21) <- "V_mean_2021-22_Summer"
writeRaster(summer21, 
            paste0(season_by_year_dir, names(summer21), ".grd"), overwrite = T)
summer21_sd <- stdev(dec21, jan22, feb22)
names(summer21_sd) <- "V_sd_2021-22_Summer"
writeRaster(summer21_sd, 
            paste0(season_by_year_dir, names(summer21_sd), ".grd"), overwrite = T)

