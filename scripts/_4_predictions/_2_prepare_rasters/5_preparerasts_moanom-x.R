# Daily rasters of SST, V, MonthlyClimSSTSD, MLD, and SSTa
# Part V - SSTa
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)



# Dependencies ------------------------------------------------------------

library(terra)
library(tidyverse)
library(lubridate)

# Daily SST regridded ACCESS .nc files 
oFold <- "ACCESS-S/regridded/do/sst"

# Read in SST climatologies
clim <- rast("cleaned_data/climatologies/month_climatology_ETBF_1981-2018_ACCESS-S-RA.grd")

# Where outputs will be stored
# daily
rast_dir_daily <- "access_rasts_preds/daily/SSTa_"
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
  names(rs) <- gsub("sst_deptht=0.50576001_", "", names(rs)) 
  names(rs) <- as.Date(names(rs) %>% as.integer, 
                       origin = as.Date(paste0(i, "-01-01"))-1)
  
  for (j in 1:nlyr(rs)) { #each day in year
    
    subbed <- rs[[j]]
    mth <- month(subbed %>% names)
    climmth <- clim[[mth]]
    sub1 <- subbed %>% crop(ext(140, 170, -50, -10))
    climmth1 <- climmth %>% crop(ext(140, 170, -50, -10))
    anom <- sub1 - climmth1 
    names(anom) <- paste0(names(anom), "_monthly")
    
   writeRaster(anom, 
                paste0(rast_dir_daily, 
                       names(anom), ".grd"), 
                overwrite = T)
    print(names(anom)) 
    
  } #daily
} #yearly



# Monthly by year averages --------------------------------------------------------

for (i in yrs) { 
  
  filey <- path[grep(i, path)]
  rs <- rast(filey) 
  rs <- crop(rs, ext(140, 170, -50, -8)) 
  names(rs) <- gsub("sst_deptht=0.50576001_", "", names(rs)) 
  names(rs) <- as.Date(names(rs) %>% as.integer, 
                       origin = as.Date(paste0(i, "-01-01"))-1)
  
  for (j in 1:12) { 
    
    new <- rs[[which(month(names(rs)) == j)]]
    av <- mean(new)
    mth <- month(names(new))[1]
    names(av) <- paste0("SST_mean_", i, "_", mth)
    climmth <- clim[[mth]]
    sub1 <- av %>% crop(ext(140, 170, -50, -10))
    climmth1 <- climmth %>% crop(ext(140, 170, -50, -10))
    anom_mth <- sub1 - climmth1 
    names(anom_mth) <- paste0(names(anom_mth))
  
    writeRaster(anom_mth, 
                paste0(month_by_year_dir, "anom_", names(av), ".grd"), 
                overwrite = T)
  
    print(j)
  } 
  print(i)
} 

# Monthly averages --------------------------------------------------------


### January ####
#2020, 2021, 2022

mthlist <- list()
mth = 1

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
r3 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2022_", 
                  mth, ".grd"))
r3 <- crop(r3, ext(140, 170, -50, -8)) #ETBF
mthlist[[3]] <- r3
rasty <- mean(c(mthlist[[1]], mthlist[[2]], mthlist[[3]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)


### February ####
#2020, 2021, 2022

mthlist <- list()
mth = 2

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
r3 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2022_", 
                  mth, ".grd"))
r3 <- crop(r3, ext(140, 170, -50, -8)) #ETBF
mthlist[[3]] <- r3
rasty <- mean(c(mthlist[[1]], mthlist[[2]], mthlist[[3]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)

### March ####
#2020, 2021

mthlist <- list()
mth = 3

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
rasty <- mean(c(mthlist[[1]], mthlist[[2]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)


### April ####
#2020, 2021

mthlist <- list()
mth = 4

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
rasty <- mean(c(mthlist[[1]], mthlist[[2]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)


### May ####
#2020, 2021

mthlist <- list()
mth = 5

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
rasty <- mean(c(mthlist[[1]], mthlist[[2]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)

### June ####
#2020, 2021

mthlist <- list()
mth = 6

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
rasty <- mean(c(mthlist[[1]], mthlist[[2]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)


### July ####
#2020, 2021

mthlist <- list()
mth = 7

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
rasty <- mean(c(mthlist[[1]], mthlist[[2]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)

### August ####
#2020, 2021

mthlist <- list()
mth = 8

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
rasty <- mean(c(mthlist[[1]], mthlist[[2]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)


### September ####
#2020, 2021

mthlist <- list()
mth = 9

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
rasty <- mean(c(mthlist[[1]], mthlist[[2]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)


### October ####
#2019, 2020, 2021

mthlist <- list()
mth = 10

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2019_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
r3 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r3 <- crop(r3, ext(140, 170, -50, -8)) #ETBF
mthlist[[3]] <- r3
rasty <- mean(c(mthlist[[1]], mthlist[[2]], mthlist[[3]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)


### November ####

mthlist <- list()
mth = 11

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2019_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
r3 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r3 <- crop(r3, ext(140, 170, -50, -8)) #ETBF
mthlist[[3]] <- r3
rasty <- mean(c(mthlist[[1]], mthlist[[2]], mthlist[[3]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)


### December ####

mthlist <- list()
mth = 12

r1 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2019_", 
                  mth, ".grd"))
r1 <- crop(r1, ext(140, 170, -50, -8)) #ETBF
mthlist[[1]] <- r1
r2 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_", 
                  mth, ".grd"))
r2 <- crop(r2, ext(140, 170, -50, -8)) #ETBF
mthlist[[2]] <- r2
r3 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_", 
                  mth, ".grd"))
r3 <- crop(r3, ext(140, 170, -50, -8)) #ETBF
mthlist[[3]] <- r3
rasty <- mean(c(mthlist[[1]], mthlist[[2]], mthlist[[3]]))
writeRaster(rasty, paste0(outty, "/", "mo_anom_mean_", mth, ".grd"), 
            overwrite = T)


# Seasonal averages -------------------------------------------------------


### Autumn ####

# average anomaly
mar20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_3.grd"))
apr20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_4.grd"))
may20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_5.grd"))
mar21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_3.grd"))
apr21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_4.grd"))
may21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_5.grd"))
autumn_anom <- mean(mar20, apr20, may20, 
                    mar21, apr21, may21)
names(autumn_anom) <- "SSTa_mean_autumn_2019-2022"
writeRaster(autumn_anom, paste0(season_dir, names(autumn_anom), ".grd"), 
            overwrite = T)


### Spring ####
sep19 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2019_9.grd"))
oct19 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2019_10.grd"))
nov19 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2019_11.grd"))
sep20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_9.grd"))
oct20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_10.grd"))
nov20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_11.grd"))
sep21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_9.grd"))
oct21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_10.grd"))
nov21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_11.grd"))
spring_anom <- mean(sep19, oct19, nov19, 
                    sep20, oct20, nov20,  
                    sep21, oct21, nov21)
names(spring_anom) <- "SSTa_mean_spring_2019-2022"
writeRaster(spring_anom, paste0(season_dir, names(spring_anom), ".grd"), 
            overwrite = T)


### Winter ####
jun20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_6.grd"))
jul20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_7.grd"))
aug20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_8.grd"))
jun21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_6.grd"))
jul21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_7.grd"))
aug21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_8.grd"))
winter_anom <- mean(jun20, jul20, aug20,  
                    jun21, jul21, aug21)
names(winter_anom) <- "SSTa_mean_winter_2019-2022"
writeRaster(winter_anom, paste0(season_dir, names(winter_anom), ".grd"), 
            overwrite = T)


### Summer ####
dec19 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2019_12.grd"))
jan20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_1.grd"))
feb20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_2.grd"))
dec20 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2020_12.grd"))
jan21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_1.grd"))
feb21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_2.grd"))
dec21 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2021_12.grd"))
jan22 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2022_1.grd"))
feb22 <- rast(paste0(month_by_year_dir, "anom_SST_mean_2022_2.grd"))
summer_anom <- mean(dec19, 
                    jan20, feb20, dec20, 
                    jan21, feb21, dec21, 
                    jan22, feb22)
names(summer_anom) <- "SSTa_mean_summer_2019-2022"
writeRaster(summer_anom, paste0(season_dir, names(summer_anom), ".grd"), 
            overwrite = T)



# Season by year ----------------------------------------------------------

### Spring 2019

spring19 <- mean(sep19, oct19, nov19)
names(spring19) <- "SSTa_mean_2019_Spring"
writeRaster(spring19, 
            paste0(season_by_year_dir, names(spring19), ".grd"), overwrite = T)
spring19_sd <- stdev(sep19, oct19, nov19)
names(spring19_sd) <- "SSTa_sd_2019_Spring"
writeRaster(spring19_sd, 
            paste0(season_by_year_dir, names(spring19_sd), ".grd"), overwrite = T)

### Summer 2019-20

summer19 <- mean(dec19, jan20, feb20)
names(summer19) <- "SSTa_mean_2019-20_Summer"
writeRaster(summer19, 
            paste0(season_by_year_dir, names(summer19), ".grd"), overwrite = T)
summer19_sd <- stdev(dec19, jan20, feb20)
names(summer19_sd) <- "SSTa_sd_2019_Summer"
writeRaster(summer19_sd, 
            paste0(season_by_year_dir, names(summer19_sd), ".grd"), overwrite = T)

### Autumn 2020

autumn20 <- mean(mar20, apr20, may20)
names(autumn20) <- "SSTa_mean_2020_Autumn"
writeRaster(autumn20, 
            paste0(season_by_year_dir, names(autumn20), ".grd"), overwrite = T)
autumn20_sd <- stdev(mar20, apr20, may20)
names(autumn20_sd) <- "SSTa_sd_2020_Autumn"
writeRaster(autumn20_sd, 
            paste0(season_by_year_dir, names(autumn20_sd), ".grd"), overwrite = T)

### Winter 2020

winter20 <- mean(jun20, jul20, aug20)
names(winter20) <- "SSTa_mean_2020_Winter"
writeRaster(winter20, 
            paste0(season_by_year_dir, names(winter20), ".grd"), overwrite = T)
winter20_sd <- stdev(jun20, jul20, aug20)
names(winter20_sd) <- "SSTa_sd_2020_Winter"
writeRaster(winter20_sd, 
            paste0(season_by_year_dir, names(winter20_sd), ".grd"), overwrite = T)

### Spring 2020

spring20 <- mean(sep20, oct20, nov20)
names(spring20) <- "SSTa_mean_2020_Spring"
writeRaster(spring20, 
            paste0(season_by_year_dir, names(spring20), ".grd"), overwrite = T)
spring20_sd <- stdev(sep20, oct20, nov20)
names(spring20_sd) <- "SSTa_sd_2020_Spring"
writeRaster(spring20_sd, 
            paste0(season_by_year_dir, names(spring20_sd), ".grd"), overwrite = T)

### Summer 2020-21

summer20 <- mean(dec20, jan21, feb21)
names(summer20) <- "SSTa_mean_2020-21_Summer"
writeRaster(summer20, 
            paste0(season_by_year_dir, names(summer20), ".grd"), overwrite = T)
summer20_sd <- stdev(dec20, jan21, feb21)
names(summer20_sd) <- "SSTa_sd_2020-21_Summer"
writeRaster(summer20_sd, 
            paste0(season_by_year_dir, names(summer20_sd), ".grd"), overwrite = T)

### Autumn 2021

autumn21 <- mean(mar21, apr21, may21)
names(autumn21) <- "SSTa_mean_2021_Autumn"
writeRaster(autumn21, 
            paste0(season_by_year_dir, names(autumn21), ".grd"), overwrite = T)
autumn21_sd <- stdev(mar21, apr21, may21)
names(autumn21_sd) <- "SSTa_sd_2021_Autumn"
writeRaster(autumn21_sd, 
            paste0(season_by_year_dir, names(autumn21_sd), ".grd"), overwrite = T)

### Winter 2021

winter21 <- mean(jun21, jul21, aug21)
names(winter21) <- "SSTa_mean_2021_Winter"
writeRaster(winter21, 
            paste0(season_by_year_dir, names(winter21), ".grd"), overwrite = T)
winter21_sd <- stdev(jun21, jul21, aug21)
names(winter21_sd) <- "SSTa_sd_2021_Winter"
writeRaster(winter21_sd, 
            paste0(season_by_year_dir, names(winter21_sd), ".grd"), overwrite = T)

### Spring 2021

spring21 <- mean(sep21, oct21, nov21)
names(spring21) <- "SSTa_mean_2021_Spring"
writeRaster(spring21, 
            paste0(season_by_year_dir, names(spring21), ".grd"), overwrite = T)
spring21_sd <- stdev(sep21, oct21, nov21)
names(spring21_sd) <- "SSTa_sd_2021_Spring"
writeRaster(spring21_sd, 
            paste0(season_by_year_dir, names(spring21_sd), ".grd"), overwrite = T)

### Summer 2021-22

summer21 <- mean(dec21, jan22, feb22)
names(summer21) <- "SSTa_mean_2021-22_Summer"
writeRaster(summer21, 
            paste0(season_by_year_dir, names(summer21), ".grd"), overwrite = T)
summer21_sd <- stdev(dec21, jan22, feb22)
names(summer21_sd) <- "SSTa_sd_2021-22_Summer"
writeRaster(summer21_sd, 
            paste0(season_by_year_dir, names(summer21_sd), ".grd"), overwrite = T)

