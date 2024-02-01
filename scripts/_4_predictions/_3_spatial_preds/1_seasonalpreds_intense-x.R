# Seasonal predictions of intensity
# Single effect - MLD
# Author: Jessica Bolin and David Schoeman

# Dependencies --------------------------------------------------------------

library(lme4) 
library(ggpubr) 
library(effects) 
library(sf)

### Kudoa data
intense_dat <- readRDS("intense_clean.RDS")
intense_dat <- intense_dat[-736,] #remove outlier from modelling script

### Environmental data
area <- c(140, 170, -50, -10)

### Fish location data
fish <- read.csv("all_spatial_FINAL.csv") 
fish <- fish[!fish$Trip == 46,] # as per data cleaning

## Shapefiles
straya <- st_read("shapefiles/Common shapefiles/australialandmass.shp")[0]
strayaeez <- st_read("shapefiles/Common shapefiles/australiaeez.shp")[0]
norfolk <- st_read("shapefiles/Common shapefiles/norfolkeez.shp")[0]
both <- st_union(strayaeez, norfolk) 

# Best model
mod_intens <- glmer.nb(TotalFish ~ scale(mean_mld1) + (1|operation), 
         data = intense_dat, 
         control = glmerControl(optCtrl = list(maxfun = 100000)))

# Internal function to create spatial predictions

create_preds <- function(x, r) {
  newrast <- r %>% rast
  values(newrast) <- 0
  newrast[vals1$cell] <- all_preds[, x] 
  newrast[values(newrast) == 0] <- NA
  names(newrast) <- x
  assign(paste0(x, "_newrast"), newrast, envir = globalenv())
  plot(newrast, col = viridis::viridis(255), main = x)
  maps::map("world", add = T, fill = T, col = "grey70")
 # points(fish$Longitude, fish$Latitude, cex = 0.1)
}

# outdir for spatial preds rasters
outdir <- "cleaned_data/spatial_rasters/season/intense"

#indir for precomputed rasters
indir <- "access_rasts_preds/seasonal"


# WINTER ------------------------------------------------------------------

winter_intense <- raster(paste0(indir, "/MLDmean_winter_2019-2022.grd"))
winter_intense <- crop(winter_intense, area)
names(winter_intense) <- "mean_mld1"
winter_intense <- mask(winter_intense, both) 
winter_intense <- mask(winter_intense, straya, inverse = T)

vals <- values(winter_intense) %>% data.frame
names(vals) <- "mean_mld1"
vals$cell <- 1:nrow(vals)
vals1 <- na.omit(vals)
pred_dat <- vals1

bb <- bootMer(mod_intens,
              FUN=function(x)
                predict(x, 
                        re.form = NA,
                        newdata = pred_dat, 
                        type = "response"),
              nsim = 1000,
              parallel = "multicore", 
              ncpus = 10)

meany <- bb$t0
se <- apply(bb$t, 2, sd)
predboot1.CI <- t(sapply(1:nrow(pred_dat),
                         function(i)
                           boot::boot.ci(bb,
                                         type="perc",
                                         index=i)$percent[4:5]))

all_preds <- cbind(meany, se, predboot1.CI)
all_preds <- all_preds %>% as.data.frame() 
names(all_preds) <- c("mean", "sd", "lowCI", "highCI")

create_preds(x = "mean", r = winter_intense)
create_preds(x = "sd", r = winter_intense)
create_preds(x = "lowCI", r = winter_intense)
create_preds(x = "highCI", r = winter_intense)

winter <- c(mean_newrast, sd_newrast, lowCI_newrast, highCI_newrast)
writeRaster(winter, paste0(outdir, "/winter_preds_intensity.grd"),
            overwrite = T)




# AUTUMN ------------------------------------------------------------------

autumn_intense <- raster(paste0(indir, "/MLDmean_autumn_2019-2022.grd"))
autumn_intense <- crop(autumn_intense, area)
names(autumn_intense) <- "mean_mld1"
autumn_intense <- mask(autumn_intense, both) 
autumn_intense <- mask(autumn_intense, straya, inverse = T)

# Parametric bootsrap 
vals <- values(autumn_intense) %>% data.frame
names(vals) <- "mean_mld1"
vals$cell <- 1:nrow(vals)
vals1 <- na.omit(vals)
pred_dat <- vals1

bb <- bootMer(mod_intens,
              FUN=function(x)
                predict(x, re.form = NA,
                        newdata = pred_dat,
                        type = "response"),
              nsim = 1000,
              parallel = "multicore", 
              ncpus = 10)

#stats
meany <- bb$t0
se <- apply(bb$t, 2, sd)
predboot1.CI <- t(sapply(1:nrow(pred_dat),
                         function(i)
                           boot::boot.ci(bb,
                                         type="perc",
                                         index=i)$percent[4:5]))

all_preds <- cbind(meany, se, predboot1.CI)
all_preds <- all_preds %>% as.data.frame() 
names(all_preds) <- c("mean", "sd", "lowCI", "highCI")

create_preds(x = "mean", r = autumn_intense)
create_preds(x = "sd", r = autumn_intense)
create_preds(x = "lowCI", r = autumn_intense)
create_preds(x = "highCI", r = autumn_intense)

autumn <- c(mean_newrast, sd_newrast, lowCI_newrast, highCI_newrast)
writeRaster(autumn, paste0(outdir, "/autumn_preds_intensity.grd"),
            overwrite = T)


# SUMMER ------------------------------------------------------------------

summer_intense <- raster(paste0(indir, "/MLDmean_summer_2019-2022.grd"))
summer_intense <- crop(summer_intense, area)
names(summer_intense) <- "mean_mld1"
summer_intense <- mask(summer_intense, both) 
summer_intense <- mask(summer_intense, straya, inverse = T)

vals <- values(summer_intense) %>% data.frame
names(vals) <- "mean_mld1"
vals$cell <- 1:nrow(vals)
vals1 <- na.omit(vals)
pred_dat <- vals1

bb <- bootMer(mod_intens,
              FUN=function(x)
                predict(x, re.form = NA,
                        newdata = pred_dat,
                        type = "response"),
              nsim = 1000, 
              parallel = "multicore", 
              ncpus = 10)

meany <- bb$t0
se <- apply(bb$t, 2, sd)
predboot1.CI <- t(sapply(1:nrow(pred_dat),
                         function(i)
                           boot::boot.ci(bb,
                                         type="perc",
                                         index=i)$percent[4:5]))

all_preds <- cbind(meany, se, predboot1.CI)
all_preds <- all_preds %>% as.data.frame() 
names(all_preds) <- c("mean", "sd", "lowCI", "highCI")

create_preds(x = "mean", r = summer_intense)
create_preds(x = "sd", r = summer_intense)
create_preds(x = "lowCI", r = summer_intense)
create_preds(x = "highCI", r = summer_intense)

# write to repo
summer <- c(mean_newrast, sd_newrast, lowCI_newrast, highCI_newrast)
writeRaster(summer, paste0(outdir, "/summer_preds_intensity.grd"),
            overwrite = T)



# SPRING ------------------------------------------------------------------

spring_intense <- raster(paste0(indir, "/MLDmean_spring_2019-2022.grd"))
spring_intense <- crop(spring_intense, area)
names(spring_intense) <- "mean_mld1"
spring_intense <- mask(spring_intense, both) 
spring_intense <- mask(spring_intense, straya, inverse = T)

vals <- values(spring_intense) %>% data.frame
names(vals) <- "mean_mld1"
vals$cell <- 1:nrow(vals)
vals1 <- na.omit(vals)
pred_dat <- vals1

bb <- bootMer(mod_intens,
              FUN=function(x)
                predict(x, re.form = NA,
                        newdata = pred_dat,
                        type = "response"),
              nsim = 1000,
              parallel = "multicore", 
              ncpus = 10)

meany <- bb$t0
se <- apply(bb$t, 2, sd)
predboot1.CI <- t(sapply(1:nrow(pred_dat),
                         function(i)
                           boot::boot.ci(bb,
                                         type="perc",
                                         index=i)$percent[4:5]))

all_preds <- cbind(meany, se, predboot1.CI)
all_preds <- all_preds %>% as.data.frame() 
names(all_preds) <- c("mean", "sd", "lowCI", "highCI")

create_preds(x = "mean", r = spring_intense)
create_preds(x = "sd", r = spring_intense)
create_preds(x = "lowCI", r = spring_intense)
create_preds(x = "highCI", r = spring_intense)

spring <- c(mean_newrast, sd_newrast, lowCI_newrast, highCI_newrast)
writeRaster(spring, paste0(outdir, "/spring_preds_intensity.grd"),
            overwrite = T)

