# Monthly predictions of intensity
# Single effect - MLD
# Author: Jessica Bolin and David Schoeman


# Dependencies--------------------------------------------------------------

library(lme4) 
library(effects) 
library(sf)
library(terra)
library(tidyverse)
library(raster)

prev_dat <- readRDS("cleaned_data/modelling/prev_clean.RDS")
area <- c(140, 170, -50, -10)
fish <- read.csv("cleaned_data/all_spatial_FINAL.csv") 
fish <- fish[!fish$Trip == 46,] 

## Shapefiles
straya <- st_read("shapefiles/Common shapefiles/australialandmass.shp")[0]
strayaeez <- st_read("shapefiles/Common shapefiles/australiaeez.shp")[0]
norfolk <- st_read("shapefiles/Common shapefiles/norfolkeez.shp")[0]
both <- st_union(strayaeez, norfolk) 

mod <- glmer(overall ~ scale(mean_sst) * scale(mean_v) +
               scale(mean_sst) * scale(mo_anom) +
               scale(mon_clim_sst_sd) + (1 | operation), 
             family = binomial,
             data = prev_dat)

# outdir for spatial preds rasters
outdir <- "cleaned_data/spatial_rasters/month/prev"
#indir for precomputed rasters
indir <- "access_rasts_preds/month_av"


# Function -----------------------------------------------------------------

prevpreds_mth <- function(sst_rast, 
                      v_rast, 
                      mo_anom_rast, 
                      monclimsstsd_rast, 
                      mth) {
  
  rastersy <- stack(raster(sst_rast) %>% crop(area),
                    raster(v_rast) %>% crop(area),
                    stack(monclimsstsd_rast)[[mth]] %>% crop(area),
                    raster(mo_anom_rast) %>% crop(area))
  names(rastersy) <- c("mean_sst", "mean_v", "mon_clim_sst_sd", "mo_anom")

  vals <- as.data.frame(values(rastersy))
  vals <- na.omit(vals)
  vals$cell <- 1:nrow(vals)
  pred_dat <- vals
  
  bb <- bootMer(mod,
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
  
  all_preds <- cbind(meany, se, predboot1.CI) %>% as.data.frame
  names(all_preds) <- c("mean", "sd", "minCI", "highCI")
  names_vec <- c("mean", "sd", "minCI", "highCI")
  
  new_rasters <- lapply(names_vec, function(name) {
    new_rast <- rast(rastersy[[1]])
    values(new_rast) <- 0
    values(new_rast)[vals$cell] <- all_preds[, name]
    values(new_rast)[values(new_rast) == 0] <- NA
    names(new_rast) <- name
    return(new_rast)
  })
  
  all_rasters <- do.call(c, new_rasters)
  all_rasters <- mask(all_rasters, both) 
  all_rasters <- mask(all_rasters, straya, inverse = T)

  writeRaster(all_rasters,
              paste0(outdir, "/", mth, "_predictions.grd"),
              overwrite = T)
  
}

# Run function ----------------------------------------------------------------

for (i in 1:12) {
 prevpreds_mth(sst_rast = paste0(indir, "/SST_mean_", i, ".grd"),
              mo_anom_rast = paste0(indir, "/mo_anom_mean_", i, ".grd"),
              v_rast = paste0(indir, "/V_mean_", i, ".grd"),
              monclimsstsd_rast = "access_rasts_preds/mon_clim_sst_sd/SST_SD_mth_clim_1981-2018.grd",
              mth = i)
  print(i)
}
