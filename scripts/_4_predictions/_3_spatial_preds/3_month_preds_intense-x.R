# Monthly predictions of intensity
# Single effect - MLD
# Author: Jessica Bolin and David Schoeman


# Dependencies--------------------------------------------------------------

library(lme4) 
library(effects) 
library(sf)
library(raster)
library(terra)

### Kudoa data
intense_dat <- readRDS("cleaned_data/modelling/intense_clean.RDS")
intense_dat <- intense_dat[-736,] #remove outlier from modelling script

### Environmental data
area <- c(140, 170, -50, -10)

### Fish location data
fish <- read.csv("cleaned_data/all_spatial_FINAL.csv") 
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

# outdir for spatial preds rasters
outdir <- "cleaned_data/spatial_rasters/month/intense"
#indir for precomputed rasters
indir <- "access_rasts_preds/month_av"


# Big preds function ----------------------------------------------------------

mth_function <- function(rast_file, month_name) {
  
  mth <- raster(rast_file)
  mth <- crop(mth, area)
  names(mth) <- "mean_mld1"
  
  vals <- values(mth) %>% data.frame
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

  newrast <- mth %>% rast
  values(newrast) <- 0
  newrast[vals1$cell] <- all_preds[, "mean"] 
  newrast[values(newrast) == 0] <- NA
  names(newrast) <- "mean"
  assign(paste0("mean", "_newrast"), newrast, envir = globalenv())
  
  newrast <- mth %>% rast
  values(newrast) <- 0
  newrast[vals1$cell] <- all_preds[, "sd"] 
  newrast[values(newrast) == 0] <- NA
  names(newrast) <- "sd"
  assign(paste0("sd", "_newrast"), newrast, envir = globalenv())
  
  newrast <- mth %>% rast
  values(newrast) <- 0
  newrast[vals1$cell] <- all_preds[, "lowCI"] 
  newrast[values(newrast) == 0] <- NA
  names(newrast) <- "lowCI"
  assign(paste0("lowCI", "_newrast"), newrast, envir = globalenv())
  
  newrast <- mth %>% rast
  values(newrast) <- 0
  newrast[vals1$cell] <- all_preds[, "highCI"] 
  newrast[values(newrast) == 0] <- NA
  names(newrast) <- "highCI"
  assign(paste0("highCI", "_newrast"), newrast, envir = globalenv())
 
  allmth <- c(mean_newrast, sd_newrast, lowCI_newrast, highCI_newrast)
  allmth <- mask(allmth, both) 
  allmth <- mask(allmth, straya, inverse = T)

  writeRaster(allmth, 
              paste0(outdir, "/", month_name, 
                     "_", "preds_intensity.grd"),
              overwrite = T)

}


# Run functions -----------------------------------------------------------

mth_function(rast_file = paste0(indir, "/MLD_mean_1.grd"),
  month_name = "Jan")
mth_function(rast_file = paste0(indir, "/MLD_mean_2.grd"),
  month_name = "Feb")
mth_function(rast_file = paste0(indir, "/MLD_mean_3.grd"),
  month_name = "Mar")
mth_function(rast_file = paste0(indir, "/MLD_mean_4.grd"),
  month_name = "Apr")
mth_function(rast_file = paste0(indir, "/MLD_mean_5.grd"),
  month_name = "May")
mth_function(rast_file = paste0(indir, "/MLD_mean_6.grd"),
  month_name = "June")
mth_function(rast_file = paste0(indir, "/MLD_mean_7.grd"),
  month_name = "July")
mth_function(rast_file = paste0(indir, "/MLD_mean_8.grd"),
  month_name = "Aug")
mth_function(rast_file = paste0(indir, "/MLD_mean_9.grd"),
  month_name = "Sep")
mth_function(rast_file = paste0(indir, "/MLD_mean_10.grd"),
  month_name = "Oct")
mth_function(rast_file = paste0(indir, "/MLD_mean_11.grd"),
  month_name = "Nov")
mth_function(rast_file = paste0(indir, "/MLD_mean_12.grd"),
  month_name = "Dec")
