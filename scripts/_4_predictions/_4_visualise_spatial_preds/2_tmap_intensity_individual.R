# Plotting and saving (tmap) predictions
# Intensity = season and monthly
# Author: Jessica Bolin


# Dependencies ------------------------------------------------------------

library(sf)
library(viridis)
library(tmap)
library(terra)
library(lubridate)

eez <- st_read("shapefiles/Common shapefiles/australiaeez.shp")
norfolk <- st_read("shapefiles/Common shapefiles/norfolkeez.shp")
both <- st_union(eez, norfolk)[0]
stray <- st_read("shapefiles/East_coast_detailed/australia_east_coast.shp")
seamount <- st_read("shapefiles/Bathy/Seamounts_Guyouts_ETBF.shp")[0]
r5 <- st_read("shapefiles/contshelf_200m.shp")
r5 <- st_crop(r5, c(xmin = 145, xmax = 154, ymin = -35, ymax = -15))

#### Season #####

outdir <- ""

summer <- rast(paste0(outdir, "summer_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
autumn <- rast(paste0(outdir, "autumn_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
winter <- rast(paste0(outdir, "winter_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
spring <- rast(paste0(outdir, "spring_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 

#### Month #####

mthdir <- ""

jan <- rast(paste0(mthdir, "Jan_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
feb <- rast(paste0(mthdir, "Feb_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
mar <- rast(paste0(mthdir, "Mar_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
apr <- rast(paste0(mthdir, "Apr_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18))
may <- rast(paste0(mthdir, "May_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
june <- rast(paste0(mthdir, "June_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
july <- rast(paste0(mthdir, "July_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
aug <- rast(paste0(mthdir, "Aug_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
sep <- rast(paste0(mthdir, "Sep_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
oct <- rast(paste0(mthdir, "Oct_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
nov <- rast(paste0(mthdir, "Nov_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
dec <- rast(paste0(mthdir, "Dec_preds_intensity.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 


# Season/Month MEAN -------------------------------------------------------

intense_dir <- ""

tm_plot_mean <- function(rasty, name) {
  
  my_breaks <- c(25000, 30000, 35000, 40000, 45000, 50000)

  tt <- tm_shape(rasty[["mean"]]) +
    tm_raster(breaks = my_breaks,
              title = "Number of spores\
in tissue",
              style = "cont",
              palette = viridis::mako(255)) + 
    tm_graticules(ticks = T, lwd = 0.5,
                  n.x = 6, n.y = 6, 
                  col = "transparent",
                  labels.size = 1) +
    tm_shape(stray) +
    tm_polygons() +
    tm_shape(seamount) +
    tm_polygons(alpha = 0.01, border.col = "white", lwd = 1) +
    tm_shape(both) +
    tm_polygons(alpha = 0.001, lty = 2, lwd = 1.5, border.col = "black") +
    tm_legend(position = c("right", "top"),
              legend.text.size = 1,
              frame = T, frame.lwd = 0.001) +
    tm_layout(main.title = paste0(name, " (study period)"),
              legend.title.size = 1.4,
              bg.color = "white",
              legend.frame = F,
              fontfamily = "Arial Narrow") +
    tm_shape(r5) +
    tm_lines(alpha = 1, col = "white", lwd = 1, lty = 2) 
  
  tmap_save(tt, filename = paste0(intense_dir, "mean/",
                                  name, "_mean_intense.png"),
            dpi = 1000)
}


## Run Season ####
tm_plot_mean(rasty = summer[["mean"]], name = "Summer")
tm_plot_mean(rasty = autumn[["mean"]], name = "Autumn")
tm_plot_mean(rasty = winter[["mean"]], name = "Winter")
tm_plot_mean(rasty = spring[["mean"]], name = "Spring")

## Run Month ####
tm_plot_mean(rasty = jan[["mean"]], name = "January")
tm_plot_mean(rasty = feb[["mean"]], name = "February")
tm_plot_mean(rasty = mar[["mean"]], name = "March")
tm_plot_mean(rasty = apr[["mean"]], name = "April")
tm_plot_mean(rasty = may[["mean"]], name = "May")
tm_plot_mean(rasty = june[["mean"]], name = "June")
tm_plot_mean(rasty = july[["mean"]], name = "July")
tm_plot_mean(rasty = aug[["mean"]], name = "August")
tm_plot_mean(rasty = sep[["mean"]], name = "September")
tm_plot_mean(rasty = oct[["mean"]], name = "October")
tm_plot_mean(rasty = nov[["mean"]], name = "November")
tm_plot_mean(rasty = dec[["mean"]], name = "December")


# Season SD ---------------------------------------------------------------

summer[["sd"]] %>% values %>% min(na.rm=T)
winter[["sd"]] %>% values %>% min(na.rm=T)
spring[["sd"]] %>% values %>% min(na.rm=T)
autumn[["sd"]] %>% values %>% min(na.rm=T)
jan[["sd"]] %>% values %>% max(na.rm=T)
feb[["sd"]] %>% values %>% max(na.rm=T)
mar[["sd"]] %>% values %>% max(na.rm=T)
apr[["sd"]] %>% values %>% max(na.rm=T)
may[["sd"]] %>% values %>% max(na.rm=T)
june[["sd"]] %>% values %>% max(na.rm=T)
july[["sd"]] %>% values %>% max(na.rm=T)
aug[["sd"]] %>% values %>% max(na.rm=T)
sep[["sd"]] %>% values %>% max(na.rm=T)
oct[["sd"]] %>% values %>% max(na.rm=T)
nov[["sd"]] %>% values %>% max(na.rm=T)
dec[["sd"]] %>% values %>% max(na.rm=T)

tmap_sd_season <- function(rasty, name) {
  
  my_breaks <- c(2000, 3000, 4000, 5000, 6000)

  tt <- tm_shape(rasty[["sd"]]) +
    tm_raster(breaks = my_breaks,
              title = "Std. Dev. \
Predictions",
              style = "cont",
              palette = viridis::mako(255)) + 
    tm_graticules(ticks = T, lwd = 0.5,
                  n.x = 6, n.y = 6, 
                  col = "transparent",
                  labels.size = 1) +
    tm_shape(stray) +
    tm_polygons() +
    tm_shape(seamount) +
    tm_polygons(alpha = 0.01, border.col = "white", lwd = 1) +
    tm_shape(both) +
    tm_polygons(alpha = 0.001, lty = 2, lwd = 1.5, border.col = "black") +
    tm_legend(position = c("right", "top"),
              legend.text.size = 1,
              frame = T, frame.lwd = 0.001) +
    tm_layout(main.title = paste0(name, " (study period)"),
              legend.title.size = 1.4,
              bg.color = "white",
              legend.frame = F,
              fontfamily = "Arial Narrow") +
    tm_shape(r5) +
    tm_lines(alpha = 1, col = "white", lwd = 1, lty = 2) 
  
  tmap_save(tt, filename = paste0(intense_dir, "sd/",
                                  name, "_sd_intense.png"),
            dpi = 1000)
} 

tmap_sd_season(summer, "Summer")
tmap_sd_season(autumn, "Autumn")
tmap_sd_season(winter, "Winter")
tmap_sd_season(spring, "Spring")

# Month SD -------------------------------------------------------------------


tmap_sd_mth <- function(rasty, name) {
  
  my_breaks <-  my_breaks <- c(2000, 3000, 4000, 5000, 6000)

  tt <- tm_shape(rasty[["sd"]]) +
    tm_raster(breaks = my_breaks,
              title = "Std. Dev. \
Predictions",
              style = "cont",
              palette = viridis::mako(255)) + 
    tm_graticules(ticks = T, lwd = 0.5,
                  n.x = 6, n.y = 6, 
                  col = "transparent",
                  labels.size = 1) +
    tm_shape(stray) +
    tm_polygons() +
    tm_shape(seamount) +
    tm_polygons(alpha = 0.01, border.col = "white", lwd = 1) +
    tm_shape(both) +
    tm_polygons(alpha = 0.001, lty = 2, lwd = 1.5, border.col = "black") +
    tm_legend(position = c("right", "top"),
              legend.text.size = 1,
              frame = T, frame.lwd = 0.001) +
    tm_layout(main.title = paste0(name, " (study period)"),
              legend.title.size = 1.4,
              bg.color = "white",
              legend.frame = F,
              fontfamily = "Arial Narrow") +
    tm_shape(r5) +
    tm_lines(alpha = 1, col = "white", lwd = 1, lty = 2) 
  
  tmap_save(tt, filename = paste0(intense_dir, "sd/",
                                  name, "_sd_intense.png"),
            dpi = 1000)
}

tmap_sd_mth(jan, "January")
tmap_sd_mth(feb, "February")
tmap_sd_mth(mar, "March")
tmap_sd_mth(apr, "April")
tmap_sd_mth(may, "May")
tmap_sd_mth(june, "June")
tmap_sd_mth(july, "July")
tmap_sd_mth(aug, "August")
tmap_sd_mth(sep, "September")
tmap_sd_mth(oct, "October")
tmap_sd_mth(nov, "November")
tmap_sd_mth(dec, "December")


# MIN CI ------------------------------------------------------------------

tm_plot_minCI <- function(rasty, name) {
  
  my_breaks <- c(15000, 21000, 28000, 34000, 40000)

  tt <- tm_shape(rasty[["lowCI"]]) +
    tm_raster(breaks = my_breaks,
              title = "Number of spores\
in tissue (min CI)",
              style = "cont",
              palette = viridis::mako(255)) + 
    tm_graticules(ticks = T, lwd = 0.5,
                  n.x = 6, n.y = 6, 
                  col = "transparent",
                  labels.size = 1) +
    tm_shape(stray) +
    tm_polygons() +
    tm_shape(seamount) +
    tm_polygons(alpha = 0.01, border.col = "white", lwd = 1) +
    tm_shape(both) +
    tm_polygons(alpha = 0.001, lty = 2, lwd = 1.5, border.col = "black") +
    tm_legend(position = c("right", "top"),
              legend.text.size = 1,
              frame = T, frame.lwd = 0.001) +
    tm_layout(main.title = paste0(name, " (study period)"),
              legend.title.size = 1.4,
              bg.color = "white",
              legend.frame = F,
              fontfamily = "Arial Narrow") +
    tm_shape(r5) +
    tm_lines(alpha = 1, col = "white", lwd = 1, lty = 2) 
  
  tmap_save(tt, filename = paste0(intense_dir, "minCI/",
                                  name, "_minCI_intense.png"),
            dpi = 1000)
}

## Run Season ####
tm_plot_minCI(rasty = summer[["lowCI"]], name = "Summer")
tm_plot_minCI(rasty = autumn[["lowCI"]], name = "Autumn")
tm_plot_minCI(rasty = winter[["lowCI"]], name = "Winter")
tm_plot_minCI(rasty = spring[["lowCI"]], name = "Spring")

## Run Month ####
tm_plot_minCI(rasty = jan[["lowCI"]], name = "January")
tm_plot_minCI(rasty = feb[["lowCI"]], name = "February")
tm_plot_minCI(rasty = mar[["lowCI"]], name = "March")
tm_plot_minCI(rasty = apr[["lowCI"]], name = "April")
tm_plot_minCI(rasty = may[["lowCI"]], name = "May")
tm_plot_minCI(rasty = june[["lowCI"]], name = "June")
tm_plot_minCI(rasty = july[["lowCI"]], name = "July")
tm_plot_minCI(rasty = aug[["lowCI"]], name = "August")
tm_plot_minCI(rasty = sep[["lowCI"]], name = "September")
tm_plot_minCI(rasty = oct[["lowCI"]], name = "October")
tm_plot_minCI(rasty = nov[["lowCI"]], name = "November")
tm_plot_minCI(rasty = dec[["lowCI"]], name = "December")



# MAX CI ------------------------------------------------------------------

tm_plot_highCI <- function(rasty, name) {
  
  my_breaks <- c(30000, 35000, 40000, 45000, 50000, 55000, 60000)
  
  tt <- tm_shape(rasty[["highCI"]]) +
    tm_raster(breaks = my_breaks,
              title = "Number of spores\
in tissue (max CI)",
              style = "cont",
              palette = viridis::mako(255)) + 
    tm_graticules(ticks = T, lwd = 0.5,
                  n.x = 6, n.y = 6, 
                  col = "transparent",
                  labels.size = 1) +
    tm_shape(stray) +
    tm_polygons() +
    tm_shape(seamount) +
    tm_polygons(alpha = 0.01, border.col = "white", lwd = 1) +
    tm_shape(both) +
    tm_polygons(alpha = 0.001, lty = 2, lwd = 1.5, border.col = "black") +
    tm_legend(position = c("right", "top"),
              legend.text.size = 1,
              frame = T, frame.lwd = 0.001) +
    tm_layout(main.title = paste0(name, " (study period)"),
              legend.title.size = 1.4,
              bg.color = "white",
              legend.frame = F,
              fontfamily = "Arial Narrow") +
    tm_shape(r5) +
    tm_lines(alpha = 1, col = "white", lwd = 1, lty = 2) 
  
  tmap_save(tt, filename = paste0(intense_dir, "highCI/",
                                  name, "_highCI_intense.png"),
            dpi = 1000)
}

## Run Season ####
tm_plot_highCI(rasty = summer[["highCI"]], name = "Summer")
tm_plot_highCI(rasty = autumn[["highCI"]], name = "Autumn")
tm_plot_highCI(rasty = winter[["highCI"]], name = "Winter")
tm_plot_highCI(rasty = spring[["highCI"]], name = "Spring")

## Run Month ####
tm_plot_highCI(rasty = jan[["highCI"]], name = "January")
tm_plot_highCI(rasty = feb[["highCI"]], name = "February")
tm_plot_highCI(rasty = mar[["highCI"]], name = "March")
tm_plot_highCI(rasty = apr[["highCI"]], name = "April")
tm_plot_highCI(rasty = may[["highCI"]], name = "May")
tm_plot_highCI(rasty = june[["highCI"]], name = "June")
tm_plot_highCI(rasty = july[["highCI"]], name = "July")
tm_plot_highCI(rasty = aug[["highCI"]], name = "August")
tm_plot_highCI(rasty = sep[["highCI"]], name = "September")
tm_plot_highCI(rasty = oct[["highCI"]], name = "October")
tm_plot_highCI(rasty = nov[["highCI"]], name = "November")
tm_plot_highCI(rasty = dec[["highCI"]], name = "December")
