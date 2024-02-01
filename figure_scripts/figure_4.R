# Figure - Seasonal prevalence of infected sword
# Mean 
# Updated Feb 24
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)

# Dependencies ------------------------------------------------------------

library(sf)
library(viridis)
library(tmap)
library(terra)
library(lubridate)

# Shapefiles 
eez <- st_read("shapefiles/Common shapefiles/australiaeez.shp")
norfolk <- st_read("shapefiles/Common shapefiles/norfolkeez.shp")
both <- st_union(eez, norfolk)[0]
stray <- st_read("shapefiles/East_coast_detailed/australia_east_coast.shp")
straya <- st_read("shapefiles/Common shapefiles/australialandmass.shp")[0]
seamount <- st_read("shapefiles/Bathy/Seamounts_Guyouts_ETBF.shp")[0]
nz <- st_read("shapefiles/Common shapefiles/newzealand_dateline_eez.shp")[0]
nc <- st_read("shapefiles/Common shapefiles/newcaledoniaeez.shp")[0]
solo <- st_read("shapefiles/Common shapefiles/solomonislandeez.shp")[0]
png <- st_read("shapefiles/Common shapefiles/pngeez.shp")
r5 <- st_read("shapefiles/contshelf_200m.shp")
r5 <- st_crop(r5, c(xmin = 145, xmax = 154, ymin = -35, ymax = -15))

# Predictions
outdir <- "" #specify out directory
summer <- rast(paste0(outdir, "summer_predictions.grd")) %>% 
  crop(., c(149, 163.9, -33, -18)) 
autumn <- rast(paste0(outdir, "autumn_predictions.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
winter <- rast(paste0(outdir, "winter_predictions.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 
spring <- rast(paste0(outdir, "spring_predictions.grd"))  %>% 
  crop(., c(149, 163.9, -33, -18)) 



# Mean predictions --------------------------------------------------------------------

prev_dir <- "outputs/prev/" #not available in repo

tm_plot_mean <- function(rasty, name) {
  
  my_breaks <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
  my_labels <- c("50", "60","70","80","90", "100")
  
  tt <<- tm_shape(rasty[["mean"]]) +
    tm_raster(breaks = my_breaks, 
              labels = my_labels,
              style = "cont",
              title = "Prob. infected \
sword (%)",
              palette = viridis::mako(255)) + 
    tm_graticules(ticks = T, 
                  lwd = 0.5, 
                  n.x = 6, 
                  n.y = 6, 
                  col = "transparent", 
                  labels.size = 1) +
    tm_shape(stray) +
    tm_polygons() +
    tm_legend(position = c("right", "top"),
              legend.text.size = 1,
              frame = T, frame.lwd = 0.001) +
    tm_shape(seamount) +
    tm_polygons(alpha = 0.01, 
                border.col = "white", 
                lwd = 1) +
    tm_shape(both) +
    tm_polygons(alpha = 0.001, 
                lty = 2, 
                lwd = 1.5, 
                border.col = "black") +
    tm_layout(legend.title.size = 1.4, bg.color = "white",
              legend.frame = F, 
              fontfamily = "Arial Narrow") +
    tm_shape(r5) +
    tm_lines(alpha = 1, 
             col = "white", 
             lwd = 1, 
             lty = 2) 
  
  tmap_save(tt, 
            filename = paste0(name, "_mean.png"),
            dpi = 1000)
  
}

tm_plot_mean(rasty = summer[["mean"]], name = "Summer")
tm_plot_mean(rasty = autumn[["mean"]], name = "Autumn")
tm_plot_mean(rasty = winter[["mean"]], name = "Winter")
tm_plot_mean(rasty = spring[["mean"]], name = "Spring")



# Australia inset ---------------------------------------------------------

straya_crop <- st_crop(straya[0], 
                       c(xmin=140, xmax=156, ymin=-45, ymax=-10))
both_crop <- st_crop(both[0], 
                     c(xmin=140, xmax=166, ymin=-47, ymax=-8))

insetmap <- tm_shape(both_crop) +
  tm_polygons(lwd = 1,
              lty = 0,
              col = "darkslategray4") +
  tm_shape(seamount) +
  tm_polygons(alpha = 0.01, 
              border.col = "white", 
              lwd = 1) +
  tm_shape(straya_crop) +
  tm_polygons(lty = 1, 
              lwd = 1, 
              border.col = "grey40",
              col = "grey80") 


