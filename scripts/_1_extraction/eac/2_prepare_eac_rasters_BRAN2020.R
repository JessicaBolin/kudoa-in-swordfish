# Prepare rasters of distance to EAC (Part I) using BRAN2020
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)
# Last updated: 2024-02-01

# Best method for PCA: SST, SSH, V, Speed (as per 1_comparemethods.R)

# PNGs and rasters created from this script can be found at:
setwd("/EAC") #path to where /EAC is stored

# Steps
# 1. Create daily rasters of EAC PCA (SST, V, SSH, Speed) over study period
# 2. Make sure they are all correct (manually fix errors) 

# Libraries and dependencies ---------------------------------------------------------------

library(raster)
library(tidyverse)
library(lubridate)
library(terra)

# Env data
area <- extent(c(xmin = 140.125, xmax = 170.125, 
                 ymin = -45, ymax = -8)) #ETBF 

dir = "bran_rasters/" 
temps <- list.files(path = dir, pattern = "temp")
vs <- list.files(path = dir, pattern = "v")
us <- list.files(path = dir, pattern = "u")
sshs <- list.files(path = dir, pattern = "eta_t")

# Sequences of dates in study period from Sep 2019 to March 2022
dateys <- seq("2019-09-01" %>% as.Date, "2022-03-01" %>% as.Date, 1)
# Remove dates that cause problems (leap year, 31st Dec)
which("2019-12-31" %>% as.Date == dateys) -> die1
which("2020-12-31" %>% as.Date == dateys) -> die2
which("2021-12-31" %>% as.Date == dateys) -> die3
which("2020-02-29" %>% as.Date == dateys) -> die4
dateys[-c(die1, die2, die3, die4)] -> dateys
head(dateys)
#[1] "2019-09-01" "2019-09-02" "2019-09-03" "2019-09-04" "2019-09-05" "2019-09-06"


# PCA Loading dfs -------------------------------------------------------------

df <- data.frame(date = as.character(), 
                 PC1_sst = numeric(), 
                 PC1_v = numeric(), 
                 PC1_speed = numeric(), 
                 PC1_ssh = numeric(), 
                 PC2_sst = numeric(), 
                 PC2_v = numeric(), 
                 PC2_speed = numeric(), 
                 PC2_ssh = numeric(), 
                 PC3_sst = numeric(), 
                 PC3_v = numeric(), 
                 PC3_speed = numeric(), 
                 PC3_ssh = numeric(), 
                 PC4_sst = numeric(), 
                 PC4_v = numeric(), 
                 PC4_speed = numeric(), 
                 PC4_ssh = numeric(), 
                 PC1_var =numeric(), 
                 PC2_var = numeric(), 
                 PC3_var = numeric(), 
                 PC4_var = numeric())

inverse <- data.frame(date = as.character(), 
                      PC1_sst = numeric(), 
                      PC1_v = numeric(), 
                      PC1_speed = numeric(), 
                      PC1_ssh = numeric(), 
                      PC2_sst = numeric(), 
                      PC2_v = numeric(), 
                      PC2_speed = numeric(), 
                      PC2_ssh = numeric(), 
                      PC3_sst = numeric(), 
                      PC3_v = numeric(), 
                      PC3_speed = numeric(), 
                      PC3_ssh = numeric(), 
                      PC4_sst = numeric(), 
                      PC4_v = numeric(), 
                      PC4_speed = numeric(), 
                      PC4_ssh = numeric(), 
                      variance1 = numeric(), 
                      variance2 = numeric(), 
                      variance3 = numeric(), 
                      variance4 = numeric())

all.stuff <- data.frame(Date = character(), 
                        Variance = numeric(), 
                        Direction = character())


# 1. Create daily EAC rasters in study period ----------------------------------------

for (k in 1:length(dateys)) { # for each day
  
  # Prepare rasters
  sst_f <- temps[grep(dateys[k], temps)]
  v_f <- vs[grep(dateys[k], vs)]
  u_f <- us[grep(dateys[k], us)]
  ssh_f <- sshs[grep(dateys[k], sshs)]
  
  sst <- rast(paste0(dir, sst_f))
  sst <- crop(sst, area) 
  v <- rast(paste0(dir, v_f))
  v <- crop(v, area) 
  u <- rast(paste0(dir, u_f))
  u <- crop(u, area) 
  speed <- sqrt(u^2 + v^2) %>% crop(area) 
  ssh <- rast(paste0(dir, ssh_f))
  ssh <- crop(ssh, area) 
  
  # Prepare and run PCA
  yearmonth <- dateys[k]
  sst.v <- sst[]
  v.v <- v[] * -1
  speed.v <- speed[]
  ssh.v <- ssh[]
  xy <- data.frame(coordinates(sst %>% raster)) 
  dat <- data.frame(sst = sst.v, v = v.v, speed = speed.v, ssh = ssh.v, 
                    x = xy$x, y = xy$y)
  names(dat) <- c("sst", "v","speed", "ssh", "x", "y")
  dat <- na.omit(dat) 
  pca1 <- prcomp(dat[,-c(5:6)], retx = TRUE, center = F, scale. = TRUE) 
  
  # Inspect sign of PC1 loadings
  if (pca1$rotation["sst","PC1"] < 0 & 
      pca1$rotation["v","PC1"] < 0 & 
      pca1$rotation["speed","PC1"] < 0 &
      pca1$rotation["ssh","PC1"] < 0) { pc1 <- as.vector(pca1$x[,1]) *-1  
  } else if (pca1$rotation["sst","PC1"] > 0 & 
             pca1$rotation["v","PC1"] > 0 & 
             pca1$rotation["speed","PC1"] > 0 &
             pca1$rotation["ssh","PC1"] < 0) {  pc1 <- as.vector(pca1$x[,1]) 
  } else { pc1 <- as.vector(pca1$x[,1])
  
  inverse[nrow(inverse) + 1,] <- c(yearmonth %>% as.character, 
                                   pca1$rotation[1,1] %>% round(3), 
                                   pca1$rotation[2,1] %>% round(3),  
                                   pca1$rotation[3,1] %>% round(3), 
                                   pca1$rotation[4,1] %>% round(3), 
                                   pca1$rotation[1,2] %>% round(3), 
                                   pca1$rotation[2,2] %>% round(3),  
                                   pca1$rotation[3,2] %>% round(3), 
                                   pca1$rotation[4,2] %>% round(3), 
                                   pca1$rotation[1,3] %>% round(3),  
                                   pca1$rotation[2,3] %>% round(3), 
                                   pca1$rotation[3,3] %>% round(3), 
                                   pca1$rotation[4,3] %>% round(3), 
                                   pca1$rotation[1,4] %>% round(3),  
                                   pca1$rotation[2,4] %>% round(3),  
                                   pca1$rotation[3,4] %>% round(3),  
                                   pca1$rotation[4,4] %>% round(3), 
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1], 
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[2], 
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[3], 
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[4]) 
  }
  
  df[nrow(df) + 1,] <- c(yearmonth %>% as.character, 
                         pca1$rotation["sst","PC1"] %>% round(3), 
                         pca1$rotation["v","PC1"] %>% round(3), 
                         pca1$rotation["speed","PC1"] %>% round(3),  
                         pca1$rotation["ssh","PC1"] %>% round(3),  
                         pca1$rotation["sst","PC2"] %>% round(3),  
                         pca1$rotation["v","PC2"] %>% round(3),  
                         pca1$rotation["speed","PC2"] %>% round(3),  
                         pca1$rotation["ssh","PC2"] %>% round(3), 
                         pca1$rotation["sst","PC3"] %>% round(3),  
                         pca1$rotation["v","PC3"] %>% round(3),  
                         pca1$rotation["speed","PC3"] %>% round(3),  
                         pca1$rotation["ssh","PC3"] %>% round(3), 
                         pca1$rotation["sst","PC4"] %>% round(3),  
                         pca1$rotation["v","PC4"] %>% round(3),  
                         pca1$rotation["speed","PC4"] %>% round(3),  
                         pca1$rotation["ssh","PC4"] %>% round(3), 
                         round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1], 
                         round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[2], 
                         round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[3],
                         round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[4])
  
  stuff <- data.frame(Date = yearmonth,
                      variance = round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1],
                      Direction = 
                        if(pca1$rotation[1,1] > 0 & 
                           pca1$rotation[2,1] > 0 & 
                           pca1$rotation[3,1] > 0 & 
                           pca1$rotation[4,1] > 0 | 
                           pca1$rotation[1,1] < 0 & 
                           pca1$rotation[2,1] < 0 & 
                           pca1$rotation[3,1] < 0 & 
                           pca1$rotation[4,1] < 0) { "Same" } else { "Different" } )
  
  all.stuff <- rbind(all.stuff, stuff)
  
  # Create a raster of PC1
  pc1.map <- v 
  pc1.map[as.numeric(row.names(dat))] <- pc1 
  
  # Save picture - indicate if failure
  png(paste0("1b_dailypngs/", yearmonth,
             if (yearmonth %>% as.character() %in% inverse$date) { paste0("_FAIL") }, ".png"))
  
  par(las = 1, cex.axis = 1, mgp = c(0.3,0.9,0), mar =c(4,3,1.5,2))
  plot(pc1.map, col = colorRamps::matlab.like(300), xaxt = "n", yaxt = "n")
  maps::map("world",add=TRUE, fill = T, col = "grey")
  dev.off()
  
  # Save raster - indicate in file name if failure
  writeRaster(pc1.map,
              paste0("1a_dailyrasters/", yearmonth,
                     if (yearmonth %>% as.character() %in% inverse$date) { paste0("_FAIL") }, 
                     ".grd"), 
              overwrite = T)
  
  print(yearmonth)
  
}


length(unique(inverse$date)); nrow(inverse) 
# [1] 88
# [1] 153 days with inversed loadings
100 - (153/nrow(df) * 100) 
#[1] 83.27869% hit rate

mean(all.stuff$variance) #[1] 0.6755956
median(all.stuff$variance) #[1] .67
sd(all.stuff$variance) #[1] 0.0132783
all.stuff$variance %>% range #[1] 0.66 0.74

all_list <- list()
all_list[[1]] <- inverse
all_list[[2]] <- all.stuff
all_list[[3]] <- df
names(all_list) <- c("inversed_loadings", "lookup_loadings", "all_loadings")
saveRDS(all_list, "rds/eac_method_loadings_step1_BRAN.RDS")



# 2. Manually fix incorrectly-computed PCAs ----------------------------------

wd <- "1a_dailyrasters"
fileys <- grep(list.files(wd), pattern = "FAIL", invert = F, value = T)
fileys <- fileys[grep(".grd$", fileys)]
newwd <- "2a_dailyrasters_fixed"

# Visual inspection of failures indicates that the z axis just needs to be inverted. 

for (i in 1:length(fileys)) {
  
  r <- rast(paste0(wd, "/",  fileys[i]))
  names(r) <- gsub("v", "", names(r))
  names(r) <- gsub("_", "", names(r))
  dater <-   names(r) 
  rr <- r * -1 #invert it
  
  writeRaster(rr, paste0(newwd, "/", dater, ".grd"), overwrite = T)
  png(paste0("2b_fixedpngs/", dater, ".png"))
  par(las = 1, cex.axis = 1, mgp = c(0.3,0.9,0), mar =c(4,3,1.5,2))
  plot(rr, col = colorRamps::matlab.like(300),xaxt = "n", yaxt = "n")
  maps::map("world",add=TRUE, fill = T, col = "grey")
  box(lwd = 2)
  dev.off()
  print(dater)
  
}

# Visual inspection of these pngs reveal two more failures:

wd <- "2a_dailyrasters_fixed"
fileys <- list.files(wd)
fileys <- fileys[grep(".grd$", fileys)]
newwd <- "2c_refixedrasters"
fix <- c("2020-09-28", "2021-07-30") #fail dates

bad1 <- grep("2020-09-28", fileys)
rr <- raster(paste0(wd, "/", fileys[bad1]))
writeRaster(rrr, paste0(newwd, "/", 
                        gsub("_FAIL.grd", "", fileys[bad1]), 
                        ".grd"), overwrite = T)
png(paste0("2d_refixedpngs/", gsub("_FAIL.grd", "", fileys[bad1]), ".png"))
par(las = 1, cex.axis = 1, mgp = c(0.3,0.9,0), mar =c(4,3,1.5,2))
plot(rr, col = colorRamps::matlab.like(300),xaxt = "n", yaxt = "n")
maps::map("world",add=TRUE, fill = T, col = "grey")
mtext(paste0(i), side = 1, line = 4.9, padj = -8, adj = 0.9, cex = 1.2, font = 2)
box(lwd = 2)
dev.off()


bad1 <- grep("2021-07-30", fileys)
rr <- raster(paste0(wd, "/", fileys[bad1]))
writeRaster(rrr, paste0(newwd, "/", 
                        gsub("_FAIL.grd", "", fileys[bad1]), 
                        ".grd"), overwrite = T)
png(paste0("2d_refixedpngs/", gsub("_FAIL.grd", "", fileys[bad1]), ".png"))
par(las = 1, cex.axis = 1, mgp = c(0.3,0.9,0), mar =c(4,3,1.5,2))
plot(rr, col = colorRamps::matlab.like(300),xaxt = "n", yaxt = "n")
maps::map("world",add=TRUE, fill = T, col = "grey")
mtext(paste0(i), side = 1, line = 4.9, padj = -8, adj = 0.9, cex = 1.2, font = 2)
box(lwd = 2)
dev.off()



# 3. (Manual) Put all good rasters and pngs into same folder --------------

# All good rasters I've moved (manually) to:
# \3a_finalrasters
# and pngs to:
# \3b_finalpngs


