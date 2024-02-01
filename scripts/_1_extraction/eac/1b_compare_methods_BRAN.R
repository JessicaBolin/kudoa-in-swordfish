# Test two methods of detecting the EAC via PCA, using BRAN2020
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)
# Created:      2023-05-29
# Last updated: 2024-02-01

# Note - only the first method is annotated

# Dependencies ------------------------------------------------------------

library(raster)
library(tidyverse)
library(lubridate)

dir = "" #enter directory here

temps <- list.files(path = dir, pattern = "temp")
temps <- temps[grep(pattern = "\\.nc$", temps)]
vs <- list.files(path = dir, pattern = "v")
vs <- vs[grep(pattern = "\\.nc$", vs)]
us <- list.files(path = dir, pattern = "u")
us <- us[grep(pattern = "\\.nc$", us)]
sshs <- list.files(path = dir, pattern = "eta_t")
sshs <- sshs[grep(pattern = "\\.nc$", sshs)]

meta <- readRDS("spatialmetadata_alltrips.RDS")
# not available in repo

comp <- meta %>% 
  plyr::ddply(c("Trip", "Vessel")) 

tv <- comp %>%  #Trip/Vessel combinations
  group_by(Trip, Vessel) %>% 
  dplyr::count() %>% 
  dplyr::select(-n) %>% data.frame

datee <- unique(comp$Date) # unique shot dates in study period
area <- extent(c(xmin=145, xmax=164, ymin=-34, ymax=-17)) # ETBF 

M1_all_list <- list() 
M2_all_list <- list() #empty list for evaluation stats


# Method 1 temp, v, speed, SSH  -----------------------------------------------------------------

df <- data.frame(date = numeric(), 
                 PC1_sst = numeric(), PC1_v = numeric(), 
                 PC1_speed = numeric(), PC1_ssh = numeric(), 
                 PC2_sst = numeric(), PC2_v = numeric(), 
                 PC2_speed = numeric(), PC2_ssh = numeric(), 
                 PC3_sst = numeric(), PC3_v = numeric(), 
                 PC3_speed = numeric(), PC3_ssh = numeric(), 
                 PC4_sst = numeric(), PC4_v = numeric(), 
                 PC4_speed = numeric(), PC4_ssh = numeric(), 
                 PC1_var =numeric(), PC2_var = numeric(), 
                 PC3_var = numeric(), PC4_var = numeric())

inverse <- data.frame(date = numeric(), 
                      PC1_sst = numeric(), PC1_v = numeric(), 
                      PC1_speed = numeric(), PC1_ssh = numeric(), 
                      PC2_sst = numeric(), PC2_v = numeric(), 
                      PC2_speed = numeric(), PC2_ssh = numeric(), 
                      PC3_sst = numeric(), PC3_v = numeric(), 
                      PC3_speed = numeric(), PC3_ssh = numeric(), 
                      PC4_sst = numeric(), PC4_v = numeric(), 
                      PC4_speed = numeric(), PC4_ssh = numeric(), 
                      variance1 = numeric(), variance2 = numeric(), 
                      variance3 = numeric(), variance4 = numeric())

all.stuff <- data.frame(Date = character(), 
                        Variance = numeric(), 
                        Direction = character(), 
                        Direction.flex = character())

for (k in 1:length(datee)) { 
  
  yearmonth <- datee[k]
  
  # Prepare corresponding daily rasters
  sst_f <- temps[grep(datee[k], temps)]
  v_f <- vs[grep(datee[k], vs)]
  u_f <- us[grep(datee[k], us)]
  ssh_f <- sshs[grep(datee[k], sshs)]
  sst <- raster(paste0(dir, "/", sst_f)) %>% crop(area)
  v <- raster(paste0(dir, "/", v_f)) %>% crop(area)
  u <- raster(paste0(dir, "/", u_f)) %>% crop(area)
  speed <- sqrt(u^2 + v^2) %>% crop(area)
  ssh <- raster(paste0(dir, "/", ssh_f)) %>% crop(area)
  
  sst <- crop(sst, extent(speed))
  v <- crop(v, extent(speed))
  ssh <- crop(ssh, extent(speed))

  # Extract values and inverse V, so southwards flow is positive
  sst.v <- sst[]; v.v <- v[] * -1; speed.v <- speed[]; ssh.v <- ssh[]
  
  # Prepare PCA
  xy <- data.frame(coordinates(sst %>% raster)) 
  dat <- data.frame(sst = sst.v, v = v.v, speed = speed.v, ssh = ssh.v, 
                    x = xy$x, y = xy$y)
  dat <- na.omit(dat) 
  # Scale values to mean and unit variance, no center as value as 0 needs to be meaningul
  pca1 <- prcomp(dat[,-c(5:6)], retx = TRUE, center = FALSE, scale. = TRUE) 
  
  # From now on, we are only interested in PC1, due to most variance.
  # We want to ensure all the loadings are the same sign, and extract them:
  # (i) If loadings are all negative, invert them so they are all positive
  # (ii) If loadings are all positive, leave them alone
  # (iii) If loadings are a combination, this is a 'failure' and will need to be
  #       manually fixed later on. 
  
  if (pca1$rotation["sst","PC1"] < 0 & 
      pca1$rotation["v","PC1"] < 0 & 
      pca1$rotation["speed","PC1"] < 0 &
      pca1$rotation["ssh","PC1"] < 0) { pc1 <- as.vector(pca1$x[,1]) *-1  
  } else if (pca1$rotation["sst","PC1"] > 0 & 
             pca1$rotation["v","PC1"] > 0 & 
             pca1$rotation["speed","PC1"] > 0 &
             pca1$rotation["ssh","PC1"] < 0) { pc1 <- as.vector(pca1$x[,1]) 
  } else { pc1 <- as.vector(pca1$x[,1])
  # If loadings are a combination (i.e., they 'failed'), record PC loadings in this df
  inverse[nrow(inverse) + 1,] <- c(yearmonth, 
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
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1], #variance PC1
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[2], #variance PC2
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[3], #variance PC3
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[4]) #variance PC4
  }
  
  # Add PC information to this df too, regardless of whether it failed or succeeded
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
  
  # Lookup table on whether the loadings were the same direction or different, 
  # and the variance of PC1
  stuff <- data.frame(Date = yearmonth %>% as.character,
                      variance = round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1],
                      Direction = 
                        if(pca1$rotation[1,1] > 0 & 
                           pca1$rotation[2,1] > 0 & 
                           pca1$rotation[3,1] > 0 & 
                           pca1$rotation[4,1] > 0 | 
                           pca1$rotation[1,1] < 0 & 
                           pca1$rotation[2,1] < 0 & 
                           pca1$rotation[3,1] < 0 & 
                           pca1$rotation[4,1] < 0) { "Same" 
                        } else { "Different"} )
  
  all.stuff <- rbind(all.stuff, stuff)
  print(yearmonth)
  
}

# Method 1- evaluation -------------------------------------------------------------------

inversed1 <- length(unique(inverse$date)) 
inversed1 #[1] 36 days with inverse loadings
100 - (inversed1/nrow(df) * 100) #[1] 87.09677 % hit rate

# PC1 variance stats
mean(all.stuff$variance) # [1] 0.6939785
median(all.stuff$variance) # [1] 0.69
sd(all.stuff$variance) #[1] 0.0189
all.stuff$variance %>% range #[1] 0.66 0.78

M1_all_list[[1]] <- inverse
M1_all_list[[2]] <- all.stuff
M1_all_list[[3]] <- df
names(M1_all_list) <- c("Method1_inverse_only", 
                        "Method1_lookup_direction", 
                        "Method1_all_loadings")


# Method 2 temp, v, speed  -----------------------------------------------------------------

df <- data.frame(date = numeric(), 
                 PC1_sst = numeric(), PC1_v = numeric(), PC1_speed = numeric(), 
                 PC2_sst = numeric(), PC2_v = numeric(), PC2_speed = numeric(), 
                 PC3_sst = numeric(), PC3_v = numeric(), PC3_speed = numeric(), 
                 PC1_var =numeric(), PC2_var = numeric(), PC3_var = numeric())

inverse <- data.frame(date = numeric(), 
                      PC1_sst = numeric(), PC1_v = numeric(), PC1_speed = numeric(),
                      PC2_sst = numeric(), PC2_v = numeric(), PC2_speed = numeric(), 
                      PC3_sst = numeric(), PC3_v = numeric(), PC3_speed = numeric(),
                      variance1 = numeric(), variance2 = numeric(), variance3 = numeric())

all.stuff <- data.frame(Date = character(), 
                        Variance = numeric(), 
                        Direction = character(), 
                        Direction.flex = character())

for (k in 1:length(datee)) { 
  
  yearmonth <- datee[k] 
  
  sst_f <- temps[grep(datee[k], temps)]
  v_f <- vs[grep(datee[k], vs)]
  u_f <- us[grep(datee[k], us)]
  sst <- raster(paste0(dir, "/", sst_f)) %>% crop(area)
  v <- raster(paste0(dir, "/", v_f)) %>% crop(area)
  u <- raster(paste0(dir, "/", u_f)) %>% crop(area)
  speed <- sqrt(u^2 + v^2) %>% crop(area)
  
  sst <- crop(sst, extent(speed))
  v <- crop(v, extent(speed))
  
  sst.v <- sst[]
  v.v <- v[] * -1
  speed.v <- speed[]
  
  xy <- data.frame(coordinates(sst)) 
  dat <- data.frame(sst = sst.v, v = v.v, speed = speed.v, 
                    x = xy$x, y = xy$y)
  dat <- na.omit(dat) 
  pca1 <- prcomp(dat[,-c(4:5)], retx = TRUE, center = FALSE, scale. = TRUE) 
  
  if (pca1$rotation["sst","PC1"] < 0 & 
      pca1$rotation["v","PC1"] < 0 & 
      pca1$rotation["speed","PC1"] < 0) {  pc1 <- as.vector(pca1$x[,1]) *-1 
  } else if (pca1$rotation["sst","PC1"] > 0 & 
             pca1$rotation["v","PC1"] > 0 & 
             pca1$rotation["speed","PC1"] > 0) { pc1 <- as.vector(pca1$x[,1]) 
  } else { pc1 <- as.vector(pca1$x[,1]) 
  inverse[nrow(inverse) + 1,] <- c(yearmonth %>% as.character(), 
                                   pca1$rotation[1,1] %>% round(3), #PC1
                                   pca1$rotation[2,1] %>% round(3), #PC1
                                   pca1$rotation[3,1] %>% round(3), #PC1
                                   pca1$rotation[1,2] %>% round(3), #PC2
                                   pca1$rotation[2,2] %>% round(3), #PC2
                                   pca1$rotation[3,2] %>% round(3), #PC2
                                   pca1$rotation[1,3] %>% round(3), #PC3
                                   pca1$rotation[2,3] %>% round(3), #PC3
                                   pca1$rotation[3,3] %>% round(3), #PC3
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1], #variance PC1
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[2], #variance PC2
                                   round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[3]) #variance PC3
  }
  
  df[nrow(df) + 1,] <- c(yearmonth %>% as.character, 
                         pca1$rotation["sst","PC1"] %>% round(3), 
                         pca1$rotation["v","PC1"] %>% round(3), 
                         pca1$rotation["speed","PC1"] %>% round(3), 
                         pca1$rotation["sst","PC2"] %>% round(3), 
                         pca1$rotation["v","PC2"] %>% round(3), 
                         pca1$rotation["speed","PC2"] %>% round(3), 
                         pca1$rotation["sst","PC3"] %>% round(3), 
                         pca1$rotation["v","PC3"] %>% round(3), 
                         pca1$rotation["speed","PC3"] %>% round(3),
                         round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1], 
                         round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[2], 
                         round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[3])
  
  stuff <- data.frame(Date = yearmonth %>% as.character(),
                      variance = round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1],
                      Direction = 
                        if(pca1$rotation[1,1] > 0 & 
                           pca1$rotation[2,1] > 0 & 
                           pca1$rotation[3,1] > 0 | 
                           pca1$rotation[1,1] < 0 & 
                           pca1$rotation[2,1] < 0 & 
                           pca1$rotation[3,1] < 0) { "Same" } 
                      else {"Different"} )
  
  all.stuff <- rbind(all.stuff, stuff)
  print(yearmonth)
  
}


# Method 2 - evaluation--------------------------------------------------------

inversed2 <- length(unique(inverse$date)) 
inversed2 #[1] 34 days with inversed loadings (failures)
100 - (inversed2/nrow(df) * 100) # [1] 87.81362 % hit rate 

# PC1 variance stats
mean(all.stuff$variance) #[1] 0.6352688
median(all.stuff$variance) #[1] 0.63
sd(all.stuff$variance) #[1] 0.03425228
all.stuff$variance %>% range #[1] 0.58 0.77

# Add to list
M2_all_list[[1]] <- inverse
M2_all_list[[2]] <- all.stuff
M2_all_list[[3]] <- df
names(M2_all_list) <- c("Method2_inverse_only", 
                        "Method2_lookup_direction", 
                        "Method2_all_loadings")


# All eval ----------------------------------------------------------------

m1_inverse <- M1_all_list$Method1_inverse_only$date %>% unique() %>% length
100 - (m1_inverse/nrow(M1_all_list$Method1_all_loadings) * 100) 
#[1] 87.09677 hit rate

m2_inverse <- M2_all_list$Method2_inverse_only$date %>% unique %>% length
100 - (m2_inverse/nrow(M2_all_list$Method2_all_loadings) * 100)
# [1] 87.81362% hit rate 

m1_meanvar <- M1_all_list$Method1_all_loadings$PC1_var %>% as.numeric %>% mean
m1_sdvar <- M1_all_list$Method1_all_loadings$PC1_var %>% as.numeric %>% sd
cat("mean of method 1:", m1_meanvar) #mean of method 1: 0.6939785
cat("sd of method 1:", m1_sdvar) #sd of method 1: 0.01893608

m2_meanvar <- M2_all_list$Method2_all_loadings$PC1_var %>% as.numeric %>% mean
m2_sdvar <- M2_all_list$Method2_all_loadings$PC1_var %>% as.numeric %>% sd
cat("mean of method 2:", m2_meanvar) #mean of method 2: 0.6352688
cat("sd of method 2:", m2_sdvar) #sd of method 2: 0.03425228

# BRAN has a worse hit rate and *slightly* worse variance explained by PC1, 
# compared to ACCESS-S2 model