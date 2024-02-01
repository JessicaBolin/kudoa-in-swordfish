# Modelling - prevalence dataset
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)


# Dependencies and data ---------------------------------------------------

library(lme4) 
library(tidyverse)
library(performance)
source("functions/corvif.R")

dat <- readRDS("intense_clean.RDS")
source("functions/model_helpers.R")

# Models ------------------------------------------------------------------

# Temp + EAC models -------------------------------------------------------

### m1: SST ####

m1 <- glmer.nb(TotalFish ~ scale(mean_sst) + (1|operation), 
               data = dat,
               control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m1)
models[nrow(models)+1,] <- mselect(m1)

### m2: (SST + sd_SST + mld1)^2 #### 

corvif(dat[,c("mean_sst", "sd_sst", "mean_mld1")])
m2 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(sd_sst) 
                            + scale(mean_mld1))^2 
               + (1|operation), data = dat, 
               control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m2)
m2a <- update(m2, ~.-scale(mean_sst):scale(sd_sst))
droppy(m2a)
m2b <- update(m2a, ~.-scale(sd_sst):scale(mean_mld1))
droppy(m2b)
m2c <- update(m2b, ~.-scale(sd_sst))
droppy(m2c)
m2d <- update(m2c, ~.-scale(mean_sst):scale(mean_mld1))
droppy(m2d)
m2e <- update(m2d, ~.-scale(mean_sst))
droppy(m2e)
models[nrow(models)+1,] <- mselect(m2e)


### m3: (SST + dist_core + mo_anom)^2 #### 

corvif(dat[,c("mean_sst", "dist_core", "mo_anom")])

m3 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dist_core) 
                            + scale(mo_anom))^2
               + (1|operation), 
               data = dat, 
               control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m3)
m3a <- update(m3, ~.-scale(dist_core):scale(mo_anom))
droppy(m3a)
m3b <- update(m3a, ~.-scale(dist_core):scale(mean_sst))
droppy(m3b)
m3c <- update(m3b, ~.-scale(dist_core))
droppy(m3c)
m3d <- update(m3c, ~.-scale(mean_sst):scale(mo_anom))
droppy(m3d)
m3e <- update(m3d, ~.-scale(mo_anom))
models[nrow(models)+1,] <- mselect(m3e)


### m4: (SST + dist_core + dhd)^2 #### 

corvif(dat[,c("mean_sst", "dist_core", "dhd")])
m4 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dist_core)
                            + scale(dhd))^2 
               + (1|operation), 
               data = dat, 
               control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m4)
m4a <- update(m4, ~. -scale(mean_sst):scale(dhd))
droppy(m4a)
m4b <- update(m4a, ~. -scale(dhd):scale(dist_core))
droppy(m4b)
m4c <- update(m4b, ~. -scale(dhd))
droppy(m4c)
m4d <- update(m4c, ~. -scale(mean_sst):scale(dist_core))
droppy(m4d)
m4e <- update(m4d, ~. -scale(dist_core))
models[nrow(models)+1,] <- mselect(m4e)


### m5: (SST + dist_core)^2 + dhd + mld1 #### 

corvif(dat[,c("mean_sst", "dist_core", "dhd", "mean_mld1")])
m5 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dist_core) 
)^2 + scale(dhd) + 
  scale(mean_mld1)
+ (1|operation), 
data = dat, 
control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m5)
m5a <- update(m5, ~.-scale(dhd))
droppy(m5a)
models[nrow(models)+1,] <- mselect(m5a)


### m6: (SST + dist_core)^2 + dhd + seas_clim_sst_sd #### 
# to avoid convergence

corvif(dat[,c("mean_sst", "dist_core", "dhd", "seas_clim_sst_sd")])
m6 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dist_core))^2 
               + scale(dhd) + scale(seas_clim_sst_sd)
               + (1|operation), 
               data = dat, 
               control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m6)
m6a <- update(m6, ~.-scale(dhd))
droppy(m6a)
m6b <- update(m6a, ~.-scale(seas_clim_sst_sd))
droppy(m6b)
m6c <- update(m6b, ~.-scale(mean_sst):scale(dist_core))
droppy(m6c)
m6d <- update(m6c, ~.-scale(dist_core))
models[nrow(models)+1,] <- mselect(m6d)


### m7: (SST + dist_core)^2 +  dhd +mo_anom ####

corvif(dat[,c("mean_sst", "dist_core", "dhd", "mo_anom")])
m7 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dist_core) 
)^2 +  scale(dhd) + scale(mo_anom)
+ (1|operation), 
data = dat, 
control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m7)
m7a <- update(m7, ~.-scale(dhd))
droppy(m7a)
m7b <- update(m7a, ~.-scale(mo_anom))
droppy(m7b)
m7c <- update(m7b, ~.-scale(mean_sst):scale(dist_core))
droppy(m7c)
m7d <- update(m7c, ~.-scale(dist_core))
droppy(m7d)
models[nrow(models)+1,] <- mselect(m7d)



### m8: (SST + dist_core + dhd)^2 + se_anom ####

corvif(dat[,c("mean_sst", "dist_core", "dhd", "se_anom")])
m8 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dist_core) 
                            + scale(dhd))^2 
               + scale(se_anom)
               + (1|operation), 
               data = dat, 
               control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m8)
m8a <- update(m8, ~.-scale(mean_sst):scale(dhd))
droppy(m8a)
m8b <- update(m8a, ~.-scale(dhd):scale(dist_core))
droppy(m8b)
m8c <- update(m8b, ~.-scale(dhd))
droppy(m8c)
m8d <- update(m8c, ~.-scale(mean_sst):scale(dist_core))
droppy(m8d)
m8e <- update(m8d, ~.-scale(dist_core))
droppy(m8e)
m8f <- update(m8e, ~.-scale(se_anom))
models[nrow(models)+1,] <- mselect(m8f)



### m9: (SST + dhd)^2 + ssh  + dist_core ####

corvif(dat[,c("mean_sst", "dist_core", "dhd", "mean_ssh_corrected")])
m9 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dhd))^2 
               + scale(mean_ssh_corrected)
               + scale(dist_core) + (1|operation), 
               data = dat, 
               control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m9)
m9a <- update(m9, ~.-scale(dist_core))
droppy(m9a)
m9b <- update(m9a, ~.-scale(mean_ssh_corrected))
droppy(m9b)
m9c <- update(m9b, ~.-scale(mean_sst):scale(dhd))
droppy(m9c)
m9d <- update(m9c, ~.-scale(dhd))
droppy(m9d)
models[nrow(models)+1,] <- mselect(m9d)


### m10: (SST + dist_core)^2 + dhd + mean_v ####

corvif(dat[,c("mean_sst", "dist_core", "dhd", "mean_v")])
m10 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dist_core))^2 
                + scale(dhd) + scale(mean_v)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m10)
m10a <- update(m10, ~.-scale(dhd))
droppy(m10a)
m10b <- update(m10a, ~.-scale(mean_v))
droppy(m10b)
m10c <- update(m10b, ~.-scale(mean_sst):scale(dist_core))
droppy(m10c)
m10d <- update(m10c, ~.-scale(dist_core))
droppy(m10d)
models[nrow(models)+1,] <- mselect(m10d)


### m11: (SST  + dhd)^2 + mean_eke + dist_core ####

corvif(dat[,c("mean_sst", "dist_core", "dhd", "mean_eke")])
m11 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dhd))^2 
                +  scale(dist_core) +
                  scale(mean_eke)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m11)
m11a <- update(m11, ~.-scale(mean_sst):scale(dhd))
droppy(m11a)
m11b <- update(m11a, ~.-scale(dhd))
droppy(m11b)
m11c <- update(m11b, ~.-scale(dist_core))
droppy(m11c)
m11d <- update(m11c, ~.-scale(mean_eke))
droppy(m11d)
models[nrow(models)+1,] <- mselect(m11d)


### m12: (SST + mld + dhd)^2  ####

corvif(dat[,c("mean_sst", "mean_mld1", "dhd")])
m12 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_mld1) 
                             + scale(dhd))^2 
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m12)
m12a <- update(m12, ~.-scale(mean_sst):scale(dhd))
droppy(m12a)
m12b <- update(m12a, ~.-scale(dhd):scale(mean_mld1))
droppy(m12b)
m12c <- update(m12b, ~.-scale(dhd))
droppy(m12c)
m12d <- update(m12c, ~. - scale(mean_sst):scale(mean_mld1))
droppy(m12d)
m12e <- update(m12d, ~. -scale(mean_sst))
models[nrow(models)+1,] <- mselect(m12e)


### m13: (SST + dhd)^2 + sd_sst  + dist_core####

corvif(dat[,c("mean_sst", "dist_core", "dhd", "sd_sst")])
m13 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(dhd))^2 
                + scale(sd_sst)
                + scale(dist_core)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m13)
m13a <- update(m13, ~.-scale(mean_sst):scale(dhd))
droppy(m13a)
m13b <- update(m13a, ~.-scale(dhd))
droppy(m13b)
m13c <- update(m13b, ~.-scale(dist_core))
droppy(m13c)
m13d <- update(m13c, ~.-scale(sd_sst))
droppy(m13d)
models[nrow(models)+1,] <- mselect(m13d)



### m14: SST + distcore + sd_sst + mo_anom + v + mld + eke + ssh ####

corvif(dat[,c("mean_sst", "dist_core", "sd_sst", "mo_anom", 
              "mean_v", "mean_eke", "mean_ssh_corrected", "mean_mld1")])

m14 <- glmer.nb(TotalFish ~ scale(mean_sst) + scale(dist_core) 
                + scale(sd_sst) + scale(mo_anom) +
                  scale(mean_v) + scale(mean_mld1) + scale(mean_eke) 
                + scale(mean_ssh_corrected) 
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m14)
m14a <- update(m14, ~.-scale(mo_anom))
droppy(m14a)
m14b <- update(m14a, ~. -scale(mean_eke))
droppy(m14b)
m14c <- update(m14b, ~. -scale(mean_v))
droppy(m14c)
m14d <- update(m14c, ~.-scale(mean_sst))
droppy(m14d)
m14e <- update(m14d, ~.-scale(mean_ssh_corrected))
droppy(m14e)
m14f <- update(m14e, ~. -scale(dist_core))
droppy(m14f)
m14g <- update(m14f, ~.-scale(sd_sst))
droppy(m14g)
models[nrow(models)+1,] <- mselect(m14g)


# Seamount models ----------------------------------------------------------


#### m15: Seamount * sd_depth  ####
corvif(dat[,c("SeamountDistKM", "sd_depth")])

m15 <- glmer.nb(TotalFish ~ scale(SeamountDistKM) * scale(sd_depth)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m15)
m15a <- update(m15, ~.-scale(SeamountDistKM):scale(sd_depth))
droppy(m15a)
m15b <- update(m15a, ~.-scale(SeamountDistKM))
droppy(m15b)
models[nrow(models)+1,] <- mselect(m15b)



#### m16: (Seamount * mean_sst)^2 + sd_depth ####

corvif(dat[,c("SeamountDistKM", "sd_depth", "mean_sst")])

m16 <- glmer.nb(TotalFish ~ (scale(SeamountDistKM) + scale(mean_sst))^2 
                + scale(sd_depth)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m16)
m16a <- update(m16, ~. -scale(SeamountDistKM):scale(mean_sst))
droppy(m16a)
m16b <- update(m16a, ~. - scale(SeamountDistKM))
droppy(m16b)
m16c <- update(m16b, ~.-scale(mean_sst))
droppy(m16c)
models[nrow(models)+1,] <- mselect(m16c)


#### m17: (Seamount + sst + sd_depth + eke)^2 ####

corvif(dat[,c("SeamountDistKM", "sd_depth", "mean_sst","mean_eke")])
m17 <- glmer.nb(TotalFish ~ (scale(SeamountDistKM) + scale(mean_sst) 
)^2 + scale(mean_eke) + scale(sd_depth)
+ (1|operation), 
data = dat, 
control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m17)
m17a <- update(m17, ~.-scale(SeamountDistKM):scale(mean_sst))
droppy(m17a)
m17b <- update(m17a, ~. - scale(SeamountDistKM))
droppy(m17b)
m17c <- update(m17b, ~. - scale(mean_eke))
droppy(m17c)
m17d <- update(m17c, ~. -scale(mean_sst))
droppy(m17d)
models[nrow(models)+1,] <- mselect(m17d)


#### m18: (Seamount + sst)^2  + mld ####

corvif(dat[,c("SeamountDistKM", "mean_mld1", "mean_sst")])

m18 <- glmer.nb(TotalFish ~ (scale(SeamountDistKM) + scale(mean_sst))^2 
                + scale(mean_mld1)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m18)
m18a <- update(m18, ~.-scale(SeamountDistKM):scale(mean_sst))
droppy(m18a)
m18b <- update(m18a, ~. - scale(mean_sst))
droppy(m18b)
m18c <- update(m18b, ~. - scale(SeamountDistKM))
droppy(m18c)
models[nrow(models)+1,] <- mselect(m18c)


#### m19: (Seamount + sst + sst_sd )^2 + sd_depth ####

corvif(dat[,c("SeamountDistKM", "sd_sst", "mean_sst", "sd_depth")])

m19 <- glmer.nb(TotalFish ~ (scale(SeamountDistKM) + scale(mean_sst) 
                             + scale(sd_sst))^2
                + scale(sd_depth)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m19)
m19a <- update(m19, ~. -scale(SeamountDistKM):scale(mean_sst))
droppy(m19a)
m19b <- update(m19a, ~.-scale(sd_sst):scale(mean_sst))
droppy(m19b)
m19c <- update(m19b, ~.-scale(SeamountDistKM):scale(sd_sst))
droppy(m19c)
m19d <- update(m19c, ~.-scale(SeamountDistKM))
droppy(m19d)
m19e <- update(m19d, ~.-scale(sd_sst))
droppy(m19e)
m19f <- update(m19e, ~.-scale(mean_sst))
models[nrow(models)+1,] <- mselect(m19f)


### m20: (Seamount + distcore + sst)^2 + sd_depth ####

corvif(dat[,c("mean_sst", "dist_core", "sd_sst", "SeamountDistKM")])

m20 <- glmer.nb(TotalFish ~ (scale(SeamountDistKM) + scale(mean_sst) 
                             + scale(dist_core))^2 + scale(sd_depth)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m20)
m20a <- update(m20, ~.-scale(SeamountDistKM):scale(mean_sst))
droppy(m20a)
m20b <- update(m20a, ~.-scale(SeamountDistKM):scale(dist_core))
droppy(m20b)
m20c <- update(m20b, ~.-scale(SeamountDistKM))
droppy(m20c)
m20d <- update(m20c, ~.-scale(mean_sst):scale(dist_core))
droppy(m20d)
m20e <- update(m20d, ~. -scale(dist_core))
droppy(m20e)
m20f <- update(m20e, ~. -scale(mean_sst))
models[nrow(models)+1,] <- mselect(m20f)


#### m21: Seamount + sst + eke + sst_sd + dist_core + sd_depth  ####

corvif(dat[,c("mean_sst", "dist_core", "sd_sst", "mean_eke", 
              "SeamountDistKM", "sd_depth")])

m21 <- glmer.nb(TotalFish ~ scale(SeamountDistKM) + scale(mean_sst) 
                + scale(dist_core) + scale(sd_sst) + scale(mean_eke) 
                + scale(sd_depth)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m21)
m21a <- update(m21, ~.-scale(mean_sst))
droppy(m21a)
m21b <- update(m21a, ~.-scale(mean_eke))
droppy(m21b)
m21c <- update(m21b, ~.-scale(sd_sst))
droppy(m21c)
m21d <- update(m21c, ~.-scale(SeamountDistKM))
droppy(m21d)
m21e <- update(m21d, ~.-scale(dist_core))
droppy(m21e)
models[nrow(models)+1,] <- mselect(m21e)



#### m22: (Depth + depth_sd + mld)^2 + sst ####

corvif(dat[,c("mean_depth", "sd_depth", "mean_mld1", "mean_sst")])
m22 <- glmer.nb(TotalFish ~ (scale(mean_depth) + scale(sd_depth) 
)^2 + scale(mean_sst) + scale(mean_mld1)
+ (1|operation), 
data = dat, 
control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m22)
m22a <- update(m22, ~.-scale(mean_depth):scale(sd_depth))
droppy(m22a)
m22b <- update(m22a, ~.-scale(mean_sst))
droppy(m22b)
m22c <- update(m22b, ~.-scale(mean_depth))
droppy(m22c)
models[nrow(models)+1,] <- mselect(m22c)

#### m23: (Depth, sd_depth)^2 +  dhd + mean_sst ####

corvif(dat[,c("mean_depth", "sd_depth", "dhd", "mean_sst")])

m23 <- glmer.nb(TotalFish ~ (scale(mean_depth) + scale(sd_depth))^2 
                + scale(dhd) + scale(mean_sst)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m23)
m23a <- update(m23, ~.-scale(mean_depth):scale(sd_depth))
droppy(m23a)
m23b <- update(m23a, ~.-scale(mean_depth))
droppy(m23b)
m23c <- update(m23b, ~.-scale(dhd))
droppy(m23c)
m23d <- update(m23c, ~.-scale(mean_sst))
models[nrow(models)+1,] <- mselect(m23d)


# Oceanography models -----------------------------------------------------

##### m24: SST + dist_core + mean_v + mld + eke + seamount + sd_depth ####

corvif(dat[,c("mean_sst", "dist_core", "mean_eke", "SeamountDistKM",
              "mean_mld1", "sd_depth")])

m24 <- glmer.nb(TotalFish ~ scale(SeamountDistKM) + scale(mean_sst) 
                + scale(dist_core) +
                  scale(mean_eke)  +
                  scale(mean_mld1) + scale(sd_depth)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m24)
m24a <- update(m24, ~.-scale(mean_eke))
droppy(m24a)
m24b <- update(m24a, ~.-scale(mean_sst))
droppy(m24b)
m24c <- update(m24b, ~.-scale(dist_core))
droppy(m24c)
m24d <- update(m24c, ~.-scale(SeamountDistKM))
droppy(m24d)
models[nrow(models)+1,] <- mselect(m24d)


#### m25: (SST + ssh)^2 +  mld1 + eke ####
corvif(dat[,c("mean_sst", "mean_ssh_corrected", "mean_mld1", "mean_eke")])

m25 <- glmer.nb(TotalFish ~ (scale(mean_sst)  
                             + scale(mean_ssh_corrected))^2
                + scale(mean_eke) + scale(mean_mld1)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m25)
m25a <- update(m25, ~.-scale(mean_eke))
droppy(m25a)
models[nrow(models)+1,] <- mselect(m25a)


#### m26: (SST + ssh + eke)^2 + v ####

corvif(dat[,c("mean_sst", "mean_ssh_corrected", "mean_v", "mean_eke")])
m26 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_eke) + 
                               scale(mean_ssh_corrected))^2
                + scale(mean_v)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m26)
m26a <- update(m26, ~.-scale(mean_v))
droppy(m26a)
m26b <- update(m26a, ~.-scale(mean_sst):scale(mean_eke))
droppy(m26b)
m26c <- update(m26b, ~.-scale(mean_ssh_corrected):scale(mean_eke))
droppy(m26c)
m26d <- update(m26c, ~.-scale(mean_eke))
droppy(m26d)
m26e <- update(m26d, ~.-scale(mean_sst):scale(mean_ssh_corrected))
droppy(m26e)
m26f <- update(m26e, ~.-scale(mean_ssh_corrected))
droppy(m26f)
models[nrow(models)+1,] <- mselect(m26f)


# Productivity models -----------------------------------------------------

#### m27: (Chl-a + mld + ssh + eke)^2 ####
corvif(dat[,c("mean_chla", "mean_eke", "mean_mld1", "mean_ssh_corrected")])

m27 <- glmer.nb(TotalFish ~ (scale(mean_chla) + scale(mean_mld1))^2 
                + scale(mean_eke) + scale(mean_ssh_corrected)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))

droppy(m27)
m27a <- update(m27, ~.-scale(mean_eke))
droppy(m27a)
m27b <- update(m27a, ~.-scale(mean_ssh_corrected))
droppy(m27b)
models[nrow(models)+1,] <- mselect(m27b)


#### m28: (chla + mld)^2 + sd_depth + seamount ####

corvif(dat[,c("mean_chla", "mean_mld1", "SeamountDistKM", "sd_depth")])

m28 <- glmer.nb(TotalFish ~ (scale(mean_chla) + scale(mean_mld1))^2 
                + scale(sd_depth) + scale(SeamountDistKM) 
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m28)
m28a <- update(m28, ~.-scale(SeamountDistKM))
droppy(m28a)
m28b <- update(m28a, ~.-scale(sd_depth))
droppy(m28b)
models[nrow(models)+1,] <- mselect(m28b)


# Month models ------------------------------------------------------------

# What about effect of month? 
corvif(dat[,c("mean_sst", "Month")])
glmer.nb(TotalFish ~ Month 
         + (1|operation), 
         data = dat,
         control = glmerControl(optCtrl = list(maxfun = 100000)))

#### Month * MLD #####
corvif(dat[, c("Month", "mean_mld1")])
glmer.nb(TotalFish ~ Month * scale(mean_mld1)
         + (1|operation), 
         data = dat,
         control = glmerControl(optCtrl = list(maxfun = 100000)))
#### Month * sd_depth #####
glmer.nb(TotalFish ~ Month * scale(sd_depth) 
         + (1|operation), 
         data = dat,
         control = glmerControl(optCtrl = list(maxfun = 100000)))




# Non-distcore EAC models -------------------------------------------------

### m29: (SST + mean_ssh_corrected + mean_v)^2 #### 
corvif(dat[,c("mean_sst", "mean_v", "mean_ssh_corrected", "mean_eke")])

m29 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_v) 
                             + scale(mean_ssh_corrected))^2 + scale(mean_eke)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m29)
m29a <- update(m29, ~. -scale(mean_v):scale(mean_ssh_corrected))
droppy(m29a)
m29b <- update(m29a, ~. -scale(mean_sst):scale(mean_v))
droppy(m29b)
m29c <- update(m29b, ~. -scale(mean_v))
droppy(m29c)
m29d <- update(m29c, ~. -scale(mean_eke))
droppy(m29d)
m29e <- update(m29d, ~. -scale(mean_sst):scale(mean_ssh_corrected))
droppy(m29e)
m29f <- update(m29e, ~. -scale(mean_ssh_corrected))
droppy(m29f)
models[nrow(models)+1,] <- mselect(m29f)


### m30: (SST + v + dhd)^2 #### 

corvif(dat[,c("mean_sst", "mean_v", "dhd")])
m30 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_v) + scale(dhd))^2
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m30)
m30a <- update(m30, ~. -scale(mean_sst):scale(dhd))
droppy(m30a)
m30b <- update(m30a, ~. -scale(mean_sst):scale(mean_v))
droppy(m30b)
m30c <- update(m30b, ~. -scale(dhd):scale(mean_v))
droppy(m30c)
m30d <- update(m30c, ~. -scale(dhd))
droppy(m30d)
m30e <- update(m30d, ~. -scale(mean_v))
droppy(m30e)
models[nrow(models)+1,] <- mselect(m30e)


### m31:  (SST + v + dhd + mon_clim_sst_sd)^2 #### 

corvif(dat[,c("mean_sst", "mean_v", "dhd", "mon_clim_sst_sd")])
m31 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_v) 
)^2 + scale(dhd) + scale(mon_clim_sst_sd)
+ (1|operation), 
data = dat, 
control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m31)
m31a <- update(m31, ~. -scale(dhd))
droppy(m31a)
mm31b <- update(m31a, ~. - scale(mean_v):scale(mean_sst))
droppy(mm31b)
m31c <- update(mm31b, ~. - scale(mean_sst))
droppy(m31c)
m31d <- update(m31c, ~. - scale(mean_v))
droppy(m31d)
models[nrow(models)+1,] <- mselect(m31d)


### m32: (SST + mean_v + mean_eke + mon_clim_sst_sd)^2  #### 

corvif(dat[,c("mean_sst", "mean_v", "mean_eke", "mon_clim_sst_sd")])

m32 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_v) + 
                               scale(mean_eke) 
)^2  + scale(mon_clim_sst_sd)
+ (1|operation), 
data = dat,
control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m32)
m32a <- update(m32, ~. -scale(mean_sst):scale(mean_eke))
droppy(m32a)
m32b <- update(m32a, ~. -scale(mean_v):scale(mean_eke))
droppy(m32b)
m32c <- update(m32b, ~. -scale(mean_eke))
droppy(m32c)
m32d <- update(m32c, ~. -scale(mean_sst):scale(mean_v))
droppy(m32d)
m32e <- update(m32d, ~. -scale(mean_sst))
droppy(m32e)
m32f <- update(m32e, ~. -scale(mean_v))
droppy(m32f)
models[nrow(models)+1,] <- mselect(m32f)


### m33: (SST + v + mon_clim_sst_sd)^2 #### 

corvif(dat[,c("mean_sst", "mean_v", "mon_clim_sst_sd", "mean_mld1")])
m33 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_v))^2
                + scale(mean_mld1) + scale(mon_clim_sst_sd)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))

droppy(m33)
m33a <- update(m33, ~. -scale(mean_v):scale(mean_sst))
droppy(m33a)
m33b <- update(m33a, ~. -scale(mean_v))
droppy(m33b)
m33c <- update(m33b, ~. -scale(mean_sst))
droppy(m33c)
m33d <- update(m33c, ~. -scale(mean_mld1))
droppy(m33d)
models[nrow(models)+1,] <- mselect(m33d)


### m34: (SST + ssh_corrected )^2 + mon_clim_sst_sd + mld1 #### 

corvif(dat[,c("mean_sst", "mean_ssh_corrected", "mon_clim_sst_sd", "mean_mld1")])
m34 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_ssh_corrected) 
)^2 + scale(mon_clim_sst_sd) + scale(mean_mld1)
+ (1|operation), 
data = dat,
control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m34)
models[nrow(models)+1,] <- mselect(m34)


### m35: (SST + ssh_corrected )^2 + mon_clim_sst_sd + sd_depth #### 

corvif(dat[,c("mean_sst", "mean_ssh_corrected", "mon_clim_sst_sd", "sd_depth")])
m35 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_ssh_corrected))^2 
                + scale(mon_clim_sst_sd) + scale(sd_depth)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m35)
m35a <- update(m35, ~. -scale(mean_sst):scale(mean_ssh_corrected))
droppy(m35a)
m35b <- update(m35a, ~. -scale(mean_sst))
droppy(m35b)
m35c <- update(m35b, ~. -scale(mean_ssh_corrected))
droppy(m35c)
models[nrow(models)+1,] <- mselect(m35c)


### m36: (SST + mean_v + sd_depth)^2 + seas_clim_sst_sd #### 
corvif(dat[,c("mean_sst", "mean_v", "sd_depth", "seas_clim_sst_sd")])

m36 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_v) + scale(sd_depth)
)^2  +scale(seas_clim_sst_sd)
+ (1|operation), 
data = dat, 
control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m36)
m36a <- update(m36, ~. -scale(mean_sst):scale(mean_v))
droppy(m36a)
m36b <- update(m36a, ~. -scale(seas_clim_sst_sd))
droppy(m36b)
m36c <- update(m36b, ~.-scale(mean_sst):scale(sd_depth))
droppy(m36c)
m36d <- update(m36c, ~.-scale(mean_sst))
droppy(m36d)
models[nrow(models)+1,] <- mselect(m36d)


### m37: (SST + mean_v + mo_anom)^2 + dhd ####

corvif(dat[,c("mean_sst", "mean_v", "dhd", "mo_anom")])

m37 <- glmer.nb(TotalFish ~ scale(mean_sst) * scale(mean_v) + scale(mo_anom) * 
                  scale(mean_sst) + scale(mo_anom) * scale(dhd)
                + (1|operation), 
                data = dat,
                control = glmerControl(optCtrl = list(maxfun = 100000)))
drop1(m37, test = "Chi")
m37a <- update(m37, ~.-scale(dhd):scale(mo_anom))
droppy(m37a)
m37b <- update(m37a, ~.-scale(dhd))
droppy(m37b)
m37c <- update(m37b, ~.-scale(mean_sst):scale(mean_v))
droppy(m37c)
m37d <- update(m37c, ~.-scale(mean_v))
droppy(m37d)
m37e <- update(m37d, ~.-scale(mean_sst):scale(mo_anom))
droppy(m37e)
m37f <- update(m37e, ~.-scale(mo_anom))
droppy(m37f)
models[nrow(models)+1,] <- mselect(m37f)


### m38: (SST + mean_v + se_anom)^2 + dhd ####

corvif(dat[,c("mean_sst", "mean_v", "dhd", "se_anom")])
m38 <- glmer.nb(TotalFish ~ scale(mean_sst) * scale(mean_v) 
                + scale(se_anom) * scale(mean_sst) 
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m38)
m38a <- update(m38, ~.-scale(mean_sst):scale(mean_v))
droppy(m38a)
m38b <- update(m38a, ~. -scale(mean_v))
droppy(m38b)
m38c <- update(m38b, ~. -scale(mean_sst):scale(se_anom))
droppy(m38c)
m38d <- update(m38c, ~. -scale(se_anom))
models[nrow(models)+1,] <- mselect(m38d)


### m39: (SST + mean_v + eke + ssh)^2  ####

corvif(dat[,c("mean_sst", "mean_v", "mean_eke", "mean_ssh_corrected")])
m39 <- glmer.nb(TotalFish ~ (scale(mean_sst) + scale(mean_v)  
)^2 + scale(mean_ssh_corrected) + scale(mean_eke)
+ (1|operation), 
data = dat, 
control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m39)
m39a <- update(m39, ~.-scale(mean_ssh_corrected))
droppy(m39a)
m39b <- update(m39a, ~.-scale(mean_v):scale(mean_sst))
droppy(m39b)
m39c <- update(m39b, ~.-scale(mean_v))
droppy(m39c)
m39d <- update(m39c, ~.-scale(mean_eke))
droppy(m39d)
models[nrow(models)+1,] <- mselect(m39d)


### m40: (SST * mean_v) + (DHD * SST) + (DHD * mo_anom) + mon_clim_sst_sd #####

corvif(dat[,c("mean_sst", "mean_v", "dhd", "mo_anom", "mon_clim_sst_sd")])
m40 <- glmer.nb(TotalFish ~ scale(mean_sst) * scale(mean_v) +
                  scale(dhd) * scale(mean_sst) +
                  scale(mo_anom) * scale(mean_sst) + 
                  scale(mon_clim_sst_sd)
                + (1|operation), 
                data = dat, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m40)
m40a <- update(m40, ~.-scale(mean_sst):scale(dhd))
droppy(m40a)
m40b <- update(m40a, ~.-scale(dhd))
drop1(m40b, test = "Chi")
m40c <- update(m40b, ~.-scale(mean_sst):scale(mean_v))
drop1(m40c, test = 'Chi')
m40d <- update(m40c, ~.-scale(mean_v))
droppy(m40d)
m40e <- update(m40d, ~.-scale(mean_sst):scale(mo_anom))
droppy(m40e)
m40f <- update(m40e, ~.-scale(mean_sst))
droppy(m40f)
m40g <- update(m40f, ~.-scale(mo_anom))
droppy(m40g)
models[nrow(models)+1,] <- mselect(m40g)



# AIC TABLE ---------------------------------------------------------------

aics <- models[order(models$AIC),]
aics <- aics[-nrow(aics),]

aics$deltaaic <- NA
for (i in 1:nrow(aics)) {
  aics$deltaaic[i] <- bbmle::AICtab(aics[aics$AIC == min(aics$AIC),]$model %>% get,
                                    aics$model[i] %>% get)$dAIC[2]
}

head(aics)
aics
summary(m35c)
summary(m22c)
summary(m24d) #duplicatd
summary(m34)
summary(m27b)
summary(m28b) #nope
summary(m32f)
summary(m31d)
summary(m5a)
summary(m18c)
summary(m25a)
summary(m36d)
summary(m23d)
summary(m1)

aics %>% head(20) # lots of duplications for top 10
# model    R2c    R2m deviance      AIC df         L offset   optimizer singular    theta overdisp
# 36  m35c 0.1693 0.0644 21928.94 21938.94  5 -10964.47    log Nelder_Mead    FALSE 1.060777 1.316326
# 23  m22c 0.1714 0.0564 21931.37 21941.37  5 -10965.68    log Nelder_Mead    FALSE 1.063379 1.309937
# 25  m24d 0.1715 0.0564 21931.37 21941.37  5 -10965.68    log Nelder_Mead    FALSE 1.063990 1.310622
# 35   m34 0.1765 0.0692 21926.74 21942.74  8 -10963.37    log Nelder_Mead    FALSE 1.065122 1.316416
# 28  m27b 0.1660 0.0546 21930.87 21942.87  6 -10965.44    log Nelder_Mead    FALSE 1.062909 1.302099
# 29  m28b 0.1654 0.0545 21930.88 21942.88  6 -10965.44    log Nelder_Mead    FALSE 1.059767 1.298603
# 33  m32f 0.1778 0.0433 21935.23 21943.23  4 -10967.62    log Nelder_Mead    FALSE 1.064298 1.299132
# 32  m31d 0.1780 0.0433 21935.23 21943.23  4 -10967.62    log Nelder_Mead    FALSE 1.065089 1.300028
# 34  m33d 0.1781 0.0433 21935.24 21943.24  4 -10967.62    log Nelder_Mead    FALSE 1.065428 1.300411
# 41  m40g 0.1783 0.0433 21935.24 21943.24  4 -10967.62    log Nelder_Mead    FALSE 1.066346 1.301445
# 6    m5a 0.1816 0.0616 21929.58 21943.58  7 -10964.79    log Nelder_Mead    FALSE 1.066041 1.310853
# 19  m18c 0.1794 0.0399 21936.28 21944.28  4 -10968.14    log Nelder_Mead    FALSE 1.064444 1.292765
# 13  m12e 0.1794 0.0399 21936.28 21944.28  4 -10968.14    log Nelder_Mead    FALSE 1.064315 1.292623
# 15  m14g 0.1792 0.0398 21936.28 21944.28  4 -10968.14    log Nelder_Mead    FALSE 1.063552 1.291759
# 3    m2e 0.1789 0.0398 21936.28 21944.28  4 -10968.14    log Nelder_Mead    FALSE 1.062248 1.290292

aics

saveRDS(aics, "paper_final/rds/intense_aic_table.RDS")

## Choosing best model

#Top models

#m35c
formula(m35c) 
#TotalFish ~ scale(mon_clim_sst_sd) + scale(sd_depth) + (1 | operation)
formula(m22c) 
#TotalFish ~ scale(sd_depth) + scale(mean_mld1) + (1 | operation)
formula(m24d) #as above
#TotalFish ~ scale(mean_mld1) + scale(sd_depth) + (1 | operation)
formula(m34)
#TotalFish ~ (scale(mean_sst) + scale(mean_ssh_corrected))^2 + 
#scale(mon_clim_sst_sd) + scale(mean_mld1) + (1 | operation)
formula(m27b) 
#TotalFish ~ scale(mean_chla) + scale(mean_mld1) + (1 | operation) + 
#scale(mean_chla):scale(mean_mld1)
formula(m28b) 
#TotalFish ~ scale(mean_chla) + scale(mean_mld1) + (1 | operation) + 
#scale(mean_chla):scale(mean_mld1)

formula(m32f) #TotalFish ~ scale(mon_clim_sst_sd) + (1 | operation)
formula(m31d) #TotalFish ~ scale(mon_clim_sst_sd) + (1 | operation)
formula(m33d) #TotalFish ~ scale(mon_clim_sst_sd) + (1 | operation)
formula(m40g) #TotalFish ~ scale(mon_clim_sst_sd) + (1 | operation)

formula(m5a) 
#TotalFish ~ scale(mean_sst) + scale(dist_core) + scale(mean_mld1) + 
#(1 | operation) + scale(mean_sst):scale(dist_core)

formula(m18c) #TotalFish ~ scale(mean_mld1) + (1 | operation)

#Inspect residuals
DHARMa::simulateResiduals(m35c) %>% plot 
check_outliers(m35c) #no outliers - 2 deviations detected

DHARMa::simulateResiduals(m22c) %>% plot
check_outliers(m22c) #no outliers - 2 deviations detected

DHARMa::simulateResiduals(m34) %>% plot 
check_outliers(m34) #no outliers - 3 deviations detected

DHARMa::simulateResiduals(m27b) %>% plot
check_outliers(m27b) #no outliers - 2 deviation detected

DHARMa::simulateResiduals(m32f) %>% plot
check_outliers(m32f) #one outlier - 2 deviations detected (736)

DHARMa::simulateResiduals(m5a) %>% plot
check_outliers(m5a) #no outliers- 2 deviations detected 

DHARMa::simulateResiduals(m18c) %>% plot
check_outliers(m18c) #1 outliers- 1 deviations detected  (736)

#inspect deviation
DHARMa::simulateResiduals(m18c) -> testy
plot(testy)
qqq <- DHARMa::testQuantiles(testy, plot = T) 
qqq$pvals #[1] 0.04437518 0.05629943 0.30594296 (barely signfiicant...)
summary(qqq$qgamFits[[1]])$edf #2.692865 (2 knots)
#not wiggly enough (and only just significant) to warrant refitting with GAM

# remove outlier of m18c and retest
performance::check_outliers(m18c) #1 outlier detected: case 736.
m18_rm <- update(m18c, data = dat[-736,]) #rm outlier
check_outliers(m18_rm) #OK: No outliers detected.
DHARMa::simulateResiduals(m18_rm) %>% plot #not as bad
qqq <- DHARMa::testQuantiles(DHARMa::simulateResiduals(m18_rm), plot = T) 
qqq$pvals #[1] 0.03119375 0.17427728 0.55493853
summary(qqq$qgamFits[[1]])$edf #2.108586
# Only one spline significant, and *slightly* less wiggly

# Outcome. While it's not the best model in terms of AIC, m19_rm the only one that passes residual checks and outlier checks. 

## Solution
# Best model is m18c - mld only
bbmle::AICtab(m18c, m35c)  
# dAIC df
# m35c 0.0  5 
# m18c 5.3  4

# (i) is 5.3 AIC units worse than the best model, but, has best DHARMa residuals of all models 
# (ii) has lowest dispersion ratios of top 12 models
# (iii) simplicity = lower degrees of freedom than best model 