# Modelling - prevalence dataset
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)
# Updated February 2023
# NOTE: script will not run

# Dependencies and data ---------------------------------------------------

library(lme4) 
library(tidyverse)
library(performance)
library(dismo)
source("functions/corvif.R")
source("functions/model_helpers.R")

dat <- readRDS("prev_clean.RDS") 
#not available in repo
nrow(dat) #1539 sword available for modelling
presences <- subset(dat, overall == 1)
absences <- subset(dat, overall == 0)
dat$Month <- as.factor(dat$Month)

# Models ------------------------------------------------------------------

# Temp + EAC models (SST * DistCore) -------------------------------------------------------

### m1: SST ####

m1 <- glmer(overall ~ scale(mean_sst) + (1|operation), 
            data = dat, family = binomial, 
            control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m1) 
models_prev[nrow(models_prev)+1,] <- mselect_prev(m1)


### m2: (SST + sd_SST + mon_clim_sst_sd)^2 #### 

corvif(dat[,c("mean_sst", "sd_sst", "mon_clim_sst_sd")])
m2 <- glmer(overall ~ (scale(mean_sst) + scale(sd_sst) + 
                         scale(mon_clim_sst_sd))^2 + (1|operation), 
            data = dat, family = binomial,
            control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m2)
m2a <- update(m2, ~. - scale(sd_sst):scale(mon_clim_sst_sd))
droppy(m2a)
m2b <- update(m2a, ~.-scale(sd_sst):scale(mean_sst))
droppy(m2b)
m2c <- update(m2b, ~. -scale(sd_sst))
droppy(m2c)
m2d <- update(m2c, ~. -scale(mean_sst):scale(mon_clim_sst_sd))
droppy(m2d)
m2e <- update(m2d, ~. -scale(mon_clim_sst_sd))
droppy(m2e)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m2e)

### m3: (SST + dist_core + mo_anom)^2 #### 

corvif(dat[,c("mean_sst", "dist_core", "mo_anom")])

m3 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(mo_anom))^2
            + (1|operation), 
            data = dat, family = binomial,
            control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m3)
m3b <- update(m3, ~. -scale(dist_core):scale(mo_anom))
droppy(m3b)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m3b)


### m4: (SST + dist_core + dhd)^2 #### 

corvif(dat[,c("mean_sst", "dist_core", "dhd")])

m4 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(dhd))^2
            + (1|operation), 
            data = dat, family = binomial,
            control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m4)
m4b <- update(m4, ~. -scale(mean_sst):scale(dhd))
droppy(m4b)
m4c <- update(m4b, ~. -scale(dist_core):scale(dhd))
droppy(m4c)
m4d <- update(m4c, ~. -scale(dhd))
droppy(m4d)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m4d)


### m5: (SST + dist_core + dhd)^2 + mon_clim_sst_sd #### 

corvif(dat[,c("mean_sst", "dist_core", "dhd", "mon_clim_sst_sd")])
# note - even though sst and mon_clim_sst_sd are 0.706 pearson correlation,
# we keep it in model due to all GVIFs being below 3
m5 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + 
                         scale(dhd))^2 + scale(mon_clim_sst_sd)
            + (1|operation), 
            data = dat, family = binomial,
            control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m5)
m5a <- update(m5, ~. -scale(mean_sst):scale(dhd))
droppy(m5a)
m5b <- update(m5a, ~. - scale(dist_core):scale(dhd))
droppy(m5b)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m5b)


### m5a: (SST + dist_core + mon_clim_sst_sd)^2  #### 

corvif(dat[,c("mean_sst", "dist_core", "mon_clim_sst_sd")])
# note - even though sst and mon_clim_sst_sd are 0.706 pearson correlation,
# we keep it in model due to all GVIFs being below 3
m51 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + 
                          scale(mon_clim_sst_sd))^2  + scale(dhd)
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m51)
m5a1 <- update(m51, ~. -scale(dist_core):scale(mon_clim_sst_sd))
droppy(m5a1)
m5b1 <- update(m5a1, ~. -scale(mean_sst):scale(mon_clim_sst_sd))
droppy(m5b1)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m5b1)


### m6: (SST + dist_core + dhd)^2 + seas_clim_sst_sd #### 
corvif(dat[,c("mean_sst", "dist_core", "dhd", "seas_clim_sst_sd")])

m6 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(dhd))^2 + 
              scale(seas_clim_sst_sd)
            + (1|operation), 
            data = dat, family = binomial,
            control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m6)
m6a <- update(m6, ~. -scale(mean_sst):scale(dhd))
droppy(m6a)
m6b <- update(m6a, ~. -scale(dist_core):scale(dhd))
droppy(m6b)
m6c <- update(m6b, ~.-scale(seas_clim_sst_sd))
droppy(m6c)
m6d <- update(m6c, ~.-scale(dhd))
droppy(m6d)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m6d)


### m7: (SST + dist_core + dhd)^2 + mo_anom ####

corvif(dat[,c("mean_sst", "dist_core", "dhd", "mo_anom")])

m7 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(dhd))^2 
            + scale(mo_anom)
            + (1|operation), 
            data = dat, family = binomial,
            control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m7)
m7a <- update(m7, ~.-scale(mean_sst):scale(dhd))
droppy(m7a)
m7c <- update(m7a, ~.-scale(dist_core):scale(dhd))
droppy(m7c)
m7d <- update(m7c, ~.-scale(mo_anom))
droppy(m7d)
m7e <- update(m7d, ~.-scale(dhd))
droppy(m7e)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m7e)


### m8: (SST + dist_core + dhd)^2 + se_anom ####
#to avoid convergence
corvif(dat[,c("mean_sst", "dist_core", "dhd", "se_anom")])

m8 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(dhd))^2 
            + scale(se_anom)
            + (1|operation), 
            data = dat, 
            family = binomial,
            control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m8)
m8a <- update(m8, ~.-scale(mean_sst):scale(dhd))
droppy(m8a)
m8b <- update(m8a, ~. -scale(se_anom))
droppy(m8b)
m8d <- update(m8b, ~.-scale(dist_core):scale(dhd))
droppy(m8d)
m8e <- update(m8d, ~.-scale(dhd))
droppy(m8e)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m8e)


### m9: (SST + dist_core + dhd)^2 + ssh ####

corvif(dat[,c("mean_sst", "dist_core", "mean_ssh_corrected", "dhd")])

m9 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(dhd))^2 
            + scale(mean_ssh_corrected)
            + (1|operation), 
            data = dat, 
            family = binomial,
            control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m9)
m9a <- update(m9, ~.-scale(mean_ssh_corrected))
droppy(m9a)
m9c <- update(m9a, ~.- scale(mean_sst):scale(dhd))
droppy(m9c)
m9d <- update(m9c, ~.-scale(dist_core):scale(dhd))
droppy(m9d)
m9e <- update(m9d, ~.-scale(dhd))
models_prev[nrow(models_prev)+1,] <- mselect_prev(m9e)



### m10: (SST + dist_core + mean_v)^2 + dhd ####

corvif(dat[,c("mean_sst", "dist_core", "mean_v", "dhd")])

m10 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(mean_v))^2 
             + scale(dhd)
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m10)
m10b <- update(m10, ~.-scale(mean_sst):scale(mean_v))
droppy(m10b)
m10c <- update(m10b, ~.-scale(dist_core):scale(mean_v))
droppy(m10c)
m10d <- update(m10c, ~. -scale(mean_v))
droppy(m10d)
m10e <- update(m10d, ~. -scale(dhd))
models_prev[nrow(models_prev)+1,] <- mselect_prev(m10e)


### m11: (SST + dist_core + dhd + mean_eke)^2 + mean_eke ####

corvif(dat[,c("mean_sst", "dist_core", "mean_eke", "dhd")])

m11 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(dhd) + 
                          scale(mean_eke))^2  + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m11)
m11a <- update(m11, ~.-scale(dist_core):scale(mean_eke))
droppy(m11a)
m11b <- update(m11a, ~.-scale(dist_core):scale(dhd))
droppy(m11b)
m11c <- update(m11b, ~.-scale(mean_sst):scale(dhd))
droppy(m11c)
m11e <- update(m11c, ~. -scale(dhd):scale(mean_eke))
droppy(m11e)
m11f <- update(m11e, ~. -scale(dhd))
droppy(m11f)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m11f)


### m12: (SST + dist_core + mean_mld1)^2 + dhd ####

corvif(dat[,c("mean_sst", "dist_core", "mean_mld1", "dhd")])

m12 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(mean_mld1))^2
             + scale(dhd)
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m12)
m12a <- update(m12, ~.-scale(mean_sst):scale(mean_mld1))
droppy(m12a)
m12c <- update(m12a, ~.-scale(dist_core):scale(mean_sst))
droppy(m12c)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m12c)


### m13: (SST + dist_core + sst_sd)^2 + dhd ####

corvif(dat[,c("mean_sst", "dist_core", "sd_sst", "dhd")])
m13 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) + scale(sd_sst))^2
             + scale(dhd)
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m13)
m13b <- update(m13, ~.-scale(mean_sst):scale(sd_sst))
droppy(m13b)
m13c <- update(m13b, ~.-scale(dist_core):scale(sd_sst))
droppy(m13c)
m13d <- update(m13c, ~.-scale(sd_sst))
droppy(m13d)
m13e <- update(m13d, ~.-scale(dhd))
models_prev[nrow(models_prev)+1,] <- mselect_prev(m13e)


### m14: (SST + dist_core + dhd  + sd_depth)^2 ####

corvif(dat[,c("mean_sst", "dist_core", "sd_depth", "dhd")])

m14 <- glmer(overall ~ (scale(mean_sst) + scale(dist_core) 
                        + scale(sd_depth) + scale(dhd))^2
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m14)
m14a <- update(m14, ~.-scale(dist_core):scale(dhd))
droppy(m14a)
m14b <- update(m14a, ~.-scale(mean_sst):scale(dhd))
droppy(m14b)
m14c <- update(m14b, ~.-scale(dhd):scale(sd_depth))
droppy(m14c)
m14d <- update(m14c, ~.-scale(dhd))
droppy(m14d)
m14e <- update(m14d, ~.-scale(dist_core):scale(sd_depth))
droppy(m14e)
m14f <- update(m14e, ~.-scale(mean_sst):scale(sd_depth))
droppy(m14f)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m14f)


### m15: sst + dist_core + dhd + sst_sd + mo_anom + mean_v 
#+ mean_eke + ssh  + sd_depth ####

corvif(dat[,c("mean_sst", "dist_core", "sd_sst", "mo_anom", "mean_v", 
              "mean_eke", "mean_ssh_corrected", 
              "sd_depth")])

m15 <- glmer(overall ~ scale(mean_sst) + scale(dist_core) 
             + scale(sd_sst) + scale(mo_anom) +
               scale(mean_v) + scale(mean_eke) + scale(mean_ssh_corrected) + 
               scale(sd_depth)
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m15)
m15a <- update(m15, ~.-scale(sd_sst))
droppy(m15a)
m15b <- update(m15a, ~. -scale(mo_anom))
droppy(m15b)
m15c <- update(m15b, ~. -scale(dist_core))
droppy(m15c)
m15e <- update(m15c, ~.-scale(mean_ssh_corrected))
droppy(m15e)
m15f <- update(m15e, ~.-scale(mean_eke))
droppy(m15f)
m15g <- update(m15f, ~. -scale(sd_depth))
droppy(m15g)
m15g <- update(m15f, ~. -scale(mean_v))
models_prev[nrow(models_prev)+1,] <- mselect_prev(m15g)



# Seamount models_prev ----------------------------------------------------------

#### m16: Seamount * sd_depth  ####

corvif(dat[,c("SeamountDistKM", "sd_depth")])

m16 <- glmer(overall ~ scale(SeamountDistKM) * scale(sd_depth) 
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m16)
m16a <- update(m16, ~.-scale(SeamountDistKM):scale(sd_depth))
droppy(m16a)
m16b <- update(m16a, ~.-scale(SeamountDistKM))
droppy(m16b)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m16b)


#### m17: (sd_depth + seamount + sst)^2 ####

corvif(dat[,c("SeamountDistKM", "sd_depth", "mean_sst")])
m17 <- glmer(overall ~ (scale(sd_depth) + scale(mean_sst) 
                        + scale(SeamountDistKM))^2
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m17)
m17a <- update(m17, ~.- scale(mean_sst):scale(SeamountDistKM))
droppy(m17a)
m17b <- update(m17a, ~.- scale(sd_depth):scale(SeamountDistKM))
droppy(m17b)
m17c <- update(m17b, ~.- scale(SeamountDistKM))
droppy(m17c)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m17c)


#### m18: (Seamount + sst + sd_depth + eke)^2 ####

corvif(dat[,c("mean_sst", "mean_eke", "sd_depth", "SeamountDistKM")])
m18 <- glmer(overall ~ (scale(SeamountDistKM) + scale(mean_sst) 
                        + scale(mean_eke) +
                          scale(sd_depth))^2
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m18)
m18a <- update(m18, ~.-scale(SeamountDistKM):scale(mean_eke))
droppy(m18a)
m18b <- update(m18a, ~. -scale(mean_sst):scale(mean_eke))
droppy(m18b)
m18d <- update(m18b, ~. -scale(SeamountDistKM):scale(mean_sst))
droppy(m18d)
m18e <- update(m18d, ~. - scale(sd_depth):scale(mean_eke))
droppy(m18e)
m18f <- update(m18e, ~. -scale(mean_eke))
droppy(m18f)
m18g <- update(m18f, ~. -scale(SeamountDistKM):scale(sd_depth))
droppy(m18g)
m18h <- update(m18g, ~. -scale(SeamountDistKM))
droppy(m18h)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m18h)


#### m19: (Seamount + sst + sst_sd + sd_depth)^2 ####

corvif(dat[,c("mean_sst", "sd_sst", "sd_depth", "SeamountDistKM")])
m19 <- glmer(overall ~ (scale(SeamountDistKM) + scale(mean_sst) 
                        + scale(sd_sst) + scale(sd_depth))^2
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m19)
m19a <- update(m19, ~. -scale(SeamountDistKM):scale(sd_sst))
droppy(m19a)
m19c <- update(m19a, ~.-scale(sd_sst):scale(sd_depth))
droppy(m19c)
m19d <- update(m19c, ~.-scale(mean_sst):scale(sd_sst))
droppy(m19d)
m19e <- update(m19d, ~.-scale(sd_sst))
droppy(m19e)
m19f <- update(m19e, ~.-scale(SeamountDistKM):scale(mean_sst))
droppy(m19f)
m19g <- update(m19f, ~.-scale(SeamountDistKM):scale(sd_depth))
droppy(m19g)
m19h <- update(m19g, ~.-scale(SeamountDistKM))
droppy(m19h)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m19h)


### m20: (Seamount + distcore + sd_depth + sst)^2 ####

corvif(dat[,c("mean_sst", "dist_core", "sd_depth", "SeamountDistKM")])
m20 <- glmer(overall ~ (scale(SeamountDistKM) + scale(mean_sst) + 
                          scale(dist_core)
                        + scale(sd_depth))^2
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m20)
m20a <- update(m20, ~.-scale(SeamountDistKM):scale(sd_depth))
droppy(m20a)
m20b <- update(m20a, ~.-scale(SeamountDistKM):scale(dist_core))
droppy(m20b)
m20d <- update(m20b, ~.-scale(SeamountDistKM):scale(mean_sst))
droppy(m20d)
m20e <- update(m20d, ~.-scale(SeamountDistKM))
droppy(m20e)
m20f <- update(m20e, ~.-scale(dist_core):scale(sd_depth))
droppy(m20f)
m20g <- update(m20f, ~.-scale(mean_sst):scale(sd_depth))
droppy(m20g)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m20g)


#### m21: Seamount + sst + eke + sst_sd + dist_core + sd_depth  ####

corvif(dat[,c("mean_sst", "dist_core", "sd_sst", "mean_eke", 
              "SeamountDistKM", "sd_depth")])
m21 <- glmer(overall ~ scale(SeamountDistKM) + scale(mean_sst) 
             + scale(dist_core) +
               scale(sd_sst) + scale(mean_eke) + scale(sd_depth)
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m21)
m21a <- update(m21, ~.-scale(sd_sst))
droppy(m21a)
m21b <- update(m21a, ~.-scale(SeamountDistKM))
droppy(m21b)
m21c <- update(m21b, ~.-scale(mean_eke))
droppy(m21c)
m21d <- update(m21c, ~.-scale(dist_core))
droppy(m21d)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m21d)


#### m22: (Depth + depth_sd + sst)^2 + mld  ####

corvif(dat[,c("mean_depth", "sd_depth", "mean_mld1", "mean_sst")])
m22 <- glmer(overall ~ (scale(mean_depth) + scale(sd_depth) 
                        + scale(mean_mld1))^2 + scale(mean_sst)
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m22)
m22a <- update(m22, ~.-scale(mean_mld1):scale(mean_depth))
droppy(m22a)
m22b <- update(m22a, ~.-scale(sd_depth):scale(mean_depth))
droppy(m22b)
m22c <- update(m22b, ~.-scale(mean_depth))
droppy(m22c)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m22c)


#### m23: (Depth, sd_depth, dhd, mean_sst)^2 ####

corvif(dat[,c("mean_depth", "sd_depth", "dhd", "mean_sst")])
m23 <- glmer(overall ~ (scale(mean_depth) + scale(sd_depth) + scale(dhd) +
                          scale(mean_sst))^2
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m23)
m23a <- update(m23, ~.-scale(mean_depth):scale(mean_sst))
droppy(m23a)
m23b <- update(m23a, ~.-scale(sd_depth):scale(dhd))
droppy(m23b)
m23c <- update(m23b, ~.-scale(mean_depth):scale(dhd))
droppy(m23c)
m23d <- update(m23c, ~.-scale(dhd):scale(mean_sst))
droppy(m23d)
m23e <- update(m23d, ~.-scale(dhd))
droppy(m23e)
m23f <- update(m23e, ~.-scale(mean_depth):scale(sd_depth))
droppy(m23f)
m23g <- update(m23f, ~.-scale(mean_depth))
droppy(m23g)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m23g)


# Oceanography models_prev -----------------------------------------------------

##### m24: SST + dist_core + mean_v + mld + eke + seamount + sd_depth ####

corvif(dat[,c("mean_sst", "dist_core", "mean_v", "mean_eke", "SeamountDistKM",
              "mean_mld1", "sd_depth")])

m24 <- glmer(overall ~ scale(SeamountDistKM) + scale(mean_sst) 
             + scale(dist_core) +
               scale(mean_v) + scale(mean_eke) + 
               scale(mean_mld1) + scale(sd_depth)
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m24)
m24a <- update(m24, ~.-scale(SeamountDistKM))
droppy(m24a)
m24b <- update(m24a, ~.-scale(dist_core))
droppy(m24b)
m24c <- update(m24b, ~.-scale(mean_mld1))
droppy(m24c)
m24d <- update(m24c, ~.-scale(mean_eke))
droppy(m24d)
m24e <- update(m24d, ~.-scale(sd_depth))
droppy(m24e)
m24f <- update(m24e, ~.-scale(mean_v))
models_prev[nrow(models_prev)+1,] <- mselect_prev(m24f)


#### m25: (SST + ssh + eke + mld1)^2 ####

corvif(dat[,c("mean_sst", "mean_eke", "mean_mld1", 
              "mean_ssh_corrected")])
m25 <- glmer(overall ~ (scale(mean_sst) + scale(mean_eke) 
                        + scale(mean_ssh_corrected))^2 + 
               scale(mean_mld1)
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m25)
m25a <- update(m25, ~.-scale(mean_mld1))
droppy(m25a)
m25b <- update(m25a, ~.-scale(mean_eke):scale(mean_ssh_corrected))
droppy(m25b)
m25c <- update(m25b, ~.-scale(mean_sst):scale(mean_eke))
droppy(m25c)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m25c)


#### m26: (SST + ssh + eke + dhd)^2 ####

corvif(dat[,c("mean_sst", "mean_eke", "dhd", "mean_ssh_corrected")])
m26 <- glmer(overall ~ (scale(mean_sst) + scale(mean_eke) 
                        + scale(mean_ssh_corrected) 
                        + scale(dhd))^2
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m26)
m26a <- update(m26, ~.-scale(mean_eke):scale(dhd))
droppy(m26a)
m26b <- update(m26a, ~.-scale(mean_sst):scale(dhd))
droppy(m26b)
m26c <- update(m26b, ~.-scale(mean_eke):scale(mean_ssh_corrected))
droppy(m26c)
m26d <- update(m26c, ~.-scale(mean_sst):scale(mean_eke))
droppy(m26d)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m26d)

# Productivity models -----------------------------------------------------

#### m27: (Chl-a + mld + ssh + eke)^2 ####

corvif(dat[,c("mean_chla", "mean_eke", "mean_mld1", 
              "mean_ssh_corrected")])
m27 <- glmer(overall ~ (scale(mean_chla) + scale(mean_mld1) + 
                          scale(mean_ssh_corrected) + scale(mean_eke))^2 
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m27)
m27a <- update(m27, ~.-scale(mean_mld1):scale(mean_ssh_corrected))
droppy(m27a)
m27b <- update(m27a, ~.-scale(mean_mld1):scale(mean_eke))
droppy(m27b)
m27c <- update(m27b, ~.-scale(mean_ssh_corrected):scale(mean_eke))
droppy(m27c)
m27d <- update(m27c, ~.-scale(mean_chla):scale(mean_eke))
droppy(m27d)
m27e <- update(m27d, ~.-scale(mean_eke))
droppy(m27e)
m27f <- update(m27e, ~.-scale(mean_chla):scale(mean_ssh_corrected))
droppy(m27f)
m27g <- update(m27f, ~.-scale(mean_ssh_corrected))
droppy(m27g)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m27g)



#### m28: (Seamount + chla + eke + depth_sd)^2 ####

corvif(dat[,c("mean_chla", "mean_mld1", "SeamountDistKM", "sd_depth")])
m28 <- glmer(overall ~ (scale(SeamountDistKM) + scale(mean_chla) 
                        + scale(mean_mld1) 
                        + scale(sd_depth))^2
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m28)
m28a <- update(m28, ~.-scale(SeamountDistKM):scale(mean_mld1))
droppy(m28a)
m28b <- update(m28a, ~.-scale(SeamountDistKM):scale(mean_chla))
droppy(m28b)
m28c <- update(m28b, ~.-scale(mean_chla):scale(sd_depth))
droppy(m28c)
m28d <- update(m28c, ~.-scale(SeamountDistKM):scale(sd_depth))
droppy(m28d)
m28e <- update(m28d, ~.-scale(SeamountDistKM))
droppy(m28e)
m28f <- update(m28e, ~.-scale(mean_chla):scale(mean_mld1))
droppy(m28f)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m28f)


# Non-distcore EAC models -------------------------------------------------

### m29: (SST + mean_ssh_corrected + mean_v)^2 #### 
corvif(dat[,c("mean_sst", "mean_v", "mean_ssh_corrected", "mean_eke")])

m29 <- glmer(overall ~ (scale(mean_sst) + scale(mean_v) 
                        + scale(mean_ssh_corrected) + scale(mean_eke))^2
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m29)
m29a <- update(m29, ~. -scale(mean_v):scale(mean_eke))
droppy(m29a)
m29b <- update(m29a, ~. -scale(mean_sst):scale(mean_v))
droppy(m29b)
m29c <- update(m29b, ~. -scale(mean_ssh_corrected):scale(mean_eke))
droppy(m29c)
m29d <- update(m29c, ~. -scale(mean_ssh_corrected):scale(mean_v))
droppy(m29d)
m29e <- update(m29d, ~. -scale(mean_sst):scale(mean_eke))
droppy(m29e)
m29f <- update(m29e, ~. -scale(mean_v))
droppy(m29f)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m29f)


### m30: (SST + v + dhd)^2 #### 

corvif(dat[,c("mean_sst", "mean_v", "dhd")])
m30 <- glmer(overall ~ (scale(mean_sst) + scale(mean_v) + scale(dhd))^2
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m30)
m30a <- update(m30, ~. -scale(mean_sst):scale(dhd))
droppy(m30a)
sjPlot::plot_model(m30a, type = "pred", terms = c("mean_sst", "mean_v"))
sjPlot::plot_model(m30a, type = "pred", terms = c("dhd", "mean_v"))
models_prev[nrow(models_prev)+1,] <- mselect_prev(m30a)


### m31:  (SST + v + dhd + mon_clim_sst_sd)^2 #### 

corvif(dat[,c("mean_sst", "mean_v", "dhd", "mon_clim_sst_sd")])
# note - even though sst and mon_clim_sst_sd are 0.706 pearson correlation,
# we keep it in model due to all GVIFs being below 3
m31 <- glmer(overall ~ (scale(mean_sst) + scale(mean_v) + 
                          scale(dhd) + scale(mon_clim_sst_sd))^2
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m31)
m31a <- update(m31, ~. -scale(mon_clim_sst_sd):scale(dhd))
droppy(m31a)
mm31b <- update(m31a, ~. - scale(mean_v):scale(mon_clim_sst_sd))
droppy(mm31b)
m31c <- update(mm31b, ~. - scale(mean_sst):scale(mon_clim_sst_sd))
droppy(m31c)
m31d <- update(m31c, ~. - scale(mean_sst):scale(dhd))
droppy(m31d)
m31e <- update(m31d, ~. - scale(mon_clim_sst_sd))
droppy(m31e)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m31e)


### m32: (SST + mean_v + mean_eke + mon_clim_sst_sd)^2  #### 

corvif(dat[,c("mean_sst", "mean_v", "mean_eke", "mon_clim_sst_sd")])
# note - even though sst and mon_clim_sst_sd are 0.706 pearson correlation,
# we keep it in model due to all GVIFs being below 3
m32 <- glmer(overall ~ (scale(mean_sst) + scale(mean_v) + 
                          scale(mean_eke) + 
                          scale(mon_clim_sst_sd))^2 
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m32)
m32a <- update(m32, ~. -scale(mean_v):scale(mon_clim_sst_sd))
droppy(m32a)
m32b <- update(m32a, ~. -scale(mean_v):scale(mean_eke))
droppy(m32b)
m32c <- update(m32b, ~. -scale(mean_sst):scale(mean_v))
droppy(m32c)
m32d <- update(m32c, ~. -scale(mean_sst):scale(mean_eke))
droppy(m32d)
m32e <- update(m32d, ~. -scale(mean_eke):scale(mon_clim_sst_sd))
droppy(m32e)
m32f <- update(m32e, ~. -scale(mean_eke))
droppy(m32f)
m32g <- update(m32f, ~. -scale(mean_v))
droppy(m32g)
m32h <- update(m32g, ~. -scale(mean_sst):scale(mon_clim_sst_sd))
droppy(m32h)
m32i <- update(m32g, ~. -scale(mon_clim_sst_sd))
droppy(m32i)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m32i)


### m33: (SST + v + mon_clim_sst_sd)^2 #### 

corvif(dat[,c("mean_sst", "mean_v", "mon_clim_sst_sd", "dhd")])
m33 <- glmer(overall ~ (scale(mean_sst) + scale(mean_v) + scale(mon_clim_sst_sd))^2
             + scale(dhd)
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))

droppy(m33)
m33a <- update(m33, ~. -scale(mean_v):scale(mon_clim_sst_sd))
droppy(m33a)
m33b <- update(m33a, ~. -scale(mean_sst):scale(mon_clim_sst_sd))
droppy(m33b)
m33c <- update(m33b, ~. -scale(mon_clim_sst_sd))
droppy(m33c)
m33d <- update(m33c, ~. -scale(dhd))
droppy(m33d)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m33d)


### m34: (SST + ssh_corrected + mon_clim_sst_sd)^2 #### 

corvif(dat[,c("mean_sst", "mean_ssh_corrected", "mon_clim_sst_sd", "dhd")])
m34 <- glmer(overall ~ (scale(mean_sst) + scale(mean_ssh_corrected) 
                        + scale(mon_clim_sst_sd) + scale(dhd))^2
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))

droppy(m34)
m34a <- update(m34, ~. -scale(mean_ssh_corrected):scale(mon_clim_sst_sd))
droppy(m34a)
m34b <- update(m34a, ~. -scale(mean_sst):scale(dhd))
droppy(m34b)
m34c <- update(m34b, ~. -scale(mon_clim_sst_sd):scale(dhd))
droppy(m34c)
m34d <- update(m34c, ~. -scale(mon_clim_sst_sd):scale(mean_sst))
droppy(m34d)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m34d)


### m35: (SST + ssh_corrected + mon_clim_sst_sd)^2 #### 

corvif(dat[,c("mean_sst", "mean_ssh_corrected", "mon_clim_sst_sd", "dhd")])
m35 <- glmer(overall ~ (scale(mean_sst) + scale(mean_ssh_corrected))^2 
             + scale(mon_clim_sst_sd) + scale(dhd)
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m35)
m35a <- update(m35, ~. -scale(dhd))
droppy(m35a)
m35b <- update(m35a, ~. -scale(mon_clim_sst_sd))
droppy(m35b)
m35c <- update(m35b, ~. -scale(mean_sst):scale(mean_ssh_corrected))
droppy(m35c)
m35d <- update(m35c, ~. -scale(mean_ssh_corrected))
droppy(m35d)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m35d)


### m36: (SST + mean_v + dhd)^2 + seas_clim_sst_sd #### 
corvif(dat[,c("mean_sst", "mean_v", "dhd", "seas_clim_sst_sd")])

m36 <- glmer(overall ~ (scale(mean_sst) + scale(mean_v) + scale(dhd)
                        +scale(seas_clim_sst_sd))^2 
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m36)
m36a <- update(m36, ~. -scale(mean_sst):scale(dhd))
droppy(m36a)
m36b <- update(m36a, ~. -scale(mean_v):scale(seas_clim_sst_sd))
droppy(m36b)
m36c <- update(m36b, ~.-scale(mean_sst):scale(seas_clim_sst_sd))
droppy(m36c)
m36d <- update(m36c, ~.-scale(dhd):scale(seas_clim_sst_sd))
droppy(m36d)
m36e <- update(m36d, ~.-scale(seas_clim_sst_sd))
droppy(m36e)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m36e)


### m37: (SST + mean_v + mo_anom)^2 + dhd ####

corvif(dat[,c("mean_sst", "mean_v", "dhd", "mo_anom")])

m37 <- glmer(overall ~ scale(mean_sst) * scale(mean_v) + scale(mo_anom) * 
               scale(mean_sst) + scale(mo_anom) * scale(dhd)
             + (1|operation), 
             data = dat, family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
drop1(m37, test = "Chi")
m37a <- update(m37, ~.-scale(dhd):scale(mo_anom))
droppy(m37a)
m37b <- update(m37a, ~.-scale(dhd))
droppy(m37b)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m37b)
sjPlot::plot_model(m37b, type = "pred", terms = c("mean_sst", "mean_v"))


### m38: (SST + mean_v + se_anom)^2 + dhd ####

corvif(dat[,c("mean_sst", "mean_v", "dhd", "se_anom")])
m38 <- glmer(overall ~ scale(mean_sst) * scale(mean_v) + scale(se_anom) * 
               scale(mean_sst) + scale(se_anom) * scale(dhd)
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m38)
m38a <- update(m38, ~.-scale(se_anom):scale(dhd))
droppy(m38a)
m38b <- update(m38a, ~. -scale(dhd))
droppy(m38b)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m38b)


### m39: (SST + mean_v + eke + ssh)^2 + dhd ####

corvif(dat[,c("mean_sst", "mean_v", "mean_eke", "mean_ssh_corrected")])
m39 <- glmer(overall ~ (scale(mean_sst) + scale(mean_v) + 
                          scale(mean_ssh_corrected) + scale(mean_eke))^2
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))
droppy(m39)
m39a <- update(m39, ~.-scale(mean_sst):scale(mean_v))
droppy(m39a)
m39b <- update(m39a, ~.-scale(mean_v):scale(mean_eke))
droppy(m39b)
m39c <- update(m39b, ~.-scale(mean_ssh_corrected):scale(mean_eke))
droppy(m39c)
m39d <- update(m39c, ~.-scale(mean_v):scale(mean_ssh_corrected))
droppy(m39d)
m39e <- update(m39d, ~.-scale(mean_sst):scale(mean_eke))
droppy(m39e)
m39f <- update(m39e, ~.-scale(mean_v))
droppy(m39f)
models_prev[nrow(models_prev)+1,] <- mselect_prev(m39f)


### m40: (SST * mean_v) + (DHD * SST) + (DHD * mo_anom) + mon_clim_sst_sd #####

corvif(dat[,c("mean_sst", "mean_v", "dhd", "mo_anom", "mon_clim_sst_sd")])
m40 <- glmer(overall ~ scale(mean_sst) * scale(mean_v) +
               scale(dhd) * scale(mean_sst) +
               scale(dhd) * scale(mo_anom) +
               scale(mo_anom) * scale(mean_sst) + 
               scale(mon_clim_sst_sd)
             + (1|operation), 
             data = dat, 
             family = binomial,
             control = glmerControl(optCtrl = list(maxfun = 100000)))

droppy(m40)
m40a <- update(m40, ~.-scale(mo_anom):scale(dhd))
droppy(m40a)
m40b <- update(m40a, ~.-scale(dhd):scale(mean_sst))
drop1(m40b, test = "Chi")
m40c <- update(m40b, ~.-scale(dhd))
drop1(m40c, test = 'Chi')
sjPlot::plot_model(m40c, type = "pred", terms = c("mean_sst", "mean_v"))
sjPlot::plot_model(m40c, type = "pred", terms = c("mean_sst", "mo_anom"))
models_prev[nrow(models_prev)+1,] <- mselect_prev(m40c)



# Month models ------------------------------------------------------------

# What about effect of month? 
# Note - month cannot be modelled with SST due to collinearity
corvif(dat[,c("mean_sst", "Month")])

# Adding month as a variable with chla, seamount, sd_sst etc results in
# non-convergence, because we don't have enough data per month

### m41: Month ##### - doesn't converge
m41 <- glmer(overall ~ Month
             + (1|operation), 
             data = dat, family = binomial, 
             control = glmerControl(optCtrl = list(maxfun = 100000)))
#e.g.,
glmer(overall ~ Month + scale(sd_depth) + (1|operation), 
      data = dat, family = binomial, 
      control = glmerControl(optCtrl = list(maxfun = 100000)))
# convergence warning
glmer(overall ~ Month + scale(SeamountDistKM) + (1|operation),
      data = dat, family = binomial, 
      control = glmerControl(optCtrl = list(maxfun = 100000)))
glmer(overall ~ Month + scale(mean_chla) + (1|operation),
      data = dat, family = binomial, 
      control = glmerControl(optCtrl = list(maxfun = 100000)))
#convergence warning
glmer(overall ~ Month + scale(dhd) + (1|operation),
      data = dat, family = binomial, 
      control = glmerControl(optCtrl = list(maxfun = 100000)))
#convergence warning
glmer(overall ~ Month + scale(mon_clim_sst_sd) + (1|operation),
      data = dat, family = binomial, 
      control = glmerControl(optCtrl = list(maxfun = 100000)))
#convergence warning
glmer(overall ~ Month + scale(mean_sst) * scale(dist_core) + (1|operation),
      data = dat, family = binomial, 
      control = glmerControl(optCtrl = list(maxfun = 100000)))
#singular fit - collinearity


# AIC TABLE ---------------------------------------------------------------

aics <- models_prev[order(models_prev$AIC),]
aics <- aics[-nrow(aics),]
aics <- aics[-which(aics$AIC %>% duplicated()),] #remove duplicate models due to model selection
aics[,1:(ncol(aics)-3) ] %>% head()

#### Top 5 models  ####
summary(m40c) #1519
# sst * v
# sst * mo_anom
# mon_clim_sst_sd
summary(m37b) #1522
# SST * v
# sst * mo_anom
summary(m38b) #1526
## SST * v
# sst * se_anom
summary(m30a) #1526
# SST * v
# v * dhd
summary(m5b) #1527
#sst * dist_core
#dhd
#mon_clim_sst_sd


aics
aics$deltaaic <- NA

for (i in 1:nrow(aics)) {
  aics$deltaaic[i] <- bbmle::AICtab(aics[aics$AIC == min(aics$AIC),]$model %>% get,
                                    aics$model[i] %>% get)$dAIC[2]
}

aics %>% nrow
saveRDS(aics, "rds/prev_aic_table.RDS")


