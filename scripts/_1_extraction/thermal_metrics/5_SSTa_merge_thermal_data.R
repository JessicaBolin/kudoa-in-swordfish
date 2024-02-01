# Create a SSTa and merge all thermal metric data
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)

# Dependencies and data ---------------------------------------------------

library(tidyverse)
library(lubridate)

daily <- readRDS("cleaned_data/env/dat_sst_clim_latlons_daily_1981-2018.RDS")
monthly <- readRDS("cleaned_data/dat_sst_clim_latlons_monthly_1981-2018.RDS")
seasonal <- readRDS("cleaned_data/dat_sst_clim_latlons_seasonal_1981-2018.RDS")
env <- readRDS("cleaned_data/env/dataenv_surface_do.RDS")
# all not available in repo

# Merge all climatology data -----------------------------------------------

m1 <- merge(env, monthly)
dat <- merge(m1, daily)
dat <- merge(dat, seasonal)

# Calculate anomalies ------------------------------------------------------

#SSTa is observed SST for that day, minus the do/mo climatology

dat$mo_anom <- NA
dat$do_anom <- NA
dat$se_anom <- NA

#subtract daily clim from observed daily SST
for (i in 1:nrow(dat)) { #for each trip
  dat$do_anom[i] <- dat$mean_sst[i] - dat$sst_clim_do[i] 
}

#subtract monthly clim from observed daily SST
for (i in 1:nrow(dat)) { #for each trip
  dat$mo_anom[i] <- dat$mean_sst[i] - dat$mon_clim_sst[i] 
}

#subtract seasonal clim from observed daily SST
for (i in 1:nrow(dat)) { #for each trip
  dat$se_anom[i] <- dat$mean_sst[i] - dat$seas_clim_sst[i] 
}

saveRDS(dat, "cleaned_data/do_mo_seas_sst_anomalies.RDS")





