# Create a SEASONAL SST climatology from ACCESS-S RA SST daily 
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)

# Create seasonal by getting averages of each 3 month SST clim period

# Dependencies and data ---------------------------------------------------

library(terra)
library(tidyverse)

rr <- rast("cleaned_data/climatologies/month_climatology_ETBF_1981-2018_ACCESS-S-RA.grd")
dat <- readRDS("spatialmetadata_alltrips.RDS") #not available in repo
dat <- dat[, c("Latitude", "Longitude", "Date", "Trip", "Vessel", "company")]

# Seasonal averages -------------------------------------------------------

summer <- rr[[c(1,2,12)]] 
summer_u <- mean(summer)
summer_sd <- stdev(summer)
summ <- c(summer_u,summer_sd)

autumn <- rr[[c(3,4,5)]]
autumn_u <- mean(autumn)
autumn_sd <- stdev(autumn)
aut <- c(autumn_u, autumn_sd)

winter <- rr[[c(6,7,8)]]
winter_u <- mean(winter)
winter_sd <- stdev(winter)
win <- c(winter_u, winter_sd)

spring <- rr[[c(9,10,11)]]
spring_u <- mean(spring)
spring_sd <- stdev(spring)
spr <- c(spring_u, spring_sd)

par(mfrow=c(2,2))
plot(summer_u, main = "summer mean")
plot(autumn_u, main = "autumn mean")
plot(winter_u, main = "winter mean")
plot(spring_u,  main = "spring mean")

plot(summer_sd, main = "summer sd")
plot(autumn_sd, main = "autumn sd")
plot(winter_sd, main = "winter sd")
plot(spring_sd,  main = "spring sd")


# Save rasters ----------------------------------------------------------------

writeRaster(summ, 
            "cleaned_data/climatologies/seasonal_climatology_ETBF_1981-2018_ACCESS-S-RA_SUMMER.grd")
writeRaster(aut, 
            "cleaned_data/climatologies/seasonal_climatology_ETBF_1981-2018_ACCESS-S-RA_AUTUMN.grd")
writeRaster(win, 
            "cleaned_data/climatologies/seasonal_climatology_ETBF_1981-2018_ACCESS-S-RA_WINTER.grd")
writeRaster(spr, 
            "cleaned_data/climatologies/seasonal_climatology_ETBF_1981-2018_ACCESS-S-RA_SPRING.grd")



# Extract assoc seasonal clim from each lat lon -------------------------------

df_list <- list()
diry <- list.files("cleaned_data/climatologies/")

for (i in 1:nrow(dat)) { #for each latlon
  
  # Metadata corresponding to shot of interest
  seas = NULL
  datt <- dat[i,]
  pointy <- data.frame(x = datt["Longitude"], y = datt["Latitude"])
  datey <- datt$Date
  df <- data.frame(date = datey, 
                   seas_clim = NA, 
                   seas_clim_sd = NA,
                   lat = datt["Latitude"], 
                   lon = datt["Longitude"],
                   trip = datt["Trip"], 
                   vessel = datt["Vessel"], 
                   company = datt["company"])
  
  # Get associated raster
  if (month(datey) == 1) {seas = "SUMMER"}
  if (month(datey) == 2) {seas = "SUMMER"}
  if (month(datey) == 3) {seas = "AUTUMN"}
  if (month(datey) == 4) {seas = "AUTUMN"}
  if (month(datey) == 5) {seas = "AUTUMN"}
  if (month(datey) == 6) {seas = "WINTER"}
  if (month(datey) == 7) {seas = "WINTER"}
  if (month(datey) == 8) {seas = "WINTER"}
  if (month(datey) == 9) {seas = "SPRING"}
  if (month(datey) == 10) {seas = "SPRING"}
  if (month(datey) == 11) {seas = "SPRING"}
  if (month(datey) == 12) {seas = "SUMMER"}
  
  test1 <- diry[grep(seas, diry)] 
  filey <- test1[grep(".grd$", test1)]
  rasty <- rast(paste0("cleaned_data/climatologies/", filey))
  
  # Extract values 
  val_u <- terra::extract(rasty[["mean"]], pointy)
  df[, "seas_clim"] <- as.numeric(val_u[2])
  val_sd <- terra::extract(rasty[["std"]], pointy)
  df[, "seas_clim_sd"] <- as.numeric(val_sd[2])
  df_list[[i]] <- df 
  print(paste0(i, "/", nrow(dat)))
  
}

nearly_all <- do.call("rbind", df_list)
saveRDS(nearly_all, "cleaned_data/ssts_latlons_seasonal_1981-2018.RDS")
# Not available in repo


# Calculate average seasonal clim for each trip ------------------------------------

df2 <- nearly_all %>% 
  group_by(Trip, Vessel, company) %>% 
  summarise(seas_clim_sst = mean(seas_clim),
            seas_clim_sst_sd = mean(seas_clim_sd)) %>% 
  as.data.frame()


# Average of Company 2 trips ----------------------------------------------------

problems <- c(1,4,6,7)

for (i in problems) { 
  df2[df2$company == "REDACTED" &  #REDACTED
        df2$Trip ==i, ]$seas_clim_sst <- df2[df2$company == "REDACTED" &  #REDACTED
                                               df2$Trip ==i, ]$seas_clim_sst %>% mean
  df2[df2$company == "REDACTED" &  #REDACTED
        df2$Trip ==i, ]$seas_clim_sst_sd <- df2[df2$company == "REDACTED" &  #REDACTED
                                                  df2$Trip ==i, ]$seas_clim_sst_sd %>% mean
}

df2[ -c(1,4,7,9,10,11), ] -> df4 #manually remove duplicate rows
df4[df4$company == "REDACTED" & df4$Trip == 1,]$Vessel <-"REDACTED" #REDACTED

# Save to wd() ------------------------------------------------------------

saveRDS(df4, "cleaned_data/dat_sst_clim_latlons_seasonal_1981-2018.RDS")
# not available in repo
