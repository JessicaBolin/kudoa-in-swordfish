# Merge environmental data to create an environ. dataframe for modelling
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)


# Merge ACCESS-S RA and bathymetry ---------------------------------------------

dailysurf <- readRDS("do_mo_seas_sst_anomalies.RDS") 
bathy <- readRDS("overall_BATHY.RDS")

dailysurf[dailysurf$company == "REDACTED", "Vessel"] <- NA # REDACTED
dat1 <- merge(dailysurf, bathy, by = c("Trip", "Vessel", "company"))
nrow(dat1) # 68
names(dat1)[grep("BATHY", names(dat1))] <- c("mean_depth", "sd_depth")

# Merge EAC dist core -----------------------------------------------------

eac <- readRDS("distances_to_core_best_method.RDS")
dat3 <- merge(dat1, eac, by = c("Trip", "company", "Vessel"))
nrow(dat3) # 68

# Merge distance to seamount ----------------------------------------------

feature <- readRDS("DistToSeamount_Domain_ALL.RDS")
feature[feature$Company == "REDACTED",]$Vessel <- NA #REDACTED
envframe <- merge(feature, dat3)
envframe <- envframe[order(envframe$Trip),]

# Merge DHDs/HR ---------------------------------------------------

dhd <- readRDS("datasst_DHDs_heatingrate_NEW.RDS")
dhd[dhd$company == "REDACTED",]$Vessel <- NA #REDACTED
envframe1 <- merge(envframe, dhd)

# Merge chl-a -------------------------------------------------------------

chla <- readRDS("env_surface_do_CHLA.RDS")
chla[chla$company == "REDACTED",]$Vessel <- NA #REDACTED
envframe2 <- merge(envframe1, chla)
nrow(envframe2) #68

# Merge meanlon -----------------------------------------------------------

fishing <- readRDS("spatialmetadata_alltrips.RDS") 
fishing <- fishing %>% 
  filter(Trip !=2)
nrow(fishing)

#Compute 'average' lat lon for each vessel/trip, and categorise
mean_coords <- fishing %>% 
  group_by(Trip, Vessel) %>% 
  summarise(meanlon = mean(Longitude), meanlat = mean(Latitude), 
            Company = company %>% unique) %>% 
  data.frame()

#Trips 1, 4, 6 and 7 are Company 2 with multiple boats. 
#Have the average lat lon for these trips
#Trip 1
mean_coords[mean_coords$Trip == 1,]$meanlat <- mean(mean_coords[mean_coords$Trip == 1,]$meanlat)
mean_coords[mean_coords$Trip == 1,]$meanlon <- mean(mean_coords[mean_coords$Trip == 1,]$meanlon)
#Trip 4
mean_coords[mean_coords$Trip == 4,]$meanlat <- mean(mean_coords[mean_coords$Trip == 4,]$meanlat)
mean_coords[mean_coords$Trip == 4,]$meanlon <- mean(mean_coords[mean_coords$Trip == 4,]$meanlon)
#Trip 6
mean_coords[mean_coords$Trip == 6,]$meanlat <- mean(mean_coords[mean_coords$Trip == 6,]$meanlat)
mean_coords[mean_coords$Trip == 6,]$meanlon <- mean(mean_coords[mean_coords$Trip == 6,]$meanlon)
#Trip 7
mean_coords[mean_coords$Trip == 7,]$meanlat <- mean(mean_coords[mean_coords$Trip == 7,]$meanlat)
mean_coords[mean_coords$Trip == 7,]$meanlon <- mean(mean_coords[mean_coords$Trip == 7,]$meanlon)

mean_coords1 <- mean_coords[-c(1,5,8,10,11,12),] 
mean_coords1[mean_coords1$Company == "REDACTED" & mean_coords1$Trip == 1,]$Vessel <- NA #REDACTED
mean_coords1[mean_coords1$Company == "REDACTED" & mean_coords1$Trip == 3,]$Vessel <- NA #REDACTED
mean_coords1[mean_coords1$Company == "REDACTED" & mean_coords1$Trip == 4,]$Vessel <- NA #REDACTED
mean_coords1[mean_coords1$Company == "REDACTED" & mean_coords1$Trip == 5,]$Vessel <- NA #REDACTED
mean_coords1[mean_coords1$Company == "REDACTED" & mean_coords1$Trip == 6,]$Vessel <- NA #REDACTED
mean_coords1[mean_coords1$Company == "REDACTED" & mean_coords1$Trip == 7,]$Vessel <- NA #REDACTED
mean_coords1[mean_coords1$Company == "REDACTED" & mean_coords1$Trip == 13,]$Vessel <- NA #REDACTED
mean_coords1[mean_coords1$Company == "REDACTED" & mean_coords1$Trip == 14,]$Vessel <- NA #REDACTED

nrow(mean_coords1) #68
envframe3 <- left_join(envframe2, mean_coords1)
nrow(envframe3) #68
is.na(envframe3$meanlat) #no problem NAs

# Fix ---------------------------------------------------------------------

env <- envframe3[order(envframe3$Trip),] #order by trip
new <- env[!env$Trip == 2,] #nuisance trip
nrow(new) #68

saveRDS(new, "_envframe_surfacevars_withouttimelags.RDS")
