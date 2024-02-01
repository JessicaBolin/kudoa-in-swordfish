# Data exploration - intensity dataset
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)


# Dependencies and data ---------------------------------------------------

intens <- readRDS("intens_dirty.RDS")

library(tidyverse)
library(lattice) 
source("functions/corvif.R") #Zuur et al 2009
source("functions/mymultipanelggp2.R") #Zuur et al 2009
source("functions/mydotplotggp2.R") #Zuur et al 2009
source("functions/showsmooth.R")
source("functions/Mybwplot.R")

# Temporal freqency of infection ------------------------------------------

# Plot sampling frequency 

hemm <- intens %>% 
  group_by(Year, Month) %>% 
  summarise(median = median(TotalFish, na.rm=T) %>% round,
            fish = n())

#Spores per fish
hemm %>% 
  ggplot(aes(Year, Month)) +
  geom_tile(aes(fill = median)) +
  scale_y_continuous(limits = c(0.5,12.5), 
                     breaks=seq(1,12.5,1)) +
  scale_fill_continuous(name = "Spores/fish", 
                        type = "viridis") +
  geom_text(aes(label=median)) +
  ggtitle("Median spore count per fish") 

#Sampling freq
hemm %>% 
  ggplot(aes(Year, Month)) +
  geom_tile(aes(fill = fish)) +
  scale_y_continuous(limits = c(0.5,12.5), 
                     breaks=seq(1,12.5,1)) +
  scale_fill_continuous(name = "Sampling Freq", 
                        type = "viridis") +
  geom_text(aes(label=fish)) +
  ggtitle("Sampling Freq (Hemo)") 


# Response checks ---------------------------------------------------------

nrow(intens) #997
sum(intens$overall == 0) / nrow(intens) 
intens$operation %>% unique %>% length  #61

# Density plot of response
ggplot(data = intens, aes(x = TotalFish)) +
  geom_histogram(mapping = aes(y=..density..), 
                 color = "black", 
                 fill = "white",
                 bins = 20) +
  geom_density(alpha = .2, fill="#FF6666") +
  scale_y_continuous(name="Density") +
  scale_x_continuous(name = "Spore count", 
                     limits = c(0, 300000))

# Variance-mean relationship of response
dg <- group_by(intens, operation) %>% 
  summarise(m = mean(TotalFish), v = var(TotalFish)) 
ggplot(dg, aes(m, v)) + 
  geom_smooth(method = "lm", formula = y ~ x - 1, se = F, color = "blue") +
  geom_smooth(method = "lm", 
              formula = y ~ I(x^2) + offset(x) - 1, colour = "red", se = F) +
  geom_point() +
  scale_y_continuous(name="Variance") +
  xlab("Mean spore count")

# Number of sword per operation
obs <- intens %>% 
  group_by(operation) %>% 
  count %>% data.frame 
obs$operation <- 1:nrow(obs)
obs$operation <- as.factor(obs$operation)
obs <- obs[order(obs$n),]
ggplot(obs) +
  geom_point(mapping = aes(x = n, y = operation)) +
  geom_vline(xintercept = 10) +
  xlab("number of observations")


# Spores per season -------------------------------------------------------

#Raincloud plot to visualise distribution
subset(intens, TotalFish < 100000) %>% 
  ggplot(aes(x = Season, y = TotalFish, fill = Season)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -.15,
    .width = 0, #remove slab interval
    point_colour = NA) +
  geom_boxplot(
    width = 0.12,
    outlier.color = "black",
    alpha = 0.5) +
  ggdist::stat_dots(
    side = "left",
    justification = 1.1) +
  tidyquant::scale_fill_tq(theme = "green") +
  tidyquant::theme_tq() +
  labs(y = "Spores/gram of swordfish muscle",
       x = "Season",
       fill = "Season") +
  coord_flip() 


# Evidence for potential spatial correlation ---------------------------------------

# Variograms
mydata2 <- data.frame(intens$TotalFish, 
                      intens$meanlon, 
                      intens$meanlat)
names(mydata2) <- c("z", "x", "y")
sp::coordinates(mydata2) =~x+y
Var2 <- gstat::variogram(z ~ 1, mydata2, cutoff = 4)
Var2a <- gstat::variogram(z ~ 1, mydata2, cutoff = 4,
                          alpha = c(0, 45, 90, 145))

ggplot(data = Var2a, aes(x = dist, y = gamma)) + 
  geom_point() + 
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs"),
              colour = "black") +
  ylim(0,3500000000) + 
  xlab("Distance (˚)") + 
  ylab("Sample variogram") 

ggplot(data = Var2a,
       aes(x = dist, y = gamma)) + 
  geom_point() +
  geom_smooth(
    method = "gam", formula = y ~ s(x, bs = "cs")) +
  facet_wrap(~dir.hor)+
  xlab("Distance") + 
  ylab("Semi-variogram") + 
  theme(text = element_text(size=15))

# Correlograms - takes ~5 min
CORRE <- ncf::spline.correlog(x = intens$meanlon,
                              y = intens$meanlat,
                              z = intens$TotalFish,
                              xmax = 4)
plot(CORRE, ylim = c(-0.2, 0.2), xlab = "Distance (˚)")


# Collinearity amongst predictors -----------------------------------------

#chosen a priori
myvar <- c("SeamountDistKM", "mean_sst", "sd_sst", "mean_hc300",
           "sd_hc300", "mean_mld1", "mean_sss", "mean_ssh_corrected",
           "mean_v", "mean_eke", "mean_speed", "newlat",
           "mon_clim_sst", "mon_clim_sst_sd", "seas_clim_sst", "seas_clim_sst_sd",
           "mean_depth", "dist_core", "dhd", "heat_rate",
           "mo_anom", "se_anom", "mean_chla", "sd_depth")

corro <- cor(intens[,myvar]) %>% data.frame
which(corro$mean_sst > 0.7) -> removethis
which(corro$mean_sst < -0.7) -> removethis2
names(corro)[removethis2]; names(corro)[removethis]
#[1] "mean_sss"  "mean_chla"
#[1] "mean_sst"        "mean_hc300"      "newlat"          "mon_clim_sst"   
#[6] "seas_clim_sst"  

myvar2 <- c("SeamountDistKM", "mean_sst", "sd_sst", "mean_mld1",
            "mean_ssh_corrected", "mean_v", "mean_eke", "mean_speed",
            "mean_depth", "dist_core", "dhd", "heat_rate",
            "mo_anom", "sd_depth", "se_anom", "mon_clim_sst_sd",
            "seas_clim_sst_sd", "mean_chla")

corro <- cor(intens[,myvar2]) %>% data.frame
corro["SeamountDistKM", "mean_depth"] #[1] 0.6751101
corro["mean_eke", "mean_speed"] #[1] 0.9250988
corro["dhd", "heat_rate"] #[1] 0.9714501
corro["mean_mld1", "mon_clim_sst_sd"] #[1] -0.673944
corro["mean_chla", "mon_clim_sst_sd"] #[1] -0.6312078

#NOTE - VIFs checked prior to modelling

intens$Month <- as.factor(intens$Month)
Mybwplot(intens, c("mean_sst"), "Month") # Very collinear

# Outliers ---------------------------------------------------------

MyDotplot.ggp2(intens, myvar2) 
remove <- intens[which(intens$mean_eke > 0.11),]
intens <- anti_join(intens, remove)
remove <- intens[which(intens$heat_rate > 1.2),]
intens <- anti_join(intens, remove)
MyDotplot.ggp2(intens, myvar2) 

# Visualise relationships -------------------------------------------------

showsmooth(Z = intens, 
           varx = myvar2, 
           vary = "TotalFish", 
           ylab = "Response variable (log10)",
           addSmoother = TRUE,
           addRegressionLine = FALSE,
           addHorizontalLine = FALSE)

# Save df -----------------------------------------------------------------

saveRDS(intens, "intense_clean.RDS") 


