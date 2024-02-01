# Data exploration - prevalence dataset
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)


# Dependencies and data ---------------------------------------------------

library(tidyverse)
library(lattice) 
source("functions/corvif.R") #Zuur et al 2009
source("functions/mymultipanelggp2.R") #Zuur et al 2009
source("functions/mydotplotggp2.R") #Zuur et al 2009

dat3 <- readRDS("prev_dirty.RDS") #not available in repo
dat4 <- dat3

shots <- readRDS("fishingduration_alltrips copy.RDS")
# above not available in repo



# Temporal freqency of infection ------------------------------------------

# Sampling frequency tile plot
tes <- dat4 %>% 
  distinct(Trip, Fish, .keep_all = T) #subset distinct rows

gar <- tes %>% 
  group_by(Month, Year, Season) %>% 
  dplyr::count() %>% 
  data.frame()

jj <- tes %>% 
  group_by(Month, overall) %>% 
  dplyr::count() %>% 
  data.frame()
sum(jj$n) #1653

mop <- gar %>% 
  ggplot(aes(Year, Month %>% as.integer)) +
  geom_tile(aes(fill=n)) +
  scale_y_continuous(limits = c(0.5,12.5), breaks=seq(1,12.5,1)) +
  ggtitle("(a) Sampling frequency of swordfish") +
  scale_fill_continuous(name = "Sampling Freq", 
                        type = "viridis", limits = c(0, 200)) +
  geom_text(aes(label=n))

mop #display sampling freq tile plot

# Reproduce the above, but for infection prevalence
gg <- tes %>% 
  group_by(Month, Season, Year, overall) %>% 
  summarise(fish = length(Fish)) %>% 
  data.frame() %>% 
  arrange(fish, Year) %>% 
  dplyr::select("overall", "fish", "Month", "Year")

nn <- gg %>% 
  group_by(Year, Month) %>% 
  summarise(total = sum(fish)) %>% 
  data.frame() #for each year/month combo, calculate number of fish sampled

ones <- subset(gg, overall == 1) 
jj <- merge(ones, nn) #merge infected with total sampled

perrr <-  jj %>% 
  group_by(Year, Month) %>% 
  summarise(percent = round(fish/total, 3)) %>%  #calculate % infected per month
  data.frame() 

perrr <- perrr[,c(3,2,1)]
d <- data.frame(month = c(10:12, 1:12, 1:12, 1:2), 
                year = c(rep(2019,3), rep(2020, 12), 
                         rep(2021, 12), rep(2022,2)),
                percent = c(
                  subset(perrr, Year == 2019 & Month == 10)$percent, 
                  subset(perrr, Year == 2019 & Month == 11)$percent, 
                  subset(perrr, Year == 2019 & Month == 12)$percent, 
                  NA, 
                  subset(perrr, Year == 2020 & Month == 2)$percent, 
                  subset(perrr, Year == 2020 & Month == 3)$percent, 
                  NA, 
                  subset(perrr, Year == 2020 & Month == 5)$percent, 
                  subset(perrr, Year == 2020 & Month == 6)$percent, 
                  NA, 
                  subset(perrr, Year == 2020 & Month == 8)$percent, 
                  subset(perrr, Year == 2020 & Month == 9)$percent, 
                  subset(perrr, Year == 2020 & Month == 10)$percent, 
                  NA, 
                  subset(perrr, Year == 2020 & Month == 12)$percent, 
                  subset(perrr, Year == 2021 & Month == 1)$percent, 
                  subset(perrr, Year == 2021 & Month == 2)$percent, 
                  subset(perrr, Year == 2021 & Month == 3)$percent, 
                  subset(perrr, Year == 2021 & Month == 4)$percent, 
                  subset(perrr, Year == 2021 & Month == 5)$percent, 
                  subset(perrr, Year == 2021 & Month == 6)$percent, 
                  subset(perrr, Year == 2021 & Month == 7)$percent, 
                  subset(perrr, Year == 2021 & Month == 8)$percent, 
                  subset(perrr, Year == 2021 & Month == 9)$percent, 
                  subset(perrr, Year == 2021 & Month == 10)$percent, 
                  subset(perrr, Year == 2021 & Month == 11)$percent, 
                  subset(perrr, Year == 2021 & Month == 12)$percent, 
                  NA, 
                  subset(perrr, Year == 2022 & Month == 2)$percent
                )
)

d$percent <- d$percent * 100
hb <- d %>% 
  ggplot(aes(year, month)) +
  geom_tile(aes(fill=percent)) +
  scale_y_continuous(limits = c(0.5,12.5), breaks=seq(1,12.5,1)) +
  ggtitle("(b) Percentage of swordfish infected with Kudoa") +
  scale_fill_continuous(name = "% infected", type = "viridis") +
  geom_text(aes(label=percent))

hb #infection prevalence tile plot


# Response, RE and obs checks -------------------------------------------------

nrow(dat4) #1653 swordfish sampled
sum(dat4$overall == 0) / nrow(dat4) #0.2081065 not infected
dat4$operation %>% unique %>% length #66 operations/tv combos
subset(dat4, overall == 1) %>% nrow #1309 infected
subset(dat4, overall == 0) %>% nrow #344 non-infected
subset(dat4, is.na(TotalFish) & overall == 1)  %>% nrow #284 infected detected with PCR
284/1309 * 100 #21.69% of sword detected with PCR

# Number of sword per operation
obs <- dat4 %>% 
  group_by(operation) %>% 
  count %>% data.frame 
obs$operation <- 1:nrow(obs)
obs$operation <- as.factor(obs$operation)
obs <- obs[order(obs$n),]
ggplot(obs) +
  geom_point(mapping = aes(x = n, y = operation)) +
  geom_vline(xintercept = 10) +
  xlab("number of observations")

# Number of sword per month
table(dat4$Month) %>% 
  plot(xlab = "Month", ylab = "Number of sword")


# Spatial shot info ------------------------------------------------------

shots$no_shots %>% mean # Average 6.8 (7) shots per fishing trip


# Collinearity amongst predictors -----------------------------------------

#chosen a priori
myvar <- c("SeamountDistKM", "mean_sst", "sd_sst",
           "mean_hc300", "sd_hc300","mean_mld1",
           "mean_sss", "mean_ssh_corrected", "mean_v",
           "mean_eke", "mean_speed", "newlat",
           "mon_clim_sst", "mon_clim_sst_sd", "seas_clim_sst",
           "seas_clim_sst_sd", "mean_depth", "sd_depth",
           "dist_core", "dhd", "heat_rate",
           "mo_anom", "se_anom", "mean_chla")

# Collinear variables w/ SST
corro <- cor(dat4[,myvar]) %>% data.frame
which(corro$mean_sst > 0.7) -> removethis
which(corro$mean_sst < -0.7) -> removethis2
names(corro)[removethis]; names(corro)[removethis2]
#[1] "mean_sst"        "mean_hc300"      "newlat"          "mon_clim_sst"   
# "seas_clim_sst"  
#[1] "mean_sss"  "mean_chla"

newvar <- c("SeamountDistKM", "mean_sst", "sd_sst",
            "mean_mld1", "mean_ssh_corrected", "mean_v",
            "mean_eke", "mean_speed","mean_depth",
            "sd_depth", "dist_core", "dhd",
            "heat_rate", "mo_anom", "se_anom",
            "mon_clim_sst_sd", "seas_clim_sst_sd","mean_chla")

corro <- cor(dat4[,newvar]) %>% data.frame

# Cannot model these together: 
corro["SeamountDistKM", "mean_depth"] #[1] 0.718778
corro["mean_eke", "mean_speed"]  #[1] 0.9189843
corro["dhd", "heat_rate"] #[1] 0.9711941

#NOTE - VIFs checked prior to modelling


# Outliers ---------------------------------------------------------

MyDotplot.ggp2(dat4, newvar) 
# Remove extreme outliers from EKE
remove <- dat4[which(dat4$mean_eke > 0.11),]
dat4 <- anti_join(dat4, remove)

MyDotplot.ggp2(dat4, newvar) 
# Remove extreme outliers from heat_rate
remove <- dat4[which(dat4$heat_rate > 1.2),]
dat4 <- anti_join(dat4, remove)


# Collinearity with month -------------------------------------------------

source("functions/Mybwplot.R")
dat4$Month <- as.factor(dat4$Month)
Mybwplot(dat4, c("mean_sst"), "Month")
#collinearity observed. Cannot model month with SST


# Visualise response vs pred relationships -------------------------------

MyMultipanel.ggp2(Z = dat4, 
                  varx = c(newvar), 
                  vary = "overall", 
                  ylab = "Response variable",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)


# Evidence for spatial correlation ---------------------------------------

# Note - indicative only. Must check residuals for evidence of true
# spatial autocorrelation. 

# Variograms
mydata2 <- data.frame(dat4$overall, dat4$meanlon, dat4$meanlat)
names(mydata2) <- c("z", "x", "y")
sp::coordinates(mydata2) =~x+y
Var2 <- gstat::variogram(z ~ 1, mydata2, cutoff = 5)
ggplot(data = Var2, aes(x = dist, y = gamma)) +
  geom_point() +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs"),
              colour = "black")  +
  ylim(0,0.3) + 
  xlab("Distance (˚)") + ylab("Sample variogram") 

# Variogram for each direction
Var2a <- gstat::variogram(z ~ 1, mydata2, cutoff = 4,
                          alpha = c(0, 45, 90, 145))
ggplot(data = Var2a,
       aes(x = dist, y = gamma)) + 
  geom_point() +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs")) +
  facet_wrap(~dir.hor)+
  xlab("Distance") + 
  ylab("Semi-variogram") + 
  theme(text = element_text(size=15))

# Correlograms (takes ~5 min)
CORRE <- ncf::spline.correlog(x = dat4$meanlon, 
                              y = dat4$meanlat,
                              z = dat4$overall,
                              xmax = 4)
plot(CORRE, ylim = c(-0.2, 0.2), xlab = "Distance (˚)")


# Save df -----------------------------------------------------------------

nrow(dat4) #1539 swordfish to model (due to outlier removal)
saveRDS(dat4, "prev_clean.RDS")


# Visualise DNA purity and quantity from molec. data ----------------------

# Below plots are found in reproducibility suppmat PDF. 
kudoa <- read.csv("Infect_Hemo_Cleaned.csv")
# Not available in repo 

# DNA purity vs infected/non-infected sword
kudoa %>% 
  ggplot(aes(x = overall %>% as.factor, y = DNA_260_280, fill = overall)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -.15,
    .width = 0, #remove slab interval
    point_colour = NA) +
  geom_hline(mapping = aes(yintercept = 1.8), col = "grey50", lty = 2) +
  geom_boxplot(
    width = 0.12,
    outlier.color = "black",
    alpha = 0.7) + 
  theme_bw() +
  theme(legend.position = "none") +
  # tidyquant::theme_tq() +
  labs(y = "260/280 DNA ratio",
       x = "Infected vs non-infected samples") +
  coord_flip() 

# DNA quantity (ng/ul) vs infected/non-infected sword
kudoa %>% 
  ggplot(aes(x = overall %>% as.factor, y = DNA_ngul, fill = overall)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -.15,
    .width = 0, #remove slab interval
    point_colour = NA) +
  geom_boxplot(
    width = 0.12,
    outlier.color = "black",
    alpha = 0.5) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Quantity of extracted DNA from sample (ng/ul)",
       x = "Infected vs non-infected samples") +
  coord_flip() 
