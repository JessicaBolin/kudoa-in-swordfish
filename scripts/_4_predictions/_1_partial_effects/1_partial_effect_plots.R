# Partial effect plots from predictions of both models
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)

# Dependencies and data ---------------------------------------------------

library(lme4) 
library(ggpubr) 
library(lattice) 
library(RColorBrewer) 
library(effects) 
library(tidyverse) 
library(akima)
library(metR)
library(viridis)

# Below not available in repo
dat <- readRDS("prev_clean.RDS")
intense_dat <- readRDS("intense_clean.RDS")
intense_dat <- intense_dat[-736,] #remove outlier as per intensity eval


# Best models -------------------------------------------------------------

#Best model: prevalence
mod <- glmer(overall ~ scale(mean_sst) * scale(mean_v) 
             + scale(mean_sst) * scale(mo_anom)
             + scale(mon_clim_sst_sd) 
             + (1 | operation), 
             family = binomial, 
             data = dat,
             control = glmerControl(optCtrl = list(maxfun = 100000)))

#Best model: intensity
mod_intens <- glmer.nb(TotalFish ~ scale(mean_mld1) 
                       + (1|operation), 
                       data = intense_dat, 
                       control = glmerControl(optCtrl = list(maxfun = 100000)))


# Season --------------------------------------------------------------

seasmod <- glmer(overall ~ Season + (1|operation), 
                 family = binomial, data = dat)
seas_preds <- effect(term = "Season", seasmod) %>% data.frame

seasintens <- glmer.nb(TotalFish ~ Season + (1|operation), data = dat)
seas_intense_preds <- effect(term = "Season", seasintens) %>% data.frame

seas_prev_plot <- ggplot() +
  geom_linerange(data = seas_preds, 
                 aes(x = Season, ymin = lower, ymax = upper),
                 lwd = 0.8, 
                 col = "steelblue") +
  geom_point(data = seas_preds, 
             aes(x = Season, y = fit), 
             col = "steelblue",
             size= 2, 
             show.legend = T, 
             inherit.aes = F)  +
  scale_y_continuous(name = "Probability of infected swordfish",
                     limits = c(0, 1 )) +
  theme_bw() +
  theme(legend.position=c(0.45, 1.07), 
        legend.direction="horizontal",
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(1,0.1,.1,0.1),"cm"),
        axis.title = element_text(size = 14, family = "Arial Narrow",
                                  face = "bold"),
        axis.text = element_text(size = 12, family = "Arial Narrow"),
        legend.title = element_text(size = 14, family = "Arial Narrow"),
        legend.text = element_text(size = 12, family = "Arial Narrow"),
        axis.title.x = element_text(margin = margin(t = 10, r = -20))) +
  annotate("text", label = "(a)",  size  = 6,
           y = 0.99, x = 0.7, fontface = "bold", 
           family = "Arial Narrow")

seas_prev_plot

seas_int_plot <- ggplot() +
  geom_linerange(data = seas_intense_preds, 
                 aes(x = Season, ymin = lower, ymax = upper),
                 lwd = 0.8, col = "steelblue") +
  geom_point(data = seas_intense_preds, 
             aes(x = Season, y = fit), 
             col = "steelblue",
             size= 2, show.legend = T, inherit.aes = F)  +
  scale_y_continuous(name = "Spores/gram of swordfish",
                     limits = c(0, 100000)) +
  theme_bw() +
  theme(legend.position=c(0.45, 1.07), 
        legend.direction="horizontal",
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(1,0.1,.1,0.1),"cm"),
        axis.title = element_text(size = 14, family = "Arial Narrow",
                                  face = "bold"),
        axis.text = element_text(size = 12, family = "Arial Narrow"),
        legend.title = element_text(size = 14, family = "Arial Narrow"),
        legend.text = element_text(size = 12, family = "Arial Narrow"),
        axis.title.x = element_text(margin = margin(t = 10, r = -20))) +
  annotate("text", label = "(b)",  size  = 6,
           y = 99000, x = 0.7, fontface = "bold", 
           family = "Arial Narrow")

seasy <- ggarrange(seas_prev_plot, seas_int_plot)
seasy


# SST * V -----------------------------------------------------------------

#### Predictions ####

eac_preds <- effect(term = "scale(mean_sst)*scale(mean_v)", mod,
                    xlevels = list("mean_sst" = 100, 
                                   "mean_v" = c(-0.2, 0.2))) %>% data.frame

eac_preds1 <- effect(term = "scale(mean_sst)*scale(mean_v)", mod,
                     xlevels = list("mean_sst" = 100,
                                    "mean_v" = 100)) %>% data.frame

#### Interaction plot ####

eac_plot <- ggplot() +
  geom_point(data = dat, aes(x = mean_sst, y = overall, color = mean_v ),
             alpha = 0.3, show.legend = F, size = 1.8) +
  scale_color_gradient2(low = "darkslateblue", 
                        high = "slategray4",
                        midpoint = 0, limits = c(-0.2, 0.2),
                        name = "Velocity") +
  geom_ribbon(data = eac_preds, 
              aes(x = mean_sst, ymin = lower, ymax = upper, 
                  fill = mean_v %>% as.factor), alpha = 0.3) +
  geom_line(data = eac_preds, 
            aes(x = mean_sst, y = fit, col = mean_v, 
                fill = mean_v %>% as.factor),
            size= 0.8, show.legend = F, inherit.aes = F) + 
  theme_bw() + 
  scale_x_continuous(name = "Mean SST (˚C)") +
  scale_y_continuous(name = "Probability of infected swordfish") +
  scale_colour_manual(name = "Meridional Velocity (m/s)", 
                      values=c("darkslateblue", 
                               "slategray4"), 
                      labels=c("-0.2", "0.2"),
                      aesthetics = c("fill"), 
                      guide = guide_legend()) +
  theme(legend.position=c(0.45, 1.07), 
        legend.direction="horizontal",
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(1,0.1,.1,0.1),"cm"))


#### Contour of predictions ####

contpreds_eac <- ggplot() +
  geom_tile(data = eac_preds1, 
            aes(x = mean_sst, y = mean_v, z = fit, fill = fit),
            width = 0.09, height = 0.01) +
  viridis::scale_fill_viridis() +  
  geom_contour(data = eac_preds1, 
               aes(x = mean_sst, y = mean_v, z = fit, fill = fit),
               color = "black", alpha=0.5) +
  geom_text_contour(data = eac_preds1, 
                    aes(x = mean_sst, y = mean_v, z = fit)) +
  geom_point(data = dat, 
             aes(x = mean_sst, y = mean_v), size = 0.6) +
  scale_x_continuous(name = "SST (˚C)") +
  scale_y_continuous(name = "Meridional Velocity (m/s)") +
  theme(legend.position= "none")


#### Contour of raw data ####

rawdata_eac <- ggplot(dat, aes(x = mean_sst, y = mean_v)) +
  geom_density_2d_filled(alpha = 0.9) +
  geom_point(size = 0.2) + 
  geom_rug(alpha = 1/2, position = "jitter") + 
  guides(fill = guide_legend(title = "Level")) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "SST (˚C)") +
  scale_y_continuous(name = "Meridional Velocity (m/s)") + 
  geom_hline(mapping = aes(yintercept = 0), lty = 2) 

#### Density plot ####

mo100 <- subset(dat, mean_v < -0.2)
mo200 <- subset(dat, mean_v > -0.2 & mean_v < 0)
mo400 <- subset(dat, mean_v > 0 & mean_v < 0.2)
mo600 <- subset(dat, mean_v > 0.2)

dens_eac <- ggplot(dat, mapping = aes(x= mean_sst)) +
  geom_density(mo100, mapping = aes(x = mean_sst), 
               fill = "darkslateblue", alpha = 0.4) + #->0.2
  geom_density(mo200, mapping = aes(x = mean_sst), 
               fill = "steelblue3", alpha = 0.4) +
  geom_density(mo400, mapping = aes(x = mean_sst), 
               fill = "slategray4", alpha = 0.4) +
  geom_density(mo600, mapping = aes(x = mean_sst), 
               fill = "azure3", alpha = 0.4) +
  scale_x_continuous(name = "Mean SST (˚C)") +
  scale_y_continuous(name = "Density") +
  ggtitle("Meridional Velocity (m/s):", #Density plot of raw SST*V data",
          subtitle = "Purple:-0.2+, Blue:-0.2-0, Grey:0-0.2, White:0.2+")

eac_plots <- ggarrange(eac_plot, dens_eac,
                  contpreds_eac, rawdata_eac)
eac_plots

#### 3D Surface ####

cls <- colorRampPalette(rev(brewer.pal(100, 'RdBu')))(100)
wireframe(fit ~ mean_sst + mean_v, 
          data = eac_preds1, 
          zlab = list("Kudoa prob.", rot = 90), 
          xlab = list("SST"),
          ylab = list("Merid. velocity", rot = 60),
          drape = TRUE, 
          col.regions = cls, 
          scales = list(arrows = FALSE), 
          zlim = c(0, 1),
          screen = list(z = 160, x = -65), 
          lwd = 0.0001) 


# SST * mo_anom -----------------------------------------------------------

#### Predictions ####

anom_preds <- effect(term = "scale(mean_sst)*scale(mo_anom)", mod,
                     xlevels = list("mean_sst" = 100, 
                                    "mo_anom" = c(-0.5, 0.5))) %>% data.frame

anom_preds1 <- effect(term = "scale(mean_sst)*scale(mo_anom)", mod,
                      xlevels = list("mean_sst" = 100,
                                     "mo_anom" = 100)) %>% data.frame

#### Interaction plot ####

anom_plot <- ggplot() +
  geom_point(data = dat, aes(x = mean_sst, y = overall, color = mo_anom ),
             alpha = 0.3, show.legend = F, size = 1.8) +
  scale_color_gradient2(low = "darkslateblue", 
                        high = "slategray4",
                        midpoint = 0, limits = c(-0.5, 0.5),
                        name = "SSTa") +
  geom_ribbon(data = anom_preds, 
              aes(x = mean_sst, ymin = lower, ymax = upper, 
                  fill = mo_anom %>% as.factor), alpha = 0.3) +
  geom_line(data = anom_preds, 
            aes(x = mean_sst, y = fit, 
                col = mo_anom, fill = mo_anom %>% as.factor),
            size= 0.8, show.legend = F, inherit.aes = F) + 
  scale_x_continuous(name = "Mean SST (˚C)") +
  scale_y_continuous(name = "Probability of infected swordfish") +
  theme_bw() +
  scale_colour_manual(name = "SSTa (˚C)", 
                      values=c("darkslateblue", "slategray4"), 
                      labels=c("-0.5","0.5"),
                      aesthetics = c("fill"), 
                      guide = guide_legend()) +
  theme(legend.position=c(0.45, 1.06), 
        legend.direction="horizontal",
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(1,0.1,.1,0.1),"cm"))


#### Density plot ####

dist200 <- subset(dat, mo_anom < -0.5)
dist400 <- subset(dat, mo_anom > -0.5 & mo_anom < 0)
dist600 <-  subset(dat, mo_anom > -0 & mo_anom < 0.5)
dist800 <- subset(dat, mo_anom > 0.5)

dens_anom <- ggplot(dat, mapping = aes(x= mean_sst)) +
  geom_density(dist200, mapping = aes(x = mean_sst), 
               fill = "darkslateblue", alpha = 0.4) +
  geom_density(dist400, mapping = aes(x = mean_sst), 
               fill = "steelblue3", alpha = 0.4) +
  geom_density(dist600, mapping = aes(x = mean_sst), 
               fill = "slategray4", alpha = 0.4) +
  geom_density(dist800, mapping = aes(x = mean_sst), 
               fill = "azure3", alpha = 0.4) +
  ggtitle("SSTa (˚C)",
          subtitle = "Purple:-0.5+, blue:-0.5-0, grey:0-0.5, white:0.5+") +
  scale_x_continuous(name = "Mean SST (˚C)") +
  scale_y_continuous(name = "Density") 


#### Contour of predictions ####

contour_preds_anom <- ggplot() +
  geom_tile(data = anom_preds1, 
            aes(x = mean_sst, y = mo_anom, z = fit, fill = fit),
            width = 0.09, height = 0.03) +
  viridis::scale_fill_viridis()  + 
  geom_contour(data = anom_preds1, 
               aes(x = mean_sst, y = mo_anom, z = fit, fill = fit),
               color = "black", alpha=0.5) +
  geom_text_contour(data = anom_preds1, 
                    aes(x = mean_sst, y = mo_anom, z = fit)) +
  geom_point(data = dat, 
             aes(x = mean_sst, y = mo_anom), size = 0.6) +
  scale_x_continuous(name = "Mean SST (˚C)") +
  scale_y_continuous(name = "SSTa (˚C)") +
  theme(legend.position= "none")


#### Contour raw data ####

contour_raw_anom <- ggplot(dat, aes(x = mean_sst, y = mo_anom)) +
  geom_density_2d_filled(alpha = 0.9) +
  geom_point() + 
  geom_rug(alpha = 1/2, position = "jitter") + 
  guides(fill = guide_legend(title = "Level")) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "Mean SST (˚C)") +
  scale_y_continuous(name = "SSTa (˚C)") +
  geom_hline(mapping = aes(yintercept = 0), lty = 2)

anomplots <- ggarrange(anom_plot, dens_anom,
                       contour_preds_anom, contour_raw_anom)
anomplots


#### 3D Surface plot ####
wireframe(fit ~ mean_sst + mo_anom, 
          data = anom_preds1, 
          zlab = list("Kudoa prob.", rot = 90), 
          xlab = list("SST"),
          ylab = list("SSTa", rot = 50),
          drape = TRUE, 
          col.regions = cls, 
          scales = list(arrows = FALSE), 
          zlim = c(0, 1),
          screen = list(z = 140, x = -65), 
          lwd = 0.0001) 

# Single predictors -------------------------------------------------------

#### mon_clim_sst_sd ####

int_preds_monclimsstsd <- effect(term = "scale(mon_clim_sst_sd)",
                                 mod, xlevels = 
                                   list("scale(mon_clim_sst_sd)" 
                                        = 100)) %>% data.frame

ggclimsst <-  ggplot() +
  geom_point(data = dat, aes(x = mon_clim_sst_sd, y = overall), 
             col = "grey50") +
  geom_ribbon(data = int_preds_monclimsstsd, 
              aes(x = mon_clim_sst_sd, ymin = lower, ymax = upper), 
              alpha = 0.3, fill = "steelblue3") +
  geom_line(data = int_preds_monclimsstsd, 
            aes(x = mon_clim_sst_sd, y = fit), col = "steelblue4",
            size= 0.8, show.legend = T, inherit.aes = F) +
  scale_x_continuous(name = "Std. dev. monthly SST climatology (˚C)") +
  scale_y_continuous(name = "Probability of infected swordfish") +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggclimsst


# Intensity ---------------------------------------------------------------


##### MLD1 ####

mld_preds <- effect(c("scale(mean_mld1)"), mod_intens,
                    xlevels = 100) %>% data.frame

aa <- ggplot(mld_preds) +
  geom_point(data = intense_dat, aes(x = mean_mld1, y = TotalFish), 
             col = "grey50", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = mean_mld1, 
                            ymin = lower, 
                            ymax = upper), 
              alpha = 0.3, fill = "steelblue3") +
  geom_line(mapping = aes(x = mean_mld1, y = fit), 
            col = "steelblue4", size= 0.8, inherit.aes = F) +
  ylim(0, 300000) +
  ylab("Spores/gram of swordfish") +
  scale_x_continuous(name = "Mixed layer depth (m)") +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

aa
