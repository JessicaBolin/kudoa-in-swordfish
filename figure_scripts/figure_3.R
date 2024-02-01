# Figure 3 - Partial effect plots from both models
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)
# Updated Feb 2024
# Base plots created in R; embellishments done in Adobe Illustrator


# Dependencies and data ---------------------------------------------------

library(lme4) 
library(effects) 
library(tidyverse) 

dat <- readRDS("prev_clean.RDS")
intense_dat <- readRDS("intense_clean.RDS")
intense_dat <- intense_dat[-736,] #remove outlier as per intensity eval
# Above not available in repo

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


# Panel A -----------------------------------------------------------------


eac_preds <- effect(term = "scale(mean_sst)*scale(mean_v)", 
                    mod,
                    xlevels = list("mean_sst" = 100, 
                                   "mean_v" = c(-0.2, 0.2))) %>% data.frame

eac_plot <- ggplot() +
  geom_point(data = dat, 
             aes(x = mean_sst, y = overall, color = mean_v ),
             alpha = 0.3, 
             show.legend = F, 
             size = 1.8) +
  scale_color_gradient2(low = "darkslateblue", 
                        high = "orange3",
                        midpoint = 0, 
                        limits = c(-0.2, 0.2),
                        name = "Velocity") +
  geom_ribbon(data = eac_preds, 
              aes(x = mean_sst, ymin = lower, ymax = upper, 
                  fill = mean_v %>% as.factor), 
              alpha = 0.3) +
  geom_line(data = eac_preds, 
            aes(x = mean_sst, y = fit, col = mean_v, fill = mean_v %>% as.factor),
            size= 0.8, 
            show.legend = F, 
            inherit.aes = F) + 
  theme_bw() + 
  scale_x_continuous(name = "Mean SST (˚C)") +
  scale_y_continuous(name = "Probability of infected swordfish") +
  scale_colour_manual(name = "Meridional Velocity (m/s)", 
                      values=c("darkslateblue", 
                               "orange3"), 
                      labels=c("-0.2", "0.2"),
                      aesthetics = c("fill"), 
                      guide = guide_legend()) +
  theme(legend.position=c(0.45, 1.07), 
        legend.direction="horizontal",
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(1,0.1,.1,0.1),"cm"),
        axis.title = element_text(size = 14,
                                  family = "Arial Narrow",
                                  face = "bold"),
        axis.text = element_text(size = 12,
                                 family = "Arial Narrow"),
        legend.title = element_text(size = 14,
                                    family = "Arial Narrow"),
        legend.text = element_text(size = 12,
                                   family = "Arial Narrow"),
        axis.title.x = element_text(margin = margin(t = 10,
                                                    r = -20))) 



# Panel B -----------------------------------------------------------------

anom_preds <- effect(term = "scale(mean_sst)*scale(mo_anom)", 
                     mod,
                     xlevels = list("mean_sst" = 100, 
                                    "mo_anom" = c(-0.5, 0.5))) %>% data.frame

anom_plot <- ggplot() +
  geom_point(data = dat, aes(x = mean_sst, y = overall, color = mo_anom ),
             alpha = 0.3, 
             show.legend = F, 
             size = 1.8) +
  scale_color_gradient2(low = "darkslateblue", 
                        high = "orange3",
                        midpoint = 0, 
                        limits = c(-0.5, 0.5),
                        name = "SSTa") +
  geom_ribbon(data = anom_preds, 
              aes(x = mean_sst, ymin = lower, ymax = upper, 
                  fill = mo_anom %>% 
                    as.factor), 
              alpha = 0.3) +
  geom_line(data = anom_preds, 
            aes(x = mean_sst, y = fit, 
                col = mo_anom, fill = mo_anom %>% as.factor),
            size= 0.8, 
            show.legend = F, 
            inherit.aes = F) + 
  scale_x_continuous(name = "Mean SST (˚C)") +
  scale_y_continuous(name = "Probability of infected swordfish") +
  theme_bw() +
  scale_colour_manual(name = "SST anomaly (˚C)", 
                      values=c("darkslateblue", "orange3"), 
                      labels=c("-0.5","0.5"),
                      aesthetics = c("fill"), 
                      guide = guide_legend()) +
  theme(legend.position=c(0.45, 1.06), 
        legend.direction="horizontal",
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(1,0.1,.1,0.1),"cm"),
        axis.title = element_text(size = 14,
                                  family = "Arial Narrow",
                                  face = "bold"),
        axis.text = element_text(size = 12,
                                 family = "Arial Narrow"),
        legend.title = element_text(size = 14,
                                    family = "Arial Narrow"),
        legend.text = element_text(size = 12,
                                   family = "Arial Narrow"),
        axis.title.x = element_text(margin = margin(t = 10,
                                                    r = -20))) 


# Panel C -----------------------------------------------------------------

int_preds_monclimsstsd <- effect(term = "scale(mon_clim_sst_sd)",
                                 mod, 
                                 xlevels = list("scale(mon_clim_sst_sd)" = 100)) %>% data.frame

ggclimsst <-  ggplot() +
  geom_point(data = dat, aes(x = mon_clim_sst_sd, y = overall), 
             col = "grey50") +
  geom_ribbon(data = int_preds_monclimsstsd, 
              aes(x = mon_clim_sst_sd, ymin = lower, ymax = upper), 
              alpha = 0.3, 
              fill = "steelblue3") +
  geom_line(data = int_preds_monclimsstsd, 
            aes(x = mon_clim_sst_sd, y = fit), 
            col = "steelblue4",
            size= 0.8, 
            show.legend = T, 
            inherit.aes = F) +
  scale_x_continuous(name = "Standard deviation of monthly SST climatology (˚C)") +
  scale_y_continuous(name = "Probability of infected swordfish") +
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        plot.margin=unit(c(1,0.1,.1,0.1),"cm"),
        axis.title = element_text(size = 14,
                                  family = "Arial Narrow",
                                  face = "bold"),
        axis.text = element_text(size = 12,
                                 family = "Arial Narrow"),
        legend.title = element_text(size = 14,
                                    family = "Arial Narrow"),
        legend.text = element_text(size = 12,
                                   family = "Arial Narrow"),
        axis.title.x = element_text(margin = margin(t = 10,
                                                    r = -20)))

# Panel D -----------------------------------------------------------------

mld_preds <- effect(c("scale(mean_mld1)"), 
                    mod_intens,
                    xlevels = 100) %>% data.frame

mldplot <- ggplot(mld_preds) +
  geom_point(data = intense_dat, aes(x = mean_mld1, y = TotalFish), 
             col = "grey50", 
             alpha = 0.3) +
  geom_ribbon(mapping = aes(x = mean_mld1, 
                            ymin = lower, 
                            ymax = upper), 
              alpha = 0.3, 
              fill = "steelblue3") +
  geom_line(mapping = aes(x = mean_mld1, y = fit), 
            col = "steelblue4", 
            size= 0.8, 
            inherit.aes = F) +
  ylim(0, 300000) +
  ylab("Spores/gram of swordfish tissue") +
  scale_x_continuous(name = "Mixed layer depth (m)") +
  scale_y_continuous(labels = scales::comma) + 
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        plot.margin=unit(c(1,0.1,.1,0.1),"cm"),
        axis.title = element_text(size = 14,
                                  family = "Arial Narrow",
                                  face = "bold"),
        axis.text = element_text(size = 12,
                                 family = "Arial Narrow"),
        legend.title = element_text(size = 14,
                                    family = "Arial Narrow"),
        legend.text = element_text(size = 12,
                                   family = "Arial Narrow"),
        axis.title.x = element_text(margin = margin(t = 10,
                                                    r = -20)))
