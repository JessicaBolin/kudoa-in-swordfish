# Figure 2 - Seasonal prevalence/intensity of infected sword
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)
# Updated Feb 2024

# Dependencies ------------------------------------------------------------

library(effects) 
library(tidyverse)
library(ggpubr) 
library(lme4)

dat <- readRDS("prev_clean.RDS")
intense_dat <- readRDS("intense_clean.RDS")
intense_dat <- intense_dat[-736,] #remove outlier as per intensity model evaluation


# Cleaning ----------------------------------------------------------------

dat$Season <- as.factor(dat$Season)
dat$Season <- factor(dat$Season, c("Summer", "Autumn", "Winter", "Spring"))
intense_dat$Season <- as.factor(intense_dat$Season)
intense_dat$Season <- factor(intense_dat$Season, c("Summer", "Autumn", "Winter", "Spring"))


# Model predictions -------------------------------------------------------

seasmod <- glmer(overall ~ Season + (1|operation), 
                 family = binomial, data = dat)
seas_preds <- effect(term = "Season", seasmod) %>% data.frame

seasintens <- glmer.nb(TotalFish ~ Season + (1|operation), data = dat)
seas_intense_preds <- effect(term = "Season", seasintens) %>% data.frame


# Plots -------------------------------------------------------------------

seas_prev_plot <- ggplot() +
  geom_linerange(data = seas_preds, 
                 aes(x = Season, ymin = lower, ymax = upper),
                 lwd = 0.8, 
                 col = "steelblue") +
  geom_point(data = seas_preds, 
             aes(x = Season, y = fit), 
             col = "steelblue",
             size = 2, 
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
        plot.margin=unit(c(1,0.1,.1,0.1), "cm"),
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
                                                    r = -20))) +
  annotate("text", label = "(a)", size  = 6,
           y = 0.99, x = 0.7, fontface = "bold", 
           family = "Arial Narrow")

seas_int_plot <- ggplot() +
  geom_linerange(data = seas_intense_preds, 
                 aes(x = Season, ymin = lower, ymax = upper),
                 lwd = 0.8, 
                 col = "steelblue") +
  geom_point(data = seas_intense_preds, 
             aes(x = Season, y = fit), 
             col = "steelblue",
             size= 2, 
             show.legend = T, 
             inherit.aes = F)  +
  scale_y_continuous(name = "Spores/gram of swordfish",
                     limits = c(0, 100000)) +
  theme_bw() +
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
                                                    r = -20))) +
  annotate("text", label = "(b)",  size  = 6,
           y = 99000, x = 0.7, fontface = "bold", 
           family = "Arial Narrow")


# Visualise ---------------------------------------------------------------

seasy <- ggarrange(seas_prev_plot, seas_int_plot)
seasy
