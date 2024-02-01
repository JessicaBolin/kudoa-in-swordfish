# Validating best model for the intensity dataset
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)


# Dependencies ------------------------------------------------------------

library(lme4) 
library(tidyverse)
library(performance)
library(DHARMa)
library(sjPlot)

datintens <- readRDS("intense_clean.RDS") #not available in repo
datintens <- datintens[-736,] #remove outlier as per modelling script

# Season ------------------------------------------------------------------

seasmod <- glmer.nb(TotalFish ~ Season + (1|operation), data = datintens)
summary(seasmod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: Negative Binomial(1.0803)  ( log )
# Formula: TotalFish ~ Season + (1 | operation)
# Data: datintens
# 
# AIC      BIC   logLik deviance df.resid 
# 21897.7  21926.8 -10942.8  21885.7      938 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.9986 -0.7366 -0.3916  0.2772  6.6802 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# operation (Intercept) 0.09573  0.3094  
# Number of obs: 944, groups:  operation, 57
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   10.4454     0.1291  80.885   <2e-16 ***
#   SeasonSpring   0.1867     0.1565   1.193   0.2329    
# SeasonSummer   0.3761     0.1680   2.238   0.0252 *  
#   SeasonWinter  -0.3602     0.1702  -2.116   0.0343 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) SsnSpr SsnSmm
# SeasonSprng -0.817              
# SeasonSummr -0.770  0.628       
# SeasonWintr -0.739  0.606  0.566

sjPlot::plot_model(seasmod, type = "pred")
drop1(seasmod, test= "Chi")

# Single term deletions
# 
# Model:
#   TotalFish ~ Season + (1 | operation)
# npar   AIC    LRT   Pr(Chi)    
# <none>      21898                     
# Season    3 21913 21.255 9.317e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

DHARMa::simulateResiduals(seasmod) %>% plot
# KS test on uniformity plot significant. Means:
# Within group deviations from uniformity (KS test) significant. 
# There is also singificant between group variances (levene test) and 
# within group variances confirmed by category plot (KS test)
# We have non-constant group variances

# Model the dispersion between groups using glmmTMB
library(glmmTMB)
ncv <- glmmTMB(TotalFish ~ Season + (1|operation), data = datintens,
               family = nbinom2, dispformula = ~Season)
simulateResiduals(ncv) %>% plot 
# Fixed the dispersion between groups, but still sigificant dispersion within two groups,
# Try modelling within group variance with an observation level random effect
datintens$OLRE <- seq_len(nrow(datintens))
ncvv <- glmmTMB(TotalFish ~ Season + (1|operation) + (1|OLRE), 
                data=datintens, family = nbinom2, 
                dispformula = ~Season) #Model won't converge
# Remove dispformula
ncvvv <- glmmTMB(TotalFish ~ Season + (1|operation) + (1|OLRE), 
                 data=datintens, family = nbinom2)
# Non-convergence as well

# Solution. Cannot fix BOTH within and between group variances without the 
# model not converging. So, go with modelling dispersion between groups, 
# and acknowledge.


# Full model --------------------------------------------------------------

mod <- glmer.nb(TotalFish ~ scale(mean_mld1) + (1|operation), 
                data = datintens, 
                control = glmerControl(optCtrl = list(maxfun = 100000)))

# Summary -----------------------------------------------------------------

summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: Negative Binomial(1.0772)  ( log )
# Formula: TotalFish ~ scale(mean_mld1) + (1 | operation)
# Data: datintens
# Control: glmerControl(optCtrl = list(maxfun = 1e+05))
# 
# AIC      BIC   logLik deviance df.resid 
# 21907.8  21927.2 -10949.9  21899.8      940 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.9983 -0.7403 -0.3986  0.2794  6.3588 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# operation (Intercept) 0.1297   0.3601  
# Number of obs: 944, groups:  operation, 57
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      10.51575    0.06077 173.046  < 2e-16 ***
#   scale(mean_mld1) -0.17169    0.06191  -2.773  0.00555 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# scl(mn_ml1) 0.025 

AIC(mod) #AIC  (21907.78)
performance::r2(mod) 
# # R2 for Mixed Models
# 
# Conditional R2: 0.195
# Marginal R2: 0.036


# 6.1 General checks ----------------------------------------------------

### Outliers ####

check_outliers(mod) # no outliers
mod3 <- mod

### Model coefficieents ####

# Wald-Z test used to test for significance (i.e., P values) 
fixx <- fixef(mod3) %>% as.data.frame()
fixx$var <- row.names(fixx)
cis <- confint(mod3, oldNames = F) %>% as.data.frame 
cis$var <- row.names(cis)
cis <- cis[-1,]
cis <- cis[-1,]
names(cis) <- c("low", "high", "var")
ggplot(cis) +
  geom_errorbar(mapping = aes(x = var, ymin = low, 
                              ymax = high, width = 0.1)) +
  geom_point(fixx[-1,], mapping = aes(x = var, y = .)) + 
  geom_hline(yintercept = 0, lty = "dashed") +
  ylim(-1, 1) + 
  coord_flip() 


### Singularity ####
isSingular(mod3) 
#FALSE 

### Overdispersion ####

performance::check_overdispersion(mod3) -> od
od
# # Overdispersion test
# 
#    dispersion ratio =    1.278
# Pearson's Chi-Squared = 1201.141
#                 p-value =  < 0.001
# 
# Overdispersion detected.

### Optimizer sensitivity ####

#http://svmiller.com/blog/2018/06/mixed-effects-models-optimizer-checks/
AF1 <- allFit(mod3, verbose = F, maxfun = 1e5, 
              parallel = "multicore", ncpus = parallel::detectCores()-2)
is_ok <- AF1[sapply(AF1, is, "merMod")]
testy <- data.frame(optimizer = NA, loglik = NA)
for (i in 1:length(AF1)) {
  tryCatch(testy[i,]$loglik <- logLik(AF1[[i]]), error = function(e) NA)
  tryCatch(testy[i,]$optimizer <- AF1[i] %>% names, error = function(e) NA)
}
ggplot(testy, aes(y = loglik, x = optimizer)) +
  geom_point() +
  ylim(-10949, -10951) +
  coord_flip() +
  ylab(label = "Log Likelihood (L)")
#all optimizers converge to logLik values that are identical


### Modelling framework ####
newdat <- datintens

#glmmTMB
library(glmmTMB)
glmmTMB_mod <- glmmTMB(TotalFish ~ scale(mean_mld1) + (1 | operation), 
                       data = newdat, family = nbinom2(), REML = T)
get_tmb_cis <- function(x, name) {
  ci <- confint(x)
  ci <- ci[!grepl("Std.Dev|sigma", row.names(ci)), ]
  ci <- data.frame(ci)
  names(ci) <- c("conf.low", "conf.high", "estimate")
  ci$model <- name
  ci$term <- names(fixef(x)$cond)
  ci
}
tmb_nb2 <- get_tmb_cis(glmmTMB_mod, "glmmTMB")

#glmmPQL
library(MASS)
m_pql <- glmmPQL(TotalFish ~ scale(mean_mld1), random = ~ 1 | operation, 
                 data = newdat, family = nbinom2()) 
m_pql_mod <- broom.mixed::tidy(m_pql, conf.int = TRUE) %>%
  dplyr::select(conf.low, conf.high, estimate, term) %>%
  mutate(model = "MASS") %>%
  filter(term != "sd__(Intercept)") %>%
  filter(term != "sd_(Intercept)") %>%
  filter(term != "sd_Observation")

#rstan
library(rstanarm)
options(mc.cores = parallel::detectCores()) 
mstan_nb <- stan_glmer.nb(TotalFish ~ scale(mean_mld1) + (1|operation), 
                          data = newdat, chains = 4, iter = 2000)
stan_est <- broom.mixed::tidyMCMC(mstan_nb, conf.int = TRUE)
stan_nb <- stan_est %>%
  dplyr::select(conf.low, conf.high, estimate, term) %>%
  filter(term != "reciprocal_dispersion") %>%
  mutate(model = "rstanarm")
stan_nb[1:2,] -> stan_nb

#glmer.nb
glmer_nb <- broom.mixed::tidy(mod3, conf.int = TRUE) %>%
  dplyr::select(conf.low, conf.high, estimate, term) %>%
  mutate(model = "lme4") %>%
  filter(term != "sd__(Intercept)")

#glmmADMB
library(glmmADMB)
cmod_gA_L <- glmmADMB::glmmadmb(TotalFish ~ scale(mean_mld1) 
                                + (1 | operation),
                                data= newdat, 
                                family="nbinom")
confint(cmod_gA_L) %>% as.data.frame -> admb 
admb$estimate <- NA
admb[1, 3] <- coef(cmod_gA_L)[1]
admb[2, 3] <- coef(cmod_gA_L)[2]
names(admb) <- c("conf.low", "conf.high", "estimate")
admb$term <- NA
admb$term <- row.names(admb)
admb$model <- "glmmADMB"

#spaMM
library(spaMM)
m_spamm <- fitme(TotalFish ~ scale(mean_mld1) 
                 + (1|operation), 
                 data = newdat, 
                 family = "negbin2") 
iii <- confint(m_spamm, parm = names(fixef(m_spamm)),
               boot_args=list(nsim=10, seed=123, nb_cores = 8))
spamm1 <- stan_nb
spamm1$conf.high[2] <- iii[[2]]$normal[3]
spamm1$conf.low[2] <- iii[[2]]$normal[2]
spamm1$estimate[2] <- iii[[2]]$t0 %>% as.numeric
spamm1$term[2] <- rownames(iii[[2]]$normal)
spamm1$conf.high[1] <- iii[[1]]$normal[3]
spamm1$conf.low[1] <- iii[[1]]$normal[2]
spamm1$estimate[1] <- iii[[1]]$t0 %>% as.numeric
spamm1$term[1] <- rownames(iii[[1]]$normal)
spamm1$model <- "spaMM"

cis_df <- bind_rows(tmb_nb2, 
                    glmer_nb, 
                    stan_nb, 
                    m_pql_mod, 
                    admb, 
                    spamm1) 

library(ggbreak)
ree <- ggplot(cis_df, aes(term, estimate,
                          ymin = conf.low, ymax = conf.high, colour = model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2)
ree + scale_y_break(breaks = c(0.05,10.35)) +
  ggtitle("Model coefficients +/- 95% CIs","(TotalFish ~ scale(mean_MLD1) + (1|operation))")



# 6.2 Residuals --------------------------------------------------------


### Simulated Residuals ####

simulateResiduals(mod3) -> simresid
simresid %>% plot
testQuantiles(simresid, plot = T) -> quantresid 

### Spatial correlation ####

#### Correlogram ####
CORRE1 <- ncf::spline.correlog(x = newdat$meanlon, 
                               y = newdat$meanlat,
                               z = resid(mod3, type = "pearson"),
                               xmax = 4,
                               quiet = T)
plot(CORRE1, ylim = c(-0.15,0.15))

#### Variogram ####
mydata2 <- data.frame(resid(mod3, type = "pearson"), 
                      newdat$meanlon, 
                      newdat$meanlat)
names(mydata2) <- c("z", "x", "y")
sp::coordinates(mydata2) =~x+y
Var2 <- gstat::variogram(z ~ 1, mydata2, cutoff = 4)

p <- ggplot(data = Var2, aes(x = dist, y = gamma)) + 
  geom_point() + 
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs"),
              colour = "black") + 
  ylim(0,2.5) + 
  xlab("Distance (˚)") + ylab("Sample variogram") 
p 

Var2a <- gstat::variogram(z ~ 1, mydata2, cutoff = 4,
                          alpha = c(0, 45, 90, 145))
ggplot(data = Var2a,
       aes(x = dist, y = gamma)) + 
  geom_point() +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs")) +
  facet_wrap(~dir.hor)+
  ylab("Sample variogram") + 
  ylim(0, 2) + 
  theme(text = element_text(size=15)) +
  xlab("Distance (˚)")

#### Moran's I test  ####
dat5 <- newdat 
dat5 %>%
  group_by(operation, meanlon) %>%
  summarise(meanlon %>% mean) %>%
  data.frame -> meany
dups <- meany[duplicated(meany$meanlon),]$operation
dat5[dat5$operation == 
       dups[1],]$meanlon <- dat5[dat5$operation == 
                                   dups[1],]$meanlon + 0.01
groupLocations = aggregate(dat5[, c("meanlon", "meanlat")], 
                           list(dat5$operation), mean)
res2 = recalculateResiduals(simulateResiduals(mod3), 
                            group = dat5$operation %>% unique)
testSpatialAutocorrelation(res2, 
                           groupLocations$meanlon, 
                           groupLocations$meanlat)
#P = 0.915 #no spatial correlaiton in resids.


# 6.3 Random intercept --------------------------------------------------

### Normality assumption ####
re <- ranef(mod3)
qqnorm(re$operation[,"(Intercept)"], pch = 16)
qqline(re$operation[,"(Intercept)"]) # looks OK 

### ICC #### 
performance::icc(mod3) # Intraclass Correlation Coefficient = 0.165

#### Dotplot ####
lattice::dotplot(ranef(mod3, condVar = TRUE))
#Random effects follow a normal distribution.

#### Signficiance of random effect ####
#Use LRT (fit full and reduced model - reduced is model with focal variance set to zero)
fully <- mod3
empty <- MASS::glm.nb(TotalFish ~ scale(mean_mld1), data = datintens)
anova(fully, empty) 
#full model is significant. 

#### VarCorr ####
VarCorr(mod3) #0.36007 



