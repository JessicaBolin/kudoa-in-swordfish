# Validating best model for the prevalence dataset
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)

# Dependencies ------------------------------------------------------------

library(lme4)
library(tidyverse)
library(performance)
library(DHARMa)
library(sjPlot)
library(dismo)
library(AUC)

dat <- readRDS("prev_clean.RDS") #not available in repo
nrow(dat)

# Season ------------------------------------------------------------------

seasmod <- glmer(overall ~ Season + (1|operation), 
                 family = binomial, data=dat)
summary(seasmod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: overall ~ Season + (1 | operation)
# Data: dat
# 
# AIC      BIC   logLik deviance df.resid 
# 1532.5   1559.2   -761.2   1522.5     1534 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.1949  0.3163  0.4676  0.5456  0.7453 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# operation (Intercept) 0.09257  0.3043  
# Number of obs: 1539, groups:  operation, 60
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    1.1651     0.1752   6.652 2.90e-11 ***
#   SeasonSpring   0.2080     0.2177   0.955    0.339    
# SeasonSummer   1.0523     0.2605   4.040 5.34e-05 ***
#   SeasonWinter  -0.2201     0.2329  -0.945    0.345    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) SsnSpr SsnSmm
# SeasonSprng -0.795              
# SeasonSummr -0.665  0.545       
# SeasonWintr -0.746  0.607  0.507

drop1(seasmod, test = "Chi") 
# Single term deletions
# 
# Model:
#   overall ~ Season + (1 | operation)
# npar    AIC    LRT   Pr(Chi)    
# <none>      1532.5                     
# Season    3 1551.6 25.068 1.494e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

DHARMa::simulateResiduals(seasmod) %>% plot # No issues
check_outliers(seasmod) # no outliers

presences <- subset(dat, overall == 1)
absences <- subset(dat, overall == 0)
evaluation <- evaluate(presences, absences, seasmod) 
plot(evaluation, "ROC") #0.674. 

## Kappa -------------------------------------------------------------------

evaluation@kappa %>% mean  #[1] 0.1256875
evaluation@kappa %>% sd #[1] 0.06268137

## TSS ---------------------------------------------------------------------

(evaluation@TPR %>% mean + evaluation@TNR %>% mean) - 1 # [1] 0.1743546

## RMSE --------------------------------------------------------------------

performance::rmse(seasmod) #[1] 0.39467



# LOOCV: Season --------------------------------------------------------

cv_preds <- list()
dffy <- list()

for (i in 1:nrow(dat)) {
  
  df <- data.frame("AUC" = NA, 
                   "RMSE" = NA, 
                   "TSS" = NA,
                   "kappa" = NA, 
                   "pred" = NA)
  cv_train <- dat[-i,] #training data (minus one obs)
  cv_test <- dat[i,] #testing data (the obs of interest)
  glmer_cv <- update(seasmod, data = cv_train) #retrain model
  presences <- subset(cv_train, overall == 1)
  absences <- subset(cv_train, overall == 0)
  evaluation <- evaluate(presences, absences, glmer_cv) #confusion matrices
  df$AUC <- evaluation@auc
  df$RMSE <- performance::rmse(glmer_cv)
  df$TSS <- (evaluation@TPR %>% mean + evaluation@TNR %>% mean) - 1
  df$kappa <- evaluation@kappa %>% mean
  df$pred <- predict(glmer_cv, cv_test) #predict on the single obs
  dffy[[i]] <- df
  cv_preds[[i]] <- df
  print(i) 
  
}

evalstats <- bind_rows(dffy)
evalstats$AUC %>% mean #[1] 0.6740082
evalstats$AUC %>% sd #[1] 0.0007059129
evalstats$RMSE %>% mean #[1] 0.3946699
evalstats$RMSE %>% sd #0.0001998075
evalstats$TSS %>% mean  #[1] 0.1735565
evalstats$TSS %>% sd  #[1] 0.001103962
evalstats$kappa %>% mean #[1] 0.1251659
evalstats$kappa %>% sd #[1] 0.0008866902
saveRDS(cv_preds, "rds/prev_loocv_results_season.RDS")


# Best model ------------------------------------------------------------------

mod <- glmer(overall ~ scale(mean_sst) * scale(mean_v) 
             + scale(mean_sst) * scale(mo_anom) 
             + scale(mon_clim_sst_sd) 
             + (1 | operation), 
             family = binomial,
             data = dat)

# General checks ----------------------------------------------------------

aics <- readRDS("rds/prev_aic_table.RDS")[1,]
aics <- aics %>% t %>% data.frame
sjPlot::plot_model(mod, type = "pred")$mon_clim_sst_sd
sjPlot::plot_model(mod, type = "pred", terms = c("mean_sst", "mean_v"))
sjPlot::plot_model(mod, type = "pred", terms = c("mean_sst", "mo_anom"))


# Summary -----------------------------------------------------------------

summary(mod) 

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: overall ~ scale(mean_sst) * scale(mean_v) + scale(mean_sst) *  
#   scale(mo_anom) + scale(mon_clim_sst_sd) + (1 | operation)
# Data: dat
# 
# AIC      BIC   logLik deviance df.resid 
# 1519.5   1562.2   -751.8   1503.5     1531 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.6512  0.2289  0.4237  0.5719  0.7661 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# operation (Intercept) 0.01583  0.1258  
# Number of obs: 1539, groups:  operation, 60
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     1.30490    0.07929  16.458  < 2e-16 ***
#   scale(mean_sst)                 0.06216    0.10884   0.571   0.5679    
# scale(mean_v)                  -0.27097    0.09098  -2.978   0.0029 ** 
#   scale(mo_anom)                 -0.09288    0.07902  -1.175   0.2398    
# scale(mon_clim_sst_sd)          0.24958    0.10884   2.293   0.0218 *  
#   scale(mean_sst):scale(mean_v)  -0.40550    0.10180  -3.983 6.80e-05 ***
#   scale(mean_sst):scale(mo_anom) -0.48575    0.10954  -4.434 9.23e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) scl(mn_s) scl(mn_v) scl(m_n) s(___) scl(mn_sst):scl(mn_)
# scl(mn_sst)           0.204                                                         
# scale(mn_v)          -0.032  0.147                                                  
# scale(m_nm)           0.022 -0.002     0.386                                        
# scl(mn_c__)          -0.133 -0.676     0.187    -0.021                              
# scl(mn_sst):scl(mn_)  0.354  0.309     0.255     0.290   -0.378                     
# scl(mn_sst):scl(m_n) -0.075  0.161     0.291     0.405   -0.156  0.466


# 5.1 Generic checks ----------------------------------------------------------

#### outliers ####

performance::check_outliers(mod) #no outliers detected

#### coeffiencts #### 

# Wald-Z test used to test for significance (i.e., P values) 
fixx <- fixef(mod) %>% as.data.frame() #extract fixed coefs
fixx$var <- row.names(fixx)
cis <- confint(mod, oldNames = F) %>% as.data.frame 
cis$var <- row.names(cis)
cis <- cis[-1,]
names(cis) <- c("low", "high", "var")
ggplot(cis) +
  geom_errorbar(mapping = aes(x = var, ymin = low, ymax = high, width = 0.1)) +
  geom_point(fixx, mapping = aes(x = var, y = .)) + 
  geom_hline(yintercept = 0, lty = "dashed") +
  coord_flip() 


#### collinearity ####

car::vif(mod) #all below 3
performance::check_collinearity(mod) %>% plot

#### singularity ####
isSingular(mod) 
#FALSE. 

#probe model for rank deficiency
m6_pca <- lme4::rePCA(mod) 
cc <- getME(mod, "cnms") 
ccc <- unlist(cc)
dimnames(m6_pca$operation[[2]]) <- list(ccc, ccc)
m6_pca
#std dev is 0.1258101 (i.e., not 0), meaning not a singular fit. 


#### Optimizer sensitivity ####

#http://svmiller.com/blog/2018/06/mixed-effects-models-optimizer-checks/
AF1 <- allFit(mod, verbose = F, maxfun = 1e5, 
              parallel = "multicore", ncpus = parallel::detectCores()-2)
is_ok <- AF1[sapply(AF1, is, "merMod")]
testy <- data.frame(optimizer = NA, loglik = NA)
for (i in 1:length(AF1)) {
  tryCatch(testy[i,]$loglik <- logLik(AF1[[i]]), 
           error = function(e) NA)
  tryCatch(testy[i,]$optimizer <- AF1[i] %>% names, 
           error = function(e) NA)
}
ggplot(testy, aes(y = loglik, x = optimizer)) +
  geom_point() +
  ylim(-500, -1000) +
  coord_flip() +
  ylab(label = "Log Likelihood (L)")
#all optimizers converge to logLik values that are identical


# Refit model in different routines, compare CIs --------------------------

#glmmTMB
library(glmmTMB) 
glmmTMB_mod <- glmmTMB(overall ~ scale(mean_sst) * scale(mean_v) 
                       + scale(mean_sst) * scale(mo_anom) 
                       + scale(mon_clim_sst_sd)  
                       + (1 | operation), 
                       data = dat, 
                       family = binomial(), 
                       REML = T)

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
m_pql <- glmmPQL(overall ~ scale(mean_sst) * scale(mean_v) 
                 + scale(mean_sst) * scale(mo_anom) 
                 + scale(mon_clim_sst_sd) ,
                 random = ~ 1 | operation, 
                 data = dat, family = binomial()) 

m_pql_mod <- broom.mixed::tidy(m_pql, conf.int = TRUE) %>%
  dplyr::select(conf.low, conf.high, estimate, term) %>%
  mutate(model = "MASS") %>%
  filter(term != "sd__(Intercept)") %>%
  filter(term != "sd_(Intercept)") %>%
  filter(term != "sd_Observation")

#rstan
library(rstanarm)
options(mc.cores = parallel::detectCores())
mstan_nb <- stan_glmer(overall ~ scale(mean_sst) * scale(mean_v) 
                       + scale(mean_sst) * scale(mo_anom) 
                       + scale(mon_clim_sst_sd) 
                       + (1|operation), 
                       data = dat, 
                       family = binomial,
                       chains = 4, iter = 2000)

stan_est <- broom.mixed::tidyMCMC(mstan_nb, conf.int = TRUE)
stan_nb <- stan_est %>%
  dplyr::select(conf.low, conf.high, estimate, term) %>%
  filter(term != "reciprocal_dispersion") %>%
  mutate(model = "rstanarm")
stan_nb[1:7,] -> stan_nb

#glmer
glmer_b <- broom.mixed::tidy(mod, conf.int = TRUE) %>%
  dplyr::select(conf.low, conf.high, estimate, term) %>%
  mutate(model = "lme4") %>%
  filter(term != "sd__(Intercept)")


#glmmADMB
library(glmmADMB)
cmod_gA_L <- glmmADMB::glmmadmb(overall ~ scale(mean_sst) * scale(mean_v) 
                                + scale(mean_sst) * scale(mo_anom) 
                                + scale(mon_clim_sst_sd)  
                                + (1 | operation),
                                data = dat, 
                                family = "binomial")
confint(cmod_gA_L) %>% as.data.frame -> admb 

admb$estimate <- NA
admb[1, 3] <- coef(cmod_gA_L)[1]
admb[2, 3] <- coef(cmod_gA_L)[2]
admb[3, 3] <- coef(cmod_gA_L)[3]
admb[4, 3] <- coef(cmod_gA_L)[4]
admb[5, 3] <- coef(cmod_gA_L)[5]
admb[6, 3] <- coef(cmod_gA_L)[6]
admb[7, 3] <- coef(cmod_gA_L)[7]
names(admb) <- c("conf.low", "conf.high", "estimate")
admb$term <- NA
admb$term <- row.names(admb)
admb$model <- "glmmADMB"

#spaMM 
library(spaMM)
m_spamm <- fitme(overall ~ scale(mean_sst) * scale(mean_v) 
                 + scale(mean_sst) * scale(mo_anom) 
                 + scale(mon_clim_sst_sd)
                 + (1|operation),
                 data = dat, 
                 family = "binomial") 
iii <- confint(m_spamm, parm = names(fixef(m_spamm)),
               boot_args=list(nsim=10, seed=123, nb_cores = 8),
               verbose = F) #ignore warnings
spamm1 <- stan_nb[1,]
spamm1 <- spamm1[1:7, ]

#intercept
spamm1$conf.low[1] <- iii[[1]]$normal[2]
spamm1$conf.high[1] <- iii[[1]]$normal[3]
spamm1$estimate[1] <- iii[[1]]$t0 %>% as.numeric
spamm1$term[1] <- rownames(iii[[1]]$normal)

#sst
spamm1$conf.high[2] <- iii[[2]]$normal[3]
spamm1$conf.low[2] <- iii[[2]]$normal[2]
spamm1$estimate[2] <- iii[[2]]$t0 %>% as.numeric
spamm1$term[2] <- rownames(iii[[2]]$normal)

#mean_v
spamm1$conf.high[3] <- iii[[3]]$normal[3]
spamm1$conf.low[3] <- iii[[3]]$normal[2]
spamm1$estimate[3] <- iii[[3]]$t0 %>% as.numeric
spamm1$term[3] <- rownames(iii[[3]]$normal)

#mo_anom
spamm1$conf.high[4] <- iii[[4]]$normal[3]
spamm1$conf.low[4] <- iii[[4]]$normal[2]
spamm1$estimate[4] <- iii[[4]]$t0 %>% as.numeric
spamm1$term[4] <- rownames(iii[[4]]$normal)

#mon_clim_sst_sd
spamm1$conf.high[5] <- iii[[5]]$normal[3]
spamm1$conf.low[5] <- iii[[5]]$normal[2]
spamm1$estimate[5] <- iii[[5]]$t0 %>% as.numeric
spamm1$term[5] <- rownames(iii[[5]]$normal)

#int1
spamm1$conf.high[6] <- iii[[6]]$normal[3]
spamm1$conf.low[6] <- iii[[6]]$normal[2]
spamm1$estimate[6] <- iii[[6]]$t0 %>% as.numeric
spamm1$term[6] <- rownames(iii[[6]]$normal)

#int2
spamm1$conf.high[7] <- iii[[7]]$normal[3]
spamm1$conf.low[7] <- iii[[7]]$normal[2]
spamm1$estimate[7] <- iii[[7]]$t0 %>% as.numeric
spamm1$term[7] <- rownames(iii[[7]]$normal)

#interaction
spamm1$model <- "spaMM"

cis_df <- bind_rows(tmb_nb2, 
                    glmer_b, 
                    stan_nb, 
                    m_pql_mod, 
                    admb, 
                    spamm1) 

ggplot(cis_df, aes(term, estimate,
                   ymin = conf.low, 
                   ymax = conf.high, 
                   colour = model)) +
  geom_pointrange(position = position_dodge(width = 0.7), 
                  fatten = 2.3) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2)



# 5.2 Residuals ---------------------------------------------------------

##Spatial autocorrelation in residuals ####

#### Correlogram ####
CORRE1 <- ncf::spline.correlog(x = dat$meanlon, 
                               y = dat$meanlat,
                               z = resid(mod, type = "pearson"),
                               xmax = 4)
plot(CORRE1, ylim = c(-0.15, 0.15),
     main = "Spline correllogram\
     +/- 95% pointwise bootstrap CIs",
     xlab = "Distance (˚)",
     ylab = "Correlation (unitless)") 
#no spatial correlation in the residuals

#### Variogram ####

#Experimenal variogram of raw data and spline correlelogram 
mydata2 <- data.frame(resid(mod, type = "pearson"), dat$meanlon, dat$meanlat)
names(mydata2) <- c("z", "x", "y")
sp::coordinates(mydata2) =~x+y
Var2 <- gstat::variogram(z ~ 1, mydata2, cutoff = 4)

p <- ggplot(data = Var2, aes(x = dist, y = gamma))
p <- p + geom_point()
p <- p + geom_smooth(method = "gam", 
                     formula = y ~ s(x, bs = "cs"),
                     colour = "black") 
p <- p + ylim(0,1.2)
p <- p + xlab("Distance (˚)") + ylab("Sample variogram") 
p #no correlation

Var2a <- gstat::variogram(z ~ 1, mydata2, cutoff = 4,
                          alpha = c(0, 45, 90, 145))
ggplot(data = Var2a,
       aes(x = dist, y = gamma)) + 
  #  geom_blank(data = data.frame(dist = 0, gamma = 0)) +
  geom_point() +
  geom_smooth(
    method = "gam", 
    formula = y ~ s(x, bs = "cs")
  ) +
  facet_wrap(~dir.hor)+
  ylab("Sample variogram") + 
  ylim(0, 1.3) + 
  theme(text = element_text(size=15)) +
  xlab("Distance (˚)")
# Slight negative anisotropy for N-S direction but not enough to 
# warrant complicating model with spatial correlation structure. 

#### Moran's I test  ####
#for spatial autocorrelation on quantile resids
dat5 <- dat 
#unique resids for each group
dat %>% 
  group_by(operation, meanlon) %>% 
  summarise(meanlon %>% mean) %>% 
  data.frame -> meany
dups <- meany[duplicated(meany$meanlon),]$operation
dat5[dat5$operation == 
       dups[1],]$meanlon <- dat5[dat5$operation == 
                                   dups[1],]$meanlon - 0.01
groupLocations = aggregate(dat5[, c("meanlon", "meanlat")], 
                           list(dat5$operation), mean)
res2 = recalculateResiduals(simulateResiduals(mod), 
                            group = dat5$operation %>% unique)
testSpatialAutocorrelation(res2, groupLocations$meanlon, 
                           groupLocations$meanlat, plot = F)
#P = 0.5378 - No spatial correlation present in data 


#### Simulate resids ####

#simulate residuals from fitted model
simres <- DHARMa::simulateResiduals(mod) 
plot(simres)
# no issues detected

# 5.3 Random intercept --------------------------------------------------

#### ICC #### 
performance::icc(mod) # Intraclass Correlation Coefficient = 0.005

#### Dotplot ####
lattice::dotplot(ranef(mod, condVar = TRUE))
#Random effects follow a normal distribution.

#### Normally distributed #### 
re <- ranef(mod)
qqnorm(re$operation[,"(Intercept)"], pch = 16)
qqline(re$operation[,"(Intercept)"]) #more-or-less normally distributed

#### Signficiance of random effect ####
#Use LRT - reduced is model with focal variance set to zero)
fully <- mod
empty <- glm(overall ~ scale(mean_sst)*scale(mean_v)  + 
               scale(mean_sst)*scale(mo_anom) + 
               scale(mon_clim_sst_sd), data = dat, family = binomial)
anova(fully, empty) # Not significant. Random effects do not contribute significantly 
#to model fit. 


# 5.4 Eval stats of model -----------------------------------------------------

## ROC curve ---------------------------------------------------------------

presences <- subset(dat, overall == 1)
absences <- subset(dat, overall == 0)
evaluation <- evaluate(presences, absences, mod) 
plot(evaluation, "ROC") #0.668. 

## Kappa -------------------------------------------------------------------

evaluation@kappa %>% mean  #0.1187516 
evaluation@kappa %>% sd #0.06138881

## TSS ---------------------------------------------------------------------

(evaluation@TPR %>% mean + evaluation@TNR %>% mean) - 1 # 0.1667536

## RMSE --------------------------------------------------------------------

performance::rmse(mod) #0.3957625


# LOOCV ---------------------------------------------------------------

# Leave-one-out CV (LOOCV)

cv_preds <- list()
dffy <- list()

for (i in 1:nrow(dat)){
  
  df <- data.frame("AUC" = NA, "RMSE" = NA, "TSS" = NA,
                   "kappa" = NA, "pred" = NA)
  cv_train <- dat[-i,] #training data (minus one obs)
  cv_test <- dat[i,] #testing data (the obs of interest)
  glmer_cv <- update(mod, data = cv_train) #retrain model
  presences <- subset(cv_train, overall == 1)
  absences <- subset(cv_train, overall == 0)
  evaluation <- evaluate(presences, absences, glmer_cv) #confusion matrices
  df$AUC <- evaluation@auc
  df$RMSE <- performance::rmse(glmer_cv)
  df$TSS <- (evaluation@TPR %>% mean + evaluation@TNR %>% mean) - 1
  df$kappa <- evaluation@kappa %>% mean
  df$pred <- predict(glmer_cv, cv_test) #predict on the single obs
  dffy[[i]] <- df
  cv_preds[[i]] <- df
  print(i) 
  
}

evalstats <- bind_rows(dffy)
evalstats$AUC %>% mean #[1] 0.6546259
evalstats$AUC %>% sd #[1] 0.0006550996
evalstats$RMSE %>% mean #[1] 0.3957629
evalstats$RMSE %>% sd #0.0002007079
evalstats$TSS %>% mean  #[1] 0.1571654
evalstats$TSS %>% sd  #[1] 0.0009820024
evalstats$kappa %>% mean #[1] 0.108919
evalstats$kappa %>% sd #[1] 0.0007565538
saveRDS(cv_preds, "rds/prev_loocv_results.RDS")

loocv_preds <- bind_rows(cv_preds) 
loocv_predictions <- loocv_preds$pred %>% as.vector


# Threshold dependent performance measures --------------------------------

# Prepare cross-validated predictions:
thresh_dat <- data.frame(
  ID = seq_len(nrow(dat)), 
  obs = dat$overall,
  pred = loocv_predictions)

# Then, we find the optimal thresholds:     
thresh_cv <- PresenceAbsence::optimal.thresholds(DATA= thresh_dat)

#confusion matrix to compare observed vs predicted PAs based on above thresholds
cmx_maxSSS <- PresenceAbsence::cmx(DATA= thresh_dat, 
                                   threshold=thresh_cv[3,2])

# Proportion of correctly classified observations
PresenceAbsence::pcc(cmx_maxSSS, st.dev=F) #0.7504873%
# Sensitivity = true positive rate
PresenceAbsence::sensitivity(cmx_maxSSS, st.dev=F) #0.8811475%
# Specificity = true negative rate
PresenceAbsence::specificity(cmx_maxSSS, st.dev=F) #0.2507837% 
# Kappa
PresenceAbsence::Kappa(cmx_maxSSS, st.dev=F) #0.1480395 
# TSS
mecofun::TSS(cmx_maxSSS) #0.1319312 

#above metrics calculate without the confusion matrix (i.e., based on continuous probabilities)
loocv_preds$TSS %>% mean #0.1571654 
loocv_preds$kappa %>% mean #0.108919
loocv_preds$RMSE %>% mean #0.3957629 


# Threshold independent statistics -------------------------------------------

loocv_preds$AUC %>% mean #CV model AUC mean, 0.6546259
roc_model <- roc(fitted(mod), as.factor(dat$overall))
plot(roc_model, col = "grey70", lwd = 2)
AUC::auc(roc_model) #0.6545146 
