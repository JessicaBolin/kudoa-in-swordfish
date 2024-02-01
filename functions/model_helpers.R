# Custom helper functions for modelling 
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)
# Updated Feb 2024

# drop1()  --------------------------------------------------------

droppy <- function(x) {
  drop1(x, test = "Chi")[4] %>% 
    as.data.frame() %>% 
    na.omit
}


# Prevalence --------------------------------------------------------------

# Model eval metrics 
mselect_prev <- function(x, random) {
  data.frame("model" = deparse(substitute(x)), 
             "R2c" = tryCatch(performance::r2(x) %>% 
                                as.data.frame %>% 
                                dplyr::select(R2_conditional) %>% 
                                as.numeric %>% 
                                round(4), 
                              error = function(e) NA),
             "R2m" = tryCatch(performance::r2(x) %>% 
                                as.data.frame %>% 
                                dplyr::select(R2_marginal) %>% 
                                as.numeric %>% 
                                round(4), 
                              error = function(e) NA), 
             "dev" = summary(x)[14]$AICtab[4] %>% 
               as.numeric(), 
             "AIC" = AIC(x), 
             "df" = nrow(dat)-df.residual(x), 
             "L" = logLik(x),
             "opt" = summary(x)[18]$optinfo$optimizer,
             "singular"= isSingular(x),
             "rmse" = performance::rmse(x),
             "AUC" =  tryCatch(evaluate(presences, absences, x)@auc, 
                      error = function(e) NA),
             "kappa" = tryCatch(evaluate(presences, absences, x)@kappa %>% 
                                  mean,
                                error = function(e) NA),
             "tss" = tryCatch((evaluate(presences, absences, x)@TPR %>% 
                                 mean + 
                        evaluate(presences, absences, x)@TNR %>% 
                          mean) - 1, error = function(e) NA) 
  ) 
}

# Table of metrics
models_prev <- data.frame(model = NA, 
                          R2c = NA, 
                          R2m = NA, 
                          deviance = NA, 
                          AIC = NA, 
                          df = NA, 
                          L = NA, 
                          optimizer = NA, 
                          singular = NA,
                          rmse = NA, 
                          AUC = NA, 
                          kappa = NA, 
                          tss = NA)


# Intensity ---------------------------------------------------------------


mselect <- function(x) {
  data.frame("model" = deparse(substitute(x)), #name of model
             "R2c" = tryCatch(performance::r2(x) %>% 
                                as.data.frame %>% 
                                dplyr::select(R2_conditional) %>% 
                                as.numeric %>% 
                                round(4), error = function(e) NA),
             "R2m" = tryCatch(performance::r2(x) %>% 
                                as.data.frame %>% 
                                dplyr::select(R2_marginal) %>% 
                                as.numeric %>% 
                                round(4), error = function(e) NA), 
             "dev" = summary(x)[14]$AICtab[4] %>% 
               as.numeric(),
             "AIC" = AIC(x), 
             "df" = nrow(dat) - df.residual(x),
             "L" = logLik(x),
             "offset" = family(x)[2],
             "opt" = summary(x)[18]$optinfo$optimizer,
             "singular"= isSingular(x),
             "theta" = getME(x, "glmer.nb.theta"), #theta is ~ std. dev. of variance
             "overdisp" = overdisp_fun(x)[2]) 
}

models <- data.frame(model = NA, 
                     R2c = NA, 
                     R2m = NA, 
                     deviance = NA, 
                     AIC = NA, 
                     df = NA, 
                     L = NA, 
                     #formula = NA, 
                     offset = NA, 
                     optimizer=  NA, 
                     singular = NA, 
                     theta = NA, 
                     overdisp = NA)



# Overdispersion function  ------------------------------------------------

# Written by Ben Bolker 
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ#overdispersion 

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

