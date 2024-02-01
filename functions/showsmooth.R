# Jessica Bolin
#Adapted from `MyMultipanel.ggp2()` from from HighstatLibV10,  Zuur et al 2009.
#Mixed effects models and extensions in ecology with R. (2009).
#Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer.
#Copyright Highland Statistics LTD.

showsmooth <- function(Z, varx, vary, 
                       ylab = "Response variable",
                       addSmoother = FALSE,
                       addRegressionLine = FALSE,
                       addHorizontalLine = FALSE) {
  K <- length(varx)
  MyData <- data.frame(Y = rep(as.vector(as.matrix(Z[,vary])), K),
                       X = as.vector(as.matrix(Z[, varx])),
                       Var = rep(varx, each = nrow(Z))) 
  library(ggplot2)
  p <- ggplot(MyData, aes(y = Y %>% log10, x = X))
  p <- p + geom_point(cex=0.5) + ylab(ylab) + xlab("Covariates")
  p <- p + theme(text = element_text(size=15))
  if (addSmoother == TRUE) {
    p <- p + geom_smooth(se = TRUE, col = "blue", lwd = 1)
  }
  if (addRegressionLine == TRUE) {
    p <- p + geom_smooth(se = TRUE, col = "blue", lwd = 1, method = "lm")
  }
  if (addRegressionLine == TRUE) {
    p <- p + geom_smooth(se = TRUE, col = "blue", lwd = 1, method = "lm")
  }
  if (addHorizontalLine == TRUE) {
    p <- p + geom_hline(yintercept = 0)
  }
  p <- p + facet_wrap(~ Var, scales = "free_x")
  suppressMessages(print(p)) 	
}