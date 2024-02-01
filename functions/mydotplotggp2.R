
#From Zuur et al tss

MyDotplot.ggp2 <- function(Z, varx) {  
  library(ggplot2)	  
  K <- length(varx)  
  MyData <- data.frame(Y = rep(1:nrow(Z), K), 
                       X = as.vector(as.matrix(Z[, varx])),
                       Var = rep(varx, each = nrow(Z)))     
  p <- ggplot(MyData, aes(y = Y, x = X))  
  p <- p + geom_point() + ylab("Order of the data") + xlab("Range of the data") 
  p <- p + theme(text = element_text(size=15))  
  p <- p + facet_wrap(~ Var, scales = "free_x")  
  print(p)	
}