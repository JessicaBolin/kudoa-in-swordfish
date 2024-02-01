#Boxplots to check for collinearity between a continuous variable and levels of a factor
#e.g., Mybwplot(dat, "domain2", MyVar = c("vcur", "temp", "eke", "seamount", "mld")) 
#code from Zuur et al. 

Mybwplot <- function(Z, MyVar, TargetVar){
  #Multipanel boxplots
  #Z: data set
  #MyVar: character string
  #TargetVar: variable for the x-axis..must be a factor
  
  AllY <- as.vector(as.matrix(Z[,MyVar]))
  AllX <- rep(Z[,TargetVar], length(MyVar))
  ID <- rep(MyVar, each = nrow(Z))
  
  P <- bwplot(AllY ~ factor(AllX) | ID, horizontal = FALSE,
              ylab = "", xlab = "",
              scales = list(alternating = T,cex.lab = 1.5,
                            x = list(relation = "same",rot =90, abbreviate = TRUE, cex = 1.5),
                            y = list(relation = "free", draw = FALSE)),
              strip = strip.custom(bg = 'white',
                                   par.strip.text = list(cex = 1.2)),
              cex = .5,
              par.settings = list(
                box.rectangle = list(col = 1),
                box.umbrella  = list(col = 1),
                plot.symbol   = list(cex = .5, col = 1)))
  print(P)
}