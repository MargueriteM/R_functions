
###################################### PRINT LINEAR EQUATION TO GGPLOT #######################################
# from: http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph   #
# 18 November 2014                                                                                           #
##############################################################################################################

##################### Define function which prints the equation of the geom_line to the figure ##############
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}
##############################################################################################################

# then call: 
 p <- ggplot(df, aes(x=x, y=y))+geom_point()+geom_smooth(method='lm', alpha=0.1)

p + annotate("text", x=5, y=500, label=lm_eqn(lm(y~x, df)),colour="black", size=5,parse=T)

