#Generate 95% CI for slope and r2 using the balanced bootstrap approach
# From Jack Hutchings 
# https://gist.github.com/selteaf/f08b7e774c21fbf3cd1b

library(dplyr)

lm.bal <- function(x,y,n=10000,method="ols"){
  # method can be set to ordinary least squares ("ols") or reduced major axis ("rma") and is set to ols by default
  slope.observed <- if(method=="ols"){(cov(x,y)/var(x))} else if(method=="rma"){(sign(cov(x,y))*sd(y)/sd(x))}
  int.observed <- mean(y)-slope.observed*mean(x)
  r2.observed <- cor(x,y)^2
  random <- cbind(rep(x,n-1),rep(y,n-1),sample(1:((n-1)*(length(x)))))
  random <- as.data.frame(cbind(random[order(random[,3]),][,1:2],sort(rep(1:(n-1),length(x)))))
  colnames(random) <- c("x","y","rep")
  if(method=="ols"){dist <- random %>% group_by(rep) %>% summarize(slope=cov(x,y)/var(x),intercept=mean(y)-slope*mean(x),r2=cor(x,y)^2) %>% .[,-1]}
  if(method=="rma"){dist <- random %>% group_by(rep) %>% summarize(slope=sign(cov(x,y))*sd(y)/sd(x),intercept=mean(y)-slope*mean(x),r2=cor(x,y)^2) %>% .[,-1]}
  dist[n,] <- c(slope.observed,int.observed,r2.observed)
  slope.95ci <- c(sort(dist$slope)[(n*0.025)],sort(dist$slope)[(n*0.975)])
  sig.slope <- ifelse((sign(slope.95ci[1])==sign(slope.95ci[2]))==T,"< 0.05","> 0.05")
  int.95ci <- c(sort(dist$intercept)[(n*0.025)],sort(dist$intercept)[(n*0.975)])
  r2.95ci <- c(sort(dist$r2)[(n*0.025)],sort(dist$r2)[(n*0.975)])
  #Output
  list("dist" = dist, #bootstrapped distribution
       "slope.observed" = slope.observed, #observed slope (based on chosen method)
       "slope.sig" = sig.slope, #Test for overlap of 95% with 0; significance implies non-overlap
       "slope.95ci" = slope.95ci, #95% confidence intervals for the slope
       "int.observed" = int.observed, #observed intercept
       "int.95ci" = int.95ci,  #95% confidence intervals for the intercept
       "r2.observed" = r2.observed, #observed pearson's r^2
       "r2.95ci" = r2.95ci #95% Confidence Intervals for R2
  )
}