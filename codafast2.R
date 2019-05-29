##### coda fast #####
#remove the burn in iterations prior to thinning
coda.fast <- function(chains, burn.in=0, thin=1, coda){

  codal <- length(coda[[1]][,1])

  # Combine chains
  Ftab <- numeric()
  for(i in 1:chains){
    Ftab <- rbind(Ftab, coda[[i]][(burn.in:(codal-1))%%thin==0,])
  }

  #mean, sd, 95% CrI table
  pred <- matrix(nrow=dim(Ftab)[2], ncol=5)
  colnames(pred)<-c("m","sd","pc2.5","pc97.5", "med")
  rownames(pred)<-colnames(coda[[1]])

  #fill table with stats
  pred[,1]<-colMeans(Ftab) #calculate predicted mean RW's
  pred[,2]<-apply(X=Ftab, MARGIN=2, FUN=sd,na.rm=T) #calculate sd, apply column wise
  pred[,3]<-apply(X=Ftab, MARGIN=2, FUN=quantile,probs=0.025, na.rm=T) 
  pred[,4]<-apply(X=Ftab, MARGIN=2, FUN=quantile,probs=0.975, na.rm=T) 
  pred[,5]<-apply(X=Ftab, MARGIN=2, FUN=quantile,probs=0.5, na.rm=T) 

  return(pred)
}

