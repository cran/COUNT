# library(COUNT)
# data(medpar)
# mdpar <- glm.nb(los ~ hmo+white+type2+type3, data=medpar, y=TRUE, model=TRUE)

nb2.obs.pred <- function(len, model)  {
  mu <- fitted(model)
  alpha <- 1/model$theta
  amu <- alpha*mu
  avgp <- sapply(0:len, function(i)
     mean( exp(i*log(amu/(1+amu)) - (1/alpha)*log(1+amu)  + 
     log( gamma(i + 1/alpha) ) - log( gamma(i + 1) ) - 
     log( gamma(1 / alpha)))))
  trun.y <- model$y[model$y < len+1]
  propObsv <- table(trun.y) / length(trun.y)
  Diff <- c(0,propObsv)*100 - avgp[1:(len+1)]*100
  data.frame(LOS=0:len, ObsProp=c(0,propObsv)*100, avgp*100, Diff)
}

# nb2.obs.pred(len=25, model=mdpar)








