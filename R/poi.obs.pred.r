# library(COUNT)
# data(medpar)
# mdpar <- glm(los ~ hmo+white+type2+type3, family=poisson, data=medpar, y=TRUE, model=TRUE)

# Create a table of observed vs predicted Poisson counts, and difference
#  following glm()  :  see poi.obs.pred.Rd for usage
# See Hilbe, J.M (2011), Negative Binomial Regression, 2nd ed, Cambridge Univ Press
poi.obs.pred <- function(len, model)  {
  mu <- fitted(model)
  avgp <- sapply(0:len, function(i)
                        mean(exp(-mu)*(mu^i)/factorial(i)))
  trun.y <- model$y[model$y < len+1]
  propObsv <- table(trun.y) / length(trun.y)
  Diff <- c(0,propObsv)*100 - avgp[1:(len+1)]*100
  data.frame(LOS=0:len, ObsProp=c(0,propObsv)*100, avgp*100, Diff)
}

# poi.obs.pred(len=25, model=mdpar)