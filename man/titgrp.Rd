\name{titgrp}
\alias{titgrp}
\docType{data}
\title{titgrp}
\description{
The data is a grouped version of the 1912 Titanic passenger survival 
log, with individual observations reduced to 12 cases. 
}
\usage{data(titgrp)}
\format{
  A data frame with 12 observations on the following 5 variables.
  \describe{
    \item{\code{survive}}{number of passengers who survived}
    \item{\code{cases}}{number of observations having same covariate pattern}
    \item{\code{age}}{1=adult; 0=child}
    \item{\code{sex}}{1=Male; 0=female}
    \item{\code{class}}{ticket class 1= 1st class; 2= second class; 3= third class}
   }
}
\details{
titgrp is saved as a data frame.
count response=survive; offset=log(cases); can be used as a binomial model as well.  
}
\source{
Observation level data Found in many other texts
}
\references{
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
Hilbe, Joseph M (2009), Logistic Regression Models, Chapman & Hall/CRC
}
\examples{
data(titgrp)
glmtgp <- glm(survive ~ age + sex + factor(class) + offset(log(cases)), family=poisson, data=titgrp)
summary(glmtgp)
exp(coef(glmtgp))
library(MASS)
glmtgnb <- glm.nb(survive ~ age + sex + factor(class) + offset(log(cases)), data=titgrp)
summary(glmtgnb)
exp(coef(glmtgnb))
}
\keyword{datasets}
