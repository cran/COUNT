\name{badhealth}
\alias{badhealth}
\docType{data}
\title{badhealth}
\description{
From German health survey data for the year 1998 only. 
}
\usage{data(badhealth)}
\format{
  A data frame with 1,127 observations on the following 3 variables.
  \describe{
    \item{\code{numvisit}}{number of visits to doctor during 1998}
    \item{\code{badh}}{1=patient claims to be in bad health; 0=not in bad health}
    \item{\code{age}}{age of patient: 20-60}
  }
}
\details{
badhealth is saved as a data frame.
Count models use numvisit as the response variable, 0 counts are included.
}
\source{
German Health Survey, amended in Hilbe and Greene (2008).
}
\references{
Hilbe, Joseph M (2011), Negative Binomial Regression, Cambridge University Press
Hilbe, J. and W. Greene (2008). Count Response Regression Models, in ed. 
  C.R. Rao, J.P Miller, and D.C. Rao, Epidemiology and Medical Statistics, 
  Elsevier Handbook of  Statistics Series. London, UK: Elsevier.
}
\examples{
data(badhealth)
glmbadp <- glm(numvisit ~ badh + age, family=poisson, data=badhealth)
summary(glmbadp)
exp(coef(glmbadp))
library(MASS)
glmbadnb <- glm.nb(numvisit ~ badh + age, data=badhealth)
summary(glmbadnb)
exp(coef(glmbadnb))
}
\keyword{datasets}
