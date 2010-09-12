\name{loomis}
\alias{loomis}
\docType{data}
\title{loomis}
\description{
Data are taken from Loomis (2003). The study relates to a survey taken on reported 
frequency of visits to national parks during the year. The survey was taken at park 
sites, thus incurring possible effects of endogenous stratification.  
}
\usage{data(loomis)}
\format{
  A data frame with 410 observations on the following 11 variables.
  \describe{
   \item{\code{anvisits}}{number of annual visits to park}
   \item{\code{gender}}{1=male;0=female}
   \item{\code{income}}{income in US dollars per year, categorical: 4 levels}
   \item{\code{income1}}{<=$25000}
   \item{\code{income2}}{>$25000 - $55000}
   \item{\code{income3}}{>$55000 - $95000}
   \item{\code{income4}}{>$95000}
   \item{\code{travel}}{travel time, categorical: 3 levels}
   \item{\code{travel1}}{<.25 hrs}
   \item{\code{travel2}}{>=.25 - <4 hrs}
   \item{\code{travel3}}{>=4 hrs}
  }
}
\details{
loomis is saved as a data frame.
Count models typically use anvisits as response variable. 0 counts are included
}
\source{
from Loomis (2003)
}
\references{
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
Loomis, J. B. (2003). Travel cost demand model based river recreation benefit 
  estimates with on-site and household surveys: Comparative results and a 
  correction procedure, Water Resources Research, 39(4): 1105
}
\examples{
data(loomis)
glmlmp <- glm(anvisits ~ gender + factor(income) + factor(travel), family=poisson, data=loomis)
summary(glmlmp)
exp(coef(glmlmp))
library(MASS)
glmlmnb <- glm.nb(anvisits ~ gender + factor(income) + factor(travel), data=loomis)
summary(glmlmnb)
exp(coef(glmlmnb))
}
\keyword{datasets}
