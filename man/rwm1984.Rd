\name{rwm1984}
\alias{rwm1984}
\docType{data}
\title{rwm1984}
\description{
German health registry for the year 1984. 
}
\usage{data(rwm1984)}
\format{
  A data frame with 3,874 observations on the following 17 variables.
  \describe{
   \item{\code{docvis}}{number of visits to doctor during year (0-121)}
   \item{\code{hospvis}}{number of days in hospital during year (0-51)}
%   \item{\code{year}}{year; (categorical: 1984, 1985, 1986, 1987, 1988)}
   \item{\code{edlevel}}{educational level (categorical: 1-4)}
   \item{\code{age}}{age: 25-64}
   \item{\code{outwork}}{out of work=1; 0=working}
   \item{\code{female}}{female=1; 0=male}
   \item{\code{married}}{married=1; 0=not married}
   \item{\code{kids}}{have children=1; no children=0}
   \item{\code{hhninc}}{household yearly income in marks (in Marks)}
   \item{\code{educ}}{years of formal education (7-18)}
   \item{\code{self}}{self-employed=1; not self employed=0}
   \item{\code{edlevel1}}{(1/0) not high school graduate}
   \item{\code{edlevel2}}{(1/0) high school graduate}
   \item{\code{edlevel3}}{(1/0) university/college} 
   \item{\code{edlevel4}}{(1/0) graduate school}
  }
}
\details{
rwm1984 is saved as a data frame.
Count models typically use docvis as response variable. 0 counts are included
}
\source{
German Health Reform Registry, year=1984, in Hilbe and Greene (2007)
}
\references{
Hilbe, Joseph, M (2014), Modeling Count Data, Cambridge University Press
Hilbe, Joseph M (2011), Negative Binomial Regression, Cambridge University Press
Hilbe, J. and W. Greene (2008). Count Response Regression Models, in ed. 
  C.R. Rao, J.P Miller, and D.C. Rao, Epidemiology and Medical Statistics, 
  Elsevier Handbook of  Statistics Series. London, UK: Elsevier.
}
\examples{
library(MASS)
library(msme)
data(rwm1984)

glmrp <- glm(docvis ~ outwork + female + age + factor(edlevel), family=poisson, data=rwm1984)
summary(glmrp)
exp(coef(glmrp))

summary(nb2 <- nbinomial(docvis ~ outwork + female + age + factor(edlevel), data=rwm1984))
exp(coef(nb2))

summary(glmrnb <- glm.nb(docvis ~ outwork + female + age + factor(edlevel), data=rwm1984))
exp(coef(glmrnb))
}
\keyword{datasets}
