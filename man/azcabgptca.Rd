\name{azcabgptca}
\alias{azcabgptca}
\docType{data}
\title{azcabgptca}
\description{
Random subset of the 1991 Arizona Medicare data for patients hospitalized 
subsequent to undergoing  a CABG (DRGs 106, 107) or PTCA (DRG 112) 
cardiovascular procedure. 
}
\usage{data(azcabgptca)}
\format{
  A data frame with 1959 observations on the following 6 variables.
  \describe{
    \item{\code{died}}{systolic blood pressure of subject}
    \item{\code{procedure}}{1=CABG; 0=PTCA}
    \item{\code{gender}}{1=male; 0=female}
    \item{\code{age}}{age of subject}
    \item{\code{los}}{hospital length of stay}
    \item{\code{type}}{1=emerg/urgent; 0=elective}
    }
}
\details{
azcabgptca is saved as a data frame.
}
\source{
Hilbe, Negative Binomial Regression, 2nd ed, Cambridge Univ Press
}
\references{
Hilbe, Joseph M (2014), Modeling Count Data, Cambridge University Press
} 
\examples{

data(azcabgptca); attach(azcabgptca)
table(los); table(procedure, type); table(los, procedure)
summary(los)
summary(c91a <- glm(los ~ procedure+ type, family=poisson, data=azcabgptca))
modelfit(c91a)
summary(c91b <- glm(los ~ procedure+ type, family=quasipoisson, data=azcabgptca))
modelfit(c91b)
library(sandwich)
sqrt(diag(vcovHC(c91a, type="HC0")))
}
\keyword{datasets}




