\name{smoking}
\alias{smoking}
\docType{data}
\title{smoking}
\description{
A simple data set with only 6 observations. 
}
\usage{data(smoking)}
\format{
  A data frame with 6 observations on the following 4 variables.
  \describe{
    \item{\code{sbp}}{systolic blood pressure of subject}
    \item{\code{male}}{1=male; 0=female}
    \item{\code{smoker}}{1=hist of smoking; 0= no hist of smoking}
    \item{\code{age}}{age of subject}
    }
}
\details{
smoking is saved as a data frame.
}
\source{
none 
}

\references{
Hilbe, Joseph M (2014), Modeling Count Data, Cambridge University Press
} 
\examples{
sbp    <- c(131,132,122,119,123,115)
male   <- c(1,1,1,0,0,0)
smoker <- c(1,1,0,0,1,0)
age    <- c(34,36,30,32,26,23)
summary(reg1 <- lm(sbp~ male+smoker+age))
}
\keyword{datasets}


