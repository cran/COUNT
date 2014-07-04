\name{titanic}
\alias{titanic}
\docType{data}
\title{titanic}
\description{
The data is an observation-based version of the 1912 Titanic passenger survival 
log, 
}
\usage{data(titanic)}
\format{
  A data frame with 1316 observations on the following 4 variables.
  \describe{
    \item{\code{class}}{a factor with levels \code{1st class} \code{2nd class} \code{3rd class} \code{crew}}
    \item{\code{age}}{a factor with levels \code{child} \code{adults}}
    \item{\code{sex}}{a factor with levels \code{women} \code{man}}
    \item{\code{survived}}{a factor with levels \code{no} \code{yes}}
  }
}
\details{
titanic is saved as a data frame.
Used to assess risk ratios   
}
\source{
Found in many other texts
}
\references{
Hilbe, Joseph M (2014), Modeling Count Data, Cambridge University Press
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
Hilbe, Joseph M (2009), Logistic Regression Models, Chapman & Hall/CRC
}
\examples{

data(titanic)
titanic$survival <- titanic$survived == "yes"
glmlr <- glm(survival ~ age + sex + factor(class), family=binomial, data=titanic)
summary(glmlr)

}
\keyword{datasets}
