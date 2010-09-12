\name{lbw}
\alias{lbw}
\docType{data}
\title{lbw}
\description{
The data come to us from Hosmer and Lemeshow (2000). Called the low 
birth weight (lbw) data, the response is a binary variable, low, 
which indicates whether the birth weight of a baby is under 2500g 
(low=1), or over (low=0). 
}
\usage{data(lbw)}
\format{
  A data frame with 189 observations on the following 10 variables.
  \describe{
    \item{\code{low}}{1=low birthweight baby; 0=norml weight}
    \item{\code{smoke}}{1=history of mother smoking; 0=mother nonsmoker}
    \item{\code{race}}{categorical 1-3: 1=white; 2-=black; 3=other}
    \item{\code{age}}{age of mother: 14-45}
    \item{\code{lwt}}{weight (lbs) at last menstrual period: 80-250 lbs}
    \item{\code{ptl}}{number of false of premature labors: 0-3}
    \item{\code{ht}}{1=history of hypertension; 0 =no hypertension}
    \item{\code{ui}}{1=uterine irritability; 0 no irritability}
    \item{\code{ftv}}{number of physician visits in 1st trimester: 0-6}
    \item{\code{bwt}}{birth weight in grams: 709 - 4990 gr}
  }
}
\details{
lbw is saved as a data frame.
Count models can use ftv as a response variable, or convert it to grouped format
}
\source{
Hosmer, D and S. Lemeshow (2000), Applied Logistic Regression, Wiley 
}
\references{
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
Hilbe, Joseph M (2009), Logistic Regression Models, Chapman & Hall/CRC
}
\examples{
data(lbw)
glmbwp <- glm(ftv ~ low + smoke + factor(race), family=poisson, data=lbw)
summary(glmbwp)
exp(coef(glmbwp))
library(MASS)
glmbwnb <- glm.nb(ftv ~ low + smoke + factor(race), data=lbw)
summary(glmbwnb)
exp(coef(glmbwnb))
}
\keyword{datasets}
