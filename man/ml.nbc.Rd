\name{ml.nbc}

\alias{ml.nbc}

\title{
 NBC: maximum likelihood linear negative binomial regression
}

\description{
ml.nbc is a maximum likelihood function for estimating canonical linear 
negative binomial (NB-C) data. 
}

\usage{
ml.nbc(formula, data, start=NULL, verbose=FALSE)
}

\arguments{
  \item{formula}{
  an object of class '"formula"': a symbolic description of the 
  model to be fitted.  The details of model specification are given under
   'Details'.}
 \item{data}{ 
  a mandatory data frame containing the variables in the model.
}
  \item{start}{
 an optional vector of starting values for the parameters.
}
 \item{verbose}{
a logical flag to indicate whether the fit information should be printed.
}
}

\details{
ml.nbc is used like glm.nb, but without saving ancillary statistics. 
}

\value{
The function returns a dataframe with the following components:

 \item{Estimate }{ML estimate of the parameter}
 \item{SE }{Asymptotic estimate of the standard error of the estimate
 of the parameter}
 \item{Z }{The Z statistic of the asymptotic hypothesis test that the
 population value for the parameter is 0.}
 \item{LCL }{Lower 95\% confidence interval for the parameter estimate.}
 \item{UCL }{Upper 95\% confidence interval for the parameter estimate.}

}

\references{
Hilbe, J.M. (2011), Negative Binomial Regression, second edition, Cambridge University Press. 
}

\author{
Andrew Robinson, Universty of Melbourne, Australia, and
Joseph M. Hilbe, Arizona State University, and 
    Jet Propulsion Laboratory, California Institute of Technology 
}

\seealso{
\code{\link[MASS]{glm.nb}}, \code{\link{ml.nb1}}, \code{\link{ml.nb2}}
}

\examples{
# Table 10.12, Hilbe. J.M. (2011), Negative Binomial Regression, 
#   2nd ed. Cambridge University Press (adapted)

\dontrun{
data(medpar)
nobs <- 50000
x2 <- runif(nobs)
x1 <- runif(nobs)
xb <- 1.25*x1 + .1*x2 - 1.5
mu <- 1/(exp(-xb)-1)
p <- 1/(1+mu)
r <- 1
gcy <- rnbinom(nobs, size=r, prob = p)
test <- data.frame(gcy, x1, x2)
nbc <- ml.nbc(gcy ~ x1 + x2, data=test)
nbc
}
}

\keyword{ models }
