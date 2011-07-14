\name{ml.pois}

\alias{ml.pois}

\title{
 NB2: maximum likelihood Poisson regression
}

\description{
  ml.pois is a maximum likelihood function for estimating
Poisson data. Output consists of a table of parameter estimates,
standard errors, z-value, and confidence intervals. An offset may be
declared as an option.
}

\usage{
ml.pois(formula, data, offset=0, start=NULL, verbose=FALSE)
}

\arguments{
  \item{formula}{
  an object of class '"formula"': a symbolic description of the 
  model to be fitted. }
 \item{data}{ 
  a mandatory data frame containing the variables in the model.
}
 \item{offset}{ 
   this can be used to specify an _a priori_ known component to
   be included in the linear predictor during fitting.  The offset
  should be provided on the log scale.
}
  \item{start}{
 an optional vector of starting values for the parameters.
}
 \item{verbose}{
a logical flag to indicate whether the fit information should be printed.
}
}

\details{
  ml.pois is used like glm, but does not provide ancillary statistics.
}

\value{
The function returns a dataframe with the following components:

 \item{Estimate }{ML estimate of the parameters}
 \item{SE }{Asymptotic estimate of the standard error of the estimate
 of the parameter}
 \item{Z }{The Z statistic of the asymptotic hypothesis test that the
 population value for the parameter is 0.}
 \item{LCL }{Lower 95\% confidence interval for the parameter estimates.}
 \item{UCL }{Upper 95\% confidence interval for the parameter estimates.}

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
\code{\link{glm.nb}}, \code{\link{ml.nbc}}, \code{\link{ml.nb1}}
}

\examples{
# Table 8.7, Hilbe. J.M. (2011), Negative Binomial Regression, 
#   2nd ed. Cambridge University Press (adapted)
data(medpar)
medpar$type <- factor(medpar$type)
med.pois <- ml.pois(los ~ hmo + white + type, data = medpar)
med.pois

data(rwm5yr)
lyear <- log(rwm5yr$year)
rwm.poi <- ml.pois(docvis ~ outwork + age + female, offset=lyear, data =
rwm5yr)
rwm.poi
exp(rwm.poi$Estimate)
exp(rwm.poi$LCL)
exp(rwm.poi$UCL)

}

\keyword{ models }
