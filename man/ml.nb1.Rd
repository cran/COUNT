\name{ml.nb1}

\alias{ml.nb1}

\title{
 NB1: maximum likelihood linear negative binomial regression
}

\description{
ml.nb1 is a maximum likelihood function for estimating linear 
negative binomial (NB1) data. Output consists of a table of parameter 
estimates, standard errors, z-value, and confidence intervals.
}

\usage{
ml.nb1(formula, data, offset=0, start=NULL, verbose=FALSE)
}

\arguments{
  \item{formula}{
  an object of class '"formula"': a symbolic description of the 
  model to be fitted.  The details of model specification are given under
   'Details'.}
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
ml.nb1 is used like glm.nb, but without saving ancillary statistics. 
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
\code{\link[MASS]{glm.nb}}, \code{\link{ml.nbc}}, \code{\link{ml.nb2}}
}

\examples{
# Table 10.8, Hilbe. J.M. (2011), Negative Binomial Regression, 
#   2nd ed. Cambridge University Press (adapted)
data(medpar)
medpar$type <- factor(medpar$type)
med.nb1 <- ml.nb1(los ~ hmo + white + type, data = medpar)
med.nb1
}

\keyword{ models }
