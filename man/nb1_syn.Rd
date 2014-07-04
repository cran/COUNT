\name{nb1_syn}
\alias{nb1_syn}

\title{
 Negative binomial (NB1): generic synthetic linear negative binomial data and model
}
\description{
nb1_syn is a generic function for developing synthetic NB1 data and a model given
user defined specifications.
}

\usage{
nb1_syn(nobs=50000, delta=1, xv = c(1, 0.75, -1.25))
}

\arguments{
\item{nobs}{number of observations in model, Default is 50000}
\item{delta}{NB1 heterogeneity or ancillary parameter}
\item{xv}{predictor coefficient values. First argument is intercept. Use as
        xv =  c(intercept , x1_coef, x2_coef, ...)}
}

\details{
Create a synthetic linear negative binomial (NB1) regression model using the 
appropriate arguments. Model data with predictors indicated as a group with 
a period (.). See examples.
 
Data can be modeled using the ml.nb1.r function in the COUNT package, or by using the 
gamlss function in the gamlss package, using the "family=NBII" option. 
}

\value{
\item{nb1y}{Negative binomial (NB1) response; number of counts}
\item{sim.data}{synthetic data set}
%% ...
}

\references{
Hilbe, J.M. (2011), Negative Binomial Regression, second edition, Cambridge University Press. 
}

\author{
Joseph M. Hilbe, Arizona State University, and 
    Jet Propulsion Laboratory, California Institute of Technology 
Andrew Robinson, Universty of Melbourne, Australia.
}

\seealso{
\code{\link{nb2_syn}},   \code{\link{nbc_syn}}
}

\examples{

sim.data <- nb1_syn(nobs = 5000, delta = .5, xv = c(.5, 1.25, -1.5))
mynb1 <- ml.nb1(nb1y ~ . , data = sim.data)
mynb1

\dontrun{
# use gamlss to model NB1 data
library(gamlss)         
sim.data <- nb1_syn(nobs = 5000, delta = .5, xv = c(.5, 1.25, -1.5))
mynb1 <- gamlss( nb1y ~ . , family=NBII, data = sim.data)
mynb1
}

\dontrun{
# default
sim.data <- nb1_syn()
dnb1 <- ml.nb1(nb1y ~ . , data = sim.data)
dnb1
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ negative binomial }
