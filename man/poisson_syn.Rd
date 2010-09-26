\name{poisson_syn}
\alias{poisson_syn}

\title{
 Poisson : generic synthetic Poisson data and model
}
\description{
poisson_syn is a generic function for developing synthetic Poisson data and a model given
user defined specifications.
}

\usage{
poisson_syn(nobs = 50000, off = 0, xv = c(1, -.5,  1))
}

\arguments{
\item{nobs}{number of observations in model, Default is 50000}
\item{off}{optional: log of offset variable}
\item{xv}{predictor coefficient values. First argument is intercept. Use as xv =  c(intercept , x1_coef, x2_coef, ...)}
}

\details{
Create a synthetic Poisson regression model using the appropriate arguments. 
Offset optional. Model data with predictors indicated as a group with a 
period (.).  See examples.
}

\value{
\item{py}{Poisson response; number of counts}
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

\note{
%% ...
}

\seealso{
\code{\link{nb2_syn}}
}
\examples{
library(COUNT)

# standard Poisson model with two predictors and intercept
sim.data <- poisson_syn(nobs = 5000, xv = c(2, .75, -1.25))
mypo <- glm(py ~ . , family=poisson, data = sim.data)
summary(mypo)
confint(mypo)

# Poisson with offset and three predictors
oset <- rep(1:5, each=1000, times=1)*100 
loff <- log(oset)   
sim.data <- poisson_syn(nobs = 5000, off = loff, xv = c(1.2, -.75, .25, -1.3))
mypof <- glm(py ~ . + loff, family=poisson, data = sim.data)
summary(mypof)
confint(mypof)

# Poisson without offset, exponentiated coefficients, CI's
sim.data <- poisson_syn(nobs = 5000, xv = c(2, .75, -1.25))
mypo <- glm(py ~ . , family=poisson, data = sim.data)
exp(coef(mypo))
exp(confint(mypo))

\dontrun{
# default (without offset)
sim.data <- poisson_syn()
dmypo <- glm( py ~ . , family=poisson, data = sim.data)
summary(dmypo)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ Poisson }

