\name{nb2_syn}
\alias{nb2_syn}

\title{
 Negative binomial (NB2): generic synthetic negative binomial data and model
}
\description{
nb2_syn is a generic function for developing synthetic NB2 data and a model given
user defined specifications.
}

\usage{
nb2_syn(nobs = 50000, off = 0, alpha = 1, xv = c(1, 0.75, -1.5))
}

\arguments{
\item{nobs}{number of observations in model, Default is 50000}
\item{alpha}{NB2 heterogeneity or ancillary parameter}
\item{off}{optional: log of offset variable}
\item{xv}{predictor coefficient values. First argument is intercept. Use as
        xv =  c(intercept , x1_coef, x2_coef, ...)}
}

\details{
Create a synthetic negative binomial (NB2) regression model using the appropriate 
arguments. Model data with predictors indicated as a group with a period (.).  
Offset optional. If no offset is desired, drop "off= loff" from nb2_syn function 
call and  "+ loff" from glm.nb function call. See examples.

Data can be estimated using the glm.nb() function, or the ml.nb2() function in 
the COUNT package, or by using the gamlss function in the gamlss package, with 
"family=NBI" option.
}

\value{
\item{nby}{Negative binomial response; number of counts}
\item{sim.data}{synthetic data set}
%% ...
}

\references{
Hilbe, J.M. (2011), Negative Binomial Regression, second edition, Cambridge University Press. 
}

\author{
Andrew Robinson, Universty of Melbourne, Australia, and
Joseph M. Hilbe, Arizona State University, 
    Jet Propulsion Laboratory, California Institute of Technology 
}
\note{
%% ...
}

\seealso{
\code{\link{poisson_syn}}
}
\examples{
library(COUNT)           

sim.data <- nb2_syn(nobs = 5000, alpha = .5, xv = c(2, .75, -1.25))
mynb2 <- glm.nb(nby ~ . , data = sim.data)
summary(mynb2)
confint(mynb2)

# with offset
oset <- rep(1:5, each=1000, times=1)*100 
loff <- log(oset)   
sim.data <- nb2_syn(nobs = 5000, off = loff, alpha = .5, xv = c(1.2, -.75, .25, -1.3))
mypof <- glm.nb(nby ~ . + loff, data = sim.data)
summary(mypof)
confint(mypof)

# without offset, exponentiated coefficients, CI's
sim.data <- nb2_syn(nobs = 5000, alpha = .75, xv = c(1, .5, -1.4))
mynbf <- glm.nb(nby ~ . , data = sim.data)
exp(coef(mynbf))
exp(confint(mynbf))

\dontrun{
# default, without offset
sim.data <- nb2_syn()
dnb2 <- glm.nb(nby ~ . , data = sim.data)
summary(dnb2)
}

# use ml.nb2.r function
sim.data <- nb2_syn(nobs = 5000, alpha = .5, xv = c(2, .75, -1.25))
mynb2x <- ml.nb2(nby ~ . , data = sim.data)
mynb2x

\dontrun{
# use gamlss function for modeling data after sim.data created
library(gamlss)
sim.data <- nb2_syn(nobs = 5000, alpha = .5, xv = c(2, .75, -1.25))
gamnb <- gamlss(nby ~ ., family=NBI, data = sim.data)
gamnb
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ negative binomial }
