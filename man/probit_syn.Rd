\name{probit_syn}
\alias{probit_syn}

\title{
 Probit regression : generic synthetic binary/binomial probit data and model
}

\description{
probit_syn is a generic function for developing synthetic probit regression data and 
a model given user defined specifications.
}

\usage{
probit_syn(nobs=50000, d=1,  xv = c(1, 0.5, -1.5))
}

\arguments{
\item{nobs}{number of observations in model, Default is 50000}
\item{d}{binomial denominator, Default is 1, a binary probit model. May 
            use a variable containing different denominator values.} 
\item{xv}{predictor coefficient values. First argument is intercept. Use as
        xv =  c(intercept , x1_coef, x2_coef, ...)}
}

\details{
Create a synthetic probit regression model using the appropriate arguments. 
Binomial denominator must be declared. For a binary probit model, d=1. A 
variable may be used as the denominator when values differ. See examples.
}

\value{
\item{py}{binomial probit numerator; number of successes}
\item{sim.data}{synthetic data set}
%% ...
}

\references{
Hilbe, J.M. (2011), Negative Binomial Regression, second edition, Cambridge University Press. 
Hilbe, J.M. (2009), Logistic Regression Models, Chapman & Hall/CRCD
}
\author{
Joseph M. Hilbe, Arizona State University, and 
    Jet Propulsion Laboratory, California Institute of Technology 
Andrew Robinson, Universty of Melbourne, Australia.
}

\seealso{
\code{\link{logit_syn}}
}
\examples{
library(COUNT)           

# Binary probit regression (denominator=1)
sim.data <-probit_syn(nobs = 5000, d = 1, xv = c(1, .5, -1.5))
myprobit <- glm(cbind(py,dpy) ~ ., family=binomial(link="probit"), data = sim.data)
summary(myprobit)
confint(myprobit)

# Binary probit regression with 3 predictors (denominator=1)
sim.data <-probit_syn(nobs = 5000, d = 1, xv = c(1, .75, -1.5, 1.15))
myprobit <- glm(cbind(py,dpy) ~ ., family=binomial(link="probit"), data = sim.data)
summary(myprobit)
confint(myprobit)

# Binomial or grouped probit regression with defined denominator, den
den <- rep(1:5, each=1000, times=1)*100
sim.data <- probit_syn(nobs = 5000, d = den, xv = c(1, .5, -1.5))
gpy <- glm(cbind(py,dpy) ~ ., family=binomial(link="probit"), data = sim.data)
summary(gpy)

\dontrun{
# default
sim.data <- probit_syn()
dprobit <- glm(cbind(py,dpy) ~ . , family=binomial(link="probit"), data = sim.data)
summary(dprobit)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ probit }
\keyword{ binomial }


