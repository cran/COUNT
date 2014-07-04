\name{logit_syn}
\alias{logit_syn}

\title{
 Logistic regression : generic synthetic binary/binomial logistic data and model
}
\description{
logit_syn is a generic function for developing synthetic logistic regression data and 
a model given user defined specifications.
}

\usage{
logit_syn(nobs=50000, d=1,  xv  = c(1, 0.5, -1.5))
}

\arguments{
\item{nobs}{number of observations in model, Default is 50000}
\item{d}{binomial denominator, Default is 1, a binary logistic model. May 
            use a variable containing different denominator values.} 
\item{xv}{predictor coefficient values. First argument is intercept. Use as
        xv =  c(intercept , x1_coef, x2_coef, ...)}
}

\details{
Create a synthetic logistic regression model using the appropriate arguments. 
Binomial denominator must be declared. For a binary logistic model, d=1. A 
variable may be used as the denominator when values differ. See examples.
}

\value{
\item{by}{binomial logistic numerator; number of successes}
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
\code{\link{probit_syn}}
}
\examples{
# Binary logistic regression (denominator=1)
sim.data <-logit_syn(nobs = 500, d = 1, xv = c(1, .5, -1.5))
mylogit <- glm(cbind(by,dby) ~ ., family=binomial(link="logit"), data = sim.data)
summary(mylogit)
confint(mylogit)

# Binary logistic regression with odds ratios (denominator=1); 3 predictors
sim.data <-logit_syn(nobs = 500, d = 1, xv = c(1, .75, -1.5, 1.15))
mylogit <- glm(cbind(by,dby) ~ ., family=binomial(link="logit"), data = sim.data)
exp(coef(mylogit))
exp(confint(mylogit))

# Binomial or grouped logistic regression with defined denominator, den
den <- rep(1:5, each=100, times=1)*100
sim.data <- logit_syn(nobs = 500, d = den, xv = c(1, .5, -1.5))
gby <- glm(cbind(by,dby) ~ ., family=binomial(link="logit"), data = sim.data)
summary(gby)

\dontrun{
# default
sim.data <- logit_syn(nobs=500, d=1,  xv = c(2, -.55, 1.15))
dlogit <- glm(cbind(by,dby) ~ . , family=binomial(link="logit"), data = sim.data)
summary(dlogit)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ logit }
\keyword{ binomial }


