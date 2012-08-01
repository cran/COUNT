\name{nbc_syn}
\alias{nbc_syn}

\title{
 Negative binomial (NB-C): generic synthetic canonical negative binomial data and model
}
\description{
nbc_syn is a generic function for developing synthetic NB-C data and a model given
user defined specifications.
}

\usage{
nbc_syn(nobs=50000, alpha=1.15, xv = c(-1.5, -1.25, -.1))
}

\arguments{
\item{nobs}{number of observations in model, Default is 50000}
\item{alpha}{NB-C heterogeneity or ancillary parameter}
\item{xv}{predictor coefficient values. First argument is intercept. Use as
        xv =  c(intercept , x1_coef, x2_coef, ...)}
} 

\details{
Create a synthetic canonial negative binomial (NB-C) regression model using the 
appropriate arguments. Model data with predictors indicated as a group with 
a period (.). Data can be modeled using the ml.nbc.r function in the COUNT 
package. See examples.
}

\value{
\item{nbcy}{Canonical negative binomial (NB-C) response; number of counts}
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
\code{\link{nb2_syn}},   \code{\link{nb1_syn}}
}
\examples{

\dontrun{
sim.data <- nbc_syn(nobs = 50000, alpha = 1.15, xv = c(-1.5, -1.25, -.1))
mynbc <- ml.nbc(nbcy ~ . , data = sim.data)
mynbc

# default
sim.data <- nbc_syn()
dnbc <- ml.nbc(nbcy ~ . , data = sim.data)
dnbc
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ negative binomial }
