\name{modelfit}
\alias{modelfit}

\title{
 Fit Statistics for generalized linear models
}
\description{
modelfit is used following a glm() or glm.nb() model to produce a list of model fit statistics.
}
\usage{
modelfit(x)
}

\arguments{
  \item{x}{
the only argument is the name of the fitted glm or glm.nb function model}
}

\details{
modelfit is to be used as a post-estimation function, following the use of glm() or glm.nb(). 
}
\value{

\item{obs}{number of model observatiions}
\item{aic}{AIC statistic}
\item{xvars}{number of model predictors}
\item{rdof}{residial degrees of freedom}
\item{aic_n}{AIC, 'aic'/'obs'}
\item{ll}{log-likelihood}
\item{bic_r}{BIC - Raftery parameterization}
\item{bic_l}{BIC - log-likelihood Standard definition (Stata)}
\item{bic_qh}{Hannan-Quinn IC statistic (Limdep)}
%% ...
}
\references{
Hilbe, J.M. (2011), Negative Binomial Regression, second edition, Cambridge University Press. 

Hilbe, J.M. (2009), Logistic Regression Models, Chapman  Hall/CRC
}
\author{
Joseph M. Hilbe, Arizona State University, and 
    Jet Propulsion Laboratory, California Institute of technology 
}
\note{
modelfit.r must be loaded into memory in order to be effectve. 
Users may past modelfit.r into script editor to run, as well as load it.
}

\seealso{
\code{\link{glm}}, \code{\link[MASS]{glm.nb}}
}
\examples{
## Hilbe (2011), Table 9.17
library(MASS)
data(lbwgrp)
nb9_3 <- glm.nb(lowbw ~ smoke + race2 + race3 + offset(log(cases)), data=lbwgrp)
summary(nb9_3)
exp(coef(nb9_3))
modelfit(nb9_3) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
