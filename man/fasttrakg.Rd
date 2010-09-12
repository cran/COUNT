\name{fasttrakg}
\alias{fasttrakg}
\docType{data}
\title{
fasttrakg
}
\description{
%%  ~~ Description of the dataset. ~~
Data are from the Canadian National Cardiovascular Disease registry 
called, FASTRAK. years covered at 1996-1998. They have been grouped 
by covariate patterns from individual observations. 
}
\usage{data(fasttrakg)}
\format{
  A data frame with 15 observations on the following 9 variables.
  \describe{
    \item{\code{die}}{number died from MI}
    \item{\code{cases}}{number of cases with same covariate pattern}
    \item{\code{anterior}}{1=anterior site MI; 0=inferior site MI}
    \item{\code{hcabg}}{1=history of CABG; 0=no history of CABG}
    \item{\code{killip}}{Killip level of cardiac event severity (1-4)}
    \item{\code{kk1}}{(1/0) angina; not MI}
    \item{\code{kk2}}{(1/0) moderate severity cardiac event}
    \item{\code{kk3}}{(1/0) Severe cardiac event}
    \item{\code{kk4}}{(1/0) Severe cardiac event; death}
 }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
fasttrakg is saved as a data frame.
Count models use died as response numerator and cases as the demoninator  
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
1996-1998 FASTRAK data, Hoffman-LaRoche Canada,
National Health Economics & Research Co.
}
\references{
%%  ~~ found in following texts ~~
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
Hilbe, Joseph M (2009), Logistic Regression Models, Chapman & Hall/CRC
}
\examples{
 data(fasttrakg)
glmfp <- glm(die ~ anterior + factor(killip) + offset(log(cases)), family=poisson, data=fasttrakg)
summary(glmfp)
exp(coef(glmfp))
#glmfnb < -glm.nb(die ~ anterior + factor(killip) + offset(log(cases)), data=fasttrakg)
#summary(glmfnb)
#exp(coef(glmfnb))
}
\keyword{datasets}
