\name{azprocedure}
\alias{azprocedure}
\docType{data}
\title{azprocedure}
\description{
Data come from the 1991 Arizona cardiovascular patient files. A subset of the 
fields was selected to model the differential length of stay for patients entering 
the hospital to receive one of two standard cardiovascular procedures: CABG and PTCA. 
CABG is the standard acronym for Coronary Artery Bypass Graft, where the flow of 
blood in a diseased or blocked coronary artery or vein has been grafted to bypass 
the diseased sections. PTCA, or Percutaneous Transluminal Coronary Angioplasty, is 
a method of placing a balloon in a blocked coronary artery to open it to blood flow. 
It is a much less severe method of treatment for those having coronary blockage, with 
a corresponding reduction in risk. 
}
\usage{data(azprocedure)}
\format{
  A data frame with 3589 observations on the following 6 variables.
  \describe{
    \item{\code{los}}{length of hospital stay}
    \item{\code{procedure}}{1=CABG;0=PTCA}
    \item{\code{sex}}{1=Male; 0=female}
    \item{\code{admit}}{1=Urgent/Emerg; 0=elective (type of admission)}
    \item{\code{age75}}{1= Age>75; 0=Age<=75}
    \item{\code{hospital}}{encrypted facility code (string)}
  }
}
\details{
azprocedure is saved as a data frame.
Count models use los as response variable. 0 counts are structurally excluded  
}
\source{
1991 Arizona Medpar data, cardiovascular patient files, 
National Health Economics & Research Co.
}
\references{
Hilbe, Joseph M (2014), Modeling Count Data, Cambridge University Press
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
Hilbe, Joseph M (2009), Logistic Regression Models, Chapman & Hall/CRC
}
\examples{
library(MASS)
library(msme)

data(azprocedure)

glmazp <- glm(los ~ procedure + sex + admit, family=poisson, data=azprocedure)
summary(glmazp)
exp(coef(glmazp))

nb2 <- nbinomial(los ~ procedure + sex + admit, data=azprocedure)
summary(nb2)
exp(coef(nb2))

glmaznb <- glm.nb(los ~ procedure + sex + admit, data=azprocedure)
summary(glmaznb)
exp(coef(glmaznb))
}
\keyword{datasets}
