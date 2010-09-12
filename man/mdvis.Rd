\name{mdvis}
\alias{mdvis}
\docType{data}
\title{mdvis}
\description{
Data from a subset of the German Socio-Economic Panel  (SOEP). The subset was created 
by Rabe-Hesketh and Skrondal (2005). Only working women are included in these data. 
Beginning in 1997, German health reform in part entailed a 200% increase in patient 
co-payment as well as limits in provider reimbursement. Patients were surveyed for the 
one year panel (1996) prior to and the one year panel (1998) after reform to assess 
whether the number of physician visits by patients declined - which was the goal of 
reform legislation. 
     The response, or variable to be explained by the model, is numvisit, which 
indicates the number of patient visits to a physician's office during a three month period.   
}
\usage{data(mdvis)}
\format{
  A data frame with 2,227 observations on the following 13 variables.
  \describe{
   \item{\code{numvisit}}{visits to MD office 3mo prior}
   \item{\code{reform}}{1=interview yr post-reform: 1998;0=pre-reform:1996}
   \item{\code{badh}}{1=bad health; 0 = not bad health}
   \item{\code{age}}{Age(yrs 20-60)}
   \item{\code{educ}}{education(1:7-10;2=10.5-12;3=HSgrad+)}
   \item{\code{educ1}}{educ1= 7-10 years}
   \item{\code{educ2}}{educ2= 10.5-12 years}
   \item{\code{educ3}}{educ3= post secondary or high school}
   \item{\code{agegrp}}{age: 1=20-39; 2=40-49; 3=50-60}
   \item{\code{age1}}{age 20-39}
   \item{\code{age2}}{age 40-49}
   \item{\code{age3}}{age 50-60}
   \item{\code{loginc}}{log(household income in DM)}
  }
}
\details{
mdvis is saved as a data frame.
Count models typically use docvis as response variable. 0 counts are included
}
\source{
German Socio-Economic Panel (SOEP), 1995 pre-reform; 1998 post reform. Created
by Rabe-Hesketh and Skrondal (2005).
}
\references{
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
Hilbe, Joseph M (2009), Logistic Regression Models, Chapman & Hall/CRC
Rabe-Hesketh, S. and A. Skrondal (2005). Multilevel and Longitudinal Modeling Using Stata, 
  College Station: Stata Press.
}
\examples{
data(mdvis)
glmmdp <- glm(numvisit ~ reform + factor(educ) + factor(agegrp), family=poisson, data=mdvis)
summary(glmmdp)
exp(coef(glmmdp))
library(MASS)
glmmdnb <- glm.nb(numvisit ~ reform + factor(educ) + factor(agegrp), data=mdvis)
summary(glmmdnb)
exp(coef(glmmdnb))
}
\keyword{datasets}
