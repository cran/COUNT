\name{nuts}
\alias{nuts}
\docType{data}
\title{nuts}
\description{
Squirrel data set (nuts) from Zuur, Hilbe, and Ieno (2013).  As originally 
reported by Flaherty et al (2012), researchers recorded information about 
squirrel behavior and forest attributes across various plots in 
Scotland's Abernathy Forest. The study focused on the following variables.
  response      cones	 number of cones stripped by red squirrels per plot
  predictor  	sntrees  standardized number of trees per plot
              	sheight standardized mean tree height per plot
               	scover   standardized percentage of canopy cover per plot
The stripped cone count was only taken when the mean diameter of trees was under 0.6m (dbh). 
}
\usage{data(nuts)}
\format{
  A data frame with 52 observations on the following 5 variables.
  \describe{
    \item{\code{cones}}{total fish counted per site}
    \item{\code{ntrees}}{mean water depth per site}
    \item{\code{dbh}}{adjusted area of site}
    \item{\code{height}}{total fish counted per site}
    \item{\code{cover}}{mean water depth per site}
    \item{\code{sntrees}}{standardized mean water depth per site}
    \item{\code{sheight}}{standardized total fish counted per site}
    \item{\code{scover}}{standardized mean water depth per site}
    }
}
\details{
nuts is saved as a data frame.
Count models use ntrees as response variable. Counts start at 3  
}
\source{
Zuur, Hilbe, Ieno (2013), A Beginner's Guide to GLM and GLMM using R, 
}
\references{
Hilbe, Joseph M (2014), Modeling Count Data, Cambridge University Press
Zuur, Hilbe, Ieno (2013), A Beginner's Guide to GLM and GLMM using R, Highlands.
Flaherty, S et al (2012), "The impact of forest stand structure on red 
      squirrels habitat use", Forestry 85:437-444.
} 
\examples{
data(nuts)
nut <- subset(nuts, dbh<.6)
summary(PO <- glm(cones ~ sntrees + sheight + scover, family=quasipoisson, data=nut))
}
\keyword{datasets}





