\name{nb2.obs.pred}
\alias{nb2.obs.pred}

\title{
 Table of negative binomial counts: observed vs predicted proportions and difference
}
\description{ 
nb2.obs.pred is used to produce a table of a negative binomial model count response with mean observed 
vs mean predicted proportions, and their difference.
}
\usage{
nb2.obs.pred(len, model)
}

\arguments{
\item{len}{ highest count for the table}
\item{model}{ name of the negative binomial model created}
}


\details{
nb2.obs.pred is used to determine where disparities exist in the mean observed and predicted proportions 
in the range of model counts. nb2.obs.pred is used in Table 9.28 and other places in Hilbe (2011).
nb2.obs.pred follows glm.nb(), where both y=TRUE and model=TRUE options must be used. 
}

\value{
\item{Count}{count value}
\item{obsPropFreq}{Observed proportion of counts}
\item{avgp}{Predicted proportion of counts}
\item{Diff}{Difference in observed vs predicted}
%% ...
}

\references{
Hilbe, J.M. (2011), Negative Binomial Regression, second edition, Cambridge University Press. 
}
\author{
Joseph M. Hilbe, Arizona State University, and 
    Jet Propulsion Laboratory, California Institute of Technology 
Andrew Robinson, University of Melbourne, Australia
}

\seealso{
\code{\link{myTable}}
}
\examples{
library(COUNT)

data(medpar)
mdpar <- glm.nb(los ~ hmo+white+type2+type3, data=medpar, y=TRUE, model=TRUE)
nb2.obs.pred(len=25, model=mdpar)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }




