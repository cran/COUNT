\name{myTable}
\alias{myTable}

\title{
 Frequency table 
}
\description{ 
mytable is used to produce a table of frequencies, proportion and cumulative proportions for a count variable
}
\usage{
myTable(x)
}

\arguments{
  \item{x}{
the only argument is the name of the count variable}
}

\details{
myTable is used as either a diagnostic to view the distribution of a count variable, or as a 
frequency distribution display in its own right. myTable is given in Table 9.40 in Hilbe (2011).
}
\value{

\item{x}{count value}
\item{Freq}{Frequency of count}
\item{Prop}{Proportion}
\item{CumProp}{Cumulative proportion}

%% ...
}
\references{
Hilbe, J.M. (2011), Negative Binomial Regression, second edition, Cambridge University Press. 
Hilbe, J.M. (2009), Logistic Regression Models, Chapman  Hall/CRC
}
\author{
Joseph M. Hilbe, Arizona State University, and 
    Jet Propulsion Laboratory, California Institute of Technology 
}

\seealso{
\code{\link{modelfit}}
}
\examples{
data(medpar)
myTable(medpar$los)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }
