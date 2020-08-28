\name{computers}
\alias{computers}
\docType{data}
\title{Computer Repair Data}
\description{
  These data give the repair times of N=14 computers.
}
\usage{data(computers)}
\format{
  A data frame with 14 observations on the following 2 variables.
  \describe{
    \item{\code{Units}}{The number of parts requiring replacement}
    \item{\code{Minutes}}{Time taken to repair the computer (minutes)}
  }
}
\details{
  
}
\source{
  Chatterjee, S. and B. Price (1977) \emph{Regression Analysis by
    Example} New York: Wiley
}
\references{
   Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
 New York: Springer
}
\examples{
data(computers)
\dontrun{
plot(computers, main = "Computer repair data")}
}
\keyword{datasets}
