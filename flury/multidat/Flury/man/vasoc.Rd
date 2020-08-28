\name{vasoc}
\alias{vasoc}
\docType{data}
\title{Vasoconstriction}
\description{
  Effect of the rate and volume of air breathed in by human subjects on
  vasoconstriction in the skin of their fingers.
}
\usage{data(vasoc)}
\format{
  'vasoc' is a data frame with 39 observations on the following 3 variables.
  \describe{
    \item{\code{Volume}}{Volume of air inspired}
    \item{\code{Rate}}{Rate of air inspired}
    \item{\code{Y}}{Binary indicator, 1 = vasoconstriction, 0 = no-vasoconstriction}
  }
}
\details{

}
\source{
  Finney, D.J. (1947) The estimation from original records of the
  relationship between dose and quantal response \emph{Biometrika} 34:320-334
}
\references{
   Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer
}
\examples{
data(vasoc)
\dontrun{
with(vasoc, plot(Volume, Rate, pch = Y+15, col = Y+1))}

}
\keyword{datasets}
