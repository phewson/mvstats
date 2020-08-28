\name{flea.beetles}
\alias{flea.beetles}
\docType{data}
\title{Flea Beetles data}
\description{
  Two species of flea beetle (Haltica oleracea, H. carduourum), with 4
  body measurements
}
\usage{data(flea.beetles)}
\format{
  A data frame with 39 observations; 19 from Haltica oleracea and 20
  from H. carduourum (denoted by a factor) and four measurements.
  \describe{
    \item{\code{Species}}{a factor with levels \code{oleracea} \code{carduorum}}
    \item{\code{TG}}{Distange of the Transverse Groove to the posterior
      border of the prothorax (microns)}
    \item{\code{Elytra}}{Length of the Elytra (in units of 0.01mm)}
    \item{\code{Second.Antenna}}{Length of the second antennal joint (microns)}
    \item{\code{Third.Antenna}}{Length of the third antennal joint (microns)}
  }
}
\details{
 
}
\source{
  Lubischew, A.A. (1962) \dQuote{On the use of discriminant functions in
  taxonomy}. \emph{Biometrics} 18:455-477
}
\references{
   Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
   New York: Springer}
\examples{
data(flea.beetles)
\dontrun{pairs(flea.beetles[,-1],
  lower.panel = function(x, y){ points(x, y,
  pch = unclass(flea.beetles[,1]),
  col = unclass(flea.beetles[,1]))},
  main = "Pairwise scatter plots for Lubischew's Flea Beetle data")
}}
}
\keyword{datasets}
