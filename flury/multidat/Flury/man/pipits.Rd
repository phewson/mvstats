\name{pipits}
\alias{pipits}
\docType{data}
\title{Wing length of Water Pipits}
\description{
  Measurements of Wing Length for N=381 Water Pipits (\emph{Anthus spinoletta}).
}
\usage{data(pipits)}
\format{
  A data frame with 17 observations summarising the frequency of wing
  lengths of 381 Water Pipits.
  \describe{
    \item{\code{Wing.Length}}{Wing Length (mm) }
    \item{\code{Frequency}}{Frequency observed}
  }
}
\details{

}
\source{
  Flury, B.D., J.-P. Airoldi and J.-P. Biber (1992) \dQuote{Gender
  identification of water pipits using mixtures of distributions}
  \emph{Journal of Theoretical Biology} 158:465-480
}
\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
 New York: Springer
}
\examples{
data(pipits)
\dontrun{plot(pipits, type = "l",
main = "Frequency polygon for wing length of water pipits")}
}
\keyword{datasets}
