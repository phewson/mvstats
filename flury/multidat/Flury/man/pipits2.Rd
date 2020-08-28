\name{pipits2}
\alias{pipits2}
\docType{data}
\title{Wing length of water pipits}
\description{
  Measurements of Wing Length for 381 Water Pipits caught at Tour du
  Valat, France.
}
\usage{data(pipits2)}
\format{
  'pipits2' is a data frame with 15 observations summarising the
  frequency of wing lengths of 381 Water Pipits.
  \describe{
    \item{\code{Wing.length}}{Wing Length (mm)}
    \item{\code{Number}}{Frequency observed}
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
data(pipits2)
\dontrun{
plot(pipits2, type = "l")}

}
\keyword{datasets}
