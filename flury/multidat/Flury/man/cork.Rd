\name{cork}
\alias{cork}
\docType{data}
\title{Directional Cork deposit data}
\description{
  Cork deposits on North, East, West and South of 28 cork trees.
}
\usage{data(cork)}
\format{
  A data frame 28 observations on cork deposits from the North, East,
  South and West.
  \describe{
    \item{\code{North}}{Deposit on North of tree (mm)}
    \item{\code{East}}{Eastern deposit (mm)}
    \item{\code{South}}{Southern deposit (mm)}
    \item{\code{West}}{Western deposit (mm)}
  }
}
\details{
}
\source{
  Rao, C.R. (1948) "Tests of significance in multivariate analysis"
  \emph{Biometrika} 35:58-79
}
\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
 New York: Springer
}
\examples{
data(cork)
\dontrun{
pairs(cork)}
}
\keyword{datasets}
