\name{sibling.heads}
\alias{sibling.heads}
\docType{data}
\title{Sibling Heads}
\description{
  Skull length and breadth measured on siblings in 25 families
}
\usage{data(sibling.heads)}
\format{
  A data frame with 25 observations on the following 4 variables.
  \describe{
    \item{\code{S1Length}}{Head length of first son}
    \item{\code{S1Breadth}}{Head breadth of first son}
    \item{\code{S2Length}}{Head length of second son}
    \item{\code{S2Breadth}}{Head breadth of second son}
  }
}
\details{
 
}
\source{
  Frets, G.P. (1921) \dQuote{Heredity of head form in man} \emph{Genetica} 3:193-384
}
\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
 New York: Springer
}
\examples{
data(sibling.heads)
\dontrun{
pairs(sibling.heads)}
}
\keyword{datasets}
