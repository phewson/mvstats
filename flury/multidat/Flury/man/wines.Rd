\name{wines}
\alias{wines}
\docType{data}
\title{Chemical composition of wines}
\description{
  These data have been collected on the chemical composition of Weisser
  Riesling wines from three countries; South Africa,Germany and Italy
}
\usage{data(wines)}
\format{
  'wines' is a data frame with 26 observations, one factor denoting the
  country of origin and 15 quantitative variables denoting 15 free
  monoterpenes and C[13]-norisoprenoids.   It is thought these influence
  the wine's aroma.
  \describe{
    \item{\code{Country}}{a factor with levels \code{South Africa} \code{Germany} \code{Italy}}
    \item{\code{Y1}}{a numeric vector}
    \item{\code{Y2}}{a numeric vector}
    \item{\code{Y3}}{a numeric vector}
    \item{\code{Y4}}{a numeric vector}
    \item{\code{Y5}}{a numeric vector}
    \item{\code{Y6}}{a numeric vector}
    \item{\code{Y7}}{a numeric vector}
    \item{\code{Y8}}{a numeric vector}
    \item{\code{Y9}}{a numeric vector}
    \item{\code{Y10}}{a numeric vector}
    \item{\code{Y11}}{a numeric vector}
    \item{\code{Y12}}{a numeric vector}
    \item{\code{Y13}}{a numeric vector}
    \item{\code{Y14}}{a numeric vector}
    \item{\code{Y15}}{a numeric vector}
  }
}
\details{
  There are a total of nine South African wines, seven German wines (all
  from Pfalz) and ten from Northern Italy (from both Trentino Alto Adige
  as Friuli)
}
\source{
  Marais, J., G. Versini, C.J. van Wyj and A. Rapp (1992) \dQuote{Effect of
  region on free and bound monoterpene and C[13]-norisoprenoid
  concentration in Weisser Riesling wines} \emph{South African Journal of
    Enology and Viniculture} 13:71-77
}
\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
 Springer NY
}
\examples{
data(wines)
\dontrun{pairs(wines[,-1],
  lower.panel = function(x, y){ points(x, y,
  pch = unclass(wines[,1]),
  col = as.numeric(wines[,1]))},
  main = "Pairwise scatter plots for Marais wine data")
## rather congested scatter plots!}
}
\keyword{datasets}
