\name{midge}
\alias{midge}
\docType{data}
\title{Midge Data}
\description{
Data on the Antenna length and Wing length of two species of Midge
}
\usage{data(midge)}
\format{
  A data frame with 15 observations on the following 3 variables.
  \describe{
    \item{\code{Species}}{a factor with levels \code{Af} (Amerohelea
      fasciata) \code{Apf} (A. pseudofasciata)}
    \item{\code{Ant.Length}}{a numeric vector, Antenna length (mm)}
    \item{\code{Wing.Length}}{a numeric vector, Wing length (mm)}
  }
}
\details{
  These data concern two newly discovered species of American predaceous
  midges.   Interest surrounds the possibility of classifying these two
  species based on external measurements alone.   Two possible measures
  are recorded here.
}
\source{
 Grogan, W.L, and W.W. Wirth (1981) \dQuote{A new American genus of predaceous
 midges related to Palpomyia and Bezzia (Diptera: Ceratopogonidae)}
 \emph{Proceedings of the Biological Society of Washington} 94:1279-1305
}
\references{
 Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
 Springer NY
}
\examples{
data(midge)
\dontrun{
with(midge, plot(Ant.Length, Wing.Length,
col = as.numeric(Species), pch = 16, main = "Scatterplot of midge data"))
legend("bottomright", pch = 16, col = c(1,2), legend = c("Af", "Apf"))}
}
\keyword{datasets}
