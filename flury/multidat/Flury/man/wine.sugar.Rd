\name{wine.sugar}
\alias{wine.sugar}
\docType{data}
\title{Sugar adulteration in wine production}
\description{
  Chemical composition of N=344 commercial samples of concentrated grape
  must used in wine production.   The four variables recorded are
  considered for discovering adulteration with added sugar from
  non-grape plants.   In unadulterated wines, myoinositol and D/H(I)
  should follow normal distributions.
}
\usage{data(wine.sugar)}
\format{
  A data frame with 344 observations on the following 4 variables.
  \describe{
    \item{\code{Myo}}{Myo-inositol mg/kg sugar}
    \item{\code{Scyllo}}{Scylloinositol mg/kg sugar}
    \item{\code{DH.I}}{D/H(I) (ppm)}
    \item{\code{DH.II}}{D/H(II) (ppm)}
  }
}
\details{

}
\source{
  Monetti, A., G. Versini, G. Dalpiaz and F. Reniero (1996) \dQuote{Sugar
  adulterations control in concentrated rectified grape musts by finite
  mixture distribution analysis of the myo- and scyllo-inositol contents
  and the D/H methyl ratio of fermentative alcohol} \emph{Journal of
    Agricultural and Food Chemistry} 44:2194-2201
}
\references{
   Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer
}
\examples{
data(wine.sugar)
\dontrun{
with(wine.sugar, plot(log(Myo), DH.I))
require(MASS)
dens <- with(wine.sugar, kde2d(log(Myo), DH.I))
contour(dens)}
}
\keyword{datasets}
