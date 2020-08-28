\name{strider}
\alias{strider}
\docType{data}
\title{Skeletal dimensions of water striders}
\description{
  Water striders live on the surface of lakes and ponds.   These insects
  grow in six distinct stages called instars; at each transition they shed
  their skin / skeleton.   Data has been collected on N=88 female water
  striders from species Limnoporus canaliculatus.   Measurements are
  reported for the first three instars.
}
\usage{data(strider)}
\format{
  A data frame with 88 observations on the following 6 variables.
  \describe{
    \item{\code{Femur1}}{Average length of femur, instar 1}
    \item{\code{Tibia1}}{Average length of tibia, instar 1}
    \item{\code{Femur2}}{Average length of femur, instar 2}
    \item{\code{Tibia2}}{Average length of tibia, instar 2}
    \item{\code{Femur3}}{Average length of femur, instar 3}
    \item{\code{Tibia3}}{Average length of tibia, instar 3}
  }
}
\details{
  The actual analysis used the variables \eqn{x_{j}^{(star)} = 100
    \log(x_{j})}{x_j star = 100 log x_j},
  for the j = 1 to 6 variables reported.
}
\source{
Data courtesy of Dr. C.P. Klingenberg, University of Alberta
}
\references{
     Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer
}
\examples{
data(strider)
\dontrun{
X <- 100 * log(strider)
cov(X) ## etc.}
}
\keyword{datasets}
