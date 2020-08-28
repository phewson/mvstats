\name{steve}
\alias{steve}
\docType{data}
\title{Steve's Basketball Data}
\description{
  Steve shot a basketball six times from each distance (1 to 20 feet),
  the results of the first throw from each distance are recorded as a
  hit or a miss
}
\usage{data(steve)}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{Distance}}{Distance between Steve and the basket}
    \item{\code{Hit.or.Miss}}{The result of Steve's first attempt at
      each distance, a factor with levels \code{H} \code{M}}
  }
}
\details{
}

\source{
    Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer
}
\references{
}
\examples{
data(steve)
\dontrun{
steve.glm <- glm(Hit.or.Miss ~ Distance, family = binomial, data =
steve)
plot(steve, main = "Steve's basketball data", pch = 16)
lines(steve$Distance, predict(steve.glm, type = "response"))}
}
\keyword{datasets}
