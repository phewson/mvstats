\name{snailsf}
\alias{snailsf}
\docType{data}
\title{Aquatic snails hiding behaviour}
\description{
  Three subjsets of aquatic snail (Potamopyrgus antipodarum) were
  studied.   Data were collected on diseased, juvenile and brooding
  females to assess the influence of infection on the behaviour of the
  snails.   To avoid predation, snails are usually less likely to be found on the tops of
  rocks during the day.
}
\usage{data(snailsf)}
\format{
  'snailsf' is a data frame with 27 observations on the following 6 variables.
  \describe{
    \item{\code{Infected}}{a factor with levels \code{0} (not infected)
      and \code{1} (infected)}
    \item{\code{Juvenile}}{a factor with levels \code{0} and \code{1} (juvenile)}
    \item{\code{Brooding.Female}}{a factor with levels \code{0} \code{1}}
    \item{\code{Time}}{Time of day, in 24 hour clock notation}
    \item{\code{Found}}{Number of snails found on the top of a rock}
    \item{\code{Collected}}{Number of snails collected}
  }
}
\details{

}
\source{
Data courtesy by E.Levri and C.Lively, Indiana University Department of
Biology.
}
\references{
   Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer
}
\examples{
data(snailsf)
\dontrun{
S <- sin(snailsf$Time * pi/12)
C <- cos(snailsf$Time * pi/12)
snail.glm <- glm(cbind(Found, Collected-Found) ~ Infected + C + S,
  family = binomial, data = snailsf)
summary(snail.glm)}
}
\keyword{datasets}
