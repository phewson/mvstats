\name{dead.beetles}
\alias{dead.beetles}
\docType{data}
\title{Beetle mortality data}
\description{
  Beetle mortality data follwoing exposure to insecticide.
}
\usage{data(dead.beetles)}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{\code{Dose}}{log[10] CS[2]mf/l}
    \item{\code{tested}}{Number of beetles tested}
    \item{\code{died}}{Number of beetles killed}
  }
}
\details{
  
}
\source{
  Bliss, C.J. (1975) The calculation of the dosage-mortality curve
  \emph{Annals of Applied Biology} 22:134-167
}
\references{
   Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
 New York: Springer
}
\examples{
data(dead.beetles)
\dontrun{
with(dead.beetles, plot(Dose, died / tested))
dead.glm <- glm(cbind(died, tested-died) ~ Dose, family = binomial, data = dead.beetles)
lines(dead.beetles$Dose, predict(dead.glm, type = "response"))}
}
\keyword{datasets}
