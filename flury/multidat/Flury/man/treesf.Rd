\name{treesf}
\alias{treesf}
\docType{data}
\title{Tree data}
\description{
  Measurement on black cherry trees
}
\usage{data(treesf)}
\format{
  'treesf' is familiar data frame with observations on 31 trees with respect to the following 3 variables.
  \describe{
    \item{\code{Diameter}}{Diameter measured one foot above ground (inches)}
    \item{\code{Height}}{Height (feet)}
    \item{\code{Volume}}{Volume (cubic feet)}
  }
}
\details{
Note that the diameter needs to be coverted to feet (12 inches in 1 foot) before use.
Assuming cone shaped trunks, the colume of wood available should be
found from:  \eqn{V = (\pi/12) D^{2} H}.   By taking logarithms of the variables
it is possible to fit \eqn{\log V = \log(\pi/12) + 2 \log D + \log H} by using lm().
}
\source{
  Ryan, B.F., B. Joiner and T.A. Ryan (1976) \emph{Minitab Handbook (2nd
    edition)} Boxton: Duxbury Press (page 278)
  }

\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
 Springer NY
}
\examples{
data(treesf)
\dontrun{
Y <- log(treesf$Volume)
X1 <- log(treesf$Diameter / 12)
X2 <- log(treesf$Height)
trees.lm <- lm(Y ~ X1 + X2)
summary(trees.lm)
log(pi/12)
vcov(trees.lm)}
}
\keyword{datasets}
