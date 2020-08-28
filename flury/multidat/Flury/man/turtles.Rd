\name{turtles}
\alias{turtles}
\docType{data}
\title{Turtle Carapace measurements (Jolicoeur and Mosimann)}
\description{
  Measurements on the turtle carapace of 24 male and 24 female painted
  turtles (Chrysemys picta marginata).
}
\usage{data(turtles)}
\format{
  'turtle.carapace' is a data frame with 48 observations on the following 4 variables.
  \describe{
    \item{\code{Gender}}{a factor with levels \code{Male} \code{Female}}
    \item{\code{Length}}{carapace length}
    \item{\code{Width}}{carapace width}
    \item{\code{Height}}{carapace height}
  }
}
\details{}

\source{
   Jolicoeur, P. and J.E. Mosimann (1960) \dQuote{Size and Shape Variation in the
   Painted Turtle: A Principal Component Analysis}, \emph{Growth}, 24:339-354
}

\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer
}

\examples{
data(turtles)
\dontrun{
pairs(turtles[,-1],
  lower.panel = function(x, y){ points(x, y,
  pch = unclass(turtles[,1]),
  col = as.numeric(turtles[,1]))},
  main = "Pairwise scatter plots for painted turtles")}
}
\keyword{datasets}
