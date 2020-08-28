\name{apples}
\alias{apples}
\docType{data}
\title{Apple rootstock data}
\description{
  Growth and size measurements on eight apple trees of each of six different rootstocks.
}
\usage{data(apples)}
\format{
  A data frame with 48 observations on the following 5 variables.
  \describe{
    \item{\code{Rootstock}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6}}
    \item{\code{Girth.4}}{Trunk girth at 4 years (10cm)}
    \item{\code{Growth.4}}{Extension growth at 4 years (m)}
    \item{\code{Girth.15}}{Trunk girth at 15 years (10cm)}
    \item{\code{Weight.15}}{Weight of tree above ground at 15 years (1000 pounds)}
}
}
\details{
 
}
\source{
  Andrews D.F. and A.M Herzberg (1985) \emph{Data} New York:Springer 
}
\references{
Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics}, New
York: Springer
}
\examples{
data(apples)
\dontrun{
pairs(apples[,-1],
  lower.panel = function(x, y){ points(x, y,
  pch = unclass(apples[,1]),
  col = as.numeric(apples[,1]))},
  main = "Pairwise scatter plots for apple rootstocks")}
}
\keyword{datasets}
