\name{f.twins}
\alias{f.twins}
\docType{data}
\title{Female Twins}
\description{
  Anthropometric data collected in the 1950's for 36 mono- and 43 dizygotic
  female twins.   See also m.twins
}
\usage{data(m.twins)}
\format{
  A data frame with 89 observations on the 6 variables and a factor
  indicating whether the twins are mono- or di-zygotic.
  \describe{
    \item{\code{Type}}{a factor with levels \code{monozygotic} \code{dizygotic}}
    \item{\code{STA1}}{Stature of first twin (cm)}
    \item{\code{HIP1}}{Hip width of first twin (cm)}
    \item{\code{CHE1}}{Chest circumference of first twin (cm)}
    \item{\code{STA2}}{Stature of second twin (cm)}
    \item{\code{HIP2}}{Hip width of second twin (cm)}
    \item{\code{CHE2}}{Chest circumference of second twin (cm)}
  }
}
\details{

}
\source{
  Data courtesy of the Institute of Anthropology, University of Hamburg

}
\references{
     Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
 New York: Springer
}
\examples{

data(f.twins)
\dontrun{pairs(f.twins[,-1],
  lower.panel = function(x, y){ points(x, y,
  pch = unclass(f.twins[,1]),
  col = as.numeric(f.twins[,1]))},
  main = "Pairwise scatter plots for male twins")}
}
\keyword{datasets}

