\name{f.voles}
\alias{f.voles}
\docType{data}
\title{Female vole data}
\description{
 Data on measurements from females of two species of voles, Microtus
 californicus and M. ochrogaster
}
\usage{data(f.voles)}
\format{
  A data frame with 86 observations with a factor denoting the species
  and a further seven variables describing various measurements in units
  of 0.1mm.
  \describe{
    \item{\code{Species}}{a factor with levels \code{californicus} \code{ochrogaster}}
    \item{\code{Age}}{Age in Days}
    \item{\code{L2.Condylo}}{Condylo incisive length}
    \item{\code{L9.Inc.Foramen}}{Incisive foramen length}
    \item{\code{L7.Alveolar}}{Alveolar length of upper molar tooth row}
    \item{\code{B3.Zyg}}{Zygomatic width}
    \item{\code{B4.Interorbital}}{Interoribital width}
    \item{\code{H1.Skull}}{Skull height}
  }
}
\details{
  The letter-number at the start of the variable name refers to the
  variable names used by the authors in the original publication.
}
\source{
   Airoldi, J.-P. and R.S. Hoffmann (1984) "Age variation in volves
  (Microtus californicus, M. ochrogaster) and its significance for
  systematic studies" Occasional papers of the Museum of Natural
  History, University of Kansas, Lawrence KS 111:1-45
}
\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer
}
\examples{
data(f.voles)
\dontrun{pairs(f.voles[,-1],
    lower.panel = function(x, y){ points(x, y,
    pch = unclass(f.voles[,1]),
    col = as.numeric(f.voles[,1]))},
    main = "Pairwise scatter plots for Female vole data")}
}
\keyword{datasets}
