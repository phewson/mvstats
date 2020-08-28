\name{microtus}
\alias{microtus}
\docType{data}
\title{Microtus classification (more vole data)}
\description{
  Microtus multiplex and M. subterraneus are difficult to distinguish
  morphologically.   Here we have 43 known multiplex, 46 known
  subterraneus and a further 199 unidentified species.
}
\usage{data(microtus)}
\format{
  A data frame with 288 observations with a factor indicating the
  species and observations on a further 8 variables.
  \describe{
    \item{\code{Group}}{a factor with levels \code{multiplex} \code{subterraneus} \code{unknown}}
    \item{\code{M1Left}}{Width of upper left molar 1 (0.001mm)}
    \item{\code{M2Left}}{Width of upper left molar 2 (0.001mm)}
    \item{\code{M3Left}}{Width of upper left molar 3 (0.001mm)}
    \item{\code{Foramen}}{Length of incisive foramen (0.001mm)}
    \item{\code{Pbone}}{Length of palatal bone (0.001mm)}
    \item{\code{Length}}{Condylo incisive length or skull length (0.01mm)}
    \item{\code{Height}}{Skull height above bullae (0.01mm)}
    \item{\code{Rostrum}}{Skull width across rostrum (0.01mm)}
  }
}
\details{
89 species have been identified by means of chromosomal analysis.
}
\source{
  Airoldi, J.-P., B. Flury, M. Salvioni (1996) dQuote{Discrimination between
  two species of Microtus using both classified and unclassified
  observations} \emph{Journal of Theoretical Biology} 177:247-262
}
\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
  New York: Springer
}
\examples{
data(microtus)
\dontrun{pairs(microtus[,-1],
    lower.panel = function(x, y){ points(x, y,
    pch = unclass(microtus[,1]),
    col = as.numeric(microtus[,1]))},
    main = "Pairwise scatter plots for Microtus data")}

}
\keyword{datasets}
