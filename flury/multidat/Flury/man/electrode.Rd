\name{electrode}
\alias{electrode}
\docType{data}
\title{Electrode Manufacture}
\description{
  Comparison of eletrode measurements from two machines
}
\usage{data(electrode)}
\format{
  A data frame with 100 observations, 50 from each machine, on the following 5 variables.
  \describe{
    \item{\code{Machine}}{a factor with levels \code{1} \code{2}}
    \item{\code{X1}}{Total diameter}
    \item{\code{X2}}{Probe diameter}
    \item{\code{X3}}{Total height}
    \item{\code{X4}}{Basal height}
    \item{\code{X5}}{Internal diameter}
  }
}
\details{
  See Figure 5.3.3 in Flury 1997, or the orginal figure 7.20 in Flury
  and Riedwyl for a better explaination of the five measurements.  The
  actual data presented are linear transformations of the original
  variables (for reasons of commercial confidentiality).
}
\source{
 Flury, B. and H. Riedwyl (1988) \emph{Multivariate Statistics: A practical
  approach}. London: Chapman and Hall
}
\references{Flury, B.D. (1997) \emph{A First Course in Multivariate
    Statistics} New York: Springer
  
}
\examples{
data(electrode)
\dontrun{pairs(apply(electrode[,-1], 2, jitter, 2),
    lower.panel = function(x, y){ points(x, y,
    pch = unclass(electrode[,1]),
    col = as.numeric(electrode[,1]))},
    main = "Pairwise scatter plots for Electrode data")}
}
\keyword{datasets}
