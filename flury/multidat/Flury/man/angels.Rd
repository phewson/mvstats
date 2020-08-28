\name{angels}
\alias{angels}
\docType{data}
\title{Angels}
\description{
  Frequency table of the wing length of 130 angels from species Angelus
  angelus and Angelus diabolicus
}
\usage{data(angels)}
\format{
  A data frame derived from a frequency table with 10 cells derived from
  130 angels
  \describe{
    \item{\code{Wing.Length}}{Wing length (mm - miracle metres)}
    \item{\code{Frequency}}{Count}
  }
}
\details{
  Rather than supply raw data, the original authors only provide
  information in ten intervals
}
\source{
  Oses, M. and J.Paul (1997) "Morphometric discrimination between two
  species of angels and estimation of their relative frequency"
  J.Celest.Morph. 1011:747-767
}
\references{
  Flury, B.D. (1997) "A First Course in Multivariate Statistics",
 Springer NY
}
\examples{
data(angels)
\dontrun{
plot(angels, type = "l")}
}
\keyword{datasets}
