\name{swiss.heads}
\alias{swiss.heads}
\docType{data}
\title{Swiss Heads}
\description{
  Six measurements on heads of swiss soldiers.
}
\usage{data(swiss.heads)}
\format{
  A data frame with 200 observations on the following 6 variables.
  \describe{
    \item{\code{MFB}}{a numeric vector, minimum frontal breadth}
    \item{\code{BAM}}{a numeric vector, breadth of angulus mandibulae}
    \item{\code{TFH}}{a numeric vector, true facial height}
    \item{\code{LGAN}}{a numeric vector, length from glabella to apex nasi}
    \item{\code{LTN}}{a numeric vector, length from tragion to nasion}
    \item{\code{LTG}}{a numeric vector, length from tragion to gnathion}
  }
}
\details{
  Data collected on head measurements of members of the Swiss Army.
  Six measures are reported here.

  swiss.heads contains details for 200 male soldiers, 20 years old
  f.swiss.heads contains details for 200 female soliders
}
\source{
  Flury, B.D. and H. Riedwyl (1988) \emph{Multivariate Statistics; A
    Practical Approach} London: Chapman and Hall
}
\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics}
  New York: Springer
}

\examples{
data(swiss.heads)
\dontrun{
pairs(swiss.heads)}
}
\keyword{datasets}
