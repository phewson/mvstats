\name{f.swiss.heads}
\alias{f.swiss.heads}
\docType{data}
\title{Female Swiss Heads}
\description{
  Six measurements on heads of female swiss soldiers.
}
\usage{data(f.swiss.heads)}
\format{
  A data frame with 59 observations on the following 6 variables.
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
  These data correspond to those reported in siwss.heads, except that
  here we have data for 59 females.
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
data(f.swiss.heads)
\dontrun{pairs(f.swiss.heads)
cor(f.swiss.heads) ## how do we compare this with:
cor(swiss.heads)}
}
\keyword{datasets}


