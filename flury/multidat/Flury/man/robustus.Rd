\name{robustus}
\alias{robustus}
\docType{data}
\title{Australopithecus robustus molar size}
\description{
\emph{Australopithecus robustus} are early hominids from Africa.   The
molar size is assumed to be larger in males than female, these data can
therefore be used with a mixture model in a study on sexual dimorphism.

}
\usage{data(robustus)}
\format{
  'robustus' is a vector with 36 observations on the breadth (mm) of the
  lower first molar of Australopithecus robustus.
 
}
\details{

}
\source{
Dong, Z. (1996) Looking into Peking Man's subsistence - a taphonomic
analysis of the middle pleistocene \emph{Homo erectus} site in China.
Unpublished PhD thesis, Department of Anthropology, Indiana University   
}
\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer
}
\examples{
data(robustus)
\dontrun{
hist(robustus, breaks =11, main = "Molar breadth")}

}
\keyword{datasets}
