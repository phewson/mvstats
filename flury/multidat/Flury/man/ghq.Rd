\name{ghq}
\alias{ghq}
\docType{data}
\title{General Health Questionnaire Data}
\description{
  Tabulated data on the number of patients requiring psychiatric
  treatment.   
  
}
\usage{data(ghq)}
\format{
  'ghq' is a data frame tabulating N= 120 patients according to their
  gender and GHQ (general health questionnaire) score.   There are 17
  different combinations of gender and GHQ score, for each combination
  the number of patients and the number requiring psychiatric treatment
  are reported.
  \describe{
    \item{\code{Gender}}{a factor with levels \code{Male} \code{Female}}
    \item{\code{GHQ}}{Score on the Generalised Heath Questionnarie}
    \item{\code{Number}}{Number of patients in that combination}
    \item{\code{PsychiatricT}}{Number of patients in that combinations
      requiring psychiatric treatment}
  }
}
\details{

}
\source{
 Silvapulle, M.J. (1981) \dQuote{On the existence of maximum likelihood
 estimators for the binomial response model} \emph{Journal of the Royal
 Statisical Society Series B} 43:310-313
}
\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
   New York: Springer}
}
\examples{
data(ghq)

}
\keyword{datasets}
