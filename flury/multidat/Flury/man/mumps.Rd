\name{mumps}
\alias{mumps}
\docType{data}
\title{Mumps antibodies}
\description{
  Data have been collected on the anti-mumps antibody level in N=385
  unvaccinated children aged 14.
}
\usage{data(mumps)}
\format{
  'mumps' is a vector with 385 observations on children age 14
  recording the log concentration of anti-mumps antibodies.
}
\details{

}
\source{
  Data courtesy of Dr. B. Neuenschwander, Swiss Federal Office of Public
  Health, Division of Epidemiology, 3097 Liebefeld, Switzerland
}
\references{
    Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics}
  New York: Springer
}
\examples{
data(mumps)
\dontrun{hist(mumps, breaks = 26,
  main = "Mumps antibody concentration", xlab = "Log antibody concentration" )}
}
\keyword{datasets}
