\name{challenger}
\alias{challenger}
\docType{data}
\title{Challenger O Ring Data}
\description{
  These record the number of rocket booster 'O' rings seen to be damaged
  on each previous shuttle flight prior to the Challenger disaster of
  20th January 1986
}
\usage{data(challenger)}
\format{
  'challenger' is a data frame with 23 observations on the following 2 variables.
  \describe{
    \item{\code{Temp}}{Ambient temperature (Fahrenheit) at launch time}
    \item{\code{Damage}}{Number of damaged 'O' rings (out of a total of 6)}
  }
}
\details{

}
\source{
 Chatterjee, S., M.S. Handcock abd J.S. Simonoff (1995) \emph{A Casebook
   for a First Course in Statistics and Data Analysis} New York: Wiley
}
\references{
   Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer
}
\examples{
data(challenger)
\dontrun{
ch <- glm(cbind(Damage, 6-Damage) ~ Temp, family = binomial, data = challenger)
with(challenger, plot(Temp, Damage/6))
lines(challenger$Temp, predict(ch, type = "response")  )
abline(v=32, col = "red", lwd = 2)## temp when challenger launched}
}
\keyword{datasets}
