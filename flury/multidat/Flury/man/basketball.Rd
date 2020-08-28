\name{basketball}
\alias{basketball}
\docType{data}
\title{Basketball Data}
\description{
  An experiment conducted on April 18th 1996 at Bryan Park, Bloomington,
  Indiana.   These data record the number of successes within six
  attempts to hit a basketball from the distance recorded
}
\usage{data(basketball)}
\format{
  'basketball' is a data frame with 20 observations on the following 5 variables.
  \describe{
    \item{\code{Distance}}{Distance from the hoop (feet)}
    \item{\code{Steve}}{Steve's score}
    \item{\code{Andy}}{Andy's Score}
    \item{\code{Chris}}{Chris' Score}
    \item{\code{Bernard}}{Bernard's Score (attempts abandoned after 10 feet)}
  }
}
\details{
 Further details on the experiment are given in Flury (1997), Table
 7.5.4 (page 532)
}
\source{ Flury, B.D. (1997) "A First Course in Multivariate Statistics",
 Springer NY
}
\references{
}
\examples{
data(basketball)
\dontrun{
bb <- glm(cbind(Andy, 6-Andy) ~ sqrt(Distance),
family = binomial, data = basketball)
with(basketball, (plot(Distance, Andy/6, main = "Andy's Success Rate") ))
lines(basketball$Distance, predict(bb, type = "response"))}
}
\keyword{datasets}
