\name{irisf}
\alias{irisf}
\docType{data}
\title{Edgar Anderson's Iris Data}

\description{This famous (Fisher's or Anderson's) iris data set gives the
     measurements in centimeters of the variables sepal length and
     width and petal length and width, respectively, for 50 flowers
     from each of 3 species: \dQuote{Iris setosa},
     \dQuote{I. versicolor}, and \dQuote{I. virginica}.}
\usage{data(irisf)}
\format{'irisf' is a data frame with 150 observations with a factor
  indicating the species and four quantitative variables:
  \describe{
    \item{\code{Species}}{a factor with levels \code{setosa} \code{versicolor} \code{virginica}}
    \item{\code{Sepal.Length}}{Sepal length}
    \item{\code{Sepal.Width}}{Sepal width}
    \item{\code{Petal.Length}}{Petal length}
    \item{\code{Petal.Width}}{Petal width}
  }
}

\source{Fisher, R. A. (1936) \dQuote{The use of multiple measurements in
    taxonomic problems}. \emph{Annals of Eugenics}, 7:179-188.

  Anderson, E (1935) \dQuote{The irises of the Gaspe Peninsula},
  \emph{Bulletin of the American Iris Society}, 59:2-5
  }

\references{
  Flury, B.D. (1997) \emph{A First Course in Multivariate Statistics},
New York: Springer}

  
\seealso{'iris' in datasets, which is presents the same data, but with
  the species as the fifth column rather than the first}

\examples{
data(iris)
\dontrun{
pairs(irisf[,-1])}

}
\keyword{datasets}
