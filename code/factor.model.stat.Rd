\name{factor.model.stat}
\alias{factor.model.stat}
\title{ Estimate Variance Matrix via Statistical Factors }
\description{
Creates a variance matrix based on the principal components of the
variables that have no missing values.
}
\usage{
factor.model.stat(x, weight = seq(1, 3, length = nobs), output = "full", 
	center = TRUE, frac.var = 0.5, iter.max = 1, nfac.miss = 1, 
	full.min = 20, reg.min = 40, sd.min = 20, quan.sd = 0.9, 
	tol = 0.001, zero.load = FALSE)
}
\arguments{
  \item{x}{ 
	required.
	A numeric matrix. 
	The rows are observations and the columns are the variables.
	In finance, this will be a matrix of returns where the rows are
	times and the columns are assets.
	For the default value of \code{weight} the most recent observation
	should be the last row.
	The number of columns may exceed the number of rows, and missing
	values are accepted.
	A column may even have all missing values.
	}
  \item{weight}{ 
	a vector of observation weights.
	The length must be equal to either the original number of rows
	in \code{x} or the number of rows in \code{x} minus the number
	of rows that contain all missing values.
	}
  \item{output}{ 
	a character string indicating the form of the result.
	It must partially match one of: \code{"full"} or
	\code{"factor"}.
	}
  \item{center}{ 
	either a logical value or a numeric vector with length equal to
	the number of columns in \code{x}.
	If \code{center} is \code{TRUE}, then the mean of each column is 
	used as the center.
	If \code{center} is \code{FALSE}, then the center for each variable
	is taken to be zero.
	}
  \item{frac.var}{ 
	a control on the number of factors to use -- the number of factors
	is chosen so that the factors account for (just over) \code{frac.var}
	of the total variability.
	}
  \item{iter.max}{ 
	the maximum number of times to iterate the search for 
	principal factors of the variables with complete data.
	}
  \item{nfac.miss}{ 
	a vector of integers giving the number of factors to use in regressions
	for variables with missing values.
	The number of factors used is equal to the i-th element of 
	\code{nfac.miss} where i is the number of missing values for the
	variable.
	Thus the values in the vector should be non-increasing.
	The last value is used when the number of missing values is greater
	than the length of \code{nfac.miss}.
	}
  \item{full.min}{ 
	an integer giving the minimum number of variables that must have
	complete data.
	}
  \item{reg.min}{ 
	the minimum number of non-missing values for a variable in order
	for a regression to be performed on the variable.
	}
  \item{sd.min}{ 
	the minimum number of non-missing values for a variable in order
	for the standard deviation to be estimated from the data.
	}
  \item{quan.sd}{ 
	the quantile of the standard deviations to use for the standard
	deviation of variables that do not have enough data for the standard
	deviation to be estimated.
	}
  \item{tol}{ 
	a number giving the tolerance for the principal factor convergence
	(using the assets with full data).
	If the maximum change in uniquenesses (in the correlation scale) is
	less than \code{tol} from one iteration to the next, then convergence
	is assumed and the iterations end.
	}
  \item{zero.load}{ 
	a logical value.
	If \code{TRUE}, then loadings for variables with missing values are
	zero except for those estimated by regression.
	If \code{FALSE}, then loadings for variables with missing values are
	the average loading for the factor (when they are not estimated
	by regression).
	}
  \item{range.factors}{ 
	a numeric vector that gives the maximum and minimum number of
	factors that are allowed to be used.
	}
}
\value{
if \code{output} is \code{"full"}, then a variance matrix with dimensions
equal to the number of columns in the input \code{x}.

If \code{output} is \code{"factor"}, then an object of class
\code{"statfacmodBurSt"} which is a list with components:
  \item{loadings }{
	a matrix of the loadings for the correlation matrix.
	}
  \item{uniquenesses }{
	the uniquenesses for the correlation matrix.
	That is, the proportion of the variance that is not explained
	by the factors.
	}
  \item{sdev }{
	the standard deviations for the variables.
	}
  \item{call }{
	an image of the call that created the object.
	}
}

\section{Details }{
Observations that are missing on all variables are deleted. 
Then a principal components factor model is estimated 
with the variables that have complete data.

For variables that have missing values, the standard deviation is estimated
when there are enough obeservations otherwise a given quantile of the 
standard deviations of the other assets is used as the estimate.
The loadings for these variables are set to be either 
the average loading for the variables with no missing data, or zero. 
The loadings for the most important factors are modified by performing a 
regression with the non-missing data for each variable (if there is enough
data to do the regression).

The treatment of variables with missing values can be quite important.
You may well benefit from specializing how missing values are handled to
your particular problem.
To do this, set the output to \code{"factor"} -- then you can modify the
loadings (and per force the uniquenesses), and the standard deviations to
fit your situation.
This may include taking sectors and countries into account, for example.

The default settings for missing value treatment are suitable for creating
a variance matrix for long-only portfolio optimization -- high volatility
and average correlation.
Take note that the proper treatment of missing values is HIGHLY dependent on
the use to which the variance matrix is to be put.

OBSERVATION WEIGHTS.
Time weights are quite helpful for estimating variances from returns.
The default weighting seems to perform reasonably well over a range of
situations.

FACTOR MODEL TO FULL MODEL.
This class of object has a method for \code{fitted} which
returns the variance matrix corresponding to the factor model representation.
}

\section{Warning }{
The default value for \code{weight} assumes that the last row is the most
recent observation and the first observation is the most ancient observation.
}

\section{Revision }{
Last revised 2006 Sep 06.
}

\author{ Burns Statistics }

\seealso{ \code{\link{fitted.statfacmodBurSt}}, \code{cov.wt}.
}

\examples{
varian1 <- factor.model.stat(retmat)

varfac <- factor.model.stat(retmat, nfac=0, zero=T, output="fact")

varian2 <- fitted(varfac) # get matrix from factor model

varian3 <- factor.model.stat(retmat, nfac=rep(c(5,3,1), c(20,40,1)))
}
