qqbetaM <- function(x, p) {
  n <- length(x)
  a <- p/2
  b <- (n-p-1)/2
  alpha <- 0.5*(a-1)/a
  beta <- 0.5*(b-1)/b
  x <- sort(x)
  y <- qbeta(((1:n)-alpha)/(n-alpha-beta+1),a,b)*(n-1)^2/n
  plot(x,y,xlab="Mahalanobis distances",ylab="Beta quantiles")
}
