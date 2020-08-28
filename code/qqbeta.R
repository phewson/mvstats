qqbeta <- function(x) {
x <- as.matrix(x)
p <- ncol(x)
n <- nrow(x)
a <- p/2
b <- (n-p-1)/2
alpha <- 0.5*(a-1)/a
beta <- 0.5*(b-1)/b
x <- sort(mahalanobis(x,apply(x,2,"mean"),var(x)))
y <- qbeta(((1:n)-alpha)/(n-alpha-beta+1),a,b)*(n-1)^2/n
plot(x,y,xlab="Squared distances",ylab="Beta quantiles")
}
