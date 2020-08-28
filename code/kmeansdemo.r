
## simulate two clusters of data
x <- c(rnorm(20,0,1), rnorm(20,4,1))
y <- c(rnorm(20,0,1), rnorm(20,4,1))
X <- cbind(x,y)
ones <- matrix(1, dim(X)[1],1)


## set up some parameters for plotting
xlims <- c(min(x), max(x)) * 1.3
ylims <- c(min(y), max(y)) * 1.3

## plot the data
plot(X, xlim = xlims, ylim = ylims)

## And add some very silly seed points
mu1 <- c(0,6)
mu2 <- c(5,-2)
seed <- list(mu1 = mu1, mu2 = mu2)
points(rbind(seed$mu1, seed$mu2), pch = c(2,3),col = c("red", "black"))
legend("topright", pch = c(1,2,3), col = c("black", "red", "black"), legend = c("Data", "Seed 1", "Seed 2" ), cex = 0.5)
##mtext(paste("Seed points: \n Group 1 ", formatC(seed[[1]],2), "Group 2 ", formatC(seed[[2]],2)))

seed <- step(X, seed$mu1, seed$mu2)
seed <- step(X, seed$mu1, seed$mu2)
seed <- step(X, seed$mu1, seed$mu2)


step <- function(X, mu1, mu2){
## classify according to current seed point (expectation step)
one <- sqrt(rowSums((X - t(t(t(mu1)) %*% t(ones)))^2))
two <- sqrt(rowSums((X - t(t(t(mu2)) %*% t(ones)))^2))
plot(x,y, col = 1 + as.numeric(one < two), pch = 16, xlim = xlims, ylim = ylims )
legend("topright", pch = c(16,16,2,3), col = c("red", "black"), legend = c("Group1", "Group2", "Seed 1", "Seed 2" ), cex = 0.5)
points(rbind(seed$mu1, seed$mu2), pch = c(2,3), col = c("red", "black"))
fixed <- (mu1 + mu2)/2
slope <- -(mu1[1] - mu2[1])/(mu1[2] - mu2[2])
abline(c(fixed[2] - slope * fixed[1], slope))

## calculate new seed points (maximisation step)
mu1 <- colMeans(X[one < two,])
mu2 <- colMeans(X[one >= two,])
return(seed = list(mu1 = mu1, mu2 = mu2))
}


perp <- function(x, y) {
   m <- (x+y)/2
   s <- - (x[1] - y[1])/(x[2] - y[2])
   abline(c(m[2] - s*m[1], s))
   invisible()
}

