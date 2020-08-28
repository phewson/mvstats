###################################################
### General ellipses
###################################################


ellipse <- function(covmat, centroid, csquare, resolution, plot = TRUE) {
angles <- seq(0, by = (2 * pi)/resolution, length = resolution)
  sd <- covmat[1,2] / sqrt(covmat[1,1] * covmat[2,2])
    projmat <- matrix(0,2,2)
    projmat[1,1] <- sqrt(covmat[1,1] %*% (1+sd)/2)
    projmat[1,2] <- -sqrt(covmat[1,1] %*% (1-sd)/2)
    projmat[2,1] <- sqrt(covmat[2,2] %*% (1+sd)/2)
    projmat[2,2] <- sqrt(covmat[2,2] %*% (1-sd)/2)
circle <- cbind(cos(angles), sin(angles))
ellipse <- t(centroid + sqrt(csquare) * projmat %*% t(circle))
if (plot == TRUE) {lines(ellipse)}
return(ellipse)
}



###################################################
### Mean ellipses
###################################################
mean.ellipse <- function (data, alpha=0.05, resolution=500) 
{
xbar <- colMeans(data)
n <- dim(data)[1]
p <- dim(data)[2]
f <- qf(1-alpha, p, n-p)
csquare <- ((n-1)/n) * (p / (n-p)) * f
cat(csquare) 
ellipse <- ellipse(cov(data), xbar, csquare, resolution)
}
