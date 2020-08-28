###################################################
### chunk number 1: loaddata
###################################################
library(Flury)
?flea.beetles
data(flea.beetles)



###################################################
### chunk number 2: calcmeans
###################################################
mu <- by(flea.beetles[,-1], flea.beetles$Species, colMeans)
mudiff <- mu[[1]] - mu[[2]]
p <- dim(flea.beetles)[2] - 1 ## how many variables are we using



###################################################
### chunk number 3: covmats
###################################################
covmats <- by(flea.beetles[,-1], flea.beetles$Species, cov)
covmats



###################################################
### chunk number 4: inversepooledS
###################################################
N <- xtabs(~flea.beetles[,1])
pooledS <- ((N[1]-1) * covmats[[1]] + (N[2]-1) * covmats[[2]]) / (N[1] + N[2] -2)
pooledS
Sinv <- solve(pooledS)
Sinv



###################################################
### chunk number 5: teststat
###################################################
scaleFact <- (N[1]*N[2]) / (N[1]+N[2])
Hotellings <-  t(mudiff) %*% Sinv %*% mudiff * scaleFact
Hotellings



###################################################
### chunk number 6: turntoF
###################################################
test <- ((N[1] + N[2] - p - 1) * Hotellings )/ ((N[1] + N[2] - 2) * p)
test



###################################################
### chunk number 7: pvalue
###################################################
pf(test, p, N[1]+N[2]-p-1,lower.tail = FALSE )



###################################################
### chunk number 8: manova
###################################################
hotel.test <- manova(as.matrix(flea.beetles[,-1]) ~ flea.beetles[,1])
summary(hotel.test, test = "Hotelling")



###################################################
### chunk number 9: genellipse
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
### chunk number 10: meanellipse
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



###################################################
### chunk number 11: plotellipse
###################################################
X <- cbind(flea.beetles[,2], flea.beetles[,3])
plot(X)
points(t(colMeans(X)), pch = 16, col = "red")
mean.ellipse(X, alpha = 0.01)
mean.ellipse(X, alpha = 0.05)



###################################################
### chunk number 12: uvconfint
###################################################
abline(v = confint(lm(X[,1]~1)))
abline(h = confint(lm(X[,2]~1)))



