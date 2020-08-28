
## Demonstrate linear combinations have normal dist
## qqnorm plot of 4 principal components from USArrests data
spot <- prcomp(USArrests)
par(mfrow = c(2,2))
apply(spot$x, 2, qqnorm)

##
## ellipse plotting - I've got horribly confused here


## a demonstration with artificial data - I don't know why this works

X <- mvrnorm(100, c(0,0), matrix(c(1,.9,.9,1),2,2))
V <- var(X)
eV <- eigen(V)

theta <- seq(0,(2*pi),length=101)
x <- 1.96 * sqrt(eV$val[1]) * cos(theta)
y <- 1.96 * sqrt(eV$val[2]) * sin(theta)

plot(X, xlim = c(-3,3), ylim = c(-3,3))
newxy <- cbind(x,y) %*% t(eV$vec)
lines(newxy)
abline(a=0,b=eV$vec[2,1]/eV$vec[1,1])
abline(a=0,b=eV$vec[2,2]/eV$vec[1,2])




### spherical data

X1 <- mvrnorm(100, c(0,0), matrix(c(1,0,0,1),2,2))
X2 <- mvrnorm(100, c(0,0), matrix(c(1,0,0,1),2,2))
X3 <- mvrnorm(100, c(0,0), matrix(c(1,0,0,1),2,2))

V1 <- var(X1)
V2 <- var(X2)
V3 <- var(X3)

eV1 <- eigen(V1)
eV2 <- eigen(V2)
eV3 <- eigen(V3)


theta <- seq(0,(2*pi),length=101)
x <- 1.96 * sqrt(eV$val[1]) * cos(theta)
y <- 1.96 * sqrt(eV$val[2]) * sin(theta)

plot(X1, xlim = c(-3,3), ylim = c(-3,3), pch = 1, col = "black")
newxy <- cbind(x,y) %*% t(eV1$vec)
lines(newxy, lty = 1, col = "black")
abline(a=0,b=eV1$vec[2,1]/eV1$vec[1,1], lty = 1, col = "black")
abline(a=0,b=eV1$vec[2,2]/eV1$vec[1,2], lty = 2, col = "black")
par(new = TRUE)
plot(X2, xlim = c(-3,3), ylim = c(-3,3), pch = 2, col = "red")
newxy <- cbind(x,y) %*% t(eV2$vec)
lines(newxy, lty = 1, col = "red")
abline(a=0,b=eV2$vec[2,1]/eV2$vec[1,1], lty = 1, col = "red")
abline(a=0,b=eV2$vec[2,2]/eV2$vec[1,2], lty = 2, col = "red")
par(new =TRUE)
plot(X3, xlim = c(-3,3), ylim = c(-3,3), pch = 3, col = "blue")
newxy <- cbind(x,y) %*% t(eV3$vec)
lines(newxy, lty = 1, col = "blue")
abline(a=0,b=eV3$vec[2,1]/eV3$vec[1,1], lty = 1, col = "blue")
abline(a=0,b=eV3$vec[2,2]/eV3$vec[1,2], lty = 2, col = "blue")
## old code above


pdf("pcastability.pdf", width = 8, height = 5)
require(MASS)
par(mfrow = c(1,2), oma = c(0,0,1,0))
### non spherical data

X1 <- mvrnorm(100, c(0,0), matrix(c(1,0.9,0.9,1),2,2))
X2 <- mvrnorm(100, c(0,0), matrix(c(1,0.9,0.9,1),2,2))
X3 <- mvrnorm(100, c(0,0), matrix(c(1,0.9,0.9,1),2,2))

V1 <- var(X1)
V2 <- var(X2)
V3 <- var(X3)

eV1 <- eigen(V1)
eV2 <- eigen(V2)
eV3 <- eigen(V3)


theta <- seq(0,(2*pi),length=101)


plot(X1, xlim = c(-3,3), ylim = c(-3,3), pch = 1, col = "black", xlab = "x1", ylab = "x2")
x <- 1.96 * sqrt(eV1$val[1]) * cos(theta)
y <- 1.96 * sqrt(eV1$val[2]) * sin(theta)
newxy <- cbind(x,y) %*% t(eV1$vec)
lines(newxy, lty = 1, col = "black")
abline(a=0,b=eV1$vec[2,1]/eV1$vec[1,1], lty = 1, col = "black")
abline(a=0,b=eV1$vec[2,2]/eV1$vec[1,2], lty = 2, col = "black")
par(new = TRUE)
plot(X2, xlim = c(-3,3), ylim = c(-3,3), pch = 2, col = "red", xlab = "x1", ylab = "x2")
newxy <- cbind(x,y) %*% t(eV2$vec)
x <- 1.96 * sqrt(eV2$val[1]) * cos(theta)
y <- 1.96 * sqrt(eV2$val[2]) * sin(theta)
lines(newxy, lty = 1, col = "red")
abline(a=0,b=eV2$vec[2,1]/eV2$vec[1,1], lty = 1, col = "red")
abline(a=0,b=eV2$vec[2,2]/eV2$vec[1,2], lty = 2, col = "red")
par(new =TRUE)
plot(X3, xlim = c(-3,3), ylim = c(-3,3), pch = 3, col = "blue", xlab = "x1", ylab = "x2")
x <- 1.96 * sqrt(eV3$val[1]) * cos(theta)
y <- 1.96 * sqrt(eV3$val[2]) * sin(theta)
newxy <- cbind(x,y) %*% t(eV3$vec)
lines(newxy, lty = 1, col = "blue")
abline(a=0,b=eV3$vec[2,1]/eV3$vec[1,1], lty = 1, col = "blue")
abline(a=0,b=eV3$vec[2,2]/eV3$vec[1,2], lty = 2, col = "blue")



### spherical data

X1 <- mvrnorm(100, c(0,0), matrix(c(1,0,0,1),2,2))
X2 <- mvrnorm(100, c(0,0), matrix(c(1,0,0,1),2,2))
X3 <- mvrnorm(100, c(0,0), matrix(c(1,0,0,1),2,2))

V1 <- var(X1)
V2 <- var(X2)
V3 <- var(X3)

eV1 <- eigen(V1)
eV2 <- eigen(V2)
eV3 <- eigen(V3)


theta <- seq(0,(2*pi),length=101)
x <- 1.96 * sqrt(eV$val[1]) * cos(theta)
y <- 1.96 * sqrt(eV$val[2]) * sin(theta)

plot(X1, xlim = c(-3,3), ylim = c(-3,3), pch = 1, col = "black", xlab = "x1", ylab = "x2")
x <- 1.96 * sqrt(eV1$val[1]) * cos(theta)
y <- 1.96 * sqrt(eV1$val[2]) * sin(theta)
newxy <- cbind(x,y) %*% t(eV1$vec)
lines(newxy, lty = 1, col = "black")
abline(a=0,b=eV1$vec[2,1]/eV1$vec[1,1], lty = 1, col = "black")
abline(a=0,b=eV1$vec[2,2]/eV1$vec[1,2], lty = 2, col = "black")
par(new = TRUE)
plot(X2, xlim = c(-3,3), ylim = c(-3,3), pch = 2, col = "red", xlab = "x1", ylab = "x2")
x <- 1.96 * sqrt(eV2$val[1]) * cos(theta)
y <- 1.96 * sqrt(eV2$val[2]) * sin(theta)
newxy <- cbind(x,y) %*% t(eV2$vec)
lines(newxy, lty = 1, col = "red")
abline(a=0,b=eV2$vec[2,1]/eV2$vec[1,1], lty = 1, col = "red")
abline(a=0,b=eV2$vec[2,2]/eV2$vec[1,2], lty = 2, col = "red")
par(new =TRUE)
plot(X3, xlim = c(-3,3), ylim = c(-3,3), pch = 3, col = "blue", xlab = "x1", ylab = "x2")
x <- 1.96 * sqrt(eV3$val[1]) * cos(theta)
y <- 1.96 * sqrt(eV3$val[2]) * sin(theta)
newxy <- cbind(x,y) %*% t(eV3$vec)
lines(newxy, lty = 1, col = "blue")
abline(a=0,b=eV3$vec[2,1]/eV3$vec[1,1], lty = 1, col = "blue")
abline(a=0,b=eV3$vec[2,2]/eV3$vec[1,2], lty = 2, col = "blue")

mtext("Stability of sample principal components with simulated data", outer = TRUE)



dev.off()








e1 <- x1 <- 1
e2 <- x2 <- 2

library(MASS)
X <- mvrnorm(100, c(0,0), matrix(c(1,.9,.9,1),2,2))
ellipse <- function(X, e1, e2){
   X.prcomp <- prcomp(X)
   eigval1 <- X.prcomp$sdev[e1]
   eigval2 <- X.prcomp$sdev[e2]

      skeleton <- seq(0,(2*pi),length=101)
      x <- 1.96 * eigval1 * cos(skeleton)
      y <- 1.96 * eigval2 * sin(skeleton)

  plot(scale(X[,x1]), scale(X[,x2]) )
    rotmat <- X.prcomp$rotation[c(e1,e2)]
   rotmat <- t(rotmat) %*% rotmat
newxy <- rotmat %*% t(cbind(x,y))
lines(newxy)
 }
##lines(newxy)
abline(a=0,X.prcomp$rotation[e1,e1]/X.prcomp$rotation[e2,e1])
abline(a=0,X.prcomp$rotation[e1,e2]/X.prcomp$rotation[e2,e2])


##abline(a=0,b=hep.ev$vec[4,4]/hep.ev$vec[1,2])

atan(c(hep.ev$vec[4,1]/hep.ev$vec[1,1], hep.ev$vec[4,4]/hep.ev$vec[1,4])) ## angle of axes
diff(atan(c(hep.ev$vec[4,1]/hep.ev$vec[1,1], hep.ev$vec[4,4]/hep.ev$vec[1,4]))) ## right angle



## this one shows the projections
require(MASS)
X <- scale(mvrnorm(25, c(2,2), matrix(c(1,0.8,0.8,1),2,2)))
eqscplot(X)
##plot(X)
X.cov <- cov(X)
X.ed <- eigen(X.cov)
proj <- X.ed$vec[,1] %*% t(X.ed$vec[,1])
y <- t(proj %*% t(X))
abline(a=0,b=X.ed$vec[2,1]/X.ed$vec[1,1])
arrows( X[,1], X[,2], y[,1],y[,2], length = 0.05, col = "green")

##eqscplot(X)
X2.lm <- lm(X[,2] ~ X[,1])
abline(X2.lm, col = "red", lwd = 2)
arrows(X[,1],X[,2], X[,1], predict(X2.lm),  length = 0.01, col = "red")
points(X[,1],X[,2], pch = 16)

##eqscplot(X)
X1.lm <- lm(X[,1] ~ X[,2])
abline(0, 1/coef(X1.lm)[2], col = "blue")
arrows(X[,1],X[,2], predict(X1.lm), X[,2],  length = 0.01, col = "blue")
points(X[,1],X[,2], pch = 16)



## Preparing the heptatholon data
## jiggling the hept. data
hept.df <- read.csv("../data/Heptatholon.csv", row.names = 1)
hept.df$X100mHurdles.S. <- hept.df$X100mHurdles.S. * -1
hept.df$X200m.sec. <- hept.df$X200m.sec. * -1
hept.df$r800m.s. <- hept.df$r800m.s. * -1


hep.scale <- scale(hept.df[,-c(1:2)])
hept.cormat <- cor(hept.df[,-c(1:2)])
hep.ev <- eigen(hept.cormat)


hep.ev$vectors[,1]
y1 <- hep.scale %*% hep.ev$vectors[,1]

## produces a scree plot from heptatholon data
par(las = 1)
plot(hep.ev$values, type = "b", main = "Scree plot from Heptathalon data", ylab = "Variance explained", xlab = "Principal component", pch = 16)

p <- prcomp(hept.df[,-c(1)], scale = TRUE)

##par(xpd = NA)
   par(xpd = NA, cex = 0.7)
biplot(p)


   
   ## home made biplot (not working)
   ##xlabs = substr(as.character(hept.df[,1]),1,2), cex = c(0.5, 1))

dot <- svd(scale(hept.df[,-c(1:2)]))
obs <- cbind(dot$u[,1], dot$u[,2]) * sqrt(150)
arrx <- dot$d[1]*dot$v[,1]## / sqrt(150)
arry <- dot$d[2]*dot$v[,2]## / sqrt(150)

plot(obs)
arrows(0, 0, arrx, arry)



## my attempt at CIs for evals
eigenCI <- function(eigenvalues, n){
rhdenom <- 1.96 * sqrt(2 / (n / length(eigenvalues)))
lower <- eigenvalues / (1 + rhdenom)
upper <- eigenvalues / (1 - rhdenom)
eigenCIs <- data.frame(lower = lower, upper = upper)
return(eigenCIs)

eigenCI(hep.ev$values, 7)

vals <-  hep.ev$values^2
q <- 3
alpha <- 0.95
alpha <- 1 - (1-alpha)/2

pi <- sum(vals[1:q]) / sum(vals)
alpha <- sum(vals[1:q]^2) / sum(vals^2)## by vector recycing
eta2 <- pi^{2} - 2 * alpha * pi + alpha^2
cat(pi, eta2)
cat("\n")
cat(pi + qnorm(alpha)*eta2)
cat("\n")
cat(pi - qnorm(alpha) * eta2)
cat("\n")



plot(hept.princomp)



Horn <- function(data, reps){
p <- dim(data)[2]
n <- dim(data)[1]
Varmat <- matrix(0,p,p)
Mean <- mean(data)
diag(Varmat) <- diag(var(data))

Evals <- princomp(data, cor = TRUE)$sdev^2
idx <- barplot(Evals, names.arg = paste("PC", c(1:7)), 
xlab = "Component", ylab = "Proportion of trace", 
main = "Proportion of trace explained")

results <- matrix(0,reps,p)
  for (i in 1:reps){
  SimData <- mvrnorm(n, Mean, Varmat)
  ExpEvalsH <- princomp(SimData, cor = TRUE)$sdev^2
  results[i,] <- ExpEvalsH
  lines(idx, ExpEvalsH, type = "b", pch = 16)
  }

lines(idx, apply(results, 2, mean), type = "b", col = "red")

legend("topright", lty = 1, pch = 16, legend = "Expected values")
Results <- data.frame(Evals = Evals, ExpEvalsH = ExpEvalsH)
}

Horn(hept.df[-1], 10)


p <- princomp(hept.df[,-1], scale = TRUE)


## stick function
stickometer <- function(p){
  vec <- 1 / (1:p)
  stick <- vector("numeric", p) 
  stick[1] <- sum(vec)
     for (i in 2:p){
     stick[i] <- sum(vec[-(1:(i-1))])}
  stick <- 1/p * stick
  names(stick) <- paste("Comp.", c(1:p), sep = "")
  return(stick)
}

stick <- stickometer(7)
proptrace <- p$sdev^2 / sum(p$sdev^2)

stick
proptrace

idx <- barplot(proptrace, names.arg = paste("PC", c(1:7)), 
xlab = "Component", ylab = "Proportion of trace", 
main = "Proportion of trace explained")
lines(idx, stick, type = "b", pch = 16)
legend("topright", lty = 1, pch = 16, legend = "Expected values")




## now for some bootstrapping

theta <- function(x.data, x){
eta <- prcomp(x.data[x,])
return(cbind(eta[[1]], eta[[2]]))
}



library(boot)
hep.boot <- boot(hep.scale, theta, R = 50, sim = "ordinary")
eigen.bootplot(hep.boot,8,7)


##data(USArrests)

##
##theta <- function(x.data, x){
##eta <- prcomp(x.data[x,])
##return(cbind(eta[[1]], eta[[2]]))
##
##}



##dot <- boot(USArrests, theta, 20)


There is a problem with bootstrapping principal components, the eigenvectors are fairly arbitrary.   



eigen.bootplot <- function(bootobj, start, lngth){

end <- start + lngth - 1
min.y <- min(bootobj$t[,start:end])
max.y <- max(bootobj$t[,start:end])
size <- length(bootobj$t[,1])

plot(bootobj$t[,start], type = "l", col = 1, ylim = c(min.y*1.2, max.y*1.2))

for (i in 2:lngth){
lines(bootobj$t[,start+i], col = i)
##lines(bootobj$t[,start+i], col = i)
##lines(bootobj$t[,start+i], col = i)
 }
legend(x="bottom", legend = c(start:end),col = c(1:lngth), lwd = 1, ncol = lngth)

}



hep.boot <- boot(hep.scale, theta, R = 50, sim = "ordinary")
idx <- hep.boot$t[,10] < 0

##idx <- hep.boot$t[,8] < 0
hep.boot$t[idx,c(8:14)] <- hep.boot$t[idx,c(8:14)] * -1
eigen.bootplot(hep.boot,8,7)
hep.boot$t[,c(8:14)]
hep.boot

 princomp2dist <- function(obj.princomp, retain){
 scores <- t(t(obj.princomp$scores^2) / obj.princomp$sdev)
 dtot <- apply(scores, 1, sum)
 d1 <- apply(scores[,c(1:retain)], 1, sum)
 d2 <- apply(scores[,-c(1:retain)], 1, sum)
 dists <- data.frame(dtot = dtot, d1 = d1, d2 = d2)
 return(dists)
}

hept.princomp <- princomp(hept.df[-1], scores = TRUE, scale = TRUE)
## form a princomp object
hept.m <- princomp2dist(hept.princomp, 3)
## extract distances based on 3 component representation




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
> qqbetaM(hept.m$dtot,  7)



> plot(hept.m$d1, hept.m$d2, 
  xlab = "Represented by q", ylab = "Not represented by q", 
  main = "Mahalanobis distances")
> identify(hept.m$d1, hept.m$d2, row.names(hept.df))



%library(MASS)
%truehist(hept.m$dtot, nbins = 9, ylim = c(0,0.1))
%curve(dchisq(x, df = 7), add = TRUE)


> hep.princomp <- princomp(hept.df[-1], cor = TRUE)
> hep.cor.rob <- cov.rob(hept.df[,-1], cor = TRUE)$cor
> hep.princomp.rob <- princomp(cov = hep.cor.rob)
> hep.princomp
> hep.princomp.rob
> loadings(hep.princomp)
> loadings(hep.princomp.rob)







## collection of functions based upon Bernard Flury's Book
## Originally written by Marco Bee


## Calculate the standard error of a principal component solution.

lpc <- function(X){
  UseMethod("lpc", X)
}

lpc.default <- function(X)
{

#  procedure LPC  
#  procedure LPC  

# input:     X        N by p  data matrix

# output:    B        orthogonal p by p matrix, with eigenvectors of the
#                     covariance matrix of X as columns
#            eig$values   N-vector, with eigenvalues in descending order
#            stdeB    p by p matrix, containing normal theory standard
#                      errors of the coefficients in B
#            stdelam  normal theory standard errors of the eigenvalues


#  +++ NOTE: the calculation of standard errors will fail if any two
#         eigenvalues are equal, or if any eigenvalue is zero       +++   

p <- dim(X)[2] # number of variables
N <- dim(X)[1] # number of observations 
eig <- eigen(var(X)) # eigenvalues and eigenvectors 
Ones <- matrix(1, p, p) 
Lambda <- Ones * eig$values
Q <- (t(Lambda) - Lambda + diag(p))^(-2) - diag(p) # nifty trick
Theta1 <- sweep(Q, 2, eig$values, FUN="*") 
Theta <- Theta1 * eig$values # compute matrix of theta-coefficients
stdeB <- matrix(0,p,p)       # open matrix for standard errors 
h <- 1
while (h <= p)               # start loop over eigenvectors 
{
V <- eig$vectors %*% (Theta[, h] * t(eig$vectors))
                             # V-matrix for h-th eigenvector 

stdeB[, h] <- sqrt(diag(V)/N)# standard errors for h-th eigenvector 
h <- h + 1 
}                            # end of loop 
stdelam <- sqrt(2/N) * eig$values   # standard errors of eigenvalues 
results <- list("eigenvectors" = eig$vectors, "eigenvalues" = eig$values,
"stdeB" = stdeB, "stdelam" = stdelam)
class(results) <- "lpc"
results
}


print.lpc <- function(x, alpha = NULL, bonferroni = FALSE) {
if (!is.null(alpha)){ ## calculate ci if asked
  if (bonferroni == TRUE) {alpha = alpha / length(x[[2]])}
  z <- abs(qnorm((1-alpha)/2))
}
print(x[1]) ## eigenvectors

if (!is.null(alpha)){
cat(round(alpha * 100), "\% CI: \n ")
veclo <- x[[1]] - z * x[[3]]
vechi <- x[[1]] + z * x[[3]]
print(veclo)
print(vechi)
cat("\n")
} 

print(x[2]) ## eigenvalues

if (!is.null(alpha)){
cat(round(alpha * 100), "\% CI: \n ")
vallo <- x[[2]] - z * x[[4]]
valhi <- x[[2]] + z * x[[4]]
print(vallo)
print(valhi)
cat("\n")
} 

cat("standard errors for eigenvector coefficients:")
print(x[3])

cat("standard errors for eigenvalues:")
print(x[4])
cat("\n\n")
invisible(x)
}



# S-PLUS instructions for graphing an ellipse 
# First define all parameters 

ellips <- function(A = matrix(c(1,1,1,2),2,2), m = c(3,4), const = 1, k = 1000)
{
# input:  A	positive definite symmetric matrix of dimension 2 by 2
#         m	column vector of length 2, center of ellipse
#         const	positive constant
#         k	number of points on ellipse (must be at least 2)
# output: x 	a (k by 2) matrix whose rows are the coordinates
#		of k points on the ellipse (y-m)'*A^{-1}*(y-m) = c^2

r <- A[1, 2]/sqrt(A[1, 1] * A[2, 2])
Q <- matrix(0, 2, 2)			  # construct matrix Q for
Q[1, 1] <- sqrt(A[1, 1] %*% (1+r)/2)	# transformation of circle
Q[1, 2] <- -sqrt(A[1, 1] %*% (1-r)/2)		      # to ellipse
Q[2, 1] <- sqrt(A[2, 2] %*% (1+r)/2)
Q[1, 1] <- sqrt(A[2, 2] %*% (1-r)/2)
alpha <- seq(0, by = (2 * pi)/k, length = k)	   # define angles
Z <- cbind(cos(alpha), sin(alpha)) 	   # points on unit circle
X <- t(m + const * Q %*% t(Z))  # coordinates of points on ellipse
X
}					 # end of procedure ellips

# call procedure ellips
