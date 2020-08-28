

library(Flury)
data(midge)

## extract the group specific means and covariances

mu <- by(midge[,-1], midge[,1], mean)
s <- by(midge[,-1], midge[,1], cov)

## get the mean difference, and the pooled covariance
## note that 8 in Af, and 5 in Apf

mudiff <- mu[1]$Af - mu[2]$Apf
spool <- 1/13 * (8 * s[1]$Af + 5 * s[2]$Apf)

## loadings simply obtained

a <- solve(spool) %*% mudiff

## check on the Mahlanobis distance

sqrt(t(mudiff) %*% solve(spool) %*% mudiff)

## get the mean values of z1 and z2, and determine cutoff

z1 <- mu[1]$Af %*% a
z2 <- mu[2]$Apf %*% a
midpoint <- (z1+z2)/2
midpoint

## but note, covariance of z given by D^2

covz <- t(mudiff) %*% solve(spool) %*% mudiff)
covz


midge.lda <- lda(Species ~ ., data = midge, prior = c(1,1)/2)

midge.preds <- predict(midge.lda, dimen = 2)$x

perp <- function(x, y) {
   m <- (x+y)/2
   s <- - (x[1] - y[1])/(x[2] - y[2])
   abline(c(m[2] - s*m[1], s))
   invisible()
}

plo

perp(wines.lda[1,], wines.lda[2,], wines.lda[3,])





## more than 2 groups

ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
ir.species <- factor(c(rep("s", 50), rep("c", 50), rep("v", 50)))

library(Flury)

(wines.lda <- lda(Country ~ ., data = wines, prior = c(1,1,1)/3))
wines.pred <- predict(wines.lda, dimen = 2)$x
eqscplot(wines.pred, type = "n", xlab = "first linear discriminant",
          ylab = "second linear discriminant")
text(wines.pred, labels = as.character(wines$Country),
      col = 3 + unclass(wines$Country), cex = 0.8)


plot(wines.lda, dimen = 1)
## or
plot(ir.lda, type = "density", dimen = 1)

library(Flury)
data(flea.beetles)

## extract the group specific means and covariances

mu <- by(flea.beetles[,-1], flea.beetles[,1], mean)
s <- by(flea.beetles[,-1], flea.beetles[,1], cov)

## get the mean difference, and the pooled covariance
## note that 8 in Af, and 5 in Apf

mudiff <- mu[1]$oleracea - mu[2]$carduorum
spool <- 1/13 * (8 * s[1]$oleracea + 5 * s[2]$carduorum)
spoolinv <- solve(spool)
## loadings simply obtained

a <- spoolinv %*% mudiff

## check on the Mahlanobis distance

sqrt(t(mudiff) %*% spoolinv %*% mudiff)

## get the mean values of z1 and z2, and determine cutoff

z1 <- mu[1]$oleracea %*% a
z2 <- mu[2]$carduorum %*% a
midpoint <- (z1+z2)/2
midpoint

## but note, covariance of z given by D^2

covz <- t(mudiff) %*% solve(spool) %*% mudiff
covz











n1 <- 19
n2 <- 20
p <- 4
m <- ((n1 + n2) * (n1 + n2 - 2))/(n1 * n2)

dminus <- covz - (as.vector(a)^2 / diag(spoolinv))


(diag(spoolinv) * (m + dminus)) / (n1 + n2 - p -1)

a / sqrt(diag(spoolinv) * (m + dminus^2)) / (n1 + n2 - p -1)
                       [,1]























     data(midge)
     ## Not run: 
     with(midge, plot(Ant.Length, Wing.Length,
     col = as.numeric(Species), pch = 16, main = "Scatterplot of midge data"))
     legend("bottomright", pch = 16, col = c(1,2), legend = c("Af", "Apf"))
     ## End(Not run)



rug(midge[midge$Species == "Af",2], col = "black", lwd = 2)
rug(midge[midge$Species == "Apf",2], col = "red", lwd = 2)

rug(midge[midge$Species == "Af",3], side = 2, col = "black", lwd = 2)
rug(midge[midge$Species == "Apf",3], side = 2, col = "red", lwd = 2)

box(lwd = 2)


