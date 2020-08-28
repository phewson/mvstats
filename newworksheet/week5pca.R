###################################################
### chunk number 1: loaddata
###################################################
hept.df <- read.csv("Heptathalon.csv", row.names = 1)
hept.df$X100mHurdles.S. <- hept.df$X100mHurdles.S. * -1
hept.df$X200m.sec. <- hept.df$X200m.sec. * -1
hept.df$r800m.s. <- hept.df$r800m.s. * -1



###################################################
### chunk number 2: manualpca
###################################################
hept.cormat <- cor(hept.df[,-1])
hept.covmat <- cov(hept.df[,-1])
hep1.ev <- eigen(hept.cormat)
hep1.ev
hep2.ev <- eigen(hept.covmat)
hep2.ev



###################################################
### chunk number 3: princomp
###################################################
hept.princomp <- princomp(hept.df[,-1], scale = TRUE)
summary(hept.princomp)
plot(hept.princomp) ## produces scree plot
biplot(hept.princomp) ## produces biplot
loadings(hept.princomp) ## pretty printed 
predict(hept.princomp) ## scores
par(mfrow = c(3,3))
apply(predict(hept.princomp), 2, qqnorm)



###################################################
### chunk number 4: loadFlury
###################################################
library(Flury)
data(turtles)



###################################################
### chunk number 5: turtles
###################################################
data(turtles)
  turtles.m <- subset(turtles, turtles$Gender == "Male")
  turtles.m <- 10 * log(turtles.m[,-1])
  turtles.m.prcomp <- prcomp(turtles.m)
  summary(turtles.m.prcomp)
plot(turtles.m.prcomp)
turtles.m.prcomp$sdev^2 ## extract eigenvalues
par(xpd = NA)
biplot(turtles.m.prcomp)



###################################################
### chunk number 6: cluster
###################################################
library(cluster)
library(flexclust)
data(milk)
milk.dist <- dist(milk)
milk.hclust <- hclust(milk.dist)
plot(milk.hclust)
milk.cut <- cutree(milk.hclust, 3)
z <- predict(prcomp(milk, scale = TRUE)) ##a
plot(z, col = milk.cut, pch = milk.cut, main = "a")
## run the command windows() to compare these side by side
z <- predict(prcomp(milk, scale = FALSE)) ##b
plot(z, col = milk.cut, pch = milk.cut, main = "b")



###################################################
### chunk number 7: horn
###################################################
require(MASS)

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



###################################################
### chunk number 8: hornhept
###################################################
Horn(hept.df[-1], 10)



###################################################
### chunk number 9: stick
###################################################
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



###################################################
### chunk number 10: stickhept
###################################################
 stick <- stickometer(7)
 proptrace <- hept.princomp$sdev^2 / sum(hept.princomp$sdev^2)
 stick ## checking the values
 proptrace ## checking the values
 idx <- barplot(proptrace, names.arg = paste("PC", c(1:7)), 
 xlab = "Component", ylab = "Proportion of trace", 
 main = "Proportion of trace explained")
 lines(idx, stick, type = "b", pch = 16)
 legend("topright", lty = 1, pch = 16, legend = "Expected values")



###################################################
### chunk number 11: pc2dist
###################################################
princomp2dist <- function(obj.princomp, retain){
 scores <- t(t(obj.princomp$scores^2) / obj.princomp$sdev)
 dtot <- apply(scores, 1, sum)
 d1 <- apply(scores[,c(1:retain)], 1, sum)
 d2 <- apply(scores[,-c(1:retain)], 1, sum)
 dists <- data.frame(dtot = dtot, d1 = d1, d2 = d2)
 return(dists)
}



###################################################
### chunk number 12: pc2disthept
###################################################
hept.princomp <- princomp(hept.df[-1], scores = TRUE, scale = TRUE)
## form a princomp object
hept.m <- princomp2dist(hept.princomp, 3)
## extract distances based on 3 component representation



