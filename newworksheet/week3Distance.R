###################################################
### chunk number 1: mah
###################################################
USA.mah.dist <- mahalanobis(USArrests, 
  center = mean(USArrests), 
  cov = var(USArrests))
hist(USA.mah.dist, freq = FALSE)
curve(dchisq(x, df = 4), add=TRUE, col = "red")



###################################################
### chunk number 2: qq
###################################################
n <- 50
p <- 4
qqplot(USA.mah.dist, qchisq(ppoints(n), p))




###################################################
### chunk number 3: qqbeta
###################################################
source("qqbetaM.R")
qqbetaM(USA.mah.dist, 4)



