###################################################
### chunk number 1: eda
###################################################
econ.df <- read.csv("econ.csv", row.names = 1)
econ <- econ.df[,-10]
econ.cor <- cor(econ)
econ.ev <- eigen(econ.cor)



###################################################
### chunk number 2: pcfa
###################################################
loadings4 <- matrix(0,9,4)
for (i in 1:4){
 loadings4[,i] <- sqrt(econ.ev$values[i]) * econ.ev$vectors[,i]
}
LLt <- loadings4 %*% t(loadings4)
unique <- diag(econ.cor - LLt)
error <- econ.cor - (LLt + unique)



###################################################
### chunk number 3: labeling
###################################################
row.names(loadings4) <- row.names(econ.cor)



###################################################
### chunk number 4: vals
###################################################
loadings4
unique
error



###################################################
### chunk number 5: evalsetc
###################################################
econ.ev$values
econ.ev$values / 0.09
cumsum(econ.ev$values/0.09)



###################################################
### chunk number 6: communalities
###################################################
apply(loadings4^2, 1, sum)



###################################################
### chunk number 7: rotate
###################################################
varimax(loadings4)



###################################################
### chunk number 8: init
###################################################
r2s <- vector("numeric", 9)

 for (i in 1:9){
 y <- econ[,i]
 x <- econ[,-i]
 mod <- lm(y~as.matrix(x))
 r2s[i] <- summary(mod)$r.squared
 }

 unique <- diag(1-r2s)
 diag(unique)



###################################################
### chunk number 9: repeated
###################################################
new <- econ.cor - unique
new.ev <- eigen(new)

loadings4pf <- matrix(0,9,4)
for (i in 1:4){
 loadings4pf[,i] <- sqrt(new.ev$values[i]) * new.ev$vectors[,i]
 }

 LLt <- loadings4pf %*% t(loadings4pf)

 unique.f <- econ.cor - LLt
 diag(unique) <- diag(unique.f)

 diag(unique)
 loadings4pf



###################################################
### chunk number 10: factanal
###################################################
econ.fact <- factanal(econ, factors = 4, rotation = "none")
econ.fact



###################################################
### chunk number 11: loadingsfactanal
###################################################
loadml <- loadings(econ.fact)
loadml
class(loadml) <- "matrix"
uniqueml <- econ.fact$uniquenesses
resid <- econ.cor - ( loadml%*% t(loadml) + diag(uniqueml) )
resid



###################################################
### chunk number 12: communalitiesfactanal
###################################################
apply(loadml^2, 1, sum)



###################################################
### chunk number 13: plotloadings
###################################################
plot(loadings4[,c(1:2)], pch = as.character(c(1:9)),
xlab = expression(paste(gamma,"1")), ylab = expression(paste(gamma,"2")),
main = "First and second loadings",
xlim = c(-1,1), ylim = c(-1,1))
points(varimax(loadings4)$loadings[,c(1:2)],
pch = letters[c(1:9)], col = "red")
abline(h = 0)
abline(v = 0)



