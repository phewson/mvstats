library(MASS)
library(mvtnorm)
library(flexmix)

data(ability.cov)
X <- mvrnorm(100, rep(0,6), ability.cov$cov)


f3 <-    factanal(X, factors=3) 
R3 <- f3$loadings %*% t(f3$loadings)
diag(R3) <- rep(1,6)

f2 <-    factanal(X, factors=2) 
R2 <- f2$loadings %*% t(f2$loadings)
diag(R2) <- rep(1,6)

f1 <-    factanal(X, factors=1) 
R1 <- f1$loadings %*% t(f1$loadings)
diag(R1) <- rep(1,6)

KLdiv(cbind(dmvnorm(scale(X), rep(0,6), R1), dmvnorm(scale(X), rep(0,6), R2), dmvnorm(scale(X), rep(0,6), R3)  ) )

