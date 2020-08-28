

require(MASS)
X <- mvrnorm(100, c(0,0,0,0), Sigma = matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4))
X[100,] <- c(1.68, 1.68, 1.68, 1.68)
colnames(X) <- c("x1", "x2", "x3", "x4") 
pairs(X, pch = 16)
pairs(X, col = c(rep("red", 99), "black"), pch = c(rep(16,99), 4))