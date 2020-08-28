


spot <- read.table("u:/teaching/mvm/coursework/hannamrebeccal.csv", sep = ",", header = TRUE)
r11 <-  cor(spot)[c(1:2), c(1:2)]
r12 <-  cor(spot)[c(1:2), c(3:4)]
r21 <-  cor(spot)[c(3:4), c(1:2)]
r22 <-  cor(spot)[c(3:4), c(3:4)]
cc <- solve(r12) %*% t(r21) %*% solve(r11) %*% r21

b1 <- eigen(cc, symmetric = FALSE)$vectors[,1]

b2 <- eigen(cc, symmetric = FALSE)$vectors[,2]

a1 <- solve(r11) %*% r21 %*% b1
a2 <- solve(r11) %*% r21 %*% b2






spot <- read.table("u:/teaching/mvm/hotelling.csv", header = TRUE, sep = ",")
xtable(t(as.matrix(mean(spot))))
xtable(cov(spot))
xtable(cor(spot))
