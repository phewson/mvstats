


par(mfrow = c(1,2))

x <- rbind(mvrnorm(20, c(0,-1), sigma), mvrnorm(20, c(5,4), sigma), mvrnorm(20, c(-5,4), sigma))
plot(x, pch = 16, main = "Clustering", xlab = expression(x[1]), ylab = expression(x[2]) )
lines(c(0,-6), c(4,-2))
lines(c(0,0), c(4,7))
lines(c(0,6), c(4,-1.4))


 
x <- mvrnorm(60, c(0,4), sigma)
plot(x, pch = 16, main = "Partitioning", xlab = expression(x[1]), ylab = expression(x[2]) )
lines(c(0,-6), c(4,-2))
lines(c(0,0), c(4,7))
lines(c(0,6), c(4,-1.4))


mydist <- matrix(c(0,1,1,1,1,
2,0,1,1,1,
6,5,0,1,1,
10,9,4,0,1,
9,8,5,3,0),5,5)

dot <- as.dist(t(mydist))
plot(hclust(dot))

par(mfrow = c(1,3))
plot(hclust(dot, method = "single"), main = "Single linkage \n (Nearest neighbour)", ylim = c(0,10), hang = -0.001, sub = "", xlab = "")
plot(hclust(dot, method = "complete"), main = "Complete linkage \n (Furthest neighbour)", ylim = c(0,10), hang = -0.001, sub = "", xlab = "")
plot(hclust(dot, method = "average"), main = "Average linkage", ylim = c(0,10), hang = -0.001, sub = "", xlab = "")






USArrests.dist <- dist(USArrests)
USArrests.diana <- diana(USArrests.dist)
par(mfrow = c(1,2), oma = c(0,0,2,0))
plot(USArrests.diana, which = 2, cex = 0.6, 
main = "Dendrogram", xlab = "")
plot(USArrests.diana, which = 1, main = "Banner plot", 
nmax.lab = 50, cex.axis = 0.6, max.strlen = 12)
mtext("Divisive clustering of USArrests", outer = TRUE)


US.km <- kmeans(USArrests, centers = 2)
plot(USArrests, col = US.km$cluster, pch = US.km$cluster)

plot(prcomp(USArrests, center = TRUE)$x[,c(1,2)], 
col = US.km$cluster, pch = US.km$cluster)





dot <- pam(USArrests, k = 2)
plot(dot, which = 1)
dot$medoids





x <- rbind(mvrnorm(30, c(0,0), sigma), mvrnorm(30, c(8,8), sigma), cbind(seq(0,8, by = 0.4), seq(0,8, by = 0.4) )

dot <- hclust(dist(x), method = "single")
par(mfrow = c(1,2))
plot(dot)
plot(x, pch = cutree(dot, k = 2))






 


 
