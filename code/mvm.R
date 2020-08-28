

spot <- seq(-1,1,0.01)
spot2 <- spot * pi
  
result <- matrix("numeric", length(spot2), length(USArrests[,1]))

andrews <- function(x1,x2,x3,x4){
  result <- x1 / sqrt(2) + x2 * sin(spot2) + x3 * cos(spot2) + x4 * sin(2*spot2)
  return(result)
}

attach(USArrests)

for (i in 1:length(USArrests[,1])){
result[,i] <- andrews(Murder[i], Assault[i], Rape[i], UrbanPop[i])
   }


pdf("andrews.pdf", width = 6, height = 6)
par(las = 1, bty = "n", xpd = "n")
maxi <- max(as.numeric(result))
mini <- min(as.numeric(result))

for (i in 1:length(USArrests[,1])){

  plot(spot2, result[,i], type = "l", xlab = "", ylab = "", ylim = c(mini,maxi))
if (i < length(USArrests[,1])) par(new = TRUE)
}
dev.off()



USArrests.cor <- cor(USArrests)
USArrests.cov <- cov(USArrests)

USArrests.pca <- eigen(USArrests.cor)
eigen(USArrests.cov)

barplot(USArrests.pca$values, main = "Scree plot")

USArrests.pca$values / sum(USArrests.pca$values)

USArrests.scale <- scale(USArrests)

z1 <- vector("numeric", 50)
for (i in 1:50){
z1[i] <- sum(USArrests.scale[i,] * USArrests.pca$vectors[,1])
}

z2 <- vector("numeric", 50)
for (i in 1:50){
z2[i] <- sum(USArrests.scale[i,] * USArrests.pca$vectors[,2])
}

plot(z1, z2)
points(USArrests.scale[,1], USArrests.scale[,2], pch = 16)
identify(z1,z2, rownames(USArrests))
windows()
biplot(USArrests.prcomp)

?prcomp


USArrests.prcomp <- prcomp(USArrests, scale = TRUE)
USArrests.reduced.prcomp <-  prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(USArrests.prcomp)
summary(USArrests.prcomp)
biplot(USArrests.prcomp)

pdf("cancor.pdf", width = 6, height = 6)
pairs(LifeCycleSavings, pch = 16)
cor(LifeCycleSavings)
dev.off()

LifeCycleSavingsS <- scale(LifeCycleSavings)
pop <- LifeCycleSavingsS[, 2:3]
oec <- LifeCycleSavingsS[, -(2:3)]
cancor(pop, oec)


LCS.cancor <- cancor(pop, oec)
ycoef <- as.numeric(LCS.cancor$ycoef[,1])
xcoef <- as.numeric(LCS.cancor$xcoef[,1])
v1 <-  oec %*% ycoef
u1 <-  pop %*% xcoef
plot(v1, u1)
identify(v1, u1, row.names(LifeCycleSavings))

ycoef <- as.numeric(cancor(pop, oec)$ycoef[,1])
xcoef <- as.numeric(cancor(pop, oec)$xcoef[,1])
v1 <-  oec %*% ycoef
u1 <-  pop %*% xcoef
plot(v1, u1)
identify(v1, u1, row.names(LifeCycleSavings))
