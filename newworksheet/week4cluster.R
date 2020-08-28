###################################################
### chunk number 1: eda
###################################################
library(cluster)
library(flexclust)
library(MASS)
library(TeachingDemos)
data(milk)
stars(milk, full = FALSE, draw.segments = TRUE, 
    key.loc = c(10,0.4), main= "Milk data")
faces(milk, fill = TRUE, ncol = 4, scale = TRUE, nrow = 7)



###################################################
### chunk number 2: hclust1
###################################################
milk.dist <- dist(milk)
milk.hclust <- hclust(milk.dist)
plot(milk.hclust)
milk.cut <- cutree(milk.hclust, 3)



###################################################
### chunk number 3: althclust
###################################################
milk.dist.man <- dist(milk, method = "manhattan")
milk.hclust.man <- hclust(milk.dist.man)
plot(milk.hclust.man)
milk.cut.man <- cutree(milk.hclust.man, 3)
xtabs(~milk.cut+milk.cut.man)



###################################################
### chunk number 4: altbhclust
###################################################
milk.dist.min5 <- dist(milk, method = "minkowski", p = 5)
milk.hclust.min5 <- hclust(milk.dist.min5)
plot(milk.hclust.min5)
milk.cut.min5 <- cutree(milk.hclust.min5, 3)
xtabs(~milk.cut+milk.cut.min5)



###################################################
### chunk number 5: altwards
###################################################
milk.hclust.ward <- hclust(milk.dist, method = "ward")
plot(milk.hclust.ward)
milk.cut.ward <- cutree(milk.hclust.ward, 3)
xtabs(~milk.cut+milk.cut.ward)



###################################################
### chunk number 6: posteda
###################################################
parcoord(milk, col = milk.cut, lty = milk.cut, 
  main = "milk data, four group clustering")
pairs(milk,
         lower.panel = function(x, y){ points(x, y,
         pch = milk.cut, col = milk.cut)},
         main = "Scatterplot for milk data")



###################################################
### chunk number 7: kmeans
###################################################
milk.kmeans <- kmeans(milk, centers = 3)
milk.kmeans$cluster
xtabs(~milk.cut+milk.kmeans$cluster)



###################################################
### chunk number 8: coph
###################################################
d.retained <- cophenetic(milk.hclust)
cor(milk.dist, d.retained)



###################################################
### chunk number 9: rand
###################################################
library(fpc)
cluster.stats(milk.dist, milk.cut, alt = milk.cut.man)



