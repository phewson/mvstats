data(USArrests)

US.dist <- dist(USArrests)
spot <- hclust(US.dist)

spot.cut <- cutree(spot, k = 5)
cluster.stats(US.dist, spot.cut)


spot.ward <- hclust(US.dist, method = "ward")
spot.ward.cut <- cutree(spot.ward, k = 7)
cluster.stats(US.dist, spot.cut, alt.clustering = spot.ward.cut)