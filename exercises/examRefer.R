

by(flea.beetles[,-1], flea.beetles$Species, colMeans)

by(flea.beetles[,-1], flea.beetles$Species, cov)


spot 
predict(spot)$x

xtabs(~ flea.beetles[,1] + predict(spot)$class)



plot(predict(spot)$x, (c(rep(0,39))), ylim = c(0,0.5), pch = as.numeric(flea.beetles[,1]), col = as.numeric(flea.beetles[,1]), xlab = "Discriminant Function", ylab = "Density", main = "Flea Beetle Discriminant Function" )
lines(density(predict(spot)$x[flea.beetles$Species == "oleracea"]))
lines(density(predict(spot)$x[flea.beetles$Species == "carduorum"]), col = "red", lty = 2)

text(mean(predict(spot)$x[flea.beetles$Species == "oleracea"]), 0 , "X", cex = 1.5)
text(mean(predict(spot)$x[flea.beetles$Species == "carduorum"]), 0 , "X", cex = 1.5, col = "red")
 
legend("topright", lty = c(1,2), col = c("black", "red"), legend = c("oleracea", "carduorum"))



econ <- read.csv("econ.csv")
prcomp(econ[,-c(1,11)])
 print(xtable(cor(econ[,-c(1,11)])), type = "html")
print(xtable(as.matrix(prcomp(econ[,-c(1,11)], scale = TRUE)$rotation)), type = "html")
 econ.ev <- eigen(cor(econ[,-c(1,11)]))
 for (i in 1:9){
 loadings[,i] <- sqrt(econ.ev$values[i]) * econ.ev$vectors[,i]
 }



factanal(econ[,-c(1,11)], factors = 4)


library(cluster)
X <- cmdscale(daisy(flower))
flwers <- c("Begonia", "Broom", "Camelia", "Dahlia", "Forget-me-not", "Fuchsia", "Geranium", "Gladiolus", "Heather", "Hydrangia", "Iris", "Lily", "Lily-of-the-valley", "Peony", "Pink", "Red Rose", "Scotch Rose","Tulip")

par(xpd = NA, bty = "n")
plot(X, type = "n", xlab = "X1", ylab = "X2", main = "Metric scaling of plant data")
text(X, flwers, cex = 0.7)
