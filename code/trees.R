
data(trees)
attach(trees)
spot <- lm(log(Volume) ~ log(Girth/12) + log(Height), data = trees)
dot <- confint(spot)

par(mfrow = c(2,2))

library(ellipse)

pdf("treesGH.pdf", width = 6, height = 6)
plot(ellipse(spot, which = c("log(Girth/12)", "log(Height)")), type = "l", lty = 3, col = "red", main = "Cone Formula", lwd = 3)
abline(v = dot[2,], lty = 2, col = "blue")
 abline(h = dot[3,], lty = 2, col = "blue")
points(2,1, pch = 4, col = "green", cex = 3)
dev.off()

pdf("treesIH.pdf", width = 6, height = 6)
plot(ellipse(spot, which = c("(Intercept)", "log(Height)")), type = "l", lty = 3, col = "red", main = "Cone Formula")
abline(v = dot[1,], lty = 2, col = "blue")
 abline(h = dot[3,], lty = 2, col = "blue")
points(log(pi/12), 1, pch = 4, col = "green", cex = 3)
dev.off()

pdf("treesIG.pdf", width = 6, height = 6)
plot(ellipse(spot, which = c("(Intercept)", "log(Girth/12)")), type = "l", lty = 3, col = "red", main = "Cone formula")
abline(v = dot[1,], lty = 2, col = "blue")
 abline(h = dot[2,], lty = 2, col = "blue")
points(log(pi/12), 2, pch = 4, col = "green", cex = 3)
dev.off()
