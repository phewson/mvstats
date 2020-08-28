 par(bty = "n", las = 1, xpd = FALSE)
curve(dnorm, lwd = 2, xlim = c(-5,8))
par(new = TRUE)
curve(dnorm(x,3,1), lwd = 2, col = "red", lty = 2, xlim = c(-5,8))
lines(c(1.5,1.5), c(0,1), lty = 3)
arrows(1.5, 0.4, 4, 0.4, length = 0.05)
##arrows(1.5, 0.4, -2, 0.4, length = 0.05)
arrows(1.5, 0.4, -2, 0.4, length = 0.05, lty = 2)
text(-2,0.4, adj  = 1, "Group 1")
text(4,0.4, adj  = 0, "Group 2")





ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
ir.species <- factor(c(rep("s", 50), rep("c", 50), rep("v", 50)))

(ir.lda <- lda(log(ir), ir.species))
ir.ld <- predict(ir.lda, dimen = 2)$x
eqscplot(ir.ld, type = "n", xlab = "first linear discriminant",
          ylab = "second linear discriminant")
text(ir.ld, labels = as.character(ir.species[-143]),
      col = 3 + unclass(ir.species), cex = 0.8)

plot(ir.lda, dimen = 1)
plot(ir.lda, type = "density", dimen = 1)

lcrabs <- log(crabs[, 4:8])
crabs.grp <- factor(c("B", "b", "O", "o")[rep(1:4, each = 50)])

(dcrabs.lda <- lda(crabs$sex ~ FL + RW + CL + CW, lcrabs))
table(crabs$sex, predict(dcrabs.lda)$class)

(dcrabs.lda4 <- lda(crabs.grp ~ FL + RW + CL + CW, lcrabs))
dcrabs.pr4 <- predict(dcrabs.lda4, dimen = 2)
dcrabs.pr2 <- dcrabs.pr4$post[, c("B", "O")] %*% c(1, 1)
table(crabs$sex, dcrabs.pr2 > 0.5)

cr.t <- dcrabs.pr4$x[, 1:2]
eqscplot(cr.t, type = "n", xlab = "First LD", ylab = "Second LD")
text(cr.t, labels = as.character(crabs.grp))
perp <- function(x, y) {
   m <- (x+y)/2
   s <- - (x[1] - y[1])/(x[2] - y[2])
   abline(c(m[2] - s*m[1], s))
   invisible()
}
# For R replace @means by $means
cr.m <- lda(cr.t, crabs$sex)$means
points(cr.m, pch = 3, mkh = 0.3)
perp(cr.m[1, ], cr.m[2, ])


