library(MASS)
X <- mvrnorm(100, mu = c(0,0), Sigma = matrix(c(2,0,0,1),2,2))

X.d <- kde2d(X[,1], X[,2])
contour(X.d, levels = 0.05)

z1 <- 1/sqrt(2) * X[,1] + 1/sqrt(2) * X[,2]

d.z1 <- density(z1)


plot(d.z1)
d.z1$y <- d.z1$y * (max(d.z1$x) - min(d.z1$x)) * max(d.z1$y)
plot(d.z1)


##xlim <- c( min(d.x$x), max(d.x$x)) * 1.5
##ylim <- c(0, max(d.x$y)) * 1.5

limits <- cbind(xlim, ylim)
plot(d.x, xlim = limits[,1], ylim = limits[,2])

xline <- cbind(x = c(-3:3), y = rep(0, 7))
##not <- density(x)

rotmat <- function(pi){
rotmat <- matrix(c(cos(pi), sin(pi), -sin(pi), cos(pi)),2,2)
return(rotmat)
}

what <- as.matrix(cbind(d.z1$x, d.z1$y)) %*% rotmat(0.4 * pi)
limits2 <- limits %*% rotmat(2)

plot(1,1,type = "n", xlim = c(-10,10), ylim = c(-10,10))
lines(what)
lines(xline %*% rotmat(2) )

par(new = TRUE)
what <- as.matrix(cbind(d.x$x, d.x$y)) %*% rotmat(pi)
limits2 <- limits %*% rotmat(pi)

plot(1,1,type = "n", xlim = c(-10,10), ylim = c(-10,10))
lines(what)
lines(xline %*% rotmat(pi) )




what <- as.matrix(cbind(not$x, not$y)) %*% matrix(c(cos(2), sin(2), -sin(2), cos(2)),2,2)



what <- as.matrix(cbind(not$x, not$y*20)) %*% rotmat(-2)


plot(what, type = "l", xlim = c(-,5), ylim = c(-20,5))
##plot(what, type = "l")##, xlim = c(-20,5), ylim = c(-20,5))

par(new = TRUE)
what <- as.matrix(cbind(not$x, not$y*20)) %*% matrix(c(cos(2), sin(2), -sin(2), cos(2)),2,2)
##what[,1] <- 10 * what[,1]
plot(what, type = "l", xlim = c(-5,20), ylim = c(-20,20))
##plot(what, type = "l")##, xlim = c(-20,5), ylim = c(-20,5))






myhist <- histogram(rnorm(50)) 

pushViewport(viewport(x=0, width=0.5, just="left"))

print(myhist, newpage=FALSE)

popViewport()

pushViewport(viewport(x=0,
                         # Make the rotated width the same as
                         # the height of the page
                         width=convertUnit(unit(1, "npc"), "npc",
                           "y", "dimension", "x", "dimension"),
                         # Make the rotated height half the
                         # width of the page
                         height=convertUnit(unit(0.5, "npc"), "npc",
                           "x", "dimension", "y", "dimension"),
                         angle=270))

print(densityplot(z1), newpage=FALSE)
popViewport() 

