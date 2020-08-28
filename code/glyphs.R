> attach(USArrests)
> g1 <- (Assault - min(Assault)) / (max(Assault) - min(Assault))
> g2 <- (Murder - min(Murder)) / (max(Murder) - min(Murder))
> angle <- pi * g2 * acos(-1/pi)
> x1 <- UrbanPop + 20 * g1 + cos(angle) * 4
> y1 <- Rape + 20 * g1 + sin(angle)
> plot(UrbanPop, Rape)
> arrows(UrbanPop, Rape, x1, x2)
Error in arrows(UrbanPop, Rape, x1, x2) : Object "x2" not found
> arrows(UrbanPop, Rape, x1, y2)
Error in arrows(UrbanPop, Rape, x1, y2) : Object "y2" not found
> arrows(UrbanPop, Rape, x1, y1)
> y1 <- Rape + 5 * g1 + sin(angle)
> x1 <- UrbanPop + 5 * g1 + cos(angle) * 4
> plot(UrbanPop, Rape)
> arrows(UrbanPop, Rape, x1, y1)
> > attach(USArrests)
> g1 <- (Assault - min(Assault)) / (max(Assault) - min(Assault))
> g2 <- (Murder - min(Murder)) / (max(Murder) - min(Murder))
> angle <- pi * g2 * acos(-1/pi)
> x1 <- UrbanPop + 20 * g1 + cos(angle) * 4
> y1 <- Rape + 20 * g1 + sin(angle)
> plot(UrbanPop, Rape)
> arrows(UrbanPop, Rape, x1, x2)
Error in arrows(UrbanPop, Rape, x1, x2) : Object "x2" not found
> arrows(UrbanPop, Rape, x1, y2)
Error in arrows(UrbanPop, Rape, x1, y2) : Object "y2" not found
> arrows(UrbanPop, Rape, x1, y1)
> y1 <- Rape + 5 * g1 + sin(angle)
> x1 <- UrbanPop + 5 * g1 + cos(angle) * 4
> plot(UrbanPop, Rape)
> arrows(UrbanPop, Rape, x1, y1)
> 


## g2 sets the angle, g1 sets the length


x1 <- Assault
x2 <- Murder
x4 <- Rape
x3 <- UrbanPop

glyphs <- function(x1, x2, x3, x4, scale1 = 2, scale2 = 5){
g1 <- (x3 - min(x3)) / (max(x3) - min(x3))
g2 <- (x4 - min(x4)) / (max(x4) - min(x4))
angle <- pi * g2 * acos(-1/pi)
y1 <- x1 +  scale1 * g1 + cos(angle) * scale2
y2 <- x2 + scale1 * g1 + sin(angle)
par(bty = "n", xpd = NA)
plot(x1, x2)
arrows(x1, x2, y1, y2,  length = 0.1, col = round(x4))
}



Error in arrows(UrbanPop, Rape, x1, x2) : Object "x2" not found
> arrows(UrbanPop, Rape, x1, y2)
Error in arrows(UrbanPop, Rape, x1, y2) : Object "y2" not found
> arrows(UrbanPop, Rape, x1, y1)
> y1 <- Rape + 5 * g1 + sin(angle)
> x1 <- UrbanPop + 5 * g1 + cos(angle) * 4
> plot(UrbanPop, Rape)
> arrows(UrbanPop, Rape, x1, y1)
