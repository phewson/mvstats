library(grid)
library(MASS)
X <- mvrnorm(50,c(0,0),matrix(c(1,0.9,0.9,1),2,2) )
Y <- mvrnorm(50,c(0,2),matrix(c(1,0.9,0.9,1),2,2) )

spot <- lda(rbind(X,Y), grouping = c(rep(1,50),rep(2,50)) )

angle <- -spot$scaling[1,] / pi * 180


z1 <- density(X %*% spot$scaling)
z2 <- density(Y %*% spot$scaling)

x1 <- density(X[,1])
x2 <- density(Y[,1])

y1 <- density(X[,2])
y2 <- density(Y[,2])



pushViewport(viewport())

pushViewport(plotViewport(c(10,10,10,10)))
pushViewport(dataViewport(c(X[,1],Y[,1]),c(X[,2],Y[,2]),name="nn"))
grid.points(X[,1],X[,2], draw = TRUE, pch = 16)
grid.points(Y[,1],Y[,2], draw = TRUE, pch = 16, gp =gpar(col="blue"))
##idxX <- chull(X)
##idxX <- c(idxX, idxX[1])
##grid.lines(x=unit(X[idxX,1], "native"), y=unit(X[idxX,2], "native"))
popViewport()
popViewport()



pushViewport(viewport(width = unit(0.5,"npc"), 
  height = unit(0.2, "npc"), x = unit(0.5,"npc"), y = unit(0.2,"npc") ) )
pushViewport(dataViewport(c(x1$x,x2$x), c(x1$y,x2$y),name="density"))
 grid.lines(x = unit(x1$x,"native"),y=unit( x1$y, "native"), name = "density")
 grid.lines(x = unit(x2$x,"native"),y=unit( x2$y, "native"), name = "density", gp=gpar(col="blue"))
popViewport()
popViewport()




pushViewport(viewport(width = unit(0.5,"npc"), 
  height = unit(0.2, "npc"), x = unit(0.2,"npc"), y = unit(0.5,"npc") , angle = 90) )
pushViewport(dataViewport(c(y1$x,y2$x), c(-y1$y,-y2$y),name="density"))
 grid.lines(x = unit(y1$x,"native"),y=unit( -y1$y, "native"), name = "density")
 grid.lines(x = unit(y2$x,"native"),y=unit( -y2$y, "native"), name = "density", gp=gpar(col="blue"))
popViewport()
popViewport()




## try an optimal rotation

pushViewport(viewport(width = unit(0.5,"npc"), 
  height = unit(0.2, "npc"), x = unit(0.677,"npc"), y = unit(0.762,"npc"), angle = 116) )
pushViewport(dataViewport(c(z1$x,z2$x), c(z1$y,z2$y),name="density"))
 grid.lines(x = unit(z1$x,"native"),y=unit( z1$y, "native"), name = "density")
 grid.lines(x = unit(z2$x,"native"),y=unit( z2$y, "native"), name = "density", gp=gpar(col="blue"))
popViewport()
popViewport()







