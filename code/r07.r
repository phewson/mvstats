					R07

				Principal Components Analysis


options(digits=4, scipen=3)

# Eigenvalues and eigenvectors

A <- matrix(sample(9), nrow=3)
A

A.e <- eigen(A)
A.e

A %*% A.e$vectors[,1]
A.e$values[1] * A.e$vectors[,1]

A %*% A.e$vectors[,2]
A.e$values[2] * A.e$vectors[,2]

A %*% A.e$vectors[,3]
A.e$values[3] * A.e$vectors[,3]


# PCA Example

X <- as.matrix(read.table("H:/S5600/Data sets/ASCII/Ch04/PCA_EXAMPLE.txt"))

apply(X, 2, mean)
apply(X, 2, sd)		# already standardized

pairs(X)

library(rgl)

rgl.open()
bg3d("white")

rgl.spheres(X[,1],X[,2],X[,3],radius=.1, color=rainbow(300)[rank(X[,1])])

X.cor <- cor(X)
X.cor

X.e <- eigen(X.cor)

X.e

X.svd <- svd(X.cor)
X.svd

U <- X.e$vectors	# Get rotation matrix of eigenvectors

zapsmall(t(U) %*% U)

Z <- X %*% U		# Rotated data matrix

pairs(Z)
zapsmall(cor(Z))

rgl.clear()		# All three principal components
rgl.spheres(Z[,1],Z[,2],Z[,3],radius=.1, color=rainbow(300)[rank(X[,1])])

rgl.clear()		# First two principal components
rgl.spheres(Z[,1],Z[,2],rep(0,300),radius=.1, color=rainbow(300)[rank(X[,1])])

rgl.clear()		# First principal component
rgl.spheres(Z[,1],rep(0,300),rep(0,300),radius=.1, color=rainbow(300)[rank(X[,1])])

# Rotate back to X-space

Z1 <- cbind(Z[,1], rep(0,300), rep(0,300))
X1 <- Z1 %*% t(U)

rgl.clear()
rgl.spheres(X[,1],X[,2],X[,3],radius=.1, color=rainbow(300)[rank(X[,1])], alpha=.3)
	# Original data
rgl.spheres(X1[,1],X1[,2],X1[,3],radius=.1, color=rainbow(300)[rank(X[,1])])
	# First PC

Z2 <- cbind(rep(0,300), Z[,2], rep(0,300))
X2 <- Z2 %*% t(U)

rgl.spheres(X2[,1],X2[,2],X2[,3],radius=.1, color=rainbow(300)[rank(X[,1])])
	# Second PC

Z3 <- cbind(rep(0,300),rep(0,300), Z[,3])
X3 <- Z3 %*% t(U)

rgl.spheres(X3[,1],X3[,2],X3[,3],radius=.1, color=rainbow(300)[rank(X[,1])])
	# Third PC

Z12 <- cbind(Z[,1:2],rep(0, 300))
X12 <- Z12 %*% t(U)

rgl.clear()
rgl.spheres(X[,1],X[,2],X[,3],radius=.1, color=rainbow(300)[rank(X[,1])], alpha=.3)
rgl.spheres(X12[,1],X12[,2],X12[,3],radius=.1, color=rainbow(300)[rank(X[,1])])
	# First two PCs

# Fractions of variance

lambda <- X.e$values	# Eigenvalues of cor(X) = variances of PCs
lambda

zapsmall(cov(Z))

zapsmall(cor(Z))

diag(cov(Z))
sum(diag(cov(Z)))	# Note: sum = number of dimensions = sum of original correlations

# The prcomp( ) function

X.pc <- prcomp(X, scale=T)
X.pc

X.pc$rotation	# Compare to U above - same except for possible sign reversal

U	

X.pc$sdev	# Component sds
X.pc$sdev^2	# Component Variances - compare to lambda above
lambda		

pairs(X.pc$x)	# Same as Z above, again except for possible sign change


# New example: State economic data
data(state)

gsp.raw<-read.table("H:/S5600/GSP_RAW.txt",header=T)
rownames(gsp.raw) <- state.abb
pairs(gsp.raw)
stars(gsp.raw, key.loc=c(15, 1.5))
cor(gsp.raw)	# Strong correlation

gsp.raw.pc <- prcomp(gsp.raw, scale=T)
gsp.raw.pc$sdev^2	# Variances - almost everything in PC 1

gsp.raw.pc$rotation[,1:2]	# Loadings for PCs 1 and 2
	# Loadings for PC 1 almost all equal - simply a measure of economic size

# COnvert each state's earnings into a percentage for each sector

gsp.share <- gsp.raw/apply(gsp.raw, 1, sum)*100

pairs(gsp.share)
stars(gsp.share, key.loc=c(15, 1.5))
cor(gsp.share)

gsp.share.pc <- prcomp(gsp.share, scale=T)
zapsmall(gsp.share.pc$sdev^2)	# Variances - much more balanced

pairs(gsp.share.pc$x[,1:6]) 	# First 6 PCs
plot(gsp.share.pc$x[,1:2], pch=16)	# First 2
	# identify outliers
identify(gsp.share.pc$x[,1], gsp.share.pc$x[,2], state.abb)
	# Or label all points
plot(gsp.share.pc$x[,1:2], type='n')
text(gsp.share.pc$x[,1:2], state.abb)

# Loadings - first 6 PCs

gsp.share.pc$rotation[,1:6]

# Values of some outliers in original units

gsp.share[c(2,8,11,28,50),]

# biplot - loadings on 1st 2 PCs and plot of first 2 PCs

biplot(gsp.share.pc)

# How many components?

plot(gsp.share.pc)	# Scree plot
plot(gsp.share.pc$sdev^2, type="o", pch=16)	# Alternate Version

zapsmall(gsp.share.pc$sdev^2)
sum(gsp.share.pc$sdev>1)	# Kaiser's Rule - keep components with variance > 1
abline(h=1,col="grey")

summary(gsp.share.pc)

# Horn's procedure - compare to randomly permuted (bootstrapped) data

gsp.boot <- gsp.share
for (i in 1:13) {
	gsp.boot[,i] <- sample(gsp.share[,i], replace=T) }
gsp.boot.pc <- prcomp(gsp.boot, scale=T)
points(gsp.boot.pc$sdev^2, type="o")	# Add to alternate scree plot; compare
					# Repeat; add different runs in different colors


# Another example - planetary data

planets1<-read.table("H:/S5600/planets.txt")
planets1
pairs(planets1)

# Log transform values except rings (add one to number Moons)

planets <- cbind(logDist=log(planets1$Dist), logRadius = log(planets1$Radius), 
	logMass =log(planets1$Mass), logDensity=log(planets1$Density), 		logMoons=log(planets1$Moons+1), Rings=unclass(planets1$Rings)-1) 	 
rownames(planets)<-rownames(planets1)
planets
pairs(planets)

planets.pc <- prcomp(planets, scale=T)
planets.pc$sdev^2

plot(planets.pc$sdev^2, type="o", pch=16)
abline(h=1,col="grey")

pairs(planets.pc$x)
plot(planets.pc$x[,1:2], pch=16)

identify(planets.pc$x[,1], planets.pc$x[,2], rownames(planets))
	# Note the three clusters

planets.pc$rotation[,1:2]

biplot(planets.pc)



