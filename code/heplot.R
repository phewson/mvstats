
between <- summary(fit)$SS$Residual
within <- summary(fit)$SS$Species
evecs <- eigen(between)$vectors
evals <- eigen(between)$values

##trans <- between %*% evecs %*% diag(sqrt(evals)) %*% t(evecs)
trans <- evecs %*%  t(evecs) %*%  diag(evals)
trans2 <- evecs %*% t(evecs)

pdf("e:/man1.pdf", width = 6, height = 6)
plot(ellipse(between/49), col = "blue", type = "l", xlim =c(-3,3), ylim = c(-3,3), main = "95 percentile for Between groups covariance: \n Petal Length versus Petal Width", lwd = 2)
legend("topright", col = "blue", lty = 1, lwd = 2, legend = c("Between"))
dev.off()

pdf("e:/man2.pdf", width = 6, height = 6)
plot(ellipse(between/49), col = "blue", type = "l", xlim =c(-3,3), ylim = c(-3,3), main = "95 percentile for Between groups covariance: \n Petal Length versus Petal Width", lwd = 2)
par(new = TRUE)
plot(ellipse(trans/49), col ="red", type = "l", xlim =c(-3,3), ylim = c(-3,3), xlab = "", ylab = "", lwd = 2)
legend("topright", col = c("blue","red"),  lty = 1, lwd = 2, legend = c("Between", "Rotated"))
dev.off()

pdf("e:/man3.pdf", width = 6, height = 6)
plot(ellipse(between/49), col = "blue", type = "l", xlim =c(-3,3), ylim = c(-3,3), main = "95 percentile for Between groups covariance: \n Petal Length versus Petal Width", lwd = 2)
par(new = TRUE)
plot(ellipse(trans/49), col ="red", type = "l", xlim =c(-3,3), ylim = c(-3,3), xlab = "", ylab = "", lwd = 2)
par(new = TRUE)
plot(ellipse(trans2/49), col ="green", type = "l", xlim = c(-3,3), ylim = c(-3,3), xlab = "", ylab = "", lwd = 2)
legend("topright", col = c("blue", "red", "green"), lty = 1, lwd = 2, legend = c("Between", "Rotated", "Normalised"))
dev.off()



###
#  Functions for HE ellipses
#  Michael Friendly
#  Revised: 19 Oct 2005 11:06:17
#
#  Example:
#   data(iris)
#   attach(iris)
#   YY<-iris[,1:4]
#   fit<-manova(as.matrix(YY) ~ Species)
#   source("heplot.R")
#   ellipse.manova(fit, Species, which=c(1,2))
###

# return an npoints+1 x 2 matrix for a single data ellipse 
# for two variables from a numeric data matrix or for two variables
# from a covariance matrix

"ellipse.matrix" <-
 function(data, which=c(1,2), d=length(which), 
	radius=sqrt( d * qf( level, d, n)),
	level=0.68, npoints=100, center) 
{
	size <- dim(data)
	# is it a variance-covariance matrix?
	if (size[1]==size[2] && all(data == t(data))) {
		radius = sqrt(qchisq(level, 2))
		mean <- if (!missing(center)) center
				else c(0,0)
		S = data[which,which]
	}
	else {
	   n = nrow(data)
	   mean = apply(data, 2, mean)[which]
	   S = var(data)[which,which]
	}
	
	circle <- function( np = 100 ) {
	   theta <- (0:np) * 2 * pi /np
	   cbind( cos(theta), sin(theta))
	}
	varcov.ellipse <-
	function(v, center,  radius = 1, npoints=100)  {
	t (  t (( radius * circle(np=npoints)) %*% chol(v) ) + center) 
	}

    if (!is.null(dimnames(data)[[2]])) 
      names <- dimnames(data)[[2]][which]
	else names = c("X", "Y")
	
	ell <- varcov.ellipse(S, mean, radius, npoints)
}

# Produce an HE plot of the ellipses for the H and E matrices
# from a manova object.

"ellipse.manova" <- 
 function(fit, effect, which=c(1,2), level=0.68, center.pch="+",
  	col=palette()[1:2], lty=c(1,2), lwd=2, ...) 
{
	if (!inherits(fit,"manova")) stop("Not a manova object")
#	require(car)
	sumry <- summary(fit)
	data <- model.response(model.frame(fit))
	# need accessor functions here 
	SS <-sumry$SS
	eff <-  if (is.character(effect)) effect else deparse(substitute(effect))
	dfe <- fit$df.residual
	H <- SS[[eff]]
	E <- SS[["Residuals"]]
	H <- H[which, which]  / dfe
	E <- E[which, which]  / dfe
	names <- dimnames(H)[[1]][which]
#	print(H); print(E);
	
	gmean<-colMeans(data)[which]
	print(gmean)
	### could replace ellipse.matrix with car ellipse() if it returned a result
#	ell.H <- ellipse(gmean, H, radius=, ...)

	ell.H <- ellipse.matrix(H, level=level, center=gmean)
	ell.E <- ellipse.matrix(E, level=level, center=gmean)

	setup <- function(xy, ...) 
		plot(range(xy[,1]), 
        	 range(xy[,2]), type = "n", xlab=names[1], ylab=names[2], ...)
		
	setup( rbind(ell.H,ell.E) )
	points(gmean[1], gmean[2], pch=center.pch, cex=2, col="green")
	lines(ell.E, col=col[2], lty=lty[2], lwd=lwd)
	lines(ell.H, col=col[1], lty=lty[1], lwd=lwd)
	means <- aggregate(data,list(effect),FUN=mean)
#	print(means)
	groups <- means[,1]
	means <- means[,1+which]
	print(means)

	points(means[,1], means[,2])
	text(means[,1], means[,2], labels=as.character(groups), pos=3)
	
}
