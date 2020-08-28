"factor.model.stat" <-
function (x, weight=seq(1, 3, length=nobs), output="full", center=TRUE, 
	frac.var=.5, iter.max=1, nfac.miss=1, full.min=20, reg.min=40, 
	sd.min=20, quan.sd=.90, tol=1e-3, zero.load=FALSE, 
	range.factors=c(0, Inf))
{
        fun.copyright <- "Placed in the public domain in 2006 by Burns Statistics"
	fun.version <- "factor.model.stat 006"

	subfun.ssd <- function(z, weight, sd.min) {
		nas <- is.na(z)
		if(any(nas)) {
			if(sum(!nas) < sd.min) return(NA)
			sum(weight[!nas] * z[!nas]^2) / sum(weight[!nas])
		} else {
			sum(weight * z^2)
		}
	}

	#
	# start of main function
	#

	if(is.data.frame(x)) {
		x <- as.matrix(x)
	} else {
		if(!is.matrix(x)) stop("x needs to be a matrix")
	}
	if(!is.numeric(x)) stop("x needs to be numeric")
	x[!is.finite(x)] <- NA

	xna <- is.na(x)
	allmis <- rowSums(xna) == ncol(x)
	if(any(allmis)) {
		x <- x[!allmis, , drop=FALSE]
		xna <- is.na(x)
	}
	num.mis <- colSums(xna)

	if(any(num.mis > 0)) {
		if(sum(num.mis == 0) < full.min) 
			stop("not enough columns without missing values")
		if(!length(dimnames(x)[[2]])) 
			stop("x needs column names when missing values exist")

		max.miss <- max(num.mis)
		lnfm <- length(nfac.miss)
		if(lnfm == 0) stop("nfac.miss must have positive length")
		nfac.miss <- round(nfac.miss)
		if(any(nfac.miss < 0)) 
			stop("negative values in nfac.miss")
		if(lnfm < max.miss) {
			nfac.miss <- c(nfac.miss, rep(nfac.miss[lnfm],
				max.miss - lnfm))
		}
	}

	if(length(output) != 1) stop("output needs to be a single string")
	output.menu <- c("full", "factor")
	output.num <- pmatch(output, output.menu, nomatch=0)
	if(output.num == 0) stop("unknown or ambiguous value for output")
	output <- output.menu[output.num]

	nassets <- ncol(x)
	nobs <- nrow(x)
	if(length(weight) != nobs) {
		if(length(weight) == nobs + sum(allmis)) {
			weight <- weight[!allmis]
		} else {
			stop("weight vector is the wrong length")
		}
	}
	if(any(weight < 0)) stop("negative weights not allowed")
	weight <- weight / sum(weight)

	if(is.logical(center)) {
		if(center) {
			center <- colMeans(x, na.rm=TRUE)
		} else {
			center <- rep(0, nassets)
		}
	} else if(length(center) != nassets) stop("center is the wrong length")
	x <- sweep(x, 2, center, "-")

	sdev <- sqrt(apply(x, 2, subfun.ssd, weight=weight, sd.min=sd.min))
	if(any(sdev <= 0, na.rm=TRUE)) {
		stop(paste(sum(sdev <= 0, na.rm=TRUE), 
			"asset(s) with constant returns"))
	}
	if(any(is.na(sdev))) {
		sdev[is.na(sdev)] <- quantile(sdev, quan.sd, na.rm=TRUE)
	}
	x <- scale(x, scale=sdev, center=FALSE)

	xw <- sqrt(weight) * x
	fullxw <- xw[, num.mis == 0, drop=FALSE]
	fx.svd <- svd(fullxw, nu=0)
	cumvar <- cumsum(fx.svd$d^2) / sum(fx.svd$d^2)
	nfac <- sum(cumvar < frac.var) + 1
	if(nfac > max(range.factors)) nfac <- max(range.factors)
	if(nfac < min(range.factors)) nfac <- min(range.factors)
	if(nfac > length(cumvar)) nfac <- length(cumvar)
	fseq <- 1:nfac
	loadings <- scale(fx.svd$v[, fseq, drop=FALSE], scale=fx.svd$d[fseq], 
		center=FALSE)

	if(iter.max > 0) {
		cormat <- t(fullxw) %*% fullxw
                uniqueness <- 1 - rowSums(loadings^2)
                uniqueness[uniqueness < 0] <- 0
                uniqueness[uniqueness > 1] <- 1
		start <- uniqueness
		converged <- FALSE
                for(i in 1:iter.max) {
                        cor.red <- cormat
                        diag(cor.red) <- diag(cor.red) - uniqueness
                        t.eig <- eigen(cor.red)
                        t.val <- t.eig$value[fseq]
                        t.val[t.val < 0] <- 0
                        loadings <- scale(t.eig$vector[, fseq, drop=FALSE], 
				center=FALSE, scale=1/sqrt(t.val))
                        uniqueness <- 1 - rowSums(loadings^2)
                        uniqueness[uniqueness < 0] <- 0
                        uniqueness[uniqueness > 1] <- 1
                        if(all(abs(uniqueness - start) < tol)) {
                                converged <- TRUE
                                break
                        }
                        start <- uniqueness
                }
	}
	dimnames(loadings) <- list(dimnames(fullxw)[[2]], NULL)

	if(any(num.mis)) {
		# calculate loadings for columns with NAs
		floadings <- loadings
		if(zero.load) {
			loadings <- array(0, c(nassets, nfac))
		} else {
			meanload <- colMeans(floadings)
			loadings <- t(array(meanload, c(nfac, nassets)))
		}
		dimnames(loadings) <- list(dimnames(x)[[2]], NULL)
		loadings[dimnames(floadings)[[1]], ] <- floadings
		scores <- fullxw %*% floadings
		dsquare <- fx.svd$d[1:nfac]^2
		nfac.miss[nfac.miss > nfac] <- nfac
		
		for(i in (1:nassets)[num.mis > 0 & nobs - num.mis > reg.min]) {
			t.nfac <- nfac.miss[ num.mis[i] ]
			if(t.nfac == 0) next
			t.okay <- !is.na(xw[, i])
			t.seq <- 1:t.nfac
			t.load <- lsfit(xw[t.okay, i], scores[t.okay, t.seq], 
				intercept=FALSE)$coef / dsquare[t.seq]
			loadings[i, t.seq] <- t.load
			NULL
		}
	}

	comm <- rowSums(loadings^2)
	if(any(comm > 1)) {
		# adjust loadings where communalities too large
		toobig <- comm > 1
		loadings[toobig,] <- loadings[toobig,] / sqrt(comm[toobig])
		comm[toobig] <- 1
	}
	
	switch(output, 
		full= {
			ans <- loadings %*% t(loadings)
			diag(ans) <- 1
			ans <- t(sdev * ans) * sdev
		},
		factor={
			ans <- list(loadings=loadings, uniquenesses= 1 - comm,
				sdev=sdev, call=match.call())
			class(ans) <- "statfacmodBurSt"
		})
	ans
}

