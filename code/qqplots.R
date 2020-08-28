qqmvm <-
    function(y, ylim=range(y),main="Multivariate Q-Q Plot",
	     xlab="Theoretical Quantiles", ylab="Sample Quantiles",
	     plot.it=TRUE, datax = FALSE, df1=dim(y)[[2]],...)
{
##y <- mahalanobis(y.mat, mean(y.mat), var(y.mat))

    if(has.na <- any(ina <- is.na(y))) { ## keep NA's in proper places
        yN <- y
        y <- y[!ina]
    }
    if(0 == (n <- length(y)))
        stop("y is empty or has only NAs")
    if (plot.it && missing(ylim))
        ylim <- range(y)
    x <- qchisq(ppoints(n),df1)[order(order(y))]
    if(has.na) {
        y <- x; x <- yN; x[!ina] <- y
        y <- yN
    }
    if(plot.it)
        if (datax)
            plot(y, x, main= main, xlab= ylab, ylab=xlab, xlim = ylim, ...)
        else
            plot(x, y, main= main, xlab= xlab, ylab= ylab, ylim= ylim, ...)
    invisible(if(datax) list(x = y, y = x) else list(x = x, y = y))
}

qqlinemvm <- function(y, datax = FALSE, df1 = dim(y.mat)[[2]], ...)
{
##y <- mahalanobis(y.mat, mean(y.mat), var(y.mat))
    y <- quantile(y[!is.na(y)],c(0.25, 0.75))
    x <- qchisq(c(0.25, 0.75), df1)
    if (datax) {
        slope <- diff(x)/diff(y)
        int <- x[1] - slope*y[1]
    } else {
        slope <- diff(y)/diff(x)
        int <- y[1]-slope*x[1]
    }
    abline(int, slope, ...)
}


USA.dist <- mahalanobis(USArrests, mean(USArrests), var(USArrests))
qqmvm(USArrests)
qqlinemvm(USArrests)


dot <- princomp(USArrests)
got <- dot$score

dot <- prcomp(USArrests, retx = TRUE)
got <- dot$x

truehist(mahalanobis(got, mean(got), var(got)), nbins = 9)
curve(dchisq(x, df = 4), add = TRUE)




USA.mah.dist <- mahalanobis(USArrests, mean(USArrests), var(USArrests))
n <- 50
df1 <- 4
qqplot(dot, qchisq(n,df1))
