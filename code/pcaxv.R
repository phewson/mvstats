

pcaxv <- function(X){
  UseMethod("pcaxv", X)
}



pcaxv.default <- function(X, g = 5){

N <- dim(X)[1]
p <- dim(X)[2]

index <- sample(c(1:N) )
groups <- gl(g, N %/% g)

Q <- matrix(0, g, p)
  for (i in 1:g){
  dot <- prcomp(X[-index[groups == i],])
  Q[i,] <- colSums((as.matrix(scale(X))[index[groups == i],] %*% dot$rotation)^2)
  }

colmeans <- colSums(Q) / N
PRESS <- cumsum(colmeans[c(p:1)])/ c(1:p)
PRESS <- PRESS[c(p:1)]
names(PRESS) <- paste("C", c(0:(p-1)))
results <- list("PRESS" = PRESS, dm = pcaxvconstants(N,p)$dm, dr = pcaxvconstants(N,p)$dr)
class(results) <- "pcaxv"
results
}


print.pcaxv <- function(x){
cat("Components Removed \n")

print(x[[1]])
cat("\n")
##invisible(x)
}


summary.pcaxv <- function(x){
cat("PRESS for components Removed \n")

print(x[[1]])
cat("\n")
wl <- length(x$PRESS)-1
w <- rep(NA, wl)
for (i in 1:wl){
w[i] <- ((x$PRESS[i] - x$PRESS[i+1]) / x$dm[i+1] ) / (x$PRESS[i+1] / x$dr[i+1] )
}
names(w) <- paste("C", c(1:wl))
cat("W for components included \n")
print(w)
invisible(x)
}





dot <- pcaxv(as.matrix(log(turtles[,-1])))

dot
dot <- pcaxv(as.matrix(100 * log(strider)))
summary(dot)




rtota <- function(N, p, q){
  rtot <- 0
  for (i in 1:q){
  rtot <- rtot + N + p - 2 * i
  }
rtot
}


pcaxvconstants <- function(N,p){

dm <- N + p - 2 * (p - c(p:1))
dm[1] <- NA


dr <- rep(0,p)
dr[1] <- p * (N-1)
  for (i in 2:p){
  dr[i] <- p * (N - 1) - rtota(N, p, i-1)
}

results <- list(dm = dm, dr = dr)
}

