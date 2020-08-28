hep.cor.rob <- cov.rob(hept.df[,-1], cor = TRUE)$cor
hep.ev.rob <- princomp(hept.df[,-1], cov = hep.cor.rob, score = TRUE)





hep.princomp <- princomp(hept.df[-1], cor = TRUE)
hep.cor.rob <- cov.rob(hept.df[,-1], cor = TRUE)$cor
hep.princomp.rob <- princomp(cov = hep.cor.rob)
hep.princomp
hep.princomp.rob




> for (i in 1:4){
+ 1/4 * sum()
+ }


stickometer <- function(p){
  vec <- 1 / (1:p)
  stick <- vector("numeric", p) 
  stick[1] <- sum(vec)
     for (i in 2:p){
     stick[i] <- sum(vec[-(1:(i-1))])}
  stick <- 1/p * stick
  names(stick) <- paste("Comp.", c(1:p), sep = "")
  return(stick)
}

stick <- stickometer(7)
proptrace <- hep.princomp$sdev^2 / sum(hep.princomp$sdev^2)

stick
proptrace

idx <- barplot(proptrace, names.arg = paste("PC", c(1:7)), 
xlab = "Component", ylab = "Proportion of trace", 
main = "Proportion of trace explained")
lines(idx, stick, type = "b", pch = 16)
legend("topright", lty = 1, pch = 16, legend = "Expected values")


p <- 7
n <- 26



Horn <- function(data, reps){
p <- dim(data)[2]
n <- dim(data)[1]
Varmat <- matrix(0,p,p)
Mean <- mean(data)
diag(Varmat) <- diag(var(data))

Evals <- princomp(data, cor = TRUE)$sdev^2
idx <- barplot(Evals, names.arg = paste("PC", c(1:7)), 
xlab = "Component", ylab = "Proportion of trace", 
main = "Proportion of trace explained")

results <- matrix(0,reps,p)
  for (i in 1:reps){
  SimData <- mvrnorm(n, Mean, Varmat)
  ExpEvalsH <- princomp(SimData, cor = TRUE)$sdev^2
  results[i,] <- ExpEvalsH
  lines(idx, ExpEvalsH, type = "b", pch = 16)
  }

lines(idx, apply(results, 2, mean), type = "b", col = "red")

legend("topright", lty = 1, pch = 16, legend = "Expected values")
Results <- data.frame(Evals = Evals, ExpEvalsH = ExpEvalsH)
}

Horn(hept.df[-1], 10)


> Horn <- matrix(0, 7,7)
> diag(Horn) <- diag(var(hept.df[-1]))
> Horn.dat <- mvrnorm(26, mean(hept.df[-1]), Horn)
> Horn.dat$sdev
NULL
> princomp(Horn.dat$sdev)
Error in array(x, c(length(x), 1), if (!is.null(names(x))) list(names(x),  : 
        attempt to set an attribute on NULL
> princomp(Horn.dat)$sdev
    Comp.1     Comp.2     Comp.3     Comp.4     Comp.5     Comp.6     Comp.7 
4.01971724 3.55560325 1.08411068 0.81089295 0.40469506 0.19747217 0.05159993 
> princomp(Horn.dat, cor = TRUE)$sdev
   Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7 
1.4845198 1.1344731 1.1059463 0.9440526 0.8127973 0.6420013 0.5674628 
> plot(hep.princomp)
> idx <- plot(hep.princomp)
> lines(idx, princomp(Horn.dat, cor = TRUE)$sdev^2))
Error: syntax error in "lines(idx, princomp(Horn.dat, cor = TRUE)$sdev^2))"
> lines(idx, princomp(Horn.dat, cor = TRUE)$sdev^2)
Error in xy.coords(x, y) : 'x' and 'y' lengths differ
> princomp(Horn.dat, cor = TRUE)$sdev^2)
Error: syntax error in "princomp(Horn.dat, cor = TRUE)$sdev^2)"
> princomp(Horn.dat, cor = TRUE)$sdev^2
   Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7 
2.2037990 1.2870291 1.2231173 0.8912353 0.6606395 0.4121657 0.3220141 
> lines(idx, princomp(Horn.dat, cor = TRUE)$sdev^2)
Error in xy.coords(x, y) : 'x' and 'y' lengths differ
> idx
NULL
> 


