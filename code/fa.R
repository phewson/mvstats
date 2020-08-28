
## Stage 1  Initial estimates of uniqueness

r2s <- vector("numeric", 9)

for (i in 1:9){
y <- econ[,i]
x <- econ[,-i]
mod <- lm(y~as.matrix(x))
r2s[i] <- summary(mod)$r.squared
}


unique <- diag(1-r2s)
diag(unique)

## now enter loop



new <- econ.cor - unique
new.ev <- eigen(new)

loadings4pf <- matrix(0,9,4)
for (i in 1:4){
loadings4pf[,i] <- sqrt(new.ev$values[i]) * new.ev$vectors[,i]
}

LLt <- loadings4pf %*% t(loadings4pf)

unique.f <- econ.cor - LLt
diag(unique) <- diag(unique.f)

diag(unique)

loadings4pf


iter <- loop(econ.cor, unique, 1, 9, 4)



#######



new2.ev <- eigen(new2)

loadings4pf <- matrix(0,9,4)
for (i in 1:4){
loadings4pf[,i] <- sqrt(new2.ev$values[i]) * new2.ev$vectors[,i]
}

LLt <- loadings4 %*% t(loadings4)
unique <- diag(econ.cor - LLt)


error <- econ.cor - (LLt + unique)







loop <- function(covmat, umat, rep, p, q){
  new <- covmat - umat
  new.ev <- eigen(new)

  loadings <- matrix(0,p,q)
    for (i in 1:q){
    loadings[,i] <- sqrt(new.ev$values[i]) * new.ev$vectors[,i]
    }

  LLt <- loadings %*% t(loadings)

  unique.f <- covmat - LLt
  diag(unique) <- diag(unique.f)

res <- list(loadings = loadings, unique = unique)
return(res)
}






loadml <- loadings(econ.fact)
class(loadml) <- "matrix"

uniqueml <- econ.fact$uniquenesses

resid <- econ.cor - ( loadml%*% t(loadml) + diag(uniqueml) )



> apply(loadml^2, 1, sum)




varimax(loadings4)






plot(loadings4[,c(1:2)], pch = as.character(c(1:9)), xlab = expression(paste(gamma,"1")), ylab = expression(paste(gamma,"2")), main = "First and second loadings", xlim = c(-1,1), ylim = c(-1,1))
points(varimax(loadings4)$loadings[,c(1:2)], pch = letters[c(1:9)], col = "red")
abline(h = 0)
abline(v = 0)








> econ <- read.csv("econ.csv", row.names = 1)
> econ.cor <- cor(econ)
> econ.ev <- eigen(econ.cor)
> loadings <- matrix(0,9,9)
> for (i in 1:9){
> loadings[,i] <- sqrt(econ.ev$values[i]) * econ.ev$vectors[,i]
> }
> econ.cor - loadings %*% t(loadings) ## should equal zero





> loadings4 <- matrix(0,9,4)
> for (i in 1:4){
> loadings4[,i] <- sqrt(econ.ev$values[i]) * econ.ev$vectors[,i]
> }
> LLt <- loadings4 %*% t(loadings4)
> unique <- diag(econ.cor - LLt)
> error <- econ.cor - (LLt + unique)






> econ.ev$values
[1] 3.482820 2.132332 1.098373 0.9984261 0.5450933
[6] 0.3836385 0.2224905 0.1367327 0.0000930
> econ.ev$values / 0.09
[1] 38.698003578 23.692581759 12.204146983 11.093622860 6.056592340
[6] 4.262650255 2.472116648 1.519252072 0.001033506
> cumsum(econ.ev$values/0.09)
[1] 38.69800 62.39059 74.59473 85.68836 91.74495 96.00760 98.47971
[8] 99.99897 100.00000







> row.names(loadings4) <- row.names(econ.cor)
> apply(loadings4^2, 1, sum)
Agriculture Mining
0.9664145 0.8630706
Manufacture PowerSupplies
0.8355980 0.8737656
Construction ServiceIndustries
0.8414721 0.7791356
Finance SocialAndPersonalServices
0.8395906 0.9025525
TransportAndCommunications
0.8103523










> r2s <- vector("numeric", 9)
>
> for (i in 1:9){
+ y <- econ[,i]
+ x <- econ[,-i]
+ mod <- lm(y~as.matrix(x))
+ r2s[i] <- summary(mod)$r.squared
+ }
>
>
> unique <- diag(1-r2s)
> diag(unique)
[1] 0.0001429627 0.0420230140 0.0006887118 0.1325518542 0.0138610514
[6] 0.0016432160 0.0043879128 0.0007569780 0.0161276459














> new <- econ.cor - unique
> new.ev <- eigen(new)
>
> loadings4pf <- matrix(0,9,4)
> for (i in 1:4){
+ loadings4pf[,i] <- sqrt(new.ev$values[i]) * new.ev$vectors[,i]
+ }
>
> LLt <- loadings4pf %*% t(loadings4pf)
>
> unique.f <- econ.cor - LLt
> diag(unique) <- diag(unique.f)
>
> diag(unique)
[1] 0.02890147 0.14965321 0.15824274 0.25101144 0.14818053 0.21896192 0.15269320
[8] 0.09280381 0.20001567
>
> loadings4pf
[,1] [,2] [,3] [,4]
[1,] -0.980556270 -0.06951113 0.06899117 -0.004043332
[2,] -0.007596438 -0.88584019 -0.20248258 0.156770695
[3,] 0.644457570 -0.53242421 -0.27602489 -0.258391989
[4,] 0.453749164 -0.35005068 -0.40926760 0.503055476
[5,] 0.607710009 -0.08927793 -0.03546047 -0.687953501
[6,] 0.711408766 0.50862889 -0.12606387 -0.018444548
[7,] 0.140377243 0.67201322 -0.59955488 0.128581528
[8,] 0.727082131 0.31778092 0.43171651 0.301966740
[9,] 0.681111481 -0.30230726 0.45690884 0.189515460


















> econ <- read.csv("econ.csv", row.names = 1)
> econ.fact <- factanal(econ, factors = 4, rotation = "none")








> loadml <- loadings(econ.fact)
> class(loadml) <- "matrix"
> uniqueml <- econ.fact$uniquenesses
> resid <- econ.cor - ( loadml%*% t(loadml) + diag(uniqueml) )
> resid












> apply(loadml^2, 1, sum)
Agriculture Mining
0.9954167 0.5937541
Manufacture PowerSupplies
0.9950263 0.9950022
Construction ServiceIndustries
0.4852833 0.8360147
Finance SocialAndPersonalServices
0.4786655 0.9950786
TransportAndCommunications
0.4676025













> varimax(loadings4)






> plot(loadings4[,c(1:2)], pch = as.character(c(1:9)),
xlab = expression(paste(gamma,"1")), ylab = expression(paste(gamma,"2")),
main = "First and second loadings",
xlim = c(-1,1), ylim = c(-1,1))
> points(varimax(loadings4)$loadings[,c(1:2)],
pch = letters[c(1:9)], col = "red")
> abline(h = 0)
> abline(v = 0)