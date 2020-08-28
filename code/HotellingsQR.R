

## get the data
library(Flury)
data(flea.beetles)

## create the design matrix
y <- matrix(0,39,2)
y[flea.beetles$Species != "oleracea",2] <- 1
y[flea.beetles$Species == "oleracea",1] <- 1

## create the augmented data matrix
Xaug <- cbind(y, flea.beetles[,2:5])

## carry out the qr decomposition
dot <- qr(X)
## get U
blot <- qr.R(dot)[3:6,3:6]
## solve u with respect to the difference in group means
mudiff <- qr.R(dot)[1,3:6]/qr.R(dot)[1,1] - qr.R(dot)[2,3:6]/qr.R(dot)[2,2]
z <- qr.solve(blot, mudiff)
## extract the corrected Hotellings
n1 <- qr.R(dot)[1,1]^2
n2 <- qr.R(dot)[2,2]^2
Hotellings <- crossprod(z) * (n1 * n2 * (n1+n2-2))/(n1+n2)

F <- (Hotellings / (n1+n2-1))*((n1+n2-4)/4)

## which is irritatingly close, but not the same!!!!!