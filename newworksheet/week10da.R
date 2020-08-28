###################################################
### chunk number 1: wines
###################################################
options(width = 40)
library(MASS)
library(Flury)
data(wines)
table(wines$Country)
wines.lda <- lda(Country ~ ., data = wines, 
    prior = c(1, 1, 1)/3)
wines.preds <- predict(wines.lda, 
    wines)



###################################################
### chunk number 2: dafigs
###################################################
eqscplot(wines.preds$x, type = "n", 
    xlab = "First discriminant function", 
    ylab = "Second discriminant function", 
    main = "Wines, all variables")
text(wines.preds$x, labels = substring(wines$Country, 
    1, 2), col = 3 + unclass(wines$Country), 
    cex = 0.8)

#centroids <- wines.lda$means %*%   wines.lda$scaling
#points(centroids, pch = c(1,2,3), col = c(1,2,3))



###################################################
### chunk number 3: confusion
###################################################
confusion <- xtabs(~wines$Country + 
    wines.preds$class)
confusion



###################################################
### chunk number 4: aper
###################################################
correct <- sum(diag(confusion))
total <- sum(confusion)
wrong <- total - correct
aper <- wrong/total
aper



###################################################
### chunk number 5: dacoefs
###################################################
wines.lda$scaling



###################################################
### chunk number 6: updateda
###################################################
wines.lda1 <- update(wines.lda, 
    . ~ . - Y1 - Y11 - Y12)
wines.preds1 <- predict(wines.lda1, 
    wines)



###################################################
### chunk number 7: plotdiscfunc
###################################################
eqscplot(wines.preds1$x, type = "n", 
     xlab = "First discriminant function", 
     ylab = "Second discriminant function", 
     main = "Wines, minus y1,y11,y12")
 text(wines.preds$x, labels = substring(wines$Country, 
     1, 2), col = 3 + unclass(wines$Country), 
     cex = 0.8)

centroids <- wines.lda1$means %*%   wines.lda1$scaling
points(centroids, pch = c(1,2,3), col = c(1,2,3))




###################################################
### chunk number 8: confuse2
###################################################
confusion <- xtabs(~wines$Country + 
    wines.preds1$class)
confusion
correct <- sum(diag(confusion))
total <- sum(confusion)
wrong <- total - correct
aper <- wrong/total
aper



###################################################
### chunk number 9: updateagain
###################################################
wines.lda2 <- update(wines.lda, 
    . ~ . - Y1 - Y3 - Y4 - Y5 - 
        Y6 - Y11 - Y12)
wines.preds2 <- predict(wines.lda2, 
    wines)
eqscplot(wines.preds2$x, type = "n", 
    xlab = "First discriminant function", 
    ylab = "Second discriminant function", 
    main = "Wines, eight variables")
text(wines.preds$x, labels = substring(wines$Country, 
    1, 2), col = 3 + unclass(wines$Country), 
    cex = 0.8)
confusion <- xtabs(~wines$Country + 
    wines.preds2$class)
correct <- sum(diag(confusion))
total <- sum(confusion)
wrong <- total - correct
aper <- wrong/total
aper



###################################################
### chunk number 10: irisda
###################################################
data(iris)
dim(iris)
train <- sample(1:150, 75)
table(iris$Species[train])



###################################################
### chunk number 11: irisldafit
###################################################
iris.lda <- lda(Species ~ ., iris, 
    prior = c(1, 1, 1)/3, subset = train)



###################################################
### chunk number 12: irispreds
###################################################
iris.preds <- predict(iris.lda, 
     iris[-train, ])



###################################################
### chunk number 13: irisresults
###################################################
eqscplot(iris.preds$x, type = "n", 
    xlab = "First discriminant function", 
    ylab = "Second discriminant function", 
    main = "Iris, all variables")
text(iris.preds$x, labels = substring(iris$Species[-train], 
    1, 2), col = 3 + unclass(iris$Species[-train]), 
    cex = 0.8)



###################################################
### chunk number 14: irisconfusion
###################################################
confusion <- xtabs(~iris$Species[-train] + 
    iris.preds$class)



###################################################
### chunk number 15: irisupdate
###################################################
iris.lda1 <- update(iris.lda, . ~ 
    . - Petal.W.)




###################################################
### chunk number 16: wilks
###################################################
evals <- wines.lda$svd^2



###################################################
### chunk number 17: wilkscalc
###################################################
prod(1/(1 + evals))




###################################################
### chunk number 18: wilkslambdacalc
###################################################
prod(1/(1 + evals[-1]))



###################################################
### chunk number 19: bartlett
###################################################
bartlett <- function(z) {
    bartl <- (z$N - 1 - 0.5 * (dim(z$means)[2] + 
        dim(z$means)[1])) * log(1 + 
        z$svd^2)
    sigb <- dchisq(sqrt(bartl), 
        df = (dim(z$means)[2] + 
            dim(z$means)[1]) - 
            length(bartl))
    cat("Lambda square\t")
    cat(bartl)
    cat("\n")
    cat("Significance\t")
    cat(sigb)
    cat("\n")
}



