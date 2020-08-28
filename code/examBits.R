
(swiss.FA <- factanal(swiss, factors = 2, rotation = "none" ))


[1] 0.698994
> loadings[,i] <- sqrt(eigen(cor(swiss))$values[i]) * eigen(cor(swiss))$vectors[,i]

> write.table(loadings, "c:/work/load.csv", sep = ",", col.names = TRUE, row.names = TRUE) 



oil <- read.csv("c:/work/lecturing/mvm/oil.csv")

oil.manova <- manova(as.matrix(oil[,c(1:5)]) ~ oil$Zone)

summary(oil.manova, test = "Wilks")

print(xtable(summary(oil.manova, test = "Wilks"), type = "html")

print(xtable(summary.aov(oil.manova)[[1]]), type = "html")
print(xtable(summary.aov(oil.manova)[[2]]), type = "html")
print(xtable(summary.aov(oil.manova)[[3]]), type = "html")
print(xtable(summary.aov(oil.manova)[[4]]), type = "html")
print(xtable(summary.aov(oil.manova)[[5]]), type = "html")

not <- lda(Zone ~ ., data = oil)

plot(predict(not)$x[,1], predict(not)$x[,2], xlab = "Function 1", ylab = "Function 2", main = "Plot of first two \n discriminant functions", pch = as.character(oil$Zone))

xtabs(~oil$Zone + predict(not)$class)



> wot <- c(-0.759,-0.449,-0.035,-0.426,0.170,0.107)
> wot2 <- wot^2
> sum(wot2)
[1] 1.000732
> sum(wot2[c(1:2)])
[1] 0.777682
> wot <- c(0.813,0.195,-0.490,-0.065,-0.032,0.237)
> wot2 <- wot^2
> sum(wot2)
[1] 1.000512
> sum(wot2[c(1:2)])
[1] 0.698994
