


library(yeastCC)
x <- exprs(yeastCC)
heatmap(na.omit(x), main = "yeastCC")

dot <- prcomp(na.omit(x))

