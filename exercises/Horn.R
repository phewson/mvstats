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

