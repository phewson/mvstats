
mvm <- read.table("u:/teaching/mvmclass.csv", sep = ",", header = TRUE)
library(TeachingDemos)
Loading required package: tcltk
Loading Tcl/Tk interface ... done
Loading required package: lattice
> dim(mvm)
[1] 17  8
> faces(mvm[,-c(1,7,8)]
+ )
> ?faces
faces(mvm[,-c(1,7,8)], scale = TRUE)
faces(mvm[,-c(1,7,8)], scale = TRUE, labels = as.character(mvm[,1]))
> text <- c("Height = face height", "Hair color = face width", "Alcohol = face shape", "Football fan = mouth height", "MVM fan = mouth width")
> faces(mvm[,-c(1,7,8)], scale = TRUE, labels = as.character(mvm[,1]), fill = TRUE)
> 





ricky <- function(data, ...){
   arthur <- cor(data)
a1 <- eigen(arthur)$values
   a2 <- eigen(arthur)$vectors
   grant <- c(as.vector(a1[1]/sum(as.vector(a1))))
   return(grant)
  } 
> den <- boot(hep, 
