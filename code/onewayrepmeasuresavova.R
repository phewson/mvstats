dat<-matrix( c(9,7,8,8,12,11,8,13,     6,5,6,3,6,7,10,9,
               10,13,8,13,12,14,14,16, 9,11,13,14,16,12,15,14),
              ncol=4, dimname=list(s=1:8, c=1:4))
 mlmfit<-lm(dat~1)
 anova(mlmfit, X=~1)