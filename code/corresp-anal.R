f.corresp.anal<-function(x){
  nrow<-dim(x)[1];   ncol<-dim(x)[2]
  dm<-c(dimnames(x)[[1]],dimnames(x)[[2]])
  rtot<-apply(x,1,sum);   ctot<-apply(x,2,sum);   tot<-sum(rtot)
  cexp<-NULL
  for (i in 1:nrow)
    for (j in 1:ncol)
      cexp<-c(cexp,rtot[i]*ctot[j]/tot)
  cexp<-matrix(cexp,ncol=ncol,byrow=T)
  cchi<-NULL
  for (i in 1:nrow)
    for (j in 1:ncol)
      cchi<-c(cchi,(x[i,j]-cexp[i,j])/sqrt(cexp[i,j]))
  cchi<-matrix(cchi,ncol=ncol,byrow=T)
  scchi<-svd(cchi)
  x<-c(scchi$u[,1],scchi$v[,1])
  y<-c(scchi$u[,2],scchi$v[,2])
  par(pty="s");  plot(x,y,type="n");   
  text(x,y,dm) #1:(ncol+nrow))
  cor.an<-list(d=scchi$d,u=scchi$u,v=scchi$v,x=x,y=y)
  return(cor.an)
}
