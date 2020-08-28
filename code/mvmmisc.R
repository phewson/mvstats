## faces from Peter Wolf's website


faces <- function(xy=rbind(1:3,5:3,3:5,5:7),which.row,fill=FALSE,nrow,ncol,
                scale=TRUE,byrow=FALSE,main,labels){

spline<-function(a,y,m=200,plot=FALSE){
    n<-length(a)
  h<-diff(a)
  dy<-diff(y)
  sigma<-dy/h
  lambda<-h[-1]/(hh<-h[-1]+h[-length(h)])
  mu<-1-lambda
  d<-6*diff(sigma)/hh
  tri.mat<-2*diag(n-2)
  tri.mat[2+  (0:(n-4))*(n-1)] <-mu[-1]
  tri.mat[    (1:(n-3))*(n-1)] <-lambda[-(n-2)]
  M<-c(0,solve(tri.mat)%*%d,0)
  x<-seq(from=a[1],to=a[n],length=m)
  anz.kl <- hist(x,breaks=a,plot=FALSE)$counts
  adj<-function(i) i-1
  i<-rep(1:(n-1),anz.kl)+1
  S.x<-  M[i-1]*(a[i]-x          )^3 / (6*h[adj(i)])  +
         M[i]  *(x        -a[i-1])^3 / (6*h[adj(i)])  +
         (y[i-1] - M[i-1]*h[adj(i)]^2 /6) * (a[i]-x)/ h[adj(i)] +
        (y[i]   - M[i]  *h[adj(i)]^2 /6) * (x-a[i-1]) / h[adj(i)]
  if(plot){ plot(x,S.x,type="l"); points(a,y)    }
  return(cbind(x,S.x))
  }

n.char<-15
xy<-rbind(xy)
if(byrow) xy<-t(xy)
if(!missing(which.row)&& all(  !is.na(match(which.row,1:dim(xy)[2]))  ))
       xy<-xy[,which.row,drop=FALSE]
mm<-dim(xy)[2];  n<-dim(xy)[1]
xnames<-dimnames(xy)[[1]]
if(is.null(xnames)) xnames<-as.character(1:n)
if(!missing(labels)) xnames<-labels
if(scale){
   xy<-apply(xy,2,function(x){
           x<-x-min(x); x<-if(max(x)>0) 2*x/max(x)-1 else x })
} else xy[]<-pmin(pmax(-1,xy),1)
xy<-rbind(xy);n.c<-dim(xy)[2]
xy<-xy[,(h<-rep(1:mm,ceiling(n.char/mm))),drop=FALSE]
if(fill) xy[,-(1:n.c)]<-0
  
face.orig<-list( 
      eye  =rbind(c(12,0),c(19,8),c(30,8),c(37,0),c(30,-8),c(19,-8),c(12,0))
     ,iris =rbind(c(20,0),c(24,4),c(29,0),c(24,-5),c(20,0))           
     ,lipso=rbind(c(0,-47),c( 7,-49),lipsiend=c( 16,-53),c( 7,-60),c(0,-62))
     ,lipsi=rbind(c(7,-54),c(0,-54))                  # add lipsiend
     ,nose =rbind(c(0,-6),c(3,-16),c(6,-30),c(0,-31)) 
     ,shape =rbind(c(0,44),c(29,40),c(51,22),hairend=c(54,11),earsta=c(52,-4),
                  earend=c(46,-36),c(38,-61),c(25,-83),c(0,-89))
     ,ear  =rbind(c(60,-11),c(57,-30))                # add earsta,earend
     ,hair =rbind(hair1=c(72,12),hair2=c(64,50),c(36,74),c(0,79)) # add hairend
)
lipso.refl.ind<-4:1
lipsi.refl.ind<-1
nose.refl.ind<-3:1
hair.refl.ind<-3:1
shape.refl.ind<-8:1
shape.xnotnull<-2:8
nose.xnotnull<-2:3
  
nr<-n^0.5; nc<-n^0.5
if(!missing(nrow)) nr<-nrow
if(!missing(ncol)) nc<-ncol
opar<-par(mfrow=c(ceiling(c(nr,nc))),oma=rep(6,4), mar=rep(.7,4))
on.exit(par(opar))
         
  
for(ind in 1:n){
  
factors<-xy[ind,] 
face <- face.orig
  
m<-mean(face$lipso[,2])
face$lipso[,2]<-m+(face$lipso[,2]-m)*(1+0.7*factors[4])
face$lipsi[,2]<-m+(face$lipsi[,2]-m)*(1+0.7*factors[4])
face$lipso[,1]<-face$lipso[,1]*(1+0.7*factors[5])
face$lipsi[,1]<-face$lipsi[,1]*(1+0.7*factors[5])
face$lipso["lipsiend",2]<-face$lipso["lipsiend",2]+20*factors[6]
  
m<-mean(face$eye[,2])
face$eye[,2] <-m+(face$eye[,2] -m)*(1+0.7*factors[7])
face$iris[,2]<-m+(face$iris[,2]-m)*(1+0.7*factors[7])
m<-mean(face$eye[,1])
face$eye[,1] <-m+(face$eye[,1] -m)*(1+0.7*factors[8])
face$iris[,1]<-m+(face$iris[,1]-m)*(1+0.7*factors[8])
  
m<-min(face$hair[,2])
face$hair[,2]<-m+(face$hair[,2]-m)*(1+0.2*factors[9])
m<-0
face$hair[,1]<-m+(face$hair[,1]-m)*(1+0.2*factors[10])
m<-0
face$hair[c("hair1","hair2"),2]<-face$hair[c("hair1","hair2"),2]+50*factors[11]
  
m<-mean(face$nose[,2])
face$nose[,2]<-m+(face$nose[,2]-m)*(1+0.7*factors[12])
face$nose[nose.xnotnull,1]<-face$nose[nose.xnotnull,1]*(1+factors[13])
     
  
m<-mean(face$shape[c("earsta","earend"),1])
face$ear[,1]<-m+(face$ear[,1]-m)* (1+0.7*factors[14])
m<-min(face$ear[,2])
face$ear[,2]<-m+(face$ear[,2]-m)* (1+0.7*factors[15])
  
face<-lapply(face,function(x){ x[,2]<-x[,2]*(1+0.2*factors[1]);x})
face<-lapply(face,function(x){ x[,1]<-x[,1]*(1+0.2*factors[2]);x})
face<-lapply(face,function(x){ x[,1]<-ifelse(x[,1]>0,
                                        ifelse(x[,2] > -30, x[,1], 
                  pmax(0,x[,1]+(x[,2]+50)*0.2*sin(1.5*(-factors[3])))),0);x})
  
invert<-function(x) cbind(-x[,1],x[,2])
face.obj<-list(
     eyer=face$eye
    ,eyel=invert(face$eye)
    ,irisr=face$iris
    ,irisl=invert(face$iris)
    ,lipso=rbind(face$lipso,invert(face$lipso[lipso.refl.ind,]))
    ,lipsi=rbind(face$lipso["lipsiend",],face$lipsi,
                 invert(face$lipsi[lipsi.refl.ind,,drop=FALSE]),
                 invert(face$lipso["lipsiend",,drop=FALSE]))
    ,earr=rbind(face$shape["earsta",],face$ear,face$shape["earend",])
    ,earl=invert(rbind(face$shape["earsta",],face$ear,face$shape["earend",]))
    ,nose=rbind(face$nose,invert(face$nose[nose.refl.ind,]))
    ,hair=rbind(face$shape["hairend",],face$hair,invert(face$hair[hair.refl.ind,]),
                invert(face$shape["hairend",,drop=FALSE]))
    ,shape=rbind(face$shape,invert(face$shape[shape.refl.ind,]))
)

plot(1,type="n",xlim=c(-105,105)*1.1, axes=FALSE,
     ylab="",ylim=c(-105,105)*1.3)
title(xnames[ind])
for(ind in seq(face.obj)) {
       x <-face.obj[[ind]][,1]; y<-face.obj[[ind]][,2]
       xx<-spline(1:length(x),x,40,FALSE)[,2]
       yy<-spline(1:length(y),y,40,FALSE)[,2]
       lines(xx,yy)
  }
  }

if(!missing(main)){
  par(opar);par(mfrow=c(1,1))
  mtext(main, 3, 3, TRUE, 0.5)
  title(main)
  }
  }





################################################################
#
# begin mosaic plot functions:
#
#   you don't need to understand these two functions:
#     "recursive.divider" and "mosaic.plot"
#   just define them by evaluating them in the R interpreter;
#   then use them according to the examples of the mktg and titanic data that follow;
#   make sure you have the vector "pastell.colors" defined from above
## from Di Cook's website
#
recursive.divider <- function(arr, hor.vert, outer.rect, col.dim, col,
      lab.dist, hor.depth=0, vert.depth=0, nolabel=0)
{
  gap <- 0.01
  h.depth <- hor.depth;    v.depth <- vert.depth
  if(is.null(dim(arr))) {
    dims <- length(arr);  labels <- names(arr)
  } else {
    dims <- dim(arr);  labels <- dimnames(arr)[[1]]
  }
  ndims <- length(dims);  ngrps <- dims[1]
  if(is.null(ngrps)) { ngrps <- 1;  ndims <- 1 }
  mat <- matrix(arr, nrow=ngrps)  # no other way to handle variable numbers of dims...
  freq <- apply(mat, 1, sum);    freq.tot <- sum(freq)
  if(freq.tot==0) {
    points((outer.rect["left"] + outer.rect["right"])/2,
           (outer.rect["bottom"] + outer.rect["top"])/2,
           pch=1, cex=.5 )
    return()
  }
  division <- freq / freq.tot
  nrects.now <- length(division);    ngaps.now <- nrects.now + 1
  nrects <- 1;    ngaps <- 0
  if(hor.vert[1]=="h") {  h.depth <- h.depth + 1
    for(j in 1:ndims) if(hor.vert[j]=="h")  {
      mrects <- dims[j]
      ngaps  <- ngaps  + nrects * (mrects + 1)
      nrects <- nrects * mrects
    }
    division <- (outer.rect["right"] - outer.rect["left"] - ngaps*gap) *
      division + (ngaps-ngaps.now)/nrects.now*gap
    division <- outer.rect["left"] + cumsum(c(0,division) + gap)
  } else {  v.depth <- v.depth + 1
    for(j in 1:ndims) if(hor.vert[j]=="v")  {
      mrects <- dims[j]
      ngaps  <- ngaps  + nrects * (mrects + 1)
      nrects <- nrects * mrects
    }
    division <- (outer.rect["top"] - outer.rect["bottom"] - ngaps*gap) *
      division + (ngaps-ngaps.now)/nrects.now*gap
    division <- outer.rect["bottom"] + cumsum(c(0,division) + gap)
  }
  for(i in 1:ngrps) {
    lab.off <- 0.1
    if(hor.vert[1]=="h") {
      inner.rect <- c(left=division[i], outer.rect["bottom"],
                      right=division[i+1]-gap, outer.rect["top"])
    } else {
      inner.rect <- c(outer.rect["left"], bottom=division[i],
                      outer.rect["right"], top=division[i+1]-gap)
    }
    if(hor.vert[1]=="h" & inner.rect["bottom"]<=gap*v.depth) {
      x <- (inner.rect["left"] + inner.rect["right"]) / 2
      y <- -lab.dist * (sum(hor.vert=="h") - 1) - lab.off
      if (!nolabel) {
        text(x=x, y=y, label=labels[i], srt=-90, adj=0)
        lines(c(inner.rect["left"],x,inner.rect["right"]), y+lab.off*c(0.5,0.1,0.5))
      }
     }
    if(hor.vert[1]=="v" & inner.rect["left"]<=gap*h.depth) {
      x <- -lab.dist*(sum(hor.vert=="v") - 1) - lab.off
      y <- (inner.rect["bottom"] + inner.rect["top"]) / 2
      if (!nolabel) {
        text(x=x, y=y, label=labels[i], adj=1)
        lines(x+lab.off*c(0.5,0.1,0.5), c(inner.rect["bottom"],y,inner.rect["top"]))
      }
     }
    if(!is.null(col.dim)) if(col.dim==ndims)
      polygon(inner.rect[c(1,3,3,1)], inner.rect[c(2,2,4,4)], col=col[i], border=0.1)
    if(length(dim(arr)) <= 1)
      polygon(inner.rect[c(1,3,3,1)], inner.rect[c(2,2,4,4)])
    if(length(dim(arr)) > 1)
      recursive.divider(arr=array(mat[i,], dim=dim(arr)[-1],
                 dimnames=as.list(dimnames(arr)[-1])),
                 hor.vert=hor.vert[-1], outer.rect=inner.rect,
                 col.dim=col.dim, col=col, lab.dist=lab.dist,
                 hor.depth=h.depth, vert.depth=v.depth,nolabel=nolabel)
  } # end for(i in 1:ngrps)...
} # end recursive.divider <- function(...
mosaic.plot <- function(arr,
                        dims=if(!is.null(dim(arr))) 1:length(dim(arr)) else 1,
                        hor.vert=c(rep("h",length(dims)-1),"v"),
                        col.dim=length(dims), col=c(1,2,3,4,5,6), 
                        lab.dist=0.15, yn.newplot=T, nolabel=F, 
                        marv = rep(0.5,4))
{
  local.arr <- as.array(arr)
  if(is.null(dimnames(local.arr))) {
    dimn <- list()
    for(i in 1:length(dim(local.arr)))
      dimn[[i]] <- paste("D",i,"/Grp",1:dim(local.arr)[i], sep="")
    names(dimn) <- paste("Dim",1:length(dim(local.arr)),sep="")
    dimnames(local.arr) <- dimn
  }
  if(length(hor.vert) < length(dims)) {
    cat("!!!!!Error in call to mosaic.plot:\n  length(hor.vert) < length(dims)\n")
    return(NULL) }
  if(is.character(dims)) {
    if(is.null(dimnames(local.arr))) {
      cat("!!!!!!Error in call to mosaic.plot:\n  array has no dimnames \n");
      return(NULL) }
    local.dims <- match(dims, names(dimnames(local.arr)))
  } else {
    local.dims <- dims
  }
  if(min(local.dims) < 1 | max(local.dims) > length(dim(local.arr))) {
    cat("!!!!!Error in call to mosaic.plot:\n  dims out of bound\n  dims=",dims,"\n");
    return(NULL); }
  if(any(is.na(local.dims))) {
    cat("!!!!!Error in call to mosaic.plot:\n  dimensions entered =",dims,
        "\n  actual dimensions  =",names(dimnames(local.arr)),"\n");  return(NULL) }
  cat("Call to mosaic.plot:\n  dimensions entered =",dims,
      "\n  actual dimensions  =",names(dimnames(local.arr)),"\n")
  local.arr <- apply(local.arr, local.dims, sum)
  if(!is.null(col.dim)) col.dim <- length(dims) - col.dim + 1
  par(mar=marv)
  if (yn.newplot) {
    plot(c(-0.2*sum(hor.vert=="v"),1.0), c(-0.2*sum(hor.vert=="h"),1.0),
       xaxt="n", yaxt="n", xlab="", ylab="", type="n", bty="n")
  } else { lab.dist<-NULL }
  recursive.divider(local.arr, hor.vert, 
    outer.rect=c(left=0,bottom=0,right=1,top=1),
    col.dim=col.dim, col=col, lab.dist=lab.dist, nolabel=nolabel)
}

#
# end mosaic plot functions
#


andrews.curves <- function(xdf, cls, npts=101, title="Classes") {
    n <- nrow(xdf)
    clss <- as.factor(cls)
    xpts <- seq(0, 2*pi, length=npts)
    X <- xpts
    for (i in 1:n) {
        xi <- unname(unlist(xdf[i, ]))
        ys <- andrews.function(xi, npts)
        X <- cbind(X, ys)
    }
    ymin <- min(X[, 2:(n+1)])
    ymax <- max(X[, 2:(n+1)])
    plot(0, 0, type="n", xlim=c(0, 2*pi), ylim=c(ymin, ymax),
         main="Andrews' Curves", xlab="", ylab="")

    clrs <- as.integer(clss)
    for (i in 2:(n+1)) {
        lines(X[, 1], X[, i], col=clrs[i-1])
    }
    legend(4, ymax, levels(clss), col=c(1:nlevels(clss)), lty=1)
    # return(X)
}

andrews.function <- function (xs, no.pts=101) {
    n <- length(xs)
    xpts <- seq(0, 2*pi, length=no.pts)
    ypts <- c()
    for (p in xpts) {
        y <- xs[1]
        for (i in 2:n) {
             if (i %% 2 == 1) { y <- y + xs[i]*sin((i %/% 2)*p) }
             else             { y <- y + xs[i]*cos((i %/% 2)*p) }
        }
        ypts <- c(ypts, y)
    }
    return(ypts)
}




## a glyph plotter (very early days this one)

glyphs <- function(x1, x2, x3, x4, scale1 = 2, scale2 = 5){
g1 <- (x3 - min(x3)) / (max(x3) - min(x3))
g2 <- (x4 - min(x4)) / (max(x4) - min(x4))
angle <- pi * g2 * acos(-1/pi)
y1 <- x1 +  scale1 * g1 + cos(angle) * scale2
y2 <- x2 + scale1 * g1 + sin(angle)
par(bty = "n", xpd = NA)
plot(x1, x2)
arrows(x1, x2, y1, y2,  length = 0.1, col = round(x4))
}
