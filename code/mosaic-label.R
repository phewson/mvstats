################################################################
#
# begin mosaic plot functions:
#
#   you don't need to understand these two functions:
#     "recursive.divider" and "mosaic.plot"
#   just define them by evaluating them in the R interpreter;
#   then use them according to the examples of the mktg and titanic data that follow;
#   make sure you have the vector "pastell.colors" defined from above
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





