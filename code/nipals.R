library(ade4)
nipals<-function(df, nf=2, rec=F){
# df est un data frame contenant eventuellement des valeurs manquantes (NA)
# nf nombre de facteurs a conserver
# rec, si rec=T, la reconstitution des donn'es sur les nf premiers axes est realisee
# **********************************************************************************
# df is a data frame which can contain missing values (NA)
# nf number of axes to keep
# rec, if rec=T, data recontsitution is performed with the nf first axes

    df <- data.frame(df)
    seuil<-1e-9 # seuil pour la convergence
    nc <- ncol(df)
    nr <- nrow(df)
    if (rec)
        x<-list(li=matrix(0,nr,nf),c1=matrix(0,nc,nf),co=matrix(0,nc,nf),
        eig=rep(0,nf),nb=rep(0,nf),rec=matrix(0,nr,nc))
    else
        x<-list(li=matrix(0,nr,nf),c1=matrix(0,nc,nf),co=matrix(0,nc,nf),
        eig=rep(0,nf),nb=rep(0,nf))
    row.names(x$c1)<-names(df)
    row.names(x$co)<-names(df)
    row.names(x$li)<-row.names(df)

    X<-scale(df, center=T, scale=T)
    for (h in 1:nf) {
        th<-X[,1]
        ph1<-rep(1/sqrt(nc),nc)
        ph2<-rep(1/sqrt(nc),nc)
        diff<-rep(1,nc)
        nb<-0
        while (sum(diff^2, na.rm=T)>seuil) {
            for (i in 1:nc) {
                the<-th[X[,i]!="NA"]
                ph2[i]<-sum(X[,i]*th, na.rm=T)/sum(the*the,na.rm=T)
            }
            ph2<-ph2/sqrt(sum(ph2*ph2,na.rm=T))
            for (i in 1:nr) {
                ph2e<-ph2[X[i,]!="NA"]
                th[i]<-sum(X[i,]*ph2, na.rm=T)/sum(ph2e*ph2e,na.rm=T)
            }
            diff<-ph2-ph1
            ph1<-ph2
        nb<-nb+1
        }
        X<-X-th%*%t(ph1)
        x$nb[h]<-nb # nombre d'iterations (number of iterations)
        x$li[,h]<-th # coordonnees des lignes (row coordinates)
        x$c1[,h]<-ph1 # coordonnees des colonnes de variance unit' (columns coordinates of unit variance)
        x$eig[h]<-sum(th*th,na.rm=T)/(nr-1) # valeurs propres (pseudo-eigenvalues)
        x$co[,h]<-x$c1[,h]*sqrt(x$eig[h]) # coord. col. de variance lambda (column coordinates of variance lambda)

    }
    if (rec) {
        for (h in 1:nf) {
            x$rec<-x$rec+x$li[,h]%*%t(x$c1[,h]) # tableau reconstitue (reconstitued data)
        }
    }
    if (rec){
    x$rec=as.data.frame(x$rec)
    names(x$rec)<-names (df)
    row.names(x$rec)<-row.names(df)
    }

    x$tab<-X
    x$call<-match.call()
    x$nf<-nf
    class(x)<-"nipals"
    return(x)
}

print.nipals<-function (x, ...)
{
    cat("NIPALS ANALYSIS\n")
    cat("class: ")
    cat(class(x))
    cat("\n$call: ")
    print(x$call)
    cat("\n$nf:", x$nf, "axis-components saved")
    cat("\neigen values: ")
    l0 <- length(x$eig)
    cat(signif(x$eig, 4)[1:(min(5, l0))])
    if (l0 > 5)
        cat(" ...\n")
    else cat("\n")
    sumry <- array("", c(2, 4), list(1:2, c("vector", "length",
        "mode", "content")))
    sumry[1, ] <- c("$nb", length(x$nb), mode(x$nb), "number of iterations")
    sumry[2, ] <- c("$eig", length(x$eig), mode(x$eig), "eigen values")
    class(sumry) <- "table"
    print(sumry)
    cat("\n")
    sumry <- array("", c(4, 4), list(1:4, c("data.frame", "nrow",
        "ncol", "content")))
    sumry[1, ] <- c("$tab", nrow(x$tab), ncol(x$tab), "modified array")
    sumry[2, ] <- c("$li", nrow(x$li), ncol(x$li), "row coordinates")
    sumry[3, ] <- c("$co", nrow(x$co), ncol(x$co), "column coordinates")
    sumry[4, ] <- c("$c1", nrow(x$c1), ncol(x$c1), "column normed scores")
    class(sumry) <- "table"
    print(sumry)
    cat("other elements: ")
    if (length(names(x))==8)
        cat("NULL\n")
    else cat("$rec", "reconstituted data", "\n")
}

scatter.nipals<-function (x, xax = 1, yax = 2, clab.row = 0.5, clab.col = 1,
    permute = FALSE, posieig = "top", sub = NULL, ...)
{
    if (!inherits(x, "nipals"))
        stop("Object of class 'nipals' expected")
    opar <- par(mar = par("mar"))
    on.exit(par(opar))
    coolig <- x$li[, c(xax, yax)]
    coocol <- x$c1[, c(xax, yax)]
    if (permute) {
        coolig <- x$co[, c(xax, yax)]
        coocol <- x$l1[, c(xax, yax)]
    }
    s.label(coolig, clab = clab.row)
    born <- par("usr")
    k1 <- min(coocol[, 1])/born[1]
    k2 <- max(coocol[, 1])/born[2]
    k3 <- min(coocol[, 2])/born[3]
    k4 <- max(coocol[, 2])/born[4]
    k <- c(k1, k2, k3, k4)
    coocol <- 0.9 * coocol/max(k)
    s.arrow(coocol, clab = clab.col, add.p = TRUE, sub = sub,
        possub = "bottomright")
    add.scatter.eig(x$eig, x$nf, xax, yax, posi = posieig, ratio = 1/4)
}

