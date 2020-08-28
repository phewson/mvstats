library(rgl) 
library(MASS) 


R <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), 3, 3) 
data <- mvrnorm(n=200, mu=c(0,0,0), Sigma=R) 
scatter3d(data[,1], data[,2], data[,3], ellipsoid=TRUE) 


data1 <- mvrnorm(n=100, mu=c(0,0,0), Sigma=R) 
data2 <- mvrnorm(n=100, mu=c(1,1,1), Sigma=R) 
data <- rbind(data1, data2) 
groups <- as.factor(c(rep("a", 100), rep("b", 100))) 
scatter3d(data[,1], data[,2], data[,3], ellipsoid=TRUE, 
    surface=FALSE, groups=groups) 


 scatter3d(flea.beetles$TG, flea.beetles$Elytra, flea.beetles$Second.Antenna, ellipsoid=TRUE, 
     surface=FALSE, groups=flea.beetles$Species, sphere.size = as.numeric(flea.beetles$Species)) 
rgl.snapshot("f:/rgl.png")




##Regards, 
## John 


##---------- snip --------------- 


ellipsoid <- function(center=c(0, 0, 0), radius=1, shape=diag(3), 
segments=51) { 
  angles <- (0:segments)*2*pi/segments 
  ecoord2 <- function(p) { 
    c(cos(p[1])*sin(p[2]), sin(p[1])*sin(p[2]), cos(p[2])) } 
  unit.sphere <- t(apply(expand.grid(angles, angles), 1, ecoord2)) 
  t(center + radius * t(unit.sphere %*% chol(shape))) 
} 



scatter3d <- function(x, y, z, xlab=deparse(substitute(x)), 
ylab=deparse(substitute(y)), 
                      zlab=deparse(substitute(z)), revolutions=0, 
bg.col=c("white", "black"), 
                      axis.col=if (bg.col == "white") "black" else "white", 
                      surface.col=c("blue", "green", "orange", "magenta", 
"cyan", "red", "yellow", "gray"), 
                      neg.res.col="red", pos.res.col="green", 
point.col="yellow", 
                      text.col=axis.col, grid.col=if (bg.col == "white") 
"black" else "gray", 
                      fogtype=c("exp2", "linear", "exp", "none"), 
                      residuals=(length(fit) == 1), surface=TRUE, grid=TRUE, 
grid.lines=26, 
                      df.smooth=NULL, df.additive=NULL, 
                      sphere.size=1, threshold=0.01, speed=1, fov=60, 
                      fit="linear", groups=NULL, parallel=TRUE, 
ellipsoid=FALSE, level=0.5, model.summary=FALSE){ 
    require(rgl) 
    require(mgcv) 
    summaries <- list() 
    if ((!is.null(groups)) && (nlevels(groups) > length(surface.col))) 
        stop(sprintf(gettextRcmdr("Number of groups (%d) exceeds number of 
colors (%d)."), 
            nlevels(groups), length(surface.col))) 
    if ((!is.null(groups)) && (!is.factor(groups))) 
stop(gettextRcmdr("groups variable must be a factor.")) 
    bg.col <- match.arg(bg.col) 
    fogtype <- match.arg(fogtype) 
    if ((length(fit) > 1) && residuals && surface) 
        stop(gettextRcmdr("cannot plot both multiple surfaces and 
residuals")) 
    xlab # cause these arguments to be evaluated 
    ylab 
    zlab 
    rgl.clear() 
    rgl.viewpoint(fov=fov) 
    rgl.bg(col=bg.col, fogtype=fogtype) 
    valid <- if (is.null(groups)) complete.cases(x, y, z) 
        else complete.cases(x, y, z, groups) 
    x <- x[valid] 
    y <- y[valid] 
    z <- z[valid] 
    if (!is.null(groups)) groups <- groups[valid] 
    x <- (x - min(x))/(max(x) - min(x)) 
    y <- (y - min(y))/(max(y) - min(y)) 
    z <- (z - min(z))/(max(z) - min(z)) 
    size <- sphere.size*((100/length(x))^(1/3))*0.015 
    if (is.null(groups)){ 
        if (size > threshold) rgl.spheres(x, y, z, color=point.col, 
radius=size) 
            else rgl.points(x, y, z, color=point.col) 
            } 
    else { 
        if (size > threshold) rgl.spheres(x, y, z, 
color=surface.col[as.numeric(groups)], radius=size) 
            else rgl.points(x, y, z, color=surface.col[as.numeric(groups)]) 
            } 
    rgl.lines(c(0,1), c(0,0), c(0,0), color=axis.col) 
    rgl.lines(c(0,0), c(0,1), c(0,0), color=axis.col) 
    rgl.lines(c(0,0), c(0,0), c(0,1), color=axis.col) 
    rgl.texts(1, 0, 0, xlab, adj=1, color=text.col) 
    rgl.texts(0, 1, 0, ylab, adj=1, color=text.col) 
    rgl.texts(0, 0, 1, zlab, adj=1, color=text.col) 
    if (ellipsoid) { 
        dfn <- 3 
        if (is.null(groups)){ 
            dfd <- length(x) - 1 
            radius <- sqrt(dfn * qf(level, dfn, dfd)) 
            ellips <- ellipsoid(center=c(mean(x), mean(y), mean(z)), 
shape=cov(cbind(x,y,z)), radius=radius) 
            quads3d(ellips[,1], ellips[,2], ellips[,3], front="lines", 
back="lines", alpha=.5, 
                lit=FALSE, col=surface.col[1]) 
            } 
        else{ 
            levs <- levels(groups) 
            for (j in 1:length(levs)){ 
                group <- levs[j] 
                select.obs <- groups == group 
                xx <- x[select.obs] 
                yy <- y[select.obs] 
                zz <- z[select.obs] 
                dfd <- length(xx) - 1 
                radius <- sqrt(dfn * qf(level, dfn, dfd)) 
                ellips <- ellipsoid(center=c(mean(xx), mean(yy), mean(zz)), 
shape=cov(cbind(xx,yy,zz)), radius=radius) 
                quads3d(ellips[,1], ellips[,2], ellips[,3], front="lines", 
back="lines", alpha=.5, 
                    lit=FALSE, col=surface.col[j]) 
                } 
            } 
        } 
    if (surface){ 
        vals <- seq(0, 1, length=grid.lines) 
        dat <- expand.grid(x=vals, z=vals) 
        for (i in 1:length(fit)){ 
            f <- match.arg(fit[i], c("linear", "quadratic", "smooth", 
"additive")) 
            if (is.null(groups)){ 
                mod <- switch(f, 
                    linear = lm(y ~ x + z), 
                    quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2)), 
                    smooth = if (is.null(df.smooth)) gam(y ~ s(x, z)) 
                        else gam(y ~ s(x, z, fx=TRUE, k=df.smooth)), 
                    additive = if (is.null(df.additive)) gam(y ~ s(x) + 
s(z)) 
                        else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) + 
                            s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1))) 
                    ) 
                if (model.summary) summaries[[f]] <- summary(mod) 
                yhat <- matrix(predict(mod, newdata=dat), grid.lines, 
grid.lines) 
                rgl.surface(vals, vals, yhat, color=surface.col[i], 
alpha=0.5, lit=FALSE) 
                if(grid) rgl.surface(vals, vals, yhat, color=grid.col, 
alpha=0.5, lit=FALSE, front="lines", back="lines") 
                if (residuals){ 
                    n <- length(y) 
                    fitted <- fitted(mod) 
                    colors <- ifelse(residuals(mod) > 0, pos.res.col, 
neg.res.col) 
                    rgl.lines(as.vector(rbind(x,x)), 
as.vector(rbind(y,fitted)), as.vector(rbind(z,z)), 
                        color=as.vector(rbind(colors,colors))) 
                    } 
                } 
            else{ 
                if (parallel){ 
                    mod <- switch(f, 
                        linear = lm(y ~ x + z + groups), 
                        quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2) + 
groups), 
                        smooth = if (is.null(df.smooth)) gam(y ~ s(x, z) + 
groups) 
                            else gam(y ~ s(x, z, fx=TRUE, k=df.smooth) + 
groups), 
                        additive = if (is.null(df.additive)) gam(y ~ s(x) + 
s(z) + groups) 
                            else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) + 
                                s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)) + 
groups) 
                        ) 
                    if (model.summary) summaries[[f]] <- summary(mod) 
                    levs <- levels(groups) 
                    for (j in 1:length(levs)){ 
                        group <- levs[j] 
                        select.obs <- groups == group 
                        yhat <- matrix(predict(mod, newdata=cbind(dat, 
groups=group)), grid.lines, grid.lines) 
                        rgl.surface(vals, vals, yhat, color=surface.col[j], 
alpha=0.5, lit=FALSE) 
                        if (grid) rgl.surface(vals, vals, yhat, 
color=grid.col, alpha=0.5, lit=FALSE, front="lines", back="lines") 
                        rgl.texts(0, predict(mod, newdata=data.frame(x=0, 
z=0, groups=group)), 0, 
                            paste(group, " "), adj=1, color=surface.col[j]) 
                        if (residuals){ 
                            yy <- y[select.obs] 
                            xx <- x[select.obs] 
                            zz <- z[select.obs] 
                            fitted <- fitted(mod)[select.obs] 
                            rgl.lines(as.vector(rbind(xx,xx)), 
as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)), 
                                col=surface.col[j]) 
                            } 
                        } 
                    } 
                else { 
                    levs <- levels(groups) 
                    for (j in 1:length(levs)){ 
                        group <- levs[j] 
                        select.obs <- groups == group 
                        mod <- switch(f, 
                            linear = lm(y ~ x + z, subset=select.obs), 
                            quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2), 
subset=select.obs), 
                            smooth = if (is.null(df.smooth)) gam(y ~ s(x, 
z), subset=select.obs) 
                                else gam(y ~ s(x, z, fx=TRUE, k=df.smooth), 
subset=select.obs), 
                            additive = if (is.null(df.additive)) gam(y ~ 
s(x) + s(z), subset=select.obs) 
                                else gam(y ~ s(x, fx=TRUE, 
k=df.additive[1]+1) + 
                                    s(z, fx=TRUE, 
k=(rev(df.additive+1)[1]+1)), subset=select.obs) 
                            ) 
                        if (model.summary) summaries[[paste(f, ".", group, 
sep="")]] <- summary(mod) 
                        yhat <- matrix(predict(mod, newdata=dat), 
grid.lines, grid.lines) 
                        rgl.surface(vals, vals, yhat, color=surface.col[j], 
alpha=0.5, lit=FALSE) 
                        rgl.surface(vals, vals, yhat, color=grid.col, 
alpha=0.5, lit=FALSE, front="lines", back="lines") 
                        rgl.texts(0, predict(mod, newdata=data.frame(x=0, 
z=0, groups=group)), 0, 
                            paste(group, " "), adj=1, color=surface.col[j]) 
                        if (residuals){ 
                            yy <- y[select.obs] 
                            xx <- x[select.obs] 
                            zz <- z[select.obs] 
                            fitted <- fitted(mod) 
                            rgl.lines(as.vector(rbind(xx,xx)), 
as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)), 
                                col=surface.col[j]) 
                            } 
                        } 
                    } 
                } 
            } 
        } 
    if (revolutions > 0) { 
        for (i in 1:revolutions){ 
            for (angle in seq(1, 360, length=360/speed)) 
rgl.viewpoint(-angle, fov=fov) 
            } 
        } 
    if (model.summary) return(summaries) else return(invisible(NULL)) 
    } 


