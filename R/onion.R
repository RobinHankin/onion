"p3d" <- 
function (x, y, z, xlim = NULL, ylim = NULL, zlim = NULL, d0=0.2, h=1, ...) 
{
    if (is.matrix(x)) {
        z <- x[, 3]
        y <- x[, 2]
        x <- x[, 1]
    }
    if (missing(zlim)) {
        z.grid <- matrix(range(z), 2, 2)
    }
    else {
        z.grid <- matrix(zlim, 2, 2)
    }
    if (missing(xlim)) {
        xlim <- range(x)
    }
    if (missing(ylim)) {
        ylim <- range(y)
    }
    res <- persp(xlim, ylim, z.grid, col = NA, border = NA, ...)
    trans3d <- function(x, y, z, pmat) {
        tr <- cbind(x, y, z, 1) %*% pmat
        list(x = tr[, 1]/tr[, 4], y = tr[, 2]/tr[, 4])
    }
    depth3d <- function(x, y, z, pmat) {
        tr <- cbind(x, y, z, 1) %*% pmat
        return(tr[, 3]/tr[, 4])
    }
    rationalize <- function(x){(x-min(x))/(max(x)-min(x))}

    out <- trans3d(x, y, z, pm = res)
    depth <- rationalize(depth3d(x,y,z,pm=res))
    out$x <- out$x[order(depth,decreasing=FALSE)]
    out$y <- out$y[order(depth,decreasing=FALSE)]
    jj <- exp(-sort(depth,decreasing=TRUE)/d0)
    if(is.null(h)){ 
      colours <- hsv(h=1,s=0, v=1-jj)
    } else {
      colours <- hsv(h=h,s=jj)
    }
    points(out,col=colours, ...)
    return(invisible(out))
}

"rotate" <- function(x,H){
t(as.matrix(H*as.quaternion(t(cbind(0,x)))/H))[,-1]
}
  
"associator" <- function(x1,x2,x3){
  return((x1*x2)*x3 - x1*(x2*x3))
}

"commutator" <- function(x1,x2){
  x1*x2 - x2*x1
}

"threeform" <- function(x1,x2,x3){
  Re(x1*(Conj(x2)*x3) - x3*(Conj(x2)*x1))/2
}

setGeneric("plot")
setMethod("plot","onion",function(x, ...){plot(Re(x),Mod(Im(x)), ...)})

setGeneric("rep")
setMethod("rep","onion",function(x,  ...){
  u <- seq(length.out=length(x))
  return(x[rep(u, ...)])
} )

`onion_g_even` <- function(x,y){(x*y + y*x)/2}
`onion_g_odd`  <- function(x,y){(x*y - y*x)/2}
`onion_e_even` <- function(x,y){(Conj(x)*y + Conj(y)*x)/2}
`onion_e_odd`  <- function(x,y){(Conj(x)*y - Conj(y)*x)/2}

`%<*>%` <- function(x,y){onion_g_even(x,y)}
`%>*<%` <- function(x,y){onion_g_odd (x,y)}
`%<.>%` <- function(x,y){onion_e_even(x,y)}
`%>.<%` <- function(x,y){onion_e_odd (x,y)}

`dotprod` <- function(x,y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(ncol(x)==1){
    x <- as.vector(x)
    return(apply(y,2,function(u){sum(u*x)}))
  } else if (ncol(y)==1){
    y <- as.vector(y)
    return(apply(x,2,function(u){sum(u*y)}))
  } else {
    return(apply(x*y,2,sum))
  }
}

`%.%` <- function(x,y){dotprod(x,y)}

setGeneric("zapsmall")
setMethod("zapsmall","onion",function(x,digits=getOption("digits")){as.onion(zapsmall(as.matrix(x),digits=digits)) })
