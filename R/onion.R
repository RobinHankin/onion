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
if(FALSE){
"R_OprodO" <- function(oct1,oct2){
  x <- as.matrix(oct1)
  y <- as.matrix(oct2)
  out <- x
  out[1,] = +x[1,]*y[1,] -x[2,]*y[2,] -x[3,]*y[3,] -x[4,]*y[4,] -x[5,]*y[5,] -x[6,]*y[6,] -x[7,]*y[7,] -x[8,]*y[8,]
  out[2,] = +x[2,]*y[1,] +x[1,]*y[2,] -x[4,]*y[3,] +x[3,]*y[4,] -x[6,]*y[5,] +x[5,]*y[6,] +x[8,]*y[7,] -x[7,]*y[8,]
  out[3,] = +x[3,]*y[1,] +x[4,]*y[2,] +x[1,]*y[3,] -x[2,]*y[4,] -x[7,]*y[5,] -x[8,]*y[6,] +x[5,]*y[7,] +x[6,]*y[8,]
  out[4,] = +x[4,]*y[1,] -x[3,]*y[2,] +x[2,]*y[3,] +x[1,]*y[4,] -x[8,]*y[5,] +x[7,]*y[6,] -x[6,]*y[7,] +x[5,]*y[8,]
  out[5,] = +x[5,]*y[1,] +x[6,]*y[2,] +x[7,]*y[3,] +x[8,]*y[4,] +x[1,]*y[5,] -x[2,]*y[6,] -x[3,]*y[7,] -x[4,]*y[8,]
  out[6,] = +x[6,]*y[1,] -x[5,]*y[2,] +x[8,]*y[3,] -x[7,]*y[4,] +x[2,]*y[5,] +x[1,]*y[6,] +x[4,]*y[7,] -x[3,]*y[8,]
  out[7,] = +x[7,]*y[1,] -x[8,]*y[2,] -x[5,]*y[3,] +x[6,]*y[4,] +x[3,]*y[5,] -x[4,]*y[6,] +x[1,]*y[7,] +x[2,]*y[8,]
  out[8,] = +x[8,]*y[1,] +x[7,]*y[2,] -x[6,]*y[3,] -x[5,]*y[4,] +x[4,]*y[5,] +x[3,]*y[6,] -x[2,]*y[7,] +x[1,]*y[8,]
  return(as.octonion(out))
}

"R_HprodH" <- function(quat1,quat2){
  x <- as.matrix(quat1)
  y <- as.matrix(quat2)
  out <- x
  out[1,] = +x[1,]*y[1,] -x[2,]*y[2,] -x[3,]*y[3,] -x[4,]*y[4,]
  out[2,] = +x[2,]*y[1,] +x[1,]*y[2,] -x[4,]*y[3,] +x[3,]*y[4,]
  out[3,] = +x[3,]*y[1,] +x[4,]*y[2,] +x[1,]*y[3,] -x[2,]*y[4,]
  out[4,] = +x[4,]*y[1,] -x[3,]*y[2,] +x[2,]*y[3,] +x[1,]*y[4,]
  return(as.quaternion(out))
}

"t.onion" <- function(x){
  x <- as.matrix(x)
  NextMethod("t")
}
"print.octonion" <- function(x, h=getOption("horiz"), ...){
  x <- as.matrix(x)
  rownames(x) <- c("Re","i","j","k","l","il","jl","kl")
  if(ncol(x)>0){
    if(is.null(colnames(x))){colnames(x) <- paste("[",1:ncol(x),"]",sep="")}
  }
  if(isTRUE(h)){
    return(invisible(print(t(x))))
  } else {
    return(invisible(print(x)))
  }
}

"print.quaternion" <- function(x, h=getOption("horiz"), ...){
  x <- as.matrix(x)
  rownames(x) <- c("Re","i","j","k")
  if(ncol(x)>0){
    if(is.null(colnames(x))){colnames(x) <- paste("[",1:ncol(x),"]",sep="")}
  }
  if(isTRUE(h)){
    return(invisible(print(t(x))))
  } else {
    return(invisible(print(x)))
  }
}
}

`plot.onion` <- function(x, ...){plot(Re(x),Mod(Im(x)), ...)}

setGeneric("rep")
setMethod("rep","onion",function(x,  ...){
  u <- seq(length.out=length(x))
  return(x[rep(u, ...)])
} )
    
`sign.onion` <- function(x){x/Mod(x)}

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
