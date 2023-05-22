`getd` <- function(o){o@d}
`getM` <- function(o){o@M}  # this is the last occurrence of '@' in this file

setValidity("onionmat",function(object){
  d <- getd(object)
  M <- getM(object)
  
  if(length(d) != length(M)){
    return("length(d) != length(M)")
  } else {
    return(TRUE)
  }
})

newonionmat <- function(d,M){
    stopifnot(length(d) == length(M))
    if(is.matrix(M)){
      M[] <- seq_along(M)
      if(is.null(rownames(M))){
        rowlabs <- seq_len(nrow(M))
      } else {
        rowlabs <- rownames(M)
      }

      if(is.null(colnames(M))){
        collabs <- seq_len(ncol(M))
      } else {
        collabs <- colnames(M)
      }
      names(d) <- apply(expand.grid(rowlabs,collabs),1,function(x){paste("[",x[1],",",x[2],"]",sep="")})
      return(new("onionmat",d=d,M=M))  # this is the only place new("onionmat") is called
    } else {  # M not a matrix, effectively 'drop':
      names(d) <- names(M)
      return(d)
    }
}

`onionmat` <- function(data=NA,nrow=1,ncol=1,byrow=FALSE,dimnames=NULL){
    M <- matrix(seq_len(nrow*ncol),nrow,ncol)
    dimnames(M) <- dimnames
    d <- 0*c(M)+data[1]
    if(byrow){
        d[c(matrix(seq_len(nrow*ncol),ncol,nrow,byrow=TRUE))] <- data
    } else {
        d[] <- data
    }
    newonionmat(d,M)
}

`as.onionmat` <- function(x){
  if(is(x,"onionmat")){
    return(x)
  } else if(is.onion(x)){
    return(newonionmat(d=x,M=matrix(seq_along(x))))
  } else {
    stop("not recognised")
  }
}
    
romat <- function(type="quaternion",nrow=5, ncol=6, ...){
  d <- switch(type, 
              octonion = roct(nrow*ncol,...),
              rquat(nrow*ncol,...)  # quaternion
              )
  out <- newonionmat(d=d,M=matrix(0,nrow,ncol,...))
  rownames(out) <- letters[seq_len(nrow)]
  colnames(out) <- datasets::state.abb[seq_len(ncol)]
  return(out)
}

rsomat  <- function(type="quaternion",nrow=5, ncol=6, ...){
  out <- romat(type=type,nrow=nrow,ncol=ncol, ...)
  jj <- switch(type,
              octonion = rsoct(nrow*ncol,...),
              rsquat(nrow*ncol,...)  # quaternion
              )
  out[] <- jj
  return(out)
}


setGeneric("nrow")
setGeneric("ncol")
setMethod("nrow","onionmat",function(x){nrow(getM(x))})
setMethod("ncol","onionmat",function(x){ncol(getM(x))})

setGeneric("diag")
setMethod("diag","onionmat",function(x){x[diag(getM(x))]})

setGeneric("diag<-")
setReplaceMethod("diag",signature(x="onionmat",value="ANY"),
                 function(x,value){
                   d <- getd(x)
                   M <- getM(x)
                   d[c(diag(M))] <- value
                   newonionmat(d,M)
                 } )

setMethod("diag",signature(x="onion"),
          function(x){
              out <- onionmat(x[1]*0,length(x),length(x))
              out[cbind(seq_along(x),seq_along(x))] <- x
              return(out)
          } )

## sum
`onionmat_allsum` <- function(x){sum(getd(x))}

## elementwise operations:
`onionmat_negative` <- function(e1){ newonionmat(-getd(e1),getM(e1)) }
`onionmat_inverse` <- function(e1){ newonionmat(1/getd(e1),getM(e1)) }
`onionmat_plus_onionmat` <- function(e1,e2){ newonionmat(getd(e1)+getd(e2),getM(e1)+getM(e2)) }
`onionmat_plus_single` <- function(e1,e2){ newonionmat(getd(e1)+e2,getM(e1)) }   # 'single' can be numeric or onion
`onionmat_power_single` <- function(e1,e2){ newonionmat(getd(e1)^e2,getM(e1)) }
`onionmat_prod_single` <- function(x,y){ newonionmat(getd(x)*y,getM(x)) }
`single_prod_onionmat` <- function(e1,e2){onionmat_prod_single(e2,e1)}  # needed because multiplication is noncommutative
`onionmat_prod_onionmat` <- function(e1,e2){ newonionmat(getd(e1)*getd(e2),getM(e1)*getM(e2)) }
`single_power_onionmat` <- function(...){stop("not defined")} 
`onionmat_power_onionmat` <- function(...){stop("not defined")}

`onionmat_plus_matrix` <- function(e1,e2){newonionmat(getd(e1)+c(e2),getM(e1)+e2) }
`onionmat_prod_matrix` <- function(e1,e2){newonionmat(getd(e1)*c(e2),getM(e1)*e2) }
`onionmat_power_matrix` <- function(e1,e2){newonionmat(getd(e1)^c(e2),getM(e1)^e2) }

`matrix_plus_onion` <- function(e1,e2){ # e1 [numeric] matrix, e2 onion
    jj <- e1
    jj[] <- seq_along(jj)
    newonionmat(c(e1)+e2,jj) # the meat
}

`matrix_prod_onion` <- function(e1,e2){ # e1 [numeric] matrix, e2 onion
    jj <- e1
    jj[] <- seq_along(jj)
    newonionmat(c(e1)*e2,jj) # the meat
}

`onion_power_matrix` <- function(e1,e2){ # e1 onion, e2 [numeric] matrix
    jj <- e2
    jj[] <- seq_along(jj)
    newonionmat(e1^c(e2),jj) # the meat
  }

`onionmat_equal_onionmat` <- function(e1,e2){
    jj <- getM(e1) == getM(e2) # traps nonconformant matrices
    out <- getd(e1) == getd(e2)
    attributes(out) <- attributes(jj)
    return(out)
}

"onionmat_arith_onionmat" <- function(e1,e2){ # e1, e2 onionmats  ELEMENTWISE
  switch(.Generic,
         "+" = onionmat_plus_onionmat(e1, e2),
         "-" = onionmat_plus_onionmat(e1, onionmat_negative(e2)),
         "*" = onionmat_prod_onionmat(e1, e2),
         "/" = onionmat_prod_onionmat(e1, onionmat_inverse(e2)),
         "^" = onionmat_power_onionmat(e1, e2),
         stop(gettextf("binary operator %s not defined for onionmats", dQuote(.Generic)))
         )
}

`onionmat_arith_matrix` <- function(e1,e2){ # e1: onionmat, e2: matrix -  ELEMENTWISE
  switch(.Generic,
         "+" = onionmat_plus_matrix(e1, e2),
         "-" = onionmat_plus_matrix(e1, -e2),
         "*" = onionmat_prod_matrix(e1, e2),
         "/" = onionmat_prod_matrix(e1, 1/e2),
         "^" = onionmat_power_matrix(e1, e2),
         stop(gettextf("binary operator %s not defined for onionmats", dQuote(.Generic)))
         )
}

`matrix_arith_onionmat` <- function(e1,e2){ # e1: matrix, e2: onionmat  -  ELEMENTWISE
  switch(.Generic,
         "+" = onionmat_plus_matrix(e2, e1),
         "-" = onionmat_plus_matrix(-e2, e1),
         "*" = onionmat_prod_matrix(e2, e1),
         "/" = onionmat_prod_matrix(1/e2, e1),
         "^" = onionmat_power_onionmat(e2, e1),
         stop(gettextf("binary operator %s not defined for onionmats", dQuote(.Generic)))
         )
}

"onionmat_arith_single" <- function(e1,e2){ # e1 onionmat, e2 numeric  POINTWISE
  switch(.Generic,
         "+" = onionmat_plus_single(e1,   e2),
         "-" = onionmat_plus_single(e1,  -e2),
         "*" = onionmat_prod_single(e1,   e2),
         "/" = onionmat_prod_single(e1, 1/e2),
         "^" = onionmat_power_single(e1,  e2),
         stop(gettextf("binary operator %s not defined for onionmats", dQuote(.Generic)))
         )
}

"single_arith_onionmat" <- function(e1,e2){ # e1 numeric, e2 onionmat  POINTWISE
  switch(.Generic,
         "+" = onionmat_plus_single(e2 , e1),  # addition is commutative
         "-" = onionmat_plus_single(-e2, e1),
         "*" = single_prod_onionmat(e1, e2),  
         "/" = single_prod_onionmat(e1, onionmat_inverse(e2)),
         "^" = single_power_onionmat(e1, e2),
         stop(gettextf("binary operator %s not defined for onionmats", dQuote(.Generic)))
         )
}

`matrix_arith_onion` <- function(e1,e2){ # e1 [numeric] matrix, e2 onion
    if(is.complex(e1)){stop("matrix must be numeric")}
    switch(.Generic,
           "+" = matrix_plus_onion(e1,  e2),
           "-" = matrix_plus_onion(e1, -e2),
           "*" = matrix_prod_onion(e1,  e2),
           "/" = matrix_prod_onion(e1,1/e2),
           "^" = onionmat_power_onionmat(e1,e2),
           stop(gettextf("binary operator %s not defined for onionmats", dQuote(.Generic)))
           )
}

`onion_arith_matrix` <- function(e1,e2){ # e1 onion, e2 [numeric] matrix
    if(is.complex(e2)){stop("matrix must be numeric")}
    switch(.Generic,
           "+" = matrix_plus_onion(e2,  e1),      # e1+e2
           "-" = matrix_plus_onion(-e2, e1),      # e1-e2
           "*" = matrix_prod_onion(e2,  e1),      # e1*e2
           "/" = matrix_prod_onion(1/e2,e1), # e1/e2
           "^" = onion_power_matrix(e1,e2),
           stop(gettextf("binary operator %s not defined for onionmats", dQuote(.Generic)))
           )
}

`onionmat_matrixprod_onionmat` <- function(x,y){
    jj <- getM(x) %*% getM(y)
    out <- newonionmat(rep(0*getd(x)[1],length(jj)),jj)
    for(i in seq_len(nrow(getM(x)))){
        for(j in seq_len(ncol(getM(y)))){
            out[i,j] <- sum(x[i,]*y[,j])
        }
    }
    rownames(out) <- rownames(jj)
    colnames(out) <- colnames(jj)
    return(out)
}

`onionmat_matrixprod_onion` <- function(x,y){
  onionmat_matrixprod_onionmat(x,as.onionmat(y))
}

`onionmat_matrixprod_numeric` <- function(x,y){
  onionmat_matrixprod_onionmat(x,as.onionmat(y+0*x[1,1]))
}

`numeric_matrixprod_onionmat` <- function(x,y){onionmat_matrixprod_onionmat(t(as.onionmat(x+0*y[1,1])),y)}
`onion_matrixprod_onionmat` <- function(x,y){onionmat_matrixprod_onionmat(t(as.onionmat(x)),y)}  # x: onion; y: onionmat

`om_cprod` <- function(x,y=x){  # t(Conj(x)) %*% y
    x <- as.onionmat(x)
    y <- as.onionmat(y)
    x <- Conj(x)
    jj <- crossprod(getM(x), getM(y))
    out <- newonionmat(rep(0*getd(x)[1],length(jj)),jj)
    for(i in seq_len(ncol(getM(x)))){
        for(j in seq_len(ncol(getM(y)))){
            out[i,j] <- sum(x[,i]*y[,j])
        }
    }
    rownames(out) <- rownames(jj)
    colnames(out) <- colnames(jj)
    return(out)
}

`om_tcprod` <- function(x,y=x){  # x %*% t(Conj(y))
    x <- as.onionmat(x)
    y <- as.onionmat(y)
    y <- Conj(y)
    jj <- tcrossprod(getM(x), getM(y))
    out <- newonionmat(rep(0*getd(x)[1],length(jj)),jj)
    for(i in seq_len(nrow(getM(x)))){
        for(j in seq_len(nrow(getM(y)))){
            out[i,j] <- sum(x[i,]*y[j,])
        }
    }
    rownames(out) <- rownames(jj)
    colnames(out) <- colnames(jj)
    return(out)
}

`om_ht` <- function(x){ t(Conj(x)) }

setGeneric("t")   # transpose

setMethod("t","onion",function(x){ 
  jj <- seq_along(x)
  names(jj) <- names(x)
  newonionmat(x,t(jj)) # the meat
})

setMethod("t","onionmat",function(x){ # NB1: this  ensures that emulator::ht() works; NB2: t(x) DOES NOT TAKE CONJUGATE
  jj <- t(getM(x))
  newonionmat(getd(x)[c(jj)],jj)
})

`herm_onion_mat` <- function(real_diagonal, onions){
  n <- length(real_diagonal)
  m <- round(n*(n-1)/2)
  stopifnot(length(onions) == m)
  mind <- diag(real_diagonal,nrow=length(real_diagonal))
  rownames(mind) <- names(real_diagonal)
  colnames(mind) <- names(real_diagonal)
  out <- newonionmat(d=rep(onions[1]*0,n*n),M=mind)
  out[upper.tri(mind)] <- onions
  out <- out + t(Conj(out))
  diag(out) <- real_diagonal
  return(out)
}

setMethod("show", "onionmat", function(object){onionmat_show(object)})

`onionmat_show` <- function(object,...){
  if(isTRUE(getOption("show_onionmats_in_place"))){
    out <- onion_to_string(getd(object))
    attributes(out) <- attributes(getM(object))
    print(noquote(out))
    return(invisible(out))
  } else {
    print(getd(object))
    print(getM(object))
    return(object)
  }
}

setGeneric("cprod",function(x,y){standardGeneric("cprod")})
setMethod("cprod",signature=c(x="onionmat",y="onionmat"),function(x,y){om_cprod(x,y)})
setMethod("cprod",signature=c(x="onionmat",y="missing"),function(x,y){om_cprod(x,x)}) # NB x
setMethod("cprod",signature=c(x="onionmat",y="ANY"),function(x,y){om_cprod(x,y)})
setMethod("cprod",signature=c(x="ANY",y="onionmat"),function(x,y){om_cprod(x,y)})
setMethod("cprod",signature=c(x="ANY",y="ANY"),function(x,y){emulator::cprod(x,y)})
setMethod("cprod",signature=c(x="ANY",y="missing"),function(x,y){emulator::cprod(x,x)}) # NB x
setMethod("cprod",signature=c(x="onion",y="onion"),function(x,y){om_cprod(x,y)})
setMethod("cprod",signature=c(x="onion",y="onion"),function(x,y){om_cprod(x,y)})
setMethod("cprod",signature=c(x="onionmat",y="onion"),function(x,y){om_cprod(x,y)})
setMethod("cprod",signature=c(x="onion",y="onionmat"),function(x,y){om_cprod(x,y)})
setMethod("cprod",signature=c(x="onion",y="missing"),function(x,y){om_cprod(x,x)})


setGeneric("tcprod",function(x,y){standardGeneric("tcprod")})
setMethod("tcprod",signature=c(x="onionmat",y="onionmat"),function(x,y){om_tcprod(x,y)})
setMethod("tcprod",signature=c(x="onionmat",y="missing"),function(x,y){om_tcprod(x,x)})  # NB x
setMethod("tcprod",signature=c(x="onionmat",y="ANY"),function(x,y){om_tcprod(x,y)})
setMethod("tcprod",signature=c(x="ANY",y="onionmat"),function(x,y){om_tcprod(x,y)})
setMethod("tcprod",signature=c(x="ANY",y="ANY"),function(x,y){emulator::tcprod(x,y)})
setMethod("tcprod",signature=c(x="ANY",y="missing"),function(x,y){emulator::tcprod(x,x)}) # NB x
setMethod("tcprod",signature=c(x="onion",y="onion"),function(x,y){om_tcprod(x,y)})
setMethod("tcprod",signature=c(x="onionmat",y="onion"),function(x,y){om_tcprod(x,y)})
setMethod("tcprod",signature=c(x="onion",y="onionmat"),function(x,y){om_tcprod(x,y)})
setMethod("tcprod",signature=c(x="onion",y="missing"),function(x,y){om_tcprod(x,x)})


setGeneric("ht",function(x){standardGeneric("ht")})
setMethod("ht",signature=c(x="onionmat"),function(x){om_ht(x)})
setMethod("ht",signature=c(x="onion"),function(x){om_ht(x)})

setMethod("+", signature(e1 = "onionmat", e2 = "missing"), function(e1,e2){e1})
setMethod("-", signature(e1 = "onionmat", e2 = "missing"), function(e1,e2){onionmat_negative(e1)})

setMethod("Arith",signature(e1 = "onionmat", e2="onionmat"), onionmat_arith_onionmat)
setMethod("Arith",signature(e1 = "onionmat", e2="numeric" ), onionmat_arith_single )
setMethod("Arith",signature(e1 = "onionmat", e2="onion"   ), onionmat_arith_single )
setMethod("Arith",signature(e1 = "numeric" , e2="onionmat"),  single_arith_onionmat)
setMethod("Arith",signature(e1 = "onion"   , e2="onionmat"),  single_arith_onionmat)

setMethod("Arith",signature(e1 = "matrix", e2="onion"), matrix_arith_onion)
setMethod("Arith",signature(e1 = "onion", e2="matrix"), onion_arith_matrix)

setMethod("Arith",signature(e1 = "matrix", e2="onionmat"), matrix_arith_onionmat)
setMethod("Arith",signature(e1 = "onionmat", e2="matrix"), onionmat_arith_matrix)

setMethod("%*%", c("onionmat","onionmat"), onionmat_matrixprod_onionmat)
setMethod("%*%", c("onionmat","onion")   , onionmat_matrixprod_onion)
setMethod("%*%", c("onionmat","numeric") , onionmat_matrixprod_numeric)

setMethod("%*%", c("numeric","onionmat") , numeric_matrixprod_onionmat)
setMethod("%*%", c("onion","onionmat")   , onion_matrixprod_onionmat)

setGeneric("matrix")
setMethod("matrix","onion",onionmat)

setGeneric("drop")
setMethod("drop","onionmat",function(x){newonionmat(getd(x),drop(getM(x)))})

setMethod("zapsmall","onionmat",function(x,digits=getOption("digits")){newonionmat(zapsmall(getd(x),digits=digits),getM(x))})
