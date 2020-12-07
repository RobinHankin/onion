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
        out <- list(d=d,M=M)
        class(out) <- "onionmat"
        return(out)
    } else {
        names(d) <- names(M)
        return(d)
    }
}

`getd` <- function(o){o$d}
`getM` <- function(o){o$M}

`onionmat` <- function(data=NA,nrow=1,ncol=1,byrow=FALSE,dimnames=NULL){
    M <- matrix(seq_len(nrow*ncol),nrow,ncol)
    dimnames(M) <- dimnames
    d <- 0*c(M)+data[1]
    if(byrow){
        d[c(matrix(seq_len(nrow*ncol),nrow,ncol,byrow=TRUE))] <- data
    } else {
        d[] <- data
    }
    newonionmat(d,M)
}

`as.quaternionmat` <- function(M){
  newonionmat(as.quaternion(c(M)),M)
}
  
`as.octonionmat` <- function(M){
  newonionmat(as.octonion(c(M)),M)
}
  
romat <- function(type="quaternion", nrow=5,ncol=6,...){
  d <- switch(type, 
              octonion = roct(nrow*ncol,...),
              rquat(nrow*ncol,...)  # quaternion
              )
  out <- newonionmat(d=d,M=matrix(0,nrow,ncol,...))
  rownames(out) <- letters[seq_len(nrow)]
  colnames(out) <- datasets::state.abb[seq_len(ncol)]
  return(out)
}

if(FALSE){

`[.onionmat` <- function(x,...){
  with(x,
       return(newonionmat(
           d[M[...]],
           M = M[...])))
}

`[<-.onionmat` <- function(x,...,value){
  with(x,{
  d[M[...]] <- value
  return(newonionmat(d,M))
  }
  )
}
}

setMethod("[", "onionmat",
          function(x, ...){
            with(x,
                 return(newonionmat(
                     d[M[...]],
                     M = M[...])))
          } )

setReplaceMethod("[",signature(x="onionmat"),
                 function(x,..., value){
                   with(x,{
                     d[M[...]] <- value
                     return(newonionmat(d,M))
                   } )
                 } )

`OMneg` <- function(e1){ newonionmat(-getd(e1),getM(e1)) }
`OMinv` <- function(e1){ newonionmat(getd(e1),1/getM(e1)) }
`OMplusOM` <- function(e1,e2){ newonionmat(getd(e1)+getd(e2),getM(e1)+getM(e2)) }
`OMplusS` <- function(e1,e2){ newonionmat(getd(e1)+e2,getM(e1)) }
`OMpowerS` <- function(e1,e2){ newonionmat(getd(e1)^e2,getM(e1)) }
`OMprodS` <- function(x,y){ newonionmat(getd(x)*y,getM(x)) }
`SprodOM` <- function(e1,e2){OMprodS(e2,e1)}  # multiplication is noncommutative
`OMprodOM` <- function(e1,e2){ newonionmat(getd(e1)*getd(e2),getM(e1)*getM(e2)) }
`OMequalOM` <- function(e1,e2){
    jj <- getM(e1) == getM(e2) # traps nonconformant matrices
    out <- getd(e1) == getd(e2)
    attributes(out) <- attributes(jj)
    return(out)
}

`SpowerOM` <- function(...){stop("not defined")} # 'S' = scalar; 'OM' = 'onionmat'
`OMpowerOM` <- function(...){stop("not defined")}
`OMquotientOM` <- function(...){stop("onionic matrices not a division algebra")}

"onionmat_arith_onionmat" <- function(e1,e2){ # e1, e2 onionmats  POINTWISE
  switch(.Generic,
         "+" = OMplusOM(e1, e2),
         "-" = OMplusOM(e1, OMneg(e2)),
         "*" = OMprodOM(e1, e2),
         "/" = OMprodOM(e1, OMinv(e2)),
         "^" = OMpowerS(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

"onionmat_arith_numeric" <- function(e1,e2){ # e1 onionmat, e2 numeric  POINTWISE
  switch(.Generic,
         "+" = OMplusS(e1, e2),
         "-" = OMplusS(e1,-e2),
         "*" = OMprodS(e1, e2),
         "/" = OMprodS(e1, 1/e2),
         "^" = OMpowerS(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

"numeric_arith_onionmat" <- function(e1,e2){ # e1 numeric, e2 onionmat  POINTWISE
  switch(.Generic,
         "+" = OMplusS(e2, e1),  # addition is commutative
         "-" = OMplusS(e2,-e1),
         "*" = OMprodS(e2, e1),  # (scalar) multiplication is commutative
         "/" = OMprodS(e2, 1/e1),
         "^" = SpowerOM(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

"onionmat_arith_onion" <- function(e1,e2){ # e1 onionmat, e2 onion  POINTWISE
  switch(.Generic,
         "+" = OMplusS(e1, e2),
         "-" = OMplusS(e1,-e2),
         "*" = OMprodS(e1, e2),
         "/" = OMprodS(e1, onion_inverse(e2)),
         "^" = SpowerOM(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

"onion_arith_onionmat" <- function(e1,e2){ # e1 onionmat, e2 onion  POINTWISE
  switch(.Generic,
         "+" = OMplusS(e2, e1),
         "-" = OMplusS(e1,-e2),
         "*" = SprodOM(e1, e2),
         "/" = OMprodS(e1, onion_inverse(e2)),
         "^" = SpowerOM(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

setGeneric("dim")
`dim.onionmat` <- function(x){dim(getM(x))}
setGeneric("nrow")
`nrow.onionmat` <- function(x){nrow(getM(x))}
setGeneric("ncol")
`ncol.onionmat` <- function(x){ncol(getM(x))}

setGeneric("rownames")
`rownames.onionmat` <- function(x){rownames(getM(x))}
setGeneric("colnames")
`colnames.onionmat` <- function(x){colnames(getM(x))}

setGeneric("dimnames")
`dimnames.onionmat` <- function(x){dimnames(getM(x))}

setGeneric("dimnames<-")
`dimnames<-.onionmat` <- function(x,value){
  m <- getM(x)
  dimnames(m) <- value
  return(newonionmat(getd(x),m))
}

setGeneric("rownames<-")
setGeneric("colnames<-")

`rownames<-.onionmat` <- function(x,value){
    m <- getM(x)
    rownames(m) <- value
    return(newonionmat(getd(x),m))
}

`colnames<-.onionmat` <- function(x,value){
    m <- getM(x)
    colnames(m) <- value
    return(newonionmat(getd(x),m))
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

`om_cprod` <- function(x,y=x){  # t(Conj(x)) %*% y
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


setMethod("Conj","onionmat",function(z){newonionmat(Conj(getd(z)),getM(z)) })

`t.onionmat` <- function(x){ # NB1: this  ensures that emulator::ht() works; NB2: t(x) DOES NOT TAKE CONJUGATE
  jj <- t(getM(x))
  newonionmat(getd(x)[jj],jj)
}

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
  out[cbind(seq_len(n),seq_len(n))] <- real_diagonal # diag() does not work
  return(out)
}

`print.onionmat` <- function(x,...){
  print(x$d)
  return(x)
}

setOldClass("onionmat")
setGeneric("cprod",function(x,y){standardGeneric("cprod")})
setMethod("cprod",signature=c(x="onionmat",y="onionmat"),function(x,y){om_cprod(x,y)})
setMethod("cprod",signature=c(x="onionmat",y="missing"),function(x,y){om_cprod(x,x)})
setMethod("cprod",signature=c(x="onionmat",y="ANY"),function(x,y){om_cprod(x,y)})
setMethod("cprod",signature=c(x="ANY",y="onionmat"),function(x,y){om_cprod(Conj(x),y)})
setMethod("cprod",signature=c(x="ANY",y="ANY"),function(x,y){emulator::cprod(Conj(x),y)})



setMethod("Arith",signature(e1 = "onionmat", e2="onionmat"), onionmat_arith_onionmat)
setMethod("Arith",signature(e1 = "onionmat", e2="numeric" ), onionmat_arith_numeric )
setMethod("Arith",signature(e1 = "numeric" , e2="onionmat"),  numeric_arith_onionmat)
setMethod("Arith",signature(e1 = "onionmat", e2="onion"   ), onionmat_arith_onion   )
setMethod("Arith",signature(e1 = "onion"   , e2="onionmat"),    onion_arith_onionmat)

setMethod("%*%", c("onionmat","onionmat"), onionmat_matrixprod_onionmat)
setMethod("%*%", c("onionmat","numeric") , OMprodS)
setMethod("%*%", c("numeric","onionmat") , OMprodS)
setMethod("%*%", c("onion","onionmat")   , OMprodS)
setMethod("%*%", c("numeric","onion")    , OMprodS)

