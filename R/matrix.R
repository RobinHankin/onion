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

`Ops.onionmat` <- function(e1,e2){
    f <- function(...){stop("odd---neither argument has class octonion?")}
    unary <- nargs() == 1
    lclass <- nchar(.Method[1]) > 0
    rclass <- !unary && (nchar(.Method[2]) > 0)
    
    if(unary){
        if (.Generic == "+") {
            return(e1)
        } else if (.Generic == "-") {
            return(OMneg(e1))
        } else {
            stop("Unary operator '", .Generic, "' is not implemented for onionmats")
        }
    }
    if (!is.element(.Generic, c("+", "-", "*", "/", "^", "==", "!=")))
        stop("operator '", .Generic, "' is not implemented for onions")

    if (.Generic == "+") { 
        if (lclass && rclass) {
            return(OMplusOM(e1, e2))
        } else if (lclass) {
            return(OMplusS(e1, e2))
        } else if (rclass) {
            return(OMplusS(e2,e1))
        } else {
            f()
        }

    } else if (.Generic == "-") { 
        if (lclass && rclass) {
            return(OMplusOM(e1, OMneg(e2)))
        } else if (lclass) {
            return(OMplusS(e1, -e2))
        } else if (rclass) {
            return(OMplusS(-e2,e1))
        } else {
            f()
        }

    } else if (.Generic == "*") {  # NB pointwise 
        if (lclass && rclass) {
            return(OMprodOM(e1, e2))
        } else if (lclass) {
            return(OMprodS(e1, e2))
        } else if (rclass) {
            return(OMprodS(e2, e1))
        } else {
            f()
        }
    } else if (.Generic == "/") {
        if (lclass && rclass) {
            return(OMquotientOM(e1, e2))  # error
        } else if (lclass) {
            return(OMprodS(e1, 1/e2))
        } else if (rclass) {
            return(OMprodS(e2, 1/e1))
        } else {
            f()
        }
    } else if (.Generic == "^") {
        if (lclass && rclass) {
            return(OMpowerOM(e1, e2))  # error
        } else if (lclass) {
            return(OMpowerS(e1, e2)) # works
        } else if (rclass) {
            return(SpowerOM(e1, e2))  # error
        } else {
            f()
        }

    } else if (.Generic == "^") {
        if (lclass && rclass) {
            return(OMpowerOM(e1, e2)) # error
        } else if (lclass) {
            return(OMpowerS(e1, e2))
        } else if (rclass) {
            return(SpowerOM(e1,e2)) # error
        } else {
            f()
        }
    } else if (.Generic == "==") {
        return(OMequalOM(e1,e2))
    } else if (.Generic == "!=") {
        return(!OMequalOM(e1,e2))
    } else {
        stop("should not reach here")
    }
}

`OMneg` <- function(e1){ newonionmat(-getd(e1),getM(e1)) }
`OMplusOM` <- function(e1,e2){ newonionmat(getd(e1)+getd(e2),getM(e1)+getM(e2)) }
`OMplusS` <- function(e1,e2){ newonionmat(getd(e1)+e2,getM(e1)) }
`OMpowerS` <- function(e1,e2){ newonionmat(getd(e1)^e2,getM(e1)) }
`OMprodS` <- function(e1,e2){ newonionmat(getd(e1)*e2,getM(e1)) }
`OMprodOM` <- function(e1,e2){ newonionmat(getd(e1)*getd(e2),getM(e1)*getM(e2)) }
`OMequalOM` <- function(e1,e2){
    jj <- getM(e1) == getM(e2) # traps nonconformant matrices
    out <- getd(e1) == getd(e2)
    attributes(out) <- attributes(jj)
    return(out)
}

`SpowerOM` <- function(...){stop("not defined")}
`OMpowerOM` <- function(...){stop("not defined")}
`OMquotientOM` <- function(...){stop("onionic matrices not a division algebra")}

## Following lines modified from the gmp package:
`%*%` <- function(x,y){ UseMethod("%*%") }
`%*%.default` <- function(x,y) {
    if(inherits(y, "onionmat")){
        return(onionmatprod(x,y))
    } else {
        return(base::"%*%"(x,y))
    }
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

`onionmatprod` <- function(x,y){
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
`Conj.onionmat` <- function(z){ newonionmat(Conj(getd(z)),getM(z)) }

`t.onionmat` <- function(x){ # NB ensures that emulator::ht() works.
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


`Re.onionmat` <- function(x){
  out <- getM(x)
  out[] <- Re(getd(x))
  out
  }

"i.onionmat" <- function(x){
  out <- getM(x)
  out[] <- i(getd(x))
  out
}

"j.onionmat" <- function(x){
  out <- getM(x)
  out[] <- j(getd(x))
  out
}

"k.onionmat" <- function(x){
  out <- getM(x)
  out[] <- k(getd(x))
  out
}

"l.onionmat" <- function(x){
  out <- getM(x)
  out[] <- l(getd(x))
  out
}

"il.onionmat" <- function(x){
  out <- getM(x)
  out[] <- il(getd(x))
  out
}

"jl.onionmat" <- function(x){
  out <- getM(x)
  out[] <- jl(getd(x))
  out
}

"kl.onionmat" <- function(x){
  out <- getM(x)
  out[] <- kl(getd(x))
  out
}

`Re.onionmat` <- function(z){
  out <- getM(z)
  out[] <- Re(getd(z))
  out
  }

`Im.onionmat` <- function(z){
  d <- getd(z)
  Re(d) <- 0
  newonionmat(d,getM(z))
  }

"i.onionmat" <- function(x){
  out <- getM(x)
  out[] <- i(getd(x))
  out
}

"j.onionmat" <- function(x){
  out <- getM(x)
  out[] <- j(getd(x))
  out
}

"k.onionmat" <- function(x){
  out <- getM(x)
  out[] <- k(getd(x))
  out
}

"l.onionmat" <- function(x){
  out <- getM(x)
  out[] <- l(getd(x))
  out
}

"il.onionmat" <- function(x){
  out <- getM(x)
  out[] <- il(getd(x))
  out
}

"jl.onionmat" <- function(x){
  out <- getM(x)
  out[] <- jl(getd(x))
  out
}

"kl.onionmat" <- function(x){
  out <- getM(x)
  out[] <- kl(getd(x))
  out
}

`Re<-.onionmat` <- function(x,value){
  d <- getd(x)
  Re(d) <- value
  newonionmat(d,getM(x))
  }

`i<-.onionmat` <- function(x,value){
  d <- getd(x)
  i(d) <- value
  newonionmat(d,getM(x))
  }

`j<-.onionmat` <- function(x,value){
  d <- getd(x)
  j(d) <- value
  newonionmat(d,getM(x))
  }

`k<-.onionmat` <- function(x,value){
  d <- getd(x)
  k(d) <- value
  newonionmat(d,getM(x))
  }

`l<-.onionmat` <- function(x,value){
  d <- getd(x)
  l(d) <- value
  newonionmat(d,getM(x))
  }

`il<-.onionmat` <- function(x,value){
  d <- getd(x)
  il(d) <- value
  newonionmat(d,getM(x))
  }

`jl<-.onionmat` <- function(x,value){
  d <- getd(x)
  jl(d) <- value
  newonionmat(d,getM(x))
  }

`kl<-.onionmat` <- function(x,value){
  d <- getd(x)
  Re(d) <- value
  newonionmat(d,getM(x))
  }


