newonionmat <- function(d,M){
    stopifnot(length(d) == length(M))
    if(is.matrix(M)){
        M[] <- seq_along(M)
        out <- list(d=d,M=M)
        class(out) <- "onionmat"
        return(out)
    } else {
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

`OMprodOM` <- function(...){ newonionmat(getd(e1)*getd(e2),getM(e1)*getM(e2)) }


