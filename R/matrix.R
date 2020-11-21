newonionmat <- function(d,M){
    if(is.matrix(M)){
        M[] <- seq_along(M)
        out <- list(d=d,M=M)
        class(out) <- "onionmat"
        return(out)
    } else {
        return(d)
    }
}

`is_ok_onionmat` <- function(x){
    stopifnot(getM(x))
    stopifnot(length(getd(x)) == length(getM(x)))
    return(TRUE)
}

`onionmat` <- function(x,rows,cols){
  M <- matrix(TRUE,rows,cols)
  stopifnot(is_ok_onionmat(x,M))
  newonionmat(x,M)
}

`getd` <- function(o){o$d}
`getm` <- function(o){o$M}

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

    } else if (.Generic == "*") {
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
            return(OMquotientS(e1, e2))  # error
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
            return(OMprodOM(e1, e2))  # error
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

ni <- function(...){stop("not yet implemented")}
OMprodOM <- ni

