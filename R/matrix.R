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
  with(x,
       stopifnot(length(x$d) == length(x$M))
       )
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


