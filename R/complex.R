
"onion_complex" <- function(z){
  switch(.Generic,
         Arg  = stop("not defined for onions"),
         Conj = onion_conjugate(z),
         Im   = onion_imag(z),
         Mod  = onion_mod(z),
         Re   = onion_re(z),
         stop(gettextf("comparison operator %s not defined for onions", dQuote(.Generic)))
         )
}

setMethod("Complex","onion", onion_complex)

onion_conjugate <- function(z){
  Im(z) <- -Im(z)
  return(z)
}

onion_imag <- function(z){
  z <- as.matrix(z)
  z[1,] <- 0
  return(as.onion(z))
}

`onion_re` <- function(z){as.matrix(z)[1,]}

setGeneric("Re<-",function(z,value){standardGeneric("Re<-")})
setGeneric("Im<-",function(x,value){standardGeneric("Im<-")})

setMethod("Re<-","onion",
          function(z,value){
            z <- as.matrix(z)
            z[1,] <- value
            return(as.onion(z))
          } )

setMethod("Re<-","onionmat",
          function(z,value){
            d <- getd(z)
            Re(d) <- value
            newonionmat(d,getM(z))
          } )

setReplaceMethod("Im",signature(x="onion"),
                 function(x,value){
                   if(is.onion(value) && all(Re(value)==0)){
                     value <- as.matrix(value)[-1,]
                   }
                   x <- as.matrix(x)
                   x[-1,] <- value
                   return(as.onion(x))
                 } )

setReplaceMethod("Im",signature(x="onionmat"),
                 function(x,value){
                   d <- getd(x)
                   if(is.onionmat(value)){value <- getd(value)}
                   Im(d) <- value
                   return(newonionmat(d,getM(x)))
                 } )

setGeneric("Norm",function(z){standardGeneric("Norm")})
setMethod("Norm","onion",function(z){colSums(as.matrix(z)^2)})
setMethod("Norm","onionmat",function(z){
    out <- getM(z)
    out[] <- Norm(getd(z))
    return(out)
} )

`onion_mod` <- function(z){sqrt(Norm(z))}

`onionmat_conjugate` <- function(z){newonionmat(Conj(getd(z)),getM(z))}
`onionmat_imag` <- function(z){newonionmat(Im(getd(z)),getM(z))}

`onionmat_mod` <- function(z){
  out <- getM(z)
  out[] <- Mod(getd(z))
  return(out)
}

`onionmat_re` <- function(z){
  out <- getM(z)
  out[] <- Re(getd(z))
  return(out)
}

"onionmat_complex" <- function(z){
  switch(.Generic,
         Arg  = stop("not defined for onions"),
         Conj = onionmat_conjugate(z),
         Im   = onionmat_imag(z),
         Mod  = onionmat_mod(z),
         Re   = onionmat_re(z),
         stop(gettextf("Complex operator %s not defined for onionmats", dQuote(.Generic)))
         )
}

setMethod("Complex","onionmat", onionmat_complex)
