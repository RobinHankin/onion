"onion_complex" <- function(z){
  switch(.Generic,
         Arg  = stop("not defined for onions"),
         Conj = onion_conjugate(z),
         Im   = onion_imag(z),
         Mod  = onion_mod(z),
         Re   = onion_re(z),
         stop(paste("Complex operator \"", .Generic, "\" not defined for Glub numbers"))
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

`onion_mod` <- function(z){sqrt(colSums(as.matrix(z)^2))}
`onion_re` <- function(z){as.matrix(z)[,1]}

setGeneric("Re<-",function(z,value){standardGeneric("Re<-")})
setGeneric("Im<-",function(x,value){standardGeneric("Im<-")})

setMethod("Re<-","onion",
          function(z,value){
            z <- as.matrix(z)
            z[1,] <- value
            return(as.onion(z))
          } )

setReplaceMethod("Im",signature(x="onion"),
                 function(x,value){
                   if(is.onion(value) && all(Re(value)==0)){
                     value <- as.matrix(value)[-1,1]
                   } else if(is.onion(value)){
                     value <- as.matrix(value)[-1,]
                   }
                   x <- as.matrix(x)
                   x[-1,] <- value
                   return(as.onion(x))
                 } )


`onion_abs` <- function(x){apply(as.matrix(x),2,function(z){sum(sqrt(z^2))})}

setGeneric("Norm",function(z){standardGeneric("Norm")})
setMethod("Norm","onion",function(z){colSums(as.matrix(z)^2)})



