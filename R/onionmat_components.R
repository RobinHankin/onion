"onionmat_complex" <- function(z){
  switch(.Generic,
         Arg  = stop("not defined for onions"),
         Conj = onionmat_conjugate(z),
         Im   = onionmat_imag(z),
         Mod  = onionmat_mod(z),
         Re   = onionmat_re(z),
         stop(paste("Complex operator \"", .Generic, "\" not defined for onions"))
         )
}
setMethod("Complex","onionmat", onionmat_complex)

onionmat_conjugate <- function(z){
  Im(z) <- -Im(z)
  return(z)
}

onionmat_imag <- function(z){
  Re(z) <- 0
  return(z)
}

`onionmat_re` <- function(z){
  out <- getM(z)
  out[] <- Re(getd(z))
  return(out)
}

`onionmat_mod` <- function(z){
  out <- getM(z)
  out[] <- Mod(getd(z))
  return(out)
}

`onionmat_imag` <- function(z){
  d <- getd(z)
  Re(d) <- 0
  newonionmat(d,getM(z))
  }

setMethod("i","onionmat", function(z){
  out <- getM(z)
  out[] <- i(getd(z))
  return(out)
} )

setMethod("j","onionmat", function(z){
  out <- getM(z)
  out[] <- j(getd(z))
  return(out)
} )

setMethod("k","onionmat", function(z){
  out <- getM(z)
  out[] <- k(getd(z))
  return(out)
} )

setMethod("l","onionmat", function(z){
  out <- getM(z)
  out[] <- l(getd(z))
  return(out)
} )

setMethod("il","onionmat", function(z){
  out <- getM(z)
  out[] <- il(getd(z))
  return(out)
} )

setMethod("jl","onionmat", function(z){
  out <- getM(z)
  out[] <- jl(getd(z))
  return(out)
} )

setMethod("kl","onionmat", function(z){
  out <- getM(z)
  out[] <- kl(getd(z))
  return(out)
} )


setReplaceMethod("i",signature(x="onionmat"), function(x,value){
  d <- getd(x)
  i(d) <- value
  return(newonionmat(d,getM(x)))
} )

setReplaceMethod("j",signature(x="onionmat"), function(x,value){
  d <- getd(x)
  j(d) <- value
  return(newonionmat(d,getM(x)))
} )

setReplaceMethod("k",signature(x="onionmat"), function(x,value){
  d <- getd(x)
  k(d) <- value
  return(newonionmat(d,getM(x)))
} )

setReplaceMethod("l",signature(x="onionmat"), function(x,value){
  d <- getd(x)
  l(d) <- value
  return(newonionmat(d,getM(x)))
} )


setReplaceMethod("il",signature(x="onionmat"), function(x,value){
  d <- getd(x)
  il(d) <- value
  return(newonionmat(d,getM(x)))
} )

setReplaceMethod("jl",signature(x="onionmat"), function(x,value){
  d <- getd(x)
  jl(d) <- value
  return(newonionmat(d,getM(x)))
} )

setReplaceMethod("kl",signature(x="onionmat"), function(x,value){
  d <- getd(x)
  kl(d) <- value
  return(newonionmat(d,getM(x)))
} )


