

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


