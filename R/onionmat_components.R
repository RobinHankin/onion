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
