`onion_cumprod` <- function(x){
  if(length(x)==1){return(x)}
  out <- x
  for(i in seq_along(x)[-1]){
    out[i] <- out[i-1]*x[i]
  }
  return(out)
}

`onion_cumsum` <- function(x){
    if(length(x)==1){
        return(x)
    } else {
        return(as.onion(t(apply(as.matrix(x),1,cumsum))))
    }
}
setMethod("Math","onion",
          function(x){
            switch(.Generic,
                   abs     = onion_abs(x),
                   sign    = onion_sign(x),
                   sqrt    = onion_sqrt(x),
                   cumprod = onion_cumprod(x),
                   cumsum  = onion_cumsum(x),
                   log     = onion_log(x,base=exp(1)),
                   acos    = onion_acos(x),
                   acosh   = onion_acosh(x),
                   asin    = onion_asin(x),
                   asinh   = onion_asinh(x),
                   atan    = onion_atan(x),
                   atanh   = onion_atanh(x),
                   exp     = onion_exp(x),
                   cos     = onion_cos(x),
                   cosh    = onion_cosh(x),
                   sin     = onion_sin(x),
                   sinh    = onion_sinh(x),
                   tan     = onion_tan(x),
                   tanh    = onion_tanh(x),
                   stop("Not yet implemented for onions")
                   )
          } )

`onion_allsum` <- function(x){as.onion(cbind(rowSums(as.matrix(x))))}
`quaternion_allprod` <- function(x){
  out <- x[1]
  for(i in seq(from=2,len=length(x)-1)){
    out <- out*x[i]
  }
  return(out)
}

`onion_abs` <- function(x){sqrt(Norm(x))}

`onion_sqrt` <- function(x){exp(log(x)/2)}

`onion_exp` <- function(x){
  t <- Re(x)
  V <- Im(x)
  mV <- Mod(V)
  out <- exp(t)*cos(mV) + V*exp(t)*sin(mV)/mV
  i <- mV==0 | is.na(mV)
  Re(out[i]) <- exp(t[i])
  Im(out[i]) <- 0
  return(out)
}

"onion_log" <- function(x,base=exp(1)){
  t <- Re(x)
  V <- Im(x)
  mV <- Mod(V)
  mX <- Mod(x)
  out <- log(Norm(x))/2 + V*atan2(mV,t)/mV
  i <- mV==0  | is.na(mV)
  Re(out[i]) <- log(Norm(x[i]))/2
  Im(out[i]) <- 0
  return(out/log(base))
}

"onion_sin" <- function(x){
  t <- Re(x)
  V <- Im(x)
  mV <- Mod(V)
  out <- sin(t)*cosh(mV) + V*cos(t)*sinh(mV)/mV
  i <- mV==0 | is.na(mV)
  Re(out[i]) <- sin(t[i])
  Im(out[i]) <- 0
  return(out)
}

"onion_cos" <- function(x){
  t <- Re(x)
  V <- Im(x)
  mV <- Mod(V)
  out <- cos(t)*cosh(mV) - V*sin(t)*sinh(mV)/mV
  i <- mV==0 | is.na(mV)
  Re(out[i]) <- cos(t[i])
  Im(out[i]) <- 0
  return(out)
}

"onion_tan" <- function(x){
  return(sin(x)/cos(x))
}

"onion_sinh" <- function(x){
  t <- Re(x)
  V <- Im(x)
  mV <- Mod(V)
  out <- sinh(t)*cos(mV) + V*cosh(t)*sin(mV)/mV
  i <- mV==0 | is.na(mV)
  Re(out[i]) <- sinh(t[i])
  Im(out[i]) <- 0
  return(out)
}

"onion_cosh" <- function(x){
  t <- Re(x)
  V <- Im(x)
  mV <- Mod(V)
  out <- cosh(t)*cos(mV) + V*sinh(t)*sin(mV)/mV
  i <- mV==0 | is.na(mV)
  Re(out[i]) <- cosh(t[i])
  Im(out[i]) <- 0
  return(out)
}

"onion_tanh" <- function(x){
  return(sinh(x)/cosh(x))
}

"onion_asinh" <- function(x){
  log(x+sqrt(x*x+1))
}

"onion_acosh" <- function(x){
  log(x+sqrt(x*x-1))
}

"onion_atanh" <- function(x){
  log((1+x)/(1-x))/2
}

"onion_asin" <- function(x){
   V <- Im(x)
   mV <- Mod(V)
   v1 <- V/mV
   out <- x
   i <- mV==0 | is.na(mV)
   out[!i] <- -v1[!i]*asinh(x[!i]*v1[!i])
   Re(out[i]) <- Re(asin(0i+Re(x[i])))
   Im(out[i]) <- 0
   return(out)
 }

"onion_acos" <- function(x){
  V <- Im(x)
  mV <- Mod(V)
  v1 <- V/mV
  out <- x
  i <- mV==0 | is.na(mV)
  out[!i] <- -v1[!i]*acosh(x[!i])
  Re(out[i]) <- Re(acos(0i+Re(x[i])))
  Im(out[i]) <- 0
  return(out)
}
   
"onion_atan" <- function(x){
  V <- Im(x)
  mV <- Mod(V)
  v1 <- V/Mod(V)
  return(-v1*atanh(x*v1))
}

`onion_sign` <- function(x){x/Mod(x)}

setMethod("round","onion",
          function(x,digits=0){
              as.onion(round(as.matrix(x),digits=digits))
          })

setMethod("round","onionmat",
          function(x,digits=0){
              newonionmat(round(getd(x),digits=digits),getM(x))
          })

