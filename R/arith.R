setMethod("+", signature(e1 = "onion", e2 = "missing"), function(e1,e2){e1})
setMethod("-", signature(e1 = "onion", e2 = "missing"), function(e1,e2){onion_negative(e1)})

## unary operators:
`onion_negative` <- function(z){as.onion(-as.matrix(z))}
`onion_inverse` <- function(z){as.onion(sweep(as.matrix(Conj(z)),2,Norm(z),FUN = "/"))}

"onion_arith_onion" <- function(e1,e2){
  switch(.Generic,
         "+" = onion_plus_onion(e1, e2),
         "-" = onion_plus_onion(e1,onion_negative(e2)),
         "*" = onion_prod_onion(e1, e2),
         "/" = onion_prod_onion(e1, onion_inverse(e2)),
         "^" = stop("onion^onion not defined"),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

"onion_arith_numeric" <- function(e1,e2){  # e1 onion, e2 numeric
  switch(.Generic,
         "+" = onion_plus_numeric (e1,  e2),
         "-" = onion_plus_numeric (e1, -e2),
         "*" = onion_prod_numeric (e1,  e2),
         "/" = onion_prod_numeric (e1,1/e2),
         "^" = onion_power_numeric(e1,  e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

"numeric_arith_onion" <- function(e1,e2){ # e1 numeric, e2 onion
  switch(.Generic,
         "+" = onion_plus_numeric(e2,  e1),
         "-" = onion_plus_numeric(-e2, e1),
         "*" = onion_prod_numeric(e2,  e1),
         "/" = onion_prod_numeric(onion_inverse(e2),e1),  # onions commute with numeric multiplication
         "^" = stop("x^onion not defined"),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

setMethod("Arith",signature(e1 = "onion"  , e2="onion"  ),   onion_arith_onion  )
setMethod("Arith",signature(e1 = "onion"  , e2="numeric"),   onion_arith_numeric)
setMethod("Arith",signature(e1 = "numeric", e2="onion"  ), numeric_arith_onion  )

`harmonize_oo` <- function(a,b){  # a=onion, b=onion; returns a list of 2 matrices
  sA <- seq_along(a) 
  sB <- seq_along(b)
  names(sA) <- names(a)
  names(sB) <- names(b)
  ind <- rbind(sA,sB)
  a <- as.matrix(a)[,ind[1,,drop=TRUE],drop=FALSE]
  b <- as.matrix(b)[,ind[2,,drop=TRUE],drop=FALSE]
  colnames(a) <- colnames(ind)
  colnames(b) <- colnames(ind)
  return(list(a,b))
}

`harmonize_on` <- function(a,b){  # a=onion, b=numeric vector; returns a list of matrix, vector
  sA <- seq_along(a) 
  sB <- seq_along(b)
  names(sA) <- names(a)
  names(sB) <- names(b)
  ind <-  rbind(sA,sB)
  a <- as.matrix(a)[,ind[1,,drop=TRUE],drop=FALSE]
  b <- b[ind[2,,drop=TRUE],drop=TRUE]  # differs here from harmonize_oo()
  colnames(a) <- colnames(ind)
  names(b) <- colnames(ind)
  return(list(a,b))
}

`onion_plus_onion` <- function(a,b){
  jj <- harmonize_oo(a,b)
  as.onion(jj[[1]]+jj[[2]])
}

`onion_plus_numeric`  <- function(a,b){
  jj <- harmonize_on(a,b)
  out <- jj[[1]]
  out[1,] <- out[1,] + jj[[2]]
  return(as.onion(out))
}

`onion_prod_onion` <- function(e1,e2){
  stopifnot(identical(class(e1),class(e2)))
  switch(class(e1),
         "quaternion" = quaternion_prod_quaternion(e1,e2),
         "octonion"   =   octonion_prod_octonion(e1,e2)
         )
}

`octonion_prod_octonion` <- function(o1,o2){
  stopifnot(is.octonion(o1) & is.octonion(o2))
  jj <- harmonize_oo(o1,o2)

  out <- .C("octonion_prod",
           as.double(jj[[1]]),
           as.double(jj[[2]]),
           as.integer(length(jj[[1]])),
           z=as.double(jj[[1]]),
           PACKAGE="onion"
           )$z
  dim(out) <- c(8,length(out)/8)
  colnames(out) <- colnames(jj[[1]])
  return(as.octonion(out))
}

`quaternion_prod_quaternion` <- function(q1,q2){
  stopifnot(is.quaternion(q1) & is.quaternion(q2))
  jj <- harmonize_oo(q1,q2)

  out <- .C("quaternion_prod",
           as.double(jj[[1]]),
           as.double(jj[[2]]),
           as.integer(length(jj[[1]])),
           z=as.double(jj[[1]]),
           PACKAGE="onion"
           )$z

  dim(out) <- c(4,length(out)/4)
  colnames(out) <- colnames(jj[[1]])
  return(as.quaternion(out))
}

`onion_prod_numeric`  <- function(a,b){  # faster than onion_prod_onion(a,as.onion(b,a))
  jj <- harmonize_on(a,b)
  as.onion(sweep(jj[[1]],2,jj[[2]],"*"))
}

`onion_power_singleinteger` <- function(o,n){
  stopifnot(length(n)==1)
  stopifnot(n == round(n))
  stopifnot(is.onion(o))
  if(n==0){
    return(1+o*0)
  } else if(n==1){
    return(o)
  } else if(n<0){
    return(Recall(onion_inverse(o),-n))  
  } else { # n>=2
    out <- o
    for(i in seq_len(n-1)){  # "-1" because we start at 'o'
      out <- out*o
    }
    return(out)
  }
}

`onion_power_numeric` <- function(o,p){  exp(log(o)*p)}

