setClass("onion",
         representation = "VIRTUAL"
         )

setClass("quaternion",
         slots    = c(x = "matrix"),
         contains = "onion"
         )

setClass("octonion",
         slots    = c(x = "matrix"),
         contains = "onion"
         )


`valid_quaternion` <- function(object){
  x <- object@x
  if(!is.numeric(x)){
    return("not numeric")
  } else if(!is.matrix(x)){
    return("not a matrix")
  } else if(nrow(x) != 4){
    return("must have 4 rows")
  } else {
    return(TRUE)
  }
}


`valid_octonion` <- function(object){
  x <- object@x
  if(!is.numeric(x)){
    return("not numeric")
  } else if(!is.matrix(x)){
    return("not a matrix")
  } else if(nrow(x) != 8){
    return("must have 8 rows")
  } else {
    return(TRUE)
  }
}
    
setValidity("quaternion", valid_quaternion)
setValidity("octonion", valid_octonion)

"is.quaternion" <- function(x){is(x,"quaternion")}
"is.octonion" <- function(x){is(x,"octonion")}
"is.onion" <- function(x){is(x,"onion")}


setAs("onion", "matrix", function(from){ from@x} )
setMethod("as.matrix",signature(x="onion"),function(x){as(x,"matrix")})

`quaternion_to_octonion` <- function(from){
  stopifnot(is.quaternion(from))
  as.octonion(rbind(as.matrix(from),matrix(0,4,length(from))))
}

`octonion_to_quaternion` <- function(from){
  stopifnot(is.octonion(from))
  as.quaternion(as.matrix(from)[1:4,,drop=FALSE])
}

setAs("quaternion","octonion",quaternion_to_octonion)
setAs("octonion","quaternion",octonion_to_quaternion)
      
setGeneric("length")
setMethod("length","onion",function(x){ncol(x@x)})

setGeneric("names")
setMethod("names","onion",function(x){colnames(x@x)})

setReplaceMethod("names",signature(x="onion"),
                 function(x,value){
                   out <- as.matrix(x)
                   colnames(out) <- value
                   return(as.onion(out))
                 } ) 

`as.onion` <- function(x,type){
  if(missing(type)){
    if(is.onion(x)){
      return(x)
    } else {
      if(nrow(x)==4){
        return(as.quaternion(x))
      } else if(nrow(x)==8){
        return(as.octonion(x))
      } else {
        stop("supplied matrix must have either 4 or 8 rows")
      }
    }
  } else {  # type supplied
    if(is.onion(type)){type <- class(type)}
    if(type == "quaternion"){
      if(is.quaternion(x)){
        return(x)
      } else if(is.octonion(x)) { 
        return(octonion_to_quaternion(x))
      } else {
        return(as.quaternion(rbind(x,matrix(0,3,length(x)))))
      }
    } else if(type == "octonion"){
      if(is.octonion(x)){
        return(x)
      } else if(is.quaternion(x)){
        return(quaternion_to_octonion(x))
      } else {
        return(as.octonion(rbind(x,matrix(0,7,length(x)))))
      }
    } else {
      stop("type not recognised")
    }
  }
}


`as.quaternion` <- function(x,single=FALSE,names=NULL){
  if(is.quaternion(x)){return(x)}
  if(is.complex(x)){
    if(single){
      stop("single cannot be TRUE with complex x")
    } else {
      return(Recall(rbind(Re(x),Im(x),matrix(0,2,length(x)))))
    }
  }
  if(is.matrix(x)){
    if(nrow(x) == 4){
      out <- x
    } else {
      if( (nrow(x)==3) && ncol(x)==3){ # SO3
        return(matrix2quaternion(x))
      } else {
      stop("If matrix supplied, it must either have four rows or be 3x3")
      }
    }
  } else if(is.array(x)){
    out <- apply(x,3,matrix2quaternion) # 4-row matrix
  } else {
    if(single){
      if(is.vector(x)){
        out <- as.matrix(x[1:4])
        if(length(x) != 4){
          warning("single set to TRUE, but a vector of length !=4 supplied. Set to length 4, Procrustes-style")
        }
      } else {
        stop("single set to TRUE, but nonvector supplied.")
      }
    } else {
      x <- as.vector(x)
      out <- kronecker(t(x),c(1,rep(0,3)))
    }
  }
  rownames(out) <- NULL
  colnames(out) <- names
  return(new("quaternion",x=out))
}

`quaternion` <- function(length.out=NULL, names=NULL, Re=0, i=0, j=0, k=0){
  if (
      (missing(Re) | length(Re)==0) &
      (missing(i)  | length( i)==0) &
      (missing(j)  | length( j)==0) &
      (missing(k)  | length( k)==0) 
      )
    {
      if(missing(length.out)){
        return(Recall(Re=0)[0])
      } else {
        return(Recall(Re=rep(0,length.out)))
      }
    }
  
  out <- as.quaternion(rbind(Re,i,j,k))
  if(!is.null(length.out)){
    if(ncol(out)==1){
      out <- as.quaternion(kronecker(out,t(rep(1,length.out))))
    } else {
      length(out) <- length.out
    }
  }
  if(!is.null(names)){
    colnames(out) <- names
  }
  return(out)
}

`octonion` <- function(length.out=NULL, names=NULL, Re=0, i=0, j=0, k=0, l=0, il=0, jl=0, kl=0){
  if (
      (missing(Re) | length(Re)==0) &
      (missing(i)  | length( i)==0) &
      (missing(j)  | length( j)==0) &
      (missing(k)  | length( k)==0) &
      (missing(l)  | length( l)==0) &
      (missing(il) | length(il)==0) &
      (missing(jl) | length(jl)==0) &
      (missing(kl) | length(kl)==0)
      )
    {
      if(missing(length.out)){
        return(Recall(Re=0)[0])
      } else {
        return(Recall(Re=rep(0,length.out)))
      }
    }
  
  out <- as.octonion(rbind(Re,i,j,k,l,il,jl,kl))
  if(!is.null(length.out)){
    if(ncol(out)==1){
      out <- as.octonion(kronecker(out,t(rep(1,length.out))))
    } else {
      length(out) <- length.out
    }
  }
  if(!is.null(names)){
    colnames(out) <- names
  }
  return(out)
}

"as.octonion" <- function(x, single=FALSE, names=NULL){
  if(is.octonion(x)){return(x)}
  if(is.quaternion(x)){return(as.octonion(rbind(x,x*0),names=names))}
  if(is.null(names(x))){names <- colnames(x)}

  if(is.complex(x)){
    if(single){
      stop("single cannot be TRUE with complex x")
    } else {
      return(Recall(rbind(Re(x),Im(x),matrix(0,6,length(x))),names=names))
    }
  }
  if(is.matrix(x)){
    if(nrow(x) == 8){
      out <- x
    } else {
      stop("If matrix supplied, it must have eight rows")
    }
  } else {
    if(single){
      if(is.vector(x)){
        out <- as.matrix(x[1:8])
        if(length(x) != 8){
          warning("single set to TRUE, but a vector of length !=8 supplied.  Setting to length 8, Procrustes-style")
        }
      } else {
        stop("single set to TRUE, but nonvector supplied.")
      }
    } else {
      x <- as.vector(x)
      out <- kronecker(t(x),c(1,rep(0,7)))
    }
  }
  colnames(out) <- names
  return(new("octonion",x=out))
}


`biggest` <- function(...){
  a <-  unlist(lapply(list(...),class))
  if("octonion" %in% a){
    return("octonion")
  } else if("quaternion" %in% a)
    {return("quaternion")
   } else {
     return("scalar")
   }
}

setMethod("str",signature(object="onion"),function(object,...){str_onion(object,...)})

`str_onion` <- function(object, vec.len=4, ...){
  string <- class(object)
  object <- as.matrix(object)
  if(!is.null(colnames(object))){
    string <- paste("Named",string,sep=" ")
  }
  cat(paste(string," [1:",ncol(object),"]\n",sep=""))
  l <- min(nrow(object),vec.len)
  if(l>0){
    cat(paste(condense(object[,1:l]),collapse=", ",sep=""))
  }
  
  if(length(object) > l){
    if(l>0){
      cat(", ")
    }
    cat("...")
    
  }
  cat("\n")
}

"condense" <- function(x){
  x <- as.matrix(x)
  out <- x
  out[x==0] <- "0"
  out[x> 0] <- "+"
  out[x< 0] <- "-"
  return(noquote(apply(out,2,paste,collapse="")))
  }

"rquat" <- function(n=5){ as.quaternion(matrix(rnorm(n*4),4,n))}
"roct" <- function(n=5){ as.octonion(matrix(rnorm(n*8),8,n))}

setMethod("[", "onion",
          function(x, i, j, drop){
            if(!missing(j)){
              warning("second argument to extractor function ignored")
            }
            return(as.onion(as.matrix(x)[,i, drop=FALSE]))
          } )

setReplaceMethod("[",signature(x="onion"),
                 function(x,i,j,value){
                   if(!missing(j)){
                     warning("second argument to extractor function ignored")
                   }
                   out <- as.matrix(x)
                   if(is.vector(value)){
                     value <- kronecker(t(value),c(1,rep(0,nrow(out)-1)))
                   }
                   out[,i] <- as.matrix(value)
                   return(as.onion(out))
                 } )

`process` <- function(A,B){
  ## takes two onions, returns a list of two same-sized matrices with appropriate (col)names

  sA <- seq_along(A)
  sB <- seq_along(B)
  names(sA) <- names(A)
  names(sB) <- names(B)
  ind <-  rbind(sA,sB)

  A <- as.matrix(A)[,ind[1,,drop=TRUE]]
  B <- as.matrix(B)[,ind[2,,drop=TRUE]]

  colnames(A) <- colnames(ind)
  colnames(B) <- colnames(ind)

  list(A,B)
}
  
setGeneric("Norm",function(z){standardGeneric("Norm")})
setMethod("Norm","onion",function(z){rowsums(as.matrix(z)^2)})

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

`onion_mod` <- function(z){ colSums(as.matrix(z)^2) }
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


setMethod("Arith",signature(e1 = "onion", e2="missing"),
          function(e1,e2){
            switch(.Generic,
                   "+" = e1,
                   "-" = onion_negative(e1),
                   stop(paste("Unary operator", .Generic,
                              "not allowed on onions"))
                   )
          } )

## unary operators:
`onion_negative` <- function(z){as.onion(-as.matrix(z))}
`onion_inverse` <- function(z){as.onion(sweep(as.matrix(Conj(z)),2,Norm(z),FUN = "/"))}



"onion_arith_onion" <- function(e1,e2){
  switch(.Generic,
         "+" = onion_plus_onion(e1, e2),
         "-" = onion_plus_onion(e1,onion_negative(e2)),
         "*" = onion_prod_onion(e1, e2),
         "/" = onion_prod_onion(e1, onion_inverse(e2)),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

"onion_arith_numeric" <- function(e1,e2){  # e1 onion, e2 numeric
  switch(.Generic,
         "+" = onion_plus_numeric(e1,  e2),
         "-" = onion_plus_numeric(e1, -e2),
         "*" = onion_prod_numeric(e1,  e2),
         "/" = onion_prod_numeric(e1,1/e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}

"numeric_arith_onion" <- function(e1,e2){ # e1 numeric, e2 onion
  switch(.Generic,
         "+" = onion_plus_numeric(e2,  e1),
         "-" = onion_plus_numeric(e2, -e1),
         "*" = onion_prod_numeric(e2,  e1),
         "/" = onion_prod_numeric(e2,1/e1),  # onions commute with numeric multiplication
         stop(paste("binary operator \"", .Generic, "\" not defined for onions"))
         )
}


setMethod("Arith",signature(e1 = "onion"  , e2="onion"  ),   onion_arith_onion  )
setMethod("Arith",signature(e1 = "onion"  , e2="numeric"),   onion_arith_numeric)
setMethod("Arith",signature(e1 = "numeric", e2="onion"  ), numeric_arith_onion  )

`onion_plus_onion` <- function(a,b){
  jj <- process(a,b)
  as.onion(jj[[1]]+jj[[2]])
}

`onion_plus_numeric`  <- function(a,b){ onion_plus_onion(a,as.onion(b,a)) }

`onion_prod_onion`    <- function(a,b){stop()}

`onion_prod_numeric`  <- function(a,b){  # faster than onion_prod_onion(a,as.onion(b,a))
  sA <- seq_along(a) 
  sB <- seq_along(b)
  names(sA) <- names(a)
  names(sB) <- names(b)
  ind <-  rbind(sA,sB)

  out <- as.onion(sweep(as.matrix(a)[,ind[1,,drop=TRUE],drop=FALSE],2,b[ind[2,,drop=TRUE]],"*"))
  names(out) <- colnames(ind)
  return(out)
}

`onion_power_numeric` <- function(a,b){stop("not implemented")
  sA <- seq_along(a) 
  sB <- seq_along(b)
  names(sA) <- names(a)
  names(sB) <- names(b)
  ind <-  rbind(sA,sB)

  out <- 

  out <- as.onion(sweep(as.matrix(a)[,ind[1,,drop=TRUE],drop=FALSE],2,b[ind[2,,drop=TRUE]],"*"))
  names(out) <- colnames(ind)
  return(out)
}

`onion_power_singlenumber` <- function(o,n){
  stopifnot(length(n)==1)
  stopifnot(n == round(n))
  stopifnot(is.onion(o))
  if(n>1){
    return(o*Recall(o,n-1)) 
  } else if(n==0){
    return(1+o*0)
  } else if(n==1){
    return(o)
  } else if(n<0){
    return(Recall(onion_inverse(o),-n))  
  } else {
    stop("this cannot happen")
  }
}

setGeneric("i",function(z){standardGeneric("i")})
setGeneric("j",function(z){standardGeneric("j")})
setGeneric("k",function(z){standardGeneric("k")})
setGeneric("l",function(z){standardGeneric("l")})

setMethod("i","onion",function(z){as.matrix(z)[2,]})
setMethod("j","onion",function(z){as.matrix(z)[3,]})
setMethod("k","onion",function(z){as.matrix(z)[4,]})

setGeneric("l" ,function(z){standardGeneric("l" )})
setGeneric("il",function(z){standardGeneric("il")})
setGeneric("jl",function(z){standardGeneric("jl")})
setGeneric("kl",function(z){standardGeneric("kl")})

setMethod("l" ,"octonion",function(z){as.matrix(z)[5,]})
setMethod("il","octonion",function(z){as.matrix(z)[6,]})
setMethod("jl","octonion",function(z){as.matrix(z)[7,]})
setMethod("kl","octonion",function(z){as.matrix(z)[8,]})

setGeneric("i<-",function(x,...){standardGeneric("i<-")})
setReplaceMethod("i",signature(x="onion"), function(x,value){
  x <-  as.matrix(x)
  x[2,] <- value
  return(as.onion(x))
} )

setGeneric("j<-",function(x,...){standardGeneric("j<-")})
setReplaceMethod("j",signature(x="onion"), function(x,value){
  x <-  as.matrix(x)
  x[3,] <- value
  return(as.onion(x))
} )

setGeneric("k<-",function(x,...){standardGeneric("k<-")})
setReplaceMethod("k",signature(x="onion"), function(x,value){
  x <-  as.matrix(x)
  x[4,] <- value
  return(as.onion(x))
} )

setGeneric("l<-",function(x,...){standardGeneric("l<-")})
setReplaceMethod("l",signature(x="octonion"), function(x,value){
  x <-  as.matrix(x)
  x[5,] <- value
  return(as.onion(x))
} )

setGeneric("il<-",function(x,...){standardGeneric("il<-")})
setReplaceMethod("il",signature(x="octonion"), function(x,value){
  x <-  as.matrix(x)
  x[6,] <- value
  return(as.onion(x))
} )

setGeneric("jl<-",function(x,...){standardGeneric("jl<-")})
setReplaceMethod("jl",signature(x="octonion"), function(x,value){
  x <-  as.matrix(x)
  x[7,] <- value
  return(as.onion(x))
} )

setGeneric("kl<-",function(x,...){standardGeneric("kl<-")})
setReplaceMethod("kl",signature(x="octonion"), function(x,value){
  x <-  as.matrix(x)
  x[8,] <- value
  return(as.onion(x))
} )



