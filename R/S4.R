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

setAs("onion", "matrix", function(from){ exp(from@x)} )
setMethod("as.matrix",signature(x="onion"),function(x){as(x,"matrix")})


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

