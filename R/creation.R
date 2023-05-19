`as.onion` <- function(x,type,single=FALSE){
  if(single){return(Recall(x=matrix(x,ncol=1),single=FALSE))}
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
    if(is.onionmat(type)){type <- class(type[1])}
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

`as.quaternion` <- function(x,single=FALSE){
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
    out <- apply(x,3,matrix2quaternion) # list of length-one quaternions
    out <- do.call("c",out)
    return(out)
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
  dimnames(out) <- list(NULL,colnames(out))
  return(new("quaternion",x=out))  # this is the only place new("quaternion",...) is called
}

`quaternion` <- function(length.out=NULL, Re=0, i=0, j=0, k=0){
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
  
  return(as.quaternion(rbind(Re,i,j,k)))
}

`octonion` <- function(length.out=NULL, Re=0, i=0, j=0, k=0, l=0, il=0, jl=0, kl=0){
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
  
  return(as.octonion(rbind(Re,i,j,k,l,il,jl,kl)))
}

"as.octonion" <- function(x, single=FALSE){
  if(is.octonion(x)){return(x)}
  if(is.quaternion(x)){return(as.octonion(rbind(as.matrix(x),as.matrix(x)*0)))}

  if(is.complex(x)){
    if(single){
      stop("single cannot be TRUE with complex x")
    } else {
      return(Recall(rbind(Re(x),Im(x),matrix(0,6,length(x)))))
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
  dimnames(out) <- list(NULL,colnames(out))
  return(new("octonion",x=out))  # this is the only place new("octonion",...) is called
}


