setMethod("str",signature(object="onion"),function(object,...){str_onion(object,...)})

`str_onion` <- function(object, vec.len=4, ...){
  string <- class(object)
  object <- as.matrix(object)
  if(!is.null(colnames(object))){
    string <- paste("Named",string,sep=" ")
  }
  n <- ncol(object)
  cat(paste(string," [1:",n,"]\n",sep=""))
  l <- min(n,vec.len)
  if(l>0){
      cat(paste(condense(object[,seq_len(l)],as.vector=TRUE),collapse=", ",sep=""))
  }
  if(n > l){
    if(l>0){
      cat(", ")
    }
    cat("...")
    
  }
  cat("\n")
}

"condense" <- function(x,as.vector=FALSE){
  x <- as.matrix(x)
  out <- x
  out[x==0] <- "0"
  out[x> 0] <- "+"
  out[x< 0] <- "-"
  if(as.vector){
    return(noquote(apply(out,2,paste,collapse="")))
  } else {
    return(noquote(out))
  }
}

# setMethod("Summary","onion",...)does not work
setMethod("sum","onion",function(x){onion_allsum(x)})
setMethod("sum","onionmat",function(x){onionmat_allsum(x)})
setMethod("prod","quaternion",function(x){quaternion_allprod(x)})
setMethod("prod","octonion",function(x){stop("octonion multiplication is not associative")})



