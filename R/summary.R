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

# setMethod("Summary","onion",...)does not work
setMethod("sum","onion",function(x){onion_allsum(x)})
setMethod("prod","quaternion",function(x){quaternion_allprod(x)})
setMethod("prod","octonion",function(x){stop("octonion multiplication is not associative")})
