setMethod("show", "onion", function(object){invisible(print(onion_show(object)))})

onion_to_string_lowlevel <- function(x,onames){
    if(all(x==0)){return("0")}
    onames[1]  <- ""
    wanted <- x!=0
    x <- x[wanted]
    onames <- onames[wanted]
    sign <- c("+","")[1 + (x<0)]
    out <-  paste(sign,x,onames,sep="",collapse="")
    if(substr(out,1,1)=="+"){out <- substr(out,2,nchar(out))}
    return(out)
}

onion_to_string <- function(x){
    noquote(apply(as.matrix(x),2,onion_to_string_lowlevel,onames=comp_names(x)))
}

comp_names <- function(x){
  if(is.onion(x)){x <- as.character(class(x))}  # otherwise we assume it is a string, e.g. "quat"
  if(is.matrix(x)){
    if(nrow(x)==4){
      return(Recall("quaternion"))
    } else if(nrow(x)==8){
      return(Recall("octonion"))
    } else {
      stop("matrix must have 4 or 8 rows")
    }
  }
    
  switch(x,
         quaternion = c("Re","i","j","k"),
         octonion = c("Re","i","j","k","l","il","jl","kl"),
         stop("currently, only quaternions and octonions have component names")
         )
}

`onion_show` <- function(x,comp=getOption("show_onions_compactly"),h=getOption("show_onions_horizontally")){
  jjnames <- comp_names(x)
  x <- as.matrix(x)

  if(ncol(x)==0){
    if(nrow(x)==4){
      cat("the NULL quaternion\n")
    } else if (nrow(x)==8){
      cat("the NULL octonion\n")
    } else {
      stop("not recognised")
    }
    return(x)
  }

  rownames(x) <- jjnames

  if(isTRUE(comp)){ 
      out <- onion_to_string(x)
  } else {
      if(is.null(colnames(x))){colnames(x) <- paste("[",seq_len(ncol(x)),"]",sep="")}
      if(isTRUE(h)){
          out <- t(x)
      } else {
          out <- x
      }
  }
  return(out)
}
