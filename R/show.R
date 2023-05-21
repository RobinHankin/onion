setMethod("show", "onion", function(object){onion_show(object)})

`.quatnames` <- c("Re","i","j","k")
`.octnames`  <- c("Re","i","j","k","l","il","jl","kl")

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
    noquote(apply(as.matrix(x),2,onion_to_string_lowlevel,onames=rownames(x)))
}

`onion_show` <- function(x,comp=getOption("show_onions_compactly"),h=getOption("show_onions_horizontally")){
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
  if(nrow(x)==4){
    rownames(x) <- .quatnames
  } else if (nrow(x)==8){
    rownames(x) <- .octnames
  } else {
    stop()
  }

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
  return(invisible(print(out)))
}


