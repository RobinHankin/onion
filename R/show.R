setMethod("show", "onion", function(object){onion_show(object)})

`onion_show` <- function(x,h=getOption("show_onions_horizontally")){
  x <- as.matrix(x)
  if(is.null(colnames(x))){colnames(x) <- paste("[",seq_len(ncol(x)),"]",sep="")}
  if(isTRUE(h)){
    return(invisible(print(t(x))))
  } else {
    return(invisible(print(x)))
  }
}


