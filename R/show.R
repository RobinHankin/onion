setMethod("show", "onion", function(object){onion_show(object)})
`onion_show` <- function(x){
  out <- as.matrix(x)
  print(out)
  return(invisible(x))
}


