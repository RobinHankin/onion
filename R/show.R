setMethod("show", "quaternion", function(object){quaternion_show(object)})
`quaternion_show` <- function(x){
  out <- as.matrix(x)
  print(out)
  return(invisible(x))
}

setMethod("show", "octonion", function(object){quaternion_show(object)})
`octonion_show` <- function(x){
  out <- as.matrix(x)
  print(out)
  return(invisible(x))
}

